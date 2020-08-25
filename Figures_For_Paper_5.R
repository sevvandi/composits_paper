########################
# Simulating data
########################
library(tidyverse)
library(composits)
library(pROC)


start_time <- Sys.time()
n       = 1000
out1 = out2 = list()

rocval = numeric(n)

for (i in 1:n){

  set.seed(2000+i-1)
  N <- 30
  K <- 2
  TT <- 500
  A <- matrix(rnorm(N*K, 0, 0.3), N, K)
  B <- matrix(c(0.8,0,0,0.5), K, K)
  C <- matrix(c(5,0,0,4), K, K)
  mu <- c(0.3, 0.7)
  D <- matrix(c(0.4,0,0,0.4), K,K)
  outliers_discre <- matrix(c(117, 2, 10, 40, 8, 200), 2, 3, byrow = TRUE)

  simobj =  Simulations(N = N,
                     TT = TT,
                     K = K,
                     A = A,
                     B = B,
                     C = C,
                     mu = mu,
                     D = D,
                     outliers_discre = outliers_discre,
                     q = 0.005)

  sim <- simobj$datasim
  out2[[i]] <- simobj$outliers_timeloc
  outliers_true = matrix(0,dim(sim)[1], 1)
  outliers_true[simobj$outliers_timeloc] = 1

  new_coords_out <- get_coords((sim))  # Check transpose
  new_coords <- new_coords_out$y
  colnames(new_coords) <- paste("V", 1:ncol(new_coords), sep = "")


  out1[[i]] <- composits::mv_tsout_ens(new_coords,
                                        m1 = NULL,
                                        ncomp = 2,
                                        sds = 1,
                                        rept = 1,
                                        compr = 1,
                                        rat = 0.05,
                                        fast = FALSE)

  outliers_ensemble <- matrix(0,dim(sim)[1], 1)
  nout <- dim(out1[[i]]$all)[1]

  for (j in nout:2){
       if (out1[[i]]$all[j,'Indices'] != (out1[[i]]$all[j-1,'Indices'] + 1) ||
           out1[[i]]$all[j,'Total_Score'] > (out1[[i]]$all[j-1,'Total_Score'])){
         outliers_ensemble[out1[[i]]$all[j, 'Indices']] = 1
       }
  }

  temp = roc(as.numeric(outliers_true), as.numeric(outliers_ensemble))
  rocval[i] = as.numeric(gsub("[^0-9.-]", "", temp$auc))

  print(i)

}

end_time <- Sys.time()

ROC = tibble(auc = rocval)

ggplot(data = ROC, aes(x = auc)) +
  geom_histogram(breaks = seq(0.7, 1, by = 0.01)) + 
  theme_bw() +
  labs(title="Histogram for AUC", x="AUC", y="Count")


#save(out1,out2,rocval,file = paste0('wd_simulation_n1000.rda'))

