library(tidyverse)

# CHANGE DIRECTORY.
setwd("C:/Users/rloa0001/Dropbox/SharpsFlats")
source("R/Simulation_function.R")

set.seed(2000)
N <- 30
K <- 2
TT <- 500
A <- matrix(rnorm(N*K, 0, 0.3),N,K)
B <- matrix(c(0.8,0,0,0.5),K,K)
C <- matrix(c(5,0,0,4),K,K)
mu <- c(0.3, 0.7)
D <- matrix(c(0.4,0,0,0.4),K,K)
outliers_discre <- matrix(c(117, 2, 10, 40, 8, 200),2,3,byrow =T)

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
test = simobj$datasim%>%
        as_tibble()
names(test) <- paste0("V", c(1:N), sep = "")


test %>%
  gather(Group, y)     %>%
  group_by(Group)     %>%
  mutate(x = 1:n())   %>%
  ungroup()           %>%
  select(Group, x, y) %>%
  ggplot(aes(x = x, y = y, color = Group)) +
  geom_line() +
  geom_vline(xintercept=simobj$outliers_persist[,1],
             linetype="dotted") +
  geom_vline(xintercept=simobj$outliers_discre[,1],
             linetype="dashed") +
  labs(title="Simulated compositional data", x="Time", y="Proportion")+
  theme_bw() +
  theme(legend.position = "none")




