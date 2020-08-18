# ----------------------------------------------------------------
# ---- TASK  - THE WORLD BANK DATA - INTERNATIONAL ARRIVALS
# ---- TASK 1  - INTERNATIONAL ARRIVALS - IDENTIFY OUTLIERS
# ---- TASK 2  - INTERNATIONAL ARRIVALS - PLOT OUTLIERS
# ---- TASK 3  - APPORTION SCORES TO REGIONS
# ---- TASK 4  - INTERNATIONAL ARRIVALS - PLOT NEW COORDS TS
# ---- TASK 5  - UNIVARIATE OUTLIERS IN TOTAL
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# ---- TASK 1  - INTERNATIONAL ARRIVALS - IDENTIFY OUTLIERS
# ----------------------------------------------------------------
library(composits)
folder <- paste(getwd(), "/Data/World_Bank_Dataset_1/", sep="")
dat <- read.csv(paste(folder, "International_Arrivals.csv", sep=""))
colnames(dat)
dat[ ,1]
dat2 <- dat[ ,-c(3,4,5:39, dim(dat)[2])]

East_Asia_Pacific_ind <- which(dat2[ ,1] =="East Asia & Pacific")
Europe_Central_Asia_ind <- which(dat2[ ,1] =="Europe & Central Asia")
Latin_America_Caribbean_ind <- which(dat2[ ,1] =="Latin America & Caribbean")
Middle_East_North_Africa_ind <- which(dat2[ ,1] =="Middle East & North Africa")
Sub_Saharan_Africa_ind <- which(dat2[ ,1] =="Sub-Saharan Africa")
South_Asia_ind <- which(dat2[ ,1] =="South Asia")
North_America_ind <- which(dat2[ ,1] =="North America")


inds <- c(East_Asia_Pacific_ind,Europe_Central_Asia_ind,  Latin_America_Caribbean_ind, Middle_East_North_Africa_ind, Sub_Saharan_Africa_ind, South_Asia_ind, North_America_ind)
dat3 <- dat2[inds, ]
dat4 <- dat3[ ,c(3:(dim(dat3)[2]-1))]
dat5 <- apply(dat4, 2, function(x) x/sum(x))

df <- t(dat5)
colnames(df) <- paste("V", 1:dim(df)[2], sep="")

# Null space coordinate transformation
nsp <- get_coords(df)
nsp$vec

# DO OUTLIER ENSEMBLE
out1 <- composits::comp_tsout_ens(df,fast=FALSE, compr=2, rat=0.1)
out2 <- out1
out <- out1$outliers
years <- rownames(df)

saveRDS(out2, paste('Data_Output/Tourism_Ensemble_Output.rds'))

# ----------------------------------------------------------------
# ---- TASK 2  - INTERNATIONAL ARRIVALS - PLOT OUTLIERS
# ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

out1$outliers$Total_Score
out1$outliers$Indices <- years[out1$outliers$Indices]
composits::draw_table(out1)


# ----------------------------------------------------------------
# ---- TASK 3  - APPORTION SCORES TO REGIONS
# ----------------------------------------------------------------
# Apportion scores to states
apportioned <- composits::apportion_scores_comp(out2)
ppp <- apportioned$scores_out
rownames(ppp) <-  c("East_Asia_&_Pacific", "Europe_&_Central_Asia", "Latin_America_&_Carribbean", "Middle_East_&_North_Africa", "Sub_Saharan_Africa", "South_Asia", "North_America")
colnames(ppp) <- out1$outliers$Indices
ppp
# write.csv(ppp, paste(getwd(), "/Data_Output/Tourism_Apportioned_Scores_for_most_outlying.csv", sep=""))


# ----------------------------------------------------------------
# ---- TASK 4  - INTERNATIONAL ARRIVALS - PLOT NEW COORDS TS
# ----------------------------------------------------------------
# Original data
df2 <- t(dat4)
colnames(df2) <- c("East_Asia_&_Pacific", "Europe_&_Central_Asia", "Latin_America_&_Carribbean", "Middle_East_&_North_Africa", "Sub_Saharan_Africa", "South_Asia", "North_America")
as_tibble(df2) %>% mutate(t = 1:n())   %>%
  pivot_longer(cols=1:7)  %>%
  ggplot2::ggplot( ggplot2::aes(x = t, y = value, color = name)) +  ggplot2::geom_line(size=0.9) + scale_x_discrete(name ="Year", limits=as.factor(c(1995:2018))) + ylab("International Arrivals Total") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot_decomposed_all(obj=out2, X = df)
new_coords <- nsp$y

plot_biplot(out2, edges = "all")
plot_biplot(out2, method="dobin", edges = "all")
plot_biplot(out2, method = "ics", edges = "all")
plot_biplot(out2, method = "ica", edges = "all")


# but the tour gives a nice overview! (commented code is for saving as gif)
#library(animation)
#old_wd <- getwd()
#setwd(paste0(old_wd,"/gif"))
set.seed(15082020)
#saveGIF(
#  animate_ts_ensemble(out2,  max_frames = 200, edges = "all"),
#  movie.name = "tourism.gif", interval=0.1)
#setwd(old_wd)
animate_ts_ensemble(out2, edges = "all")



library("tseries")
library("ggplot2")
library(tidyr)
df1 <- ts(new_coords, start=1995)
plot(df1, plot.type = "single", lty = 1:3, col = 1:6)

df2 <- cbind.data.frame(1995:2017, df1)
colnames(df2) <- c("Year", paste("X", 1:6, sep=""))
df3 <- pivot_longer(df2, cols=2:7)
colnames(df3)[2] <- 'Coord'

ggplot(df3, aes(Year, value))  + geom_line() + geom_vline(xintercept=2003, linetype = "dashed") + facet_wrap(~Coord, scales="free") + ylab("Null space coordinates")  + theme_bw()


# ----------------------------------------------------------------
# ---- TASK 5  - UNIVARIATE OUTLIERS IN TOTAL
# ----------------------------------------------------------------
tot_dat <- apply(dat4, 2, sum)
df_tot <- ts(tot_dat, start=1995)
plot.ts(df_tot, yax.flip = TRUE)
out <- composits::uv_tsout_ens(df_tot)
years <- 1995:2017

df4 <- cbind.data.frame(years, df_tot)
colnames(df4) <- c("Year", "Total")

outliers <- which(apply(out$outmat, 1, sum) >0 )
years[outliers]

ggplot(df4, aes(Year, Total))  + geom_line() + geom_vline(xintercept=years[outliers], linetype = "dashed")+  scale_x_continuous(breaks=c(1995, years[outliers])) +  ylab("Total Arrivals")  + theme_bw()
