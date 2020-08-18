# ----------------------------------------------------------------
# ---- TASK  - KAGGLE INDIA - COVID DATASET
# ---- TASK 1  - INDIA - COVID - IDENTIFY OUTLIERS
# ---- TASK 2  - INDIA - COVID - PLOT OUTLIERS
# ---- TASK 3  - INDIA - MAP
# ---- TASK 4  - INDIA - COVID - PLOT NEW COORDS TS
# ---- TASK 5  - INDIA - COVID - TOTALS - UNIVARIATE OUTLIERS
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# ---- TASK 1  - INDIA - COVID - IDENTIFY OUTLIERS
# ----------------------------------------------------------------
folder <- paste(getwd(), "/Data/Kaggle_Covid_India/", sep="")
#folder <- "C:/Users/Sevvandi/Documents/repos/SharpsFlats/Data/Kaggle_Covid_India/"
dat <- read.csv(paste(folder, "covid_19_india.csv", sep=""))

table(dat$State.UnionTerritory)

# dates have the actual dates
dates <- as.Date(dat$Date, "%d/%m/%y")
ddlen <- length(unique(dates))

st_date <- min(dates)
en_date <- max(dates)


library(dplyr)
dat1 <- dat %>% filter(State.UnionTerritory=='Telangana'| State.UnionTerritory=='Telangana***'|State.UnionTerritory== 'Telengana'|State.UnionTerritory=='Telengana***') %>% mutate(State.UnionTerritory = 'Telangana' )
dat2 <- dat %>% filter(State.UnionTerritory!='Telangana' & State.UnionTerritory!='Telangana***'& State.UnionTerritory!= 'Telengana'& State.UnionTerritory!='Telengana***')

dat3 <- merge(dat1, dat2, all=TRUE)
table(dat3$State.UnionTerritory)

dat4 <- dat3 %>% filter(State.UnionTerritory!='Cases being reassigned to states')

inds <- which(dat4$State.UnionTerritory=='Dadar Nagar Haveli' |dat4$State.UnionTerritory=='Daman and Diu'|dat4$State.UnionTerritory =='Daman & Diu')
dat4$State.UnionTerritory[inds] <- 'Dadra and Nagar Haveli and Daman and Diu'

ll <- length(unique(dat4$State.UnionTerritory))
df <- matrix(0, nrow=ddlen, ncol=ll)
col_id_state <- colnames(df) <- unique(dat4$State.UnionTerritory)

dat <- dat4
unique(dat$State.UnionTerritory)
k <- 1
dates <- as.Date(dat$Date, "%d/%m/%y")
for(i in st_date:en_date){
  inds <- which(dates==i)
  subdat <- dat[inds, ]

  for(jj in 1:dim(subdat)[1]){
    cols <- which(col_id_state %in% subdat$State.UnionTerritory[jj])
    df[k, cols] <- subdat$Confirmed[jj]
  }
  k <- k+1

}

df1 <- df/rowSums(df)
zero_sums <- apply(df1, 1, function(x) sum(x==0))
stt <- min(which(zero_sums<10))
stt <- 1
df2 <- df1[stt:dim(df1)[1], ]
uniq_dates <- unique(dates)[stt:dim(df1)[1]]

# CHANGE COORDINATES
coord_obj <- composits::get_coords(df2)
coord_obj$vec[ ,1]

# DO OUTLIER ENSEMBLE
out1 <- composits::comp_tsout_ens(df2, fast=FALSE, compr=2, rat=0.1)  #
out2 <- out1

saveRDS(out2, paste(folder, "../../Data_Output/India_Ensemble_Output.rds", sep=""))
# out1 <- readRDS("Data_Output/India_Ensemble_Output.rds")
# out2 <- out1
# ----------------------------------------------------------------
# ---- TASK 2  - INDIA - COVID - PLOT OUTLIERS
# ----------------------------------------------------------------
out1$outliers$Total_Score
out1$outliers$Indices <- uniq_dates[out1$outliers$Indices]
composits::draw_table(out1)

# Apportion scores to states
apportioned <- composits::apportion_scores_comp(out2)
ppp <- apportioned$scores_out

rownames(ppp) <- colnames(df1)
colnames(ppp) <- as.factor(uniq_dates[out1$outliers$Indices])
write.csv(ppp, paste(folder, "../../Data_Output/India_Apportioned_Scores_for_most_outlying.csv", sep=""))
ppp

# ----------------------------------------------------------------
# ---- TASK 3  - INDIA - MAP
# ----------------------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(rnaturalearthdata)
library(ggplot2)

ppp2 <- cbind.data.frame(rownames(ppp), ppp)
colnames(ppp2)[1] <- "name"
d <-  ppp2 %>%
  mutate(name = str_replace(name, " Island", "")) %>%
  mutate(name = str_replace(name, "Odisha", "Orissa")) %>%
  pivot_longer(-name, names_to = "date", values_to = "score")

india <- rnaturalearth::ne_states(country = "india", returnclass = "sf") %>%
  mutate(name = woe_name) %>%
  mutate(name = if_else(name %in% c("Daman and Diu", "Dadra and Nagar Haveli"), "Dadra and Nagar Haveli and Daman and Diu", name)) %>%
  inner_join( d, by="name")

india %>%
  ggplot() +
  geom_sf(aes(fill=score)) +
  theme_bw() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  facet_wrap(~date)



# ----------------------------------------------------------------
# ---- TASK 4  - INDIA - COVID - PLOT NEW COORDS TS
# ----------------------------------------------------------------
library("tseries")
library("ggplot2")
library(tidyr)
new_coords <- coord_obj$y
colnames(new_coords) <- paste("V", 1:dim(new_coords)[2], sep="")
plot(new_coords[ ,3])
df1 <- ts(new_coords, start=min(uniq_dates))
plot.ts(df1[ ,1:10], yax.flip = TRUE)
plot(df1, plot.type = "single", lty = 1:3, col = 1:6)

df2 <- cbind.data.frame(uniq_dates, df1)
colnames(df2) <- c("Date", paste("X", 1:dim(df1)[2], sep=""))
df2_2 <- df2[ ,c(1, 8, 11, 17, 29, 31, 34)] # 2:7  8:13  14:19 20:25  26:31  (X7 = 8, X10=11, X28, X30, x33 , X16 = 17)   8, 11, 17, 29, 31, 34
df3 <- pivot_longer(df2_2, cols=2:7)
colnames(df3)[2] <- 'Coord'

ggplot(df3, aes(Date, value))  + geom_line() + geom_vline(xintercept=as.Date('2020-03-04'), linetype = "dashed") + facet_wrap(~Coord, scales="free") + ylab("Null space coordinates")  + theme_bw()

x_sub <- df2[ ,c(1, 2, 3, 4, 5, 8, 10, 15, 27)] # 2:7  8:13  14:19 20:25  26:31  (X7 = 8, X10=11, X28, X30, x33 , X16 = 17)   8, 11, 17, 29, 31, 34
x_sub <- pivot_longer(x_sub, cols=2:9, names_to = "Coord") %>%
  mutate(Coord = factor(Coord, levels=c("X1","X2","X3","X4","X7","X9","X14","X26")))
ggplot(x_sub, aes(Date, value))  + geom_line() + geom_vline(xintercept=as.Date('2020-03-04'), linetype = "dashed") + facet_wrap(~Coord, scales="free", nrow=2) + ylab("Null space coordinates")  + theme_bw()


plot_decomposed_all(obj=out2, X = df2)

plot_biplot(out2, edges = "all")
plot_biplot(out2, method="dobin", edges = "all")
plot_biplot(out2, method="ica", edges = "all")
# plot_biplot(out2, method="ics", edges = "all")
# tour is not that useful here (and automatic scale setting isn't working well)
#library(animation)
#old_wd <- getwd()
#setwd(paste0(old_wd,"/gif"))
set.seed(12082020)
#saveGIF(
#  animate_ts_ensemble(out2, max_frames = 200, edges = "all"),
#  movie.name = "india.gif", interval=0.1)
#setwd(old_wd)
animate_ts_ensemble(out2, edges = "all")


# ----------------------------------------------------------------
# ---- TASK 5  - INDIA - COVID - TOTALS - UNIVARIATE OUTLIERS
# ----------------------------------------------------------------
tot_dat <- apply(df, 1, sum)
df_tot <- ts(tot_dat, start=as.Date(min(dates)))
plot.ts(df_tot, yax.flip = TRUE)
out <- composits::uv_tsout_ens(log(df_tot))
days <- seq(min(dates), max(dates), by=1)

df4 <- cbind.data.frame(days, log(df_tot))
colnames(df4) <- c("Day", "Log_Total")

outsum <- apply(out$outmat, 1, sum)
outliers <- which(outsum >2 )
days[outliers]

ggplot(df4, aes(Day, Log_Total)) + geom_line(size=0.8) + geom_vline(xintercept=days[outliers], linetype = "dashed")+  ylab("Log Total")  + theme_bw()
