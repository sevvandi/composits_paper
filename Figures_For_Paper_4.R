# ----------------------------------------------------------------
# ---- TASK  - SPANISH - COVID DATASET
# ---- TASK 1  - SPANISH - COVID - IDENTIFY OUTLIERS
# ---- TASK 2  - SPANISH - COVID - PLOT OUTLIERS TABLE
# ---- TASK 3 - APPORTION SCORES AND DRAW MAP OF SPAIN
# ---- TASK 4  - SPANISH - COVID - PLOT NEW COORDS TS
# ---- TASK 5  - SPAIN - COVID - TOTALS - UNIVARIATE OUTLIERS
# ----------------------------------------------------------------

# ----------------------------------------------------------------
# ---- TASK 1  - SPANISH - COVID - IDENTIFY OUTLIERS
# ----------------------------------------------------------------

folder <- paste(getwd(), "/Data/Spanish_Data/", sep="")
library(composits)
#folder <- "C:/Users/Sevvandi/Documents/repos/SharpsFlats/Data/Spanish_Data/"

dat <- read.csv(paste(folder, "deads_spain_ccaa_clean.csv", sep=""))

table(dat$State)

# dates have the actual dates
dates <- as.Date(dat$date) #, "%yyyy-%mm-%dd")
ddlen <- length(unique(dates))


st_date <- min(dates)
en_date <- max(dates)


ll <- length(unique(dat$State))
df <- matrix(0, nrow=ddlen, ncol=ll)
col_id_state <- colnames(df) <- unique(dat$State)

k <- 1
for(i in st_date:en_date){
  inds <- which(dates==i)
  subdat <- dat[inds, ]

  for(jj in 1:dim(subdat)[1]){
    cols <- which(col_id_state %in% subdat$State[jj])
    df[k, cols] <- subdat$deads[jj]
  }
  k <- k+1

}

df1 <- df/rowSums(df)
stt <- 1
df2 <- df1[stt:dim(df1)[1], ]
uniq_dates <- unique(dates)[stt:dim(df1)[1]]

# CHANGE COORDINATES
coord_obj <- composits::get_coords(df2)
coord_obj$vec[ ,1]

# DO OUTLIER ENSEMBLE
out1 <- composits::comp_tsout_ens(df2, ncomp=2, compr=2, fast=FALSE, rat=0.1)
out2 <- out1
saveRDS(out2, "Data_Output/Spain_Ensemble_Output.rds")
# out1 <- readRDS("Data_Output/Spain_Ensemble_Output.rds")
# out2 <- out1
# ----------------------------------------------------------------
# ---- TASK 2  - SPANISH - COVID - PLOT OUTLIERS TABLE
# ----------------------------------------------------------------

out1$outliers$Total_Score
out1$outliers$Indices <- uniq_dates[out1$outliers$Indices]
composits::draw_table(out1)



# ----------------------------------------------------------------
# ---- TASK 3 - APPORTION SCORES AND DRAW MAP OF SPAIN
# ----------------------------------------------------------------
# Apportion scores to states
apportioned <- apportion_scores_comp(out2)
ppp <- apportioned$scores_out

rownames(ppp) <- colnames(df1)
colnames(ppp) <- as.factor(uniq_dates[out2$outliers$Indices])

write.csv(ppp, paste(folder, "../../Data_Output/Spanish_Apportioned_Scores_for_most_outlying.csv", sep=""))

selected <- order(out2$outliers$Total_Score, decreasing=TRUE)[1:3]

sts <- c()
for(kkk in 1:length(rownames(ppp))){
  sts <- c(sts, paste(dat$State_short_name[min(which(dat$State==rownames(ppp)[kkk])) ] ) )
}

pp2 <- cbind.data.frame(as.vector(sts), round(ppp[ ,selected],3))
pp2

# DRAW MAP OF SPAIN WITH APPORTIONED SCORES
library(rnaturalearth)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(rnaturalearthdata)
library(ggplot2)

ppp2 <- cbind.data.frame(rownames(ppp), ppp[ ,selected])
colnames(ppp2)[1] <- "reg"
# taking inspiration from
# https://stackoverflow.com/questions/54279277/how-to-obtain-maps-with-regions-and-subregions
# https://github.com/aaumaitre/maps_Spain

# region names don't match exactly between data file and map
# renaming them according to ne codes

d <-   ppp2 %>%
  mutate(reg = str_replace(reg, "RI", "LO")) %>%
  mutate(reg = str_replace(reg, "NC", "NA")) %>%
  mutate(reg = str_replace(reg, "MC", "MU")) %>%
  pivot_longer(-reg, names_to = "date", values_to = "score")

# get map of spain and reshape for drawing the regions
spain <- rnaturalearth::ne_states(country = "spain", returnclass = "sf")
sp_regions <- spain %>%
  mutate(reg = str_sub(code_hasc, 4, 5)) %>%
  group_by(reg) %>%
  summarise()

# combine map info with data
# XXX dropping Canary islands for now
sp_data <- inner_join(sp_regions, d, by="reg") %>%
  filter(reg != "CN")

#template for drawing map
sp_data %>%
  ggplot() +
  geom_sf(aes(fill=score, text=reg)) +
  theme_bw() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  facet_wrap(~date)



# ----------------------------------------------------------------
# ---- TASK 4  - SPANISH - COVID - PLOT NEW COORDS TS
# ----------------------------------------------------------------
library(tseries)
library(ggplot2)
library(tidyr)

# Original coordinates
# df2 <- cbind.data.frame(uniq_dates, df2)
as_tibble(df2) %>%
  pivot_longer(cols=2:20)  %>%
  ggplot2::ggplot( ggplot2::aes(x = uniq_dates, y = value, color = name)) +  ggplot2::geom_line() + xlab("Time") + ylab("Proportion")+  ggplot2::theme_bw()


new_coords <- coord_obj$y
colnames(new_coords) <- paste("V", 1:dim(new_coords)[2], sep="")



df_new <- cbind.data.frame(uniq_dates, new_coords)
colnames(df_new) <- c("Date", paste("X", 1:dim(new_coords)[2], sep=""))
df3 <- pivot_longer(df_new, cols=2:19)
colnames(df3)[2] <- 'Coord'

ggplot(df3, aes(Date, value)) + geom_line() + geom_vline(xintercept=as.Date('2020-03-19'), linetype = "dashed") + facet_wrap(~Coord, scales="free") + ylab("Null space coordinates")  + theme_bw()


plot_decomposed_all(out2)

plot_biplot(out2, edges = "outlying")
plot_biplot(out2, method="dobin", edges = "outlying")
plot_biplot(out2, method="ica", edges="outlying")
plot_biplot(out2, method="ics", edges = "outlying")

#library(animation)
#old_wd <- getwd()
#setwd(paste0(old_wd,"/gif"))
set.seed(12082020)
#saveGIF(
#  animate_ts_ensemble(out2, max_frames = 200, edges = "outlying"),
#  movie.name = "spain.gif", interval=0.1)
#setwd(old_wd)
animate_ts_ensemble(out2, edges = "outlying")

# ----------------------------------------------------------------
# ---- TASK 5  - SPAIN - COVID - TOTALS - UNIVARIATE OUTLIERS
# ----------------------------------------------------------------
tot_dat <- apply(df, 1, sum)
df_tot <- ts(tot_dat, start=as.Date(min(dates)))
plot.ts(df_tot, yax.flip = TRUE)
out <- composits::uv_tsout_ens(df_tot)
days <- seq(min(dates), max(dates), by=1)

df4 <- cbind.data.frame(days, df_tot)
colnames(df4) <- c("Day", "Total")

outsum <- apply(out$outmat, 1, sum)
outliers <- which(outsum >2 )
days[outliers]
# [1] "2020-03-21" "2020-03-22" "2020-03-23" "2020-03-24" "2020-03-25" "2020-03-26"
# [7] "2020-03-27" "2020-03-28"


ggplot(df4, aes(Day, Total)) + geom_line() + geom_vline(xintercept=days[outliers], linetype = "dashed")+  ylab("Total")  + theme_bw()
