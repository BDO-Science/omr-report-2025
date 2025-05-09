library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)
library(sharpshootR)
###########################
#Set up dataframe
WY = year(Sys.Date())
wrJPE = 234896
threshold = wrJPE * .0117
base_df_WR<- data.frame(Date=seq(as.Date(paste0(WY-1,'-10-01')), as.Date(paste0(WY,'-07-01')), by = 'days'),
                        WR_Cumulative_50=round(threshold * .5,0), 
                        WR_Cumulative_75=round(threshold * .75,0),
                        WR_Cumulative_90=round(threshold * .9,0),
                        WR_Cumulative_100=round(threshold, 0), 
                        WR_ITL = 4698)


##################### Load Salvage Count Data from SacPAS
salvage_data <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=no&age=no')


###########################

# Write csv for the OMR report appendix
write.csv(salvage_data %>% 
            filter(as.Date(Sample.Time)>=as.Date(paste0(WY-1,'-10-01'))),
          file="Salmonids/output/WY2023_ChinookSalmonSalvageData.csv", row.names = F)

#Calculate Cumulative Loss
wr_salvage_data<-salvage_data %>% 
  mutate(Date=as.Date(Sample.Time)) %>% 
  filter(LAD.Race=="Winter", Date >= as.Date(paste0(WY-1,'-10-01'))) %>% 
  group_by(Date) %>%
  summarise(Loss=sum(Loss))

wr_salvage_data_cumulative<-left_join(base_df_WR,wr_salvage_data)
wr_salvage_data_cumulative[is.na(wr_salvage_data_cumulative)] <- 0
wr_salvage_data_cumulative <- wr_salvage_data_cumulative[order(wr_salvage_data_cumulative$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))

at <- seq(from = min(wr_salvage_data_cumulative$Date), to = max(wr_salvage_data_cumulative$Date), by = "2 months")


###### Figure for Total natural winter-run Chinook Salmon (LAD) loss for the Water Year

#Print figure
tiff(filename=file.path("Salmonids","output","Figure_WY_Cumulative_loss_Winter-run.tiff"),width=10,height = 6, units = "in",  res=300, compression ="lzw")

par(mar = c(8.1, 4.1, 4.1, 4.1))

plot(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$CumulativeLoss, ylim = c(0, 5000), xaxt = "n",col = "green", type = "l", lty = 1, lwd= 2,main="Natural Winter-Run", xlab = "", ylab = "Loss (n)")
axis(1, at=at, format(at, "%b %d %Y"), cex.axis = .7)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$WR_ITL, col = "black", type = "l", lty = 1)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$WR_Cumulative_100, col = "black", type = "l", lty = 2)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$WR_Cumulative_90, col = "grey4", type = "l", lty = 3)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$WR_Cumulative_75, col = "grey2", type = "l", lty = 4)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$WR_Cumulative_50, col = "grey1", type = "l", lty = 5)

legend("bottomleft", legend = c("Cumulative Loss", "50% Annual Loss Threshold", "75% Annual Loss Threshold","90% Annual Loss Threshold","100% Annual Loss Threshold", "Single Year ITL"), inset=c(0,-0.40),
       col=c("green","black","black","black","black", "black"), lty=c(1,5,4,3,2,1), lwd=c(2,1,1,1,1,1),bty = "n", xpd=TRUE, mar(c(7,7,11,7)), cex = 0.8)

dev.off()


###### Figure for Cumulative Total loss of natural winter-run Chinook Salmon (LAD) for the 10 yr period

base_df_WR<- data.frame(Date=seq(as.Date('2020-02-19'), as.Date(paste0(WY,'-07-01')), by = 'days'), CumBO_Loss=8738
)

deg45_line<- data.frame(Date=as.Date(c('2022-10-01','2023-10-01','2024-10-01','2025-10-01','2026-10-01','2027-10-01','2028-10-01','2029-10-01')),
                        WR_45deg=c(1259.66,2327.99,3396.32,4464.65,5532.98,6601.31,7669.64,8738)
)


#Cumulative Loss
wr_salvage_data<-salvage_data %>% mutate(Date=as.Date(Sample.Time)) %>% filter(Date>="2020-02-19") %>% filter(LAD.Race=="Winter") %>% group_by(Date) %>%
  summarise(Loss=sum(Loss))

wr_salvage_data_cumulative<-left_join(base_df_WR,wr_salvage_data)

wr_salvage_data_cumulative$Loss[is.na(wr_salvage_data_cumulative$Loss)] <- 0

wr_salvage_data_cumulative <- wr_salvage_data_cumulative[order(wr_salvage_data_cumulative$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))
wr_salvage_data_cumulative<-bind_rows(wr_salvage_data_cumulative,deg45_line)
wr_salvage_data_cumulative$CumBO_Loss=8738

at <- seq(from = as.Date('2021-01-01'), to = as.Date('2029-01-01'), by = "1 year")

#Print figure
tiff(filename=file.path("Salmonids","output","Figure_10yr_Cumulative_loss_Winter-run_threshold.tiff"),width=8,height = 6, units = "in",  res=300, compression ="lzw")

par(mar = c(8.1, 4.1, 4.1, 4.1))
plot(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$CumulativeLoss, ylim = c(0, 8800), xaxt = "n",col = "green", type = "l", lty = 1, lwd= 2,main="Natural Winter-Run", xlab = "Year", ylab = "Loss (n)")
axis(1, at=at, format(at, "%Y"), cex.axis = .7)
lines(wr_salvage_data_cumulative$Date, wr_salvage_data_cumulative$CumBO_Loss, col = "black", type = "l", lty = 1)
par(new=TRUE)
plot(deg45_line$Date, deg45_line$deg45_line, col = "blue", type = "l", lty = 2, xaxt = "n", yaxt="n", xlab=NA,ylab=NA)
legend("bottomleft", legend = c("Cumulative Loss", "Total Loss allowed for life of PA", "Hypothetical Uniform Loss"), inset=c(0,-0.4),
       col=c("green","black","blue"), lty=c(1,1,2), lwd=c(2,1,1),bty = "n", xpd=TRUE, mar(c(7,7,11,7)), cex = 0.8)

dev.off()

#######################################################
## STEELHEAD
#######################################################

#Acquire steelhead loss data

sth_salvage_data <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no')

# Write csv for the OMR report appendix
write.csv(sth_salvage_data %>% 
            filter(as.Date(Sample.Time)>=as.Date(paste0(WY-1,'-10-01'))),
          file=file.path("Salmonids", "output","WY2023_SteelheadSalvageData.csv"), row.names = F)


#Cumulative Loss
wy_salvage_data<-sth_salvage_data %>% mutate(Date=as.Date(Sample.Time)) %>% filter(Date>=as.Date(paste0(WY-1,'-10-01'))) %>% group_by(Date) %>%
  summarise(Loss=sum(Loss))
#Natural Central Valley Steelhead from December through March (loss =1,414)
#Natural Central Valley Steelhead from April through June 15 (loss = 1,552)

base_df_STH_Dec_March<- data.frame(Date=seq(as.Date(paste0(WY-1,'-10-01')), as.Date(paste0(WY,'-03-31')), by = 'days'),
                                   STH_Annual_50=1414*0.5, STH_Annual_75=1414*0.75,STH_Annual_90=1414*0.9,STH_Annual_100=1414,STH_Annual_ITL=2760
)

base_df_STH_Apr_Jun<- data.frame(Date=seq(as.Date(paste0(WY,'-04-01')), as.Date(paste0(WY,'-06-15')), by = 'days'),
                                 STH_Annual_50=1552*0.5, STH_Annual_75=1552*0.75,STH_Annual_90=1552*0.9,STH_Annual_100=1552,STH_Annual_ITL=3040
)


sth_salvage_data_cumulative_Dec_March<-left_join(base_df_STH_Dec_March,wy_salvage_data)
sth_salvage_data_cumulative_Dec_March$Loss[is.na(sth_salvage_data_cumulative_Dec_March$Loss)] <- 0
sth_salvage_data_cumulative_Dec_March <- sth_salvage_data_cumulative_Dec_March[order(sth_salvage_data_cumulative_Dec_March$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))

sth_salvage_data_cumulative_Apr_Jun<-left_join(base_df_STH_Apr_Jun,wy_salvage_data)
sth_salvage_data_cumulative_Apr_Jun$Loss[is.na(sth_salvage_data_cumulative_Apr_Jun$Loss)] <- 0
sth_salvage_data_cumulative_Apr_Jun <- sth_salvage_data_cumulative_Apr_Jun[order(sth_salvage_data_cumulative_Apr_Jun$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))

sth_salvage_data_cumulative<-rbind(sth_salvage_data_cumulative_Dec_March,sth_salvage_data_cumulative_Apr_Jun)


#Total wild steelhead loss for WY 
at <- seq(from = min(sth_salvage_data_cumulative$Date), to = max(sth_salvage_data_cumulative$Date), by = "2 months")


#Print figure
tiff(filename=file.path("Salmonids","output","Figure_WY_Cumulative_loss_Steelhead.tiff"),width=10,height = 6.5, units = "in",  res=300, compression ="lzw")

par(mar = c(8.1, 4.1, 4.1, 3.1))

plot(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$CumulativeLoss, ylim = c(0, 3500), xaxt = "n",col = "green", type = "l", lty = 1, lwd= 2,main="Natural Steelhead", xlab = "", ylab = "Loss (n)")
axis(1, at=at, format(at, "%b %d %Y"), cex.axis = .7)
lines(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$STH_Annual_ITL, col = "black", type = "l", lty = 1)
lines(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$STH_Annual_100, col = "black", type = "l", lty = 2)
lines(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$STH_Annual_90, col = "grey4", type = "l", lty = 3)
lines(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$STH_Annual_75, col = "grey2", type = "l", lty = 4)
lines(sth_salvage_data_cumulative$Date, sth_salvage_data_cumulative$STH_Annual_50, col = "grey1", type = "l", lty = 5)
abline(v=as.Date(paste0(WY,'-03-31')), col="red",lwd=3)
legend("bottomleft", legend = c("Annual Cumulative Loss", "50% Annual Loss Threshold", "75% Annual Loss Threshold","90% Annual Loss Threshold","100% Annual Loss Threshold", "Single Year ITL"), inset=c(0,-0.40),
       col=c("green","black","black","black","black","black"), lty=c(1,5,4,3,2,1), lwd=c(2,1,1,1,1,1),bty = "n", xpd=TRUE, mar(c(7,7,11,7)), cex = 0.8)

dev.off()


##########################
#Figure for 10-year cumulative loss for steelhead


#Dec_march
base_df_STH_Dec_March<- data.frame(Date=seq(as.Date('2020-02-19'), as.Date(paste0(WY,'-07-01')), by = 'days')
)


deg45_line_Dec_March<- data.frame(Date=as.Date(c('2020-02-19','2022-10-01','2023-10-01','2024-10-01','2025-10-01','2026-10-01','2027-10-01','2028-10-01','2029-10-01')),
                                  WR_45deg=c(0,1811.8,2415.6,3019.4,3623.2,4227.0,4830.8,5434.6,6038.4)
)

#Cumulative Loss
sth_dec_mar<-sth_salvage_data %>% mutate(Date=as.Date(Sample.Time)) %>% filter(Date>="2020-02-19") %>% group_by(Date) %>%
  summarise(Loss=sum(Loss)) %>% filter(month(Date) %in% c(10,11,12,1,2,3))
sth_salvage_data_cumulative_Dec_March<-left_join(base_df_STH_Dec_March,sth_dec_mar)
sth_salvage_data_cumulative_Dec_March$Loss[is.na(sth_salvage_data_cumulative_Dec_March$Loss)] <- 0
sth_salvage_data_cumulative_Dec_March <- sth_salvage_data_cumulative_Dec_March[order(sth_salvage_data_cumulative_Dec_March$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))
sth_salvage_data_cumulative_Dec_March<-bind_rows(sth_salvage_data_cumulative_Dec_March,deg45_line_Dec_March)
sth_salvage_data_cumulative_Dec_March$CumBO_Loss=6038

at1 <- seq(from = as.Date("2021-01-01"), to =as.Date("2029-01-01"), by = "1 year")

###############
#April-June
base_df_STH_Apr_Jun<- data.frame(Date=seq(as.Date('2020-02-19'), as.Date(paste0(WY,'-07-01')), by = 'days')
)


deg45_line_Apr_Jun<- data.frame(Date=as.Date(c('2020-02-19','2022-10-01','2023-10-01','2024-10-01','2025-10-01','2026-10-01','2027-10-01','2028-10-01','2029-10-01')),
                                WR_45deg=c(0,1748.6,2331.2,2913.8,3496.4,4079,4661.6,5244.2,5826.8)
)

#Cumulative Loss
sth_apr_jun<-sth_salvage_data %>% mutate(Date=as.Date(Sample.Time)) %>% filter(Date>="2020-02-19") %>% group_by(Date) %>%
  summarise(Loss=sum(Loss)) %>% filter(month(Date) %in% c(4,5,6))
sth_salvage_data_cumulative_Apr_Jun<-left_join(base_df_STH_Apr_Jun,sth_apr_jun)
sth_salvage_data_cumulative_Apr_Jun$Loss[is.na(sth_salvage_data_cumulative_Apr_Jun$Loss)] <- 0
sth_salvage_data_cumulative_Apr_Jun <- sth_salvage_data_cumulative_Apr_Jun[order(sth_salvage_data_cumulative_Apr_Jun$Date),] %>%
  mutate(CumulativeLoss = cumsum(Loss))
sth_salvage_data_cumulative_Apr_Jun<-bind_rows(sth_salvage_data_cumulative_Apr_Jun,deg45_line_Apr_Jun)
sth_salvage_data_cumulative_Apr_Jun$CumBO_Loss=5826


#Print Figure
tiff(filename=file.path("Salmonids","output","Figure_10yr_Cumulative_loss_Steelhead_overPA.tiff"),width=10,height = 6, units = "in",  res=300, compression ="lzw")

par(mfrow=c(1,2))

par(mar = c(8.1, 4.1, 4.1, 4.1))
plot(sth_salvage_data_cumulative_Dec_March$Date, sth_salvage_data_cumulative_Dec_March$CumulativeLoss, ylim = c(0, 6100), xaxt = "n",col = "green", type = "l", lty = 1, lwd= 2,main="December-March", xlab = "Year", ylab = "Loss (n)")
axis(1, at=at1, format(at1, "%Y"), cex.axis = .7)
lines(sth_salvage_data_cumulative_Dec_March$Date, sth_salvage_data_cumulative_Dec_March$CumBO_Loss, col = "black", type = "l", lty = 1)
lines(deg45_line_Dec_March$Date, deg45_line_Dec_March$WR_45deg, col = "blue", type = "l", lty = 2)

legend("bottomleft", legend = c("Steelhead Dec-Mar Cumulative Loss", "Total Loss allowed for life of PA", "Hypothetical Uniform Loss"), inset=c(0,-0.4),
       col=c("green","black","blue"), lty=c(1,1,2), lwd=c(2,1,1),bty = "n", xpd=TRUE, mar(c(7,7,11,7)), cex = 0.8)

par(mar = c(8.1, 4.1, 4.1, 4.1))
plot(sth_salvage_data_cumulative_Apr_Jun$Date, sth_salvage_data_cumulative_Apr_Jun$CumulativeLoss, ylim = c(0, 6100), xaxt = "n",col = "green", type = "l", lty = 1, lwd= 2,main="April-June", xlab = "Year", ylab = "Loss (n)")
axis(1, at=at1, format(at1, "%Y"), cex.axis = .7)
lines(sth_salvage_data_cumulative_Apr_Jun$Date, sth_salvage_data_cumulative_Apr_Jun$CumBO_Loss, col = "black", type = "l", lty = 1)
lines(deg45_line_Apr_Jun$Date, deg45_line_Apr_Jun$WR_45deg, col = "blue", type = "l", lty = 2)

legend("bottomleft", legend = c("Steelhead Apr-Jun Cumulative Loss", "Total Loss allowed for life of PA", "Hypothetical Uniform Loss"), inset=c(0,-0.4),
       col=c("green","black","blue"), lty=c(1,1,2), lwd=c(2,1,1),bty = "n", xpd=TRUE, mar(c(7,7,11,7)), cex = 0.8)
dev.off()