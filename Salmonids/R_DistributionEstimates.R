#R_DistributionEstimates.R----

# Nick Bertrand
# Start Date: Tue Jun 25 11:58:59 2024

#About----
#Project: WY2024 OMR report
#Purpose:This Script generates the figures for the distribution estimates made by Samt

#Libraries ----
library(tidyverse)
library(readr)
library(readxl)
library(grid)
library(gridExtra)

# Set working directory ----
#set to location of root object to highest tier directory
getwd()
root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/omr_report_2024/Salmonids"
setwd(root)
getwd()
#these root object use directories 
data_root<-file.path(root,"data")
viz_output_root <- file.path(root,"output")


# Load distribution estimate data ----
distribution_data<-read_excel(file.path("data","DistributionEstimates_WOMT_WY2024.xlsx"),sheet="DATA Dist WOMT Export OMRrange")
str(distribution_data)

# Split range data into upper and lower dist estimates
dist_est_split <- function(datafile=distribution_data,old_col="Natural_WR_YTE_Range",newcol1="Natural_WR_YTE_Lower",newcol2="Natural_WR_YTE_Upper"){
  distribution_data_newdata <- separate(datafile, col=old_col,
                                     c("dummy1", "dummy2"), sep="-",extra = "drop") %>% 
    mutate(dummy2=gsub("\\%", "", dummy2)) %>%
    mutate(dummy1=as.numeric(dummy1),dummy2=as.numeric(dummy2)) 
  
  colnames(distribution_data_newdata)[colnames(distribution_data_newdata) == "dummy1"] = newcol1
  colnames(distribution_data_newdata)[colnames(distribution_data_newdata) == "dummy2"] = newcol2
  
  return(distribution_data_newdata)
}

# Create new columns for upper and lower
distribution_data_edit <- dist_est_split()
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_WR_ID_Range",newcol1="Natural_WR_ID_Lower",newcol2="Natural_WR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_WR_E_Range",newcol1="Natural_WR_E_Lower",newcol2="Natural_WR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_YTE_Range",newcol1="Natural_SR_YTE_Lower",newcol2="Natural_SR_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_ID_Range",newcol1="Natural_SR_ID_Lower",newcol2="Natural_SR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SR_E_Range",newcol1="Natural_SR_E_Lower",newcol2="Natural_SR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_YTE_Range",newcol1="Hatch_WR_YTE_Lower",newcol2="Hatch_WR_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_ID_Range",newcol1="Hatch_WR_ID_Lower",newcol2="Hatch_WR_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Hatch_WR_E_Range",newcol1="Hatch_WR_E_Lower",newcol2="Hatch_WR_E_Upper")

distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_YTE_Range",newcol1="Natural_SH_YTE_Lower",newcol2="Natural_SH_YTE_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_ID_Range",newcol1="Natural_SH_ID_Lower",newcol2="Natural_SH_ID_Upper")
distribution_data_edit <- dist_est_split(datafile=distribution_data_edit,old_col="Natural_SH_E_Range",newcol1="Natural_SH_E_Lower",newcol2="Natural_SH_E_Upper")

distribution_data_edit$Date=as.Date(distribution_data_edit$Date)

#Load monitoring csv data from SacPAS----
#https://www.cbr.washington.edu/sacramento/data/query_sampling_graph.html
#query selections: 2023>Chinook,Winter,Unclipped >Sacramento River to Chipps island =.....>
#Deselect cumulative sampling data> Select raw catch
monitoring_data_raw <- read.csv(file.path("data","samplingdaily_1719344772_851.csv"))
str(monitoring_data_raw)

#Calculate percentage of sum
monitoring_data_WR <- monitoring_data_raw %>% rename(
  KL_RST=Raw.Knights.Landing.RST,
  SacSeine=Raw.Sacramento.Beach.Seines..SR080E.SR071E.SR062E.SR057E.SR055E.SR060E.AM001S.SR049E.,
  SacTrawl=Raw.Sacramento.Trawls..SR055M.SR055E.SR055W.SR055X.,
  ChippsTrawl=Raw.Chipps.Island.Trawls..SB018M.SB018N.SB018S.SB018X.) %>% 
  select(Date,KL_RST,SacSeine,SacTrawl,ChippsTrawl) %>%
  mutate(Date=as.Date(Date)) %>% rowwise() %>% mutate(SacTrawl_Seine= sum(SacSeine, SacTrawl, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Date)) %>% select(-c(SacSeine, SacTrawl)) %>%
  mutate(KL_RST = replace_na(KL_RST, 0),ChippsTrawl=replace_na(ChippsTrawl, 0)) %>%
  mutate(KL_RST_sum=cumsum(KL_RST),ChippsTrawl_sum=cumsum(ChippsTrawl),SacTrawl_Seine_sum=cumsum(SacTrawl_Seine)) %>%
  mutate(KL_RST_sum=KL_RST_sum/max(KL_RST_sum),ChippsTrawl_sum=ChippsTrawl_sum/max(ChippsTrawl_sum),SacTrawl_Seine_sum=SacTrawl_Seine_sum/max(SacTrawl_Seine_sum)) %>%
  mutate(Reverse_KL_RST = 1-KL_RST_sum, SacTrawl_Seine_sum_minus_Chipps=SacTrawl_Seine_sum-ChippsTrawl_sum)

#Create winter run LAD monitoring data per DFW's figure
monitoring_data_WR_sum <- monitoring_data_WR %>% 
  select(Date,Reverse_KL_RST,SacTrawl_Seine_sum_minus_Chipps,ChippsTrawl_sum) %>%
  gather("Category","Percent",2:4) %>% mutate(Date=as.Date(Date))%>%
  mutate(Percent=Percent*100)

monitoring_data_WR_sum$Category<-factor(monitoring_data_WR_sum$Category, levels=c('Reverse_KL_RST','SacTrawl_Seine_sum_minus_Chipps', 'ChippsTrawl_sum'))
view(monitoring_data_WR_sum)
########################
#Create figure for Natural Winter-run LAD distribution estimates----
str(distribution_data_edit)
data_WR_nat<-distribution_data_edit %>% select(Date,Natural_WR_YTE,Natural_WR_ID,Natural_WR_E) %>% gather("Category","Percent",2:4) 
#view(data_WR_nat)
#Winter-run distribution estimates ----
#remember to change axis dates
plot_distest_WR_nat <- ggplot() +  
  geom_line(data=data_WR_nat, aes(Date, Percent, colour=Category),linewidth=1) + 
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_YTE_Upper, ymin=Natural_WR_YTE_Lower), 
              alpha=0.2,fill="blue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_ID_Upper, ymin=Natural_WR_ID_Lower), 
              alpha=0.2,fill="navyblue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_WR_E_Upper, ymin=Natural_WR_E_Lower), 
              alpha=0.2,fill="red") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  ggtitle("(A)")+
  theme(plot.title=element_text(size=13), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "top") + 
  scale_colour_manual(values = c("red","navyblue","blue"),name="",labels=c("Exited the Delta","In the Delta","Yet to Enter the Delta"))+
  guides(color = guide_legend(reverse = TRUE,nrow=3,byrow=TRUE))

plot_distest_WR_nat

#Winter-run monitoring data analog to dist estimates
plot_monitoring_WR_nat <- ggplot() +  
  geom_line(data=monitoring_data_WR_sum, aes(Date, Percent, colour=Category),size=1) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  ggtitle("(B)")+
  theme(plot.title=element_text(size=13), 
        axis.text.x=element_text(size=9, color="black"), 
        axis.text.y = element_text(size=8, color="black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        strip.text = element_text(size = 7),
        legend.position = "top") + 
  scale_colour_manual(values = c("blue","navyblue","red"),name="",labels=c("100% - cumulative % Knights Landing catch","Cumulative % combined Sacramento trawl and seine catch minus Chipps Trawl catch","Cumulative % Chipps Trawl Catch"))+
  guides(color = guide_legend(nrow=3,byrow=TRUE))
  
plot_monitoring_WR_nat


#Print figure
tiff(filename="C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/omr_report_2024/Salmonids/output/Figure_DistEst_WinterRun.tiff",
     type="cairo",
     units="in", 
     width=8, #10*1, 
     height=10, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
grid.arrange(plot_distest_WR_nat, plot_monitoring_WR_nat, ncol=1,nrow=2)

dev.off()

###################

#Create figure for Spring-run LAD distribution estimates----
data_SR<-distribution_data_edit %>% select(Date,Natural_SR_YTE,Natural_SR_ID,Natural_SR_E) %>% gather("Category","Percent",2:4) 

#Spring-run distribution estimates
plot_distest_SR <- ggplot() +  
  geom_line(data=data_SR, aes(Date, Percent, colour=Category),size=1) + 
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SR_YTE_Upper, ymin=Natural_SR_YTE_Lower), 
              alpha=0.2,fill="blue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SR_ID_Upper, ymin=Natural_SR_ID_Lower), 
              alpha=0.2,fill="navyblue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SR_E_Upper, ymin=Natural_SR_E_Lower), 
              alpha=0.2,fill="red") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  theme(plot.title=element_text(size=13), 
        axis.text.x=element_text(size=9, color="black"), 
        axis.text.y = element_text(size=8, color="black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        strip.text = element_text(size = 7),
        legend.position = "top") + 
  scale_colour_manual(values = c("red","navyblue","blue"),name="",labels=c("Exited the Delta","In the Delta","Yet to Enter the Delta"))+
  guides(color = guide_legend(reverse = TRUE,nrow=3,byrow=TRUE))

plot_distest_SR

#Print figure
tiff(filename=file.path("output","Figure_DistEst_SpringRun.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=6, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
plot_distest_SR
dev.off()


###################

#Create figure for Steelhead distribution estimates----
data_SH<-distribution_data_edit %>% select(Date,Natural_SH_YTE,Natural_SH_ID,Natural_SH_E) %>% gather("Category","Percent",2:4) 

#Steelhead distribution estimates
plot_distest_SH <- ggplot() +  
  geom_line(data=data_SH, aes(Date, Percent, colour=Category),size=1) + 
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SH_YTE_Upper, ymin=Natural_SH_YTE_Lower), 
              alpha=0.2,fill="blue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SH_ID_Upper, ymin=Natural_SH_ID_Lower), 
              alpha=0.2,fill="navyblue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Natural_SH_E_Upper, ymin=Natural_SR_E_Lower), 
              alpha=0.2,fill="red") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  theme(plot.title=element_text(size=13), 
        axis.text.x=element_text(size=9, color="black"), 
        axis.text.y = element_text(size=8, color="black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        strip.text = element_text(size = 7),
        legend.position = "top") + 
  scale_colour_manual(values = c("red","navyblue","blue"),name="",labels=c("Exited the Delta","In the Delta","Yet to Enter the Delta"))+
  guides(color = guide_legend(reverse = TRUE,nrow=3,byrow=TRUE))

plot_distest_SH

#Print figure
tiff(filename=file.path("output","Figure_DistEst_Steelhead.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=6, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
plot_distest_SH
dev.off()

###################################################################

#Load hatchery winter-run acoustic dataset
#Data set copy and pasted from table 4.3
#https://oceanview.pfeg.noaa.gov/CalFishTrack/pageLSWR_2024.html
#format the date and headers correctly
acoustic_data_raw <- read_excel(file.path(data_root,"HatcheryWR_acoustic_data.xlsx"))# %>%
  #mutate(Date=as.Date(Date,"%m/%d/%Y"))
str(acoustic_data_raw)

acoustic_data <- acoustic_data_raw %>% select(Date,MeridianBr,TowerBridge, Benicia_east) %>%
  mutate(MeridianBr = replace_na(MeridianBr, 0),TowerBridge=replace_na(TowerBridge, 0),Benicia_east=replace_na(Benicia_east, 0)) %>%
  mutate(MeridianBr=cumsum(MeridianBr),TowerBridge=cumsum(TowerBridge),Benicia_east=cumsum(Benicia_east)) %>%
  mutate(MeridianBr=MeridianBr/max(MeridianBr),TowerBridge=TowerBridge/max(TowerBridge),Benicia_east=Benicia_east/max(Benicia_east)) %>%
  mutate(Reverse_MeridianBr = 1-MeridianBr, TowerBridge_minus_Benicia=TowerBridge-Benicia_east) %>%
  select(Date,Reverse_MeridianBr,TowerBridge_minus_Benicia,Benicia_east) %>%
  gather("Category","Percent",2:4) %>% mutate(Date=as.Date(Date))%>%
  mutate(Percent=Percent*100)

acoustic_data$Category<-factor(acoustic_data$Category, levels=c('Reverse_MeridianBr','TowerBridge_minus_Benicia', 'Benicia_east'))

#Hatchery winter-run acoustic tag data analog to dist estimates
plot_monitoring_WR_hat <- ggplot() +  
  geom_line(data=acoustic_data, aes(Date, Percent, colour=Category),size=1) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  ggtitle("(B)")+
  theme(plot.title=element_text(size=13), 
        axis.text.x=element_text(size=9, color="black"), 
        axis.text.y = element_text(size=8, color="black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        strip.text = element_text(size = 7),
        legend.position = "top") + 
  scale_colour_manual(values = c("blue","navyblue","red"),name="",labels=c("100% - cumulative % Meridian Bridge detections","Cumulative % Tower Bridge detections minus Benicia East","Cumulative % Benicia East detections"))+
  guides(color = guide_legend(nrow=3,byrow=TRUE))

plot_monitoring_WR_hat

#Create figure for hatchery Winter-run distribution estimates

data_WR_hat <-distribution_data_edit %>% select(Date,Hatch_WR_YTE,Hatch_WR_ID,Hatch_WR_E) %>% gather("Category","Percent",2:4) 

plot_distest_WR_hat <- ggplot() +  
  geom_line(data=data_WR_hat, aes(Date, Percent, colour=Category),size=1) + 
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Hatch_WR_YTE_Upper, ymin=Hatch_WR_YTE_Lower), 
              alpha=0.2,fill="blue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Hatch_WR_ID_Upper, ymin=Hatch_WR_ID_Lower), 
              alpha=0.2,fill="navyblue")+
  geom_ribbon(data=distribution_data_edit,aes(x=Date, ymax=Hatch_WR_E_Upper, ymin=Hatch_WR_E_Lower), 
              alpha=0.2,fill="red") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month",
               date_labels="%B",
               limits = as.Date(c('2023-10-01','2024-07-01')))+
  ggtitle("(A)")+
  theme(plot.title=element_text(size=13), 
        axis.text.x=element_text(size=9, color="black"), 
        axis.text.y = element_text(size=8, color="black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        strip.text = element_text(size = 7),
        legend.position = "top") + 
  scale_colour_manual(values = c("red","navyblue","blue"),name="",labels=c("Exited the Delta","In the Delta","Yet to Enter the Delta"))+
  guides(color = guide_legend(reverse = TRUE,nrow=3,byrow=TRUE))

plot_distest_WR_hat



#Print figure
tiff(filename=file.path("output","Figure_DistEst_WinterRun_Hatchery.tiff"),
     type="cairo",
     units="in", 
     width=8, #10*1, 
     height=10, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
grid.arrange(plot_distest_WR_hat, plot_monitoring_WR_hat, ncol=1,nrow=2)

dev.off()






##################### Test code


plot(Natural_WR_YTE ~ Date, distribution_data_edit, xaxt = "n", type = "l", col="black",main="Natural Winter-Run",ylab="Percentage of Population",xlab="")
lines(distribution_data_edit$Date, distribution_data_edit$Natural_WR_ID, col = "black", type = "l", lty = 2)
lines(distribution_data_edit$Date, distribution_data_edit$Natural_WR_E, col = "black", type = "l", lty = 3)
polygon(x = c(distribution_data_edit$Date, distribution_data_edit$Date[length(distribution_data_edit$Date)], rev(distribution_data_edit$Date), distribution_data_edit$Date[1]),
        y = c(distribution_data_edit$Natural_WR_YTE_Lower, distribution_data_edit$Natural_WR_YTE_Upper[length(distribution_data_edit$Natural_WR_YTE_Upper)], rev(distribution_data_edit$Natural_WR_YTE_Upper), distribution_data_edit$Natural_WR_YTE_Lower[1]),
        border=NA, col=gray(0.2,0.25))
axis(1, at=at, format(at, "%b %d"), cex.axis = .7)
legend(min(distribution_data$Date), 50, legend=c("Yet to enter Delta", "In Delta", "Exited Delta"),
       col=c("black", "black",'black'), lty=1:3, cex=0.8)


