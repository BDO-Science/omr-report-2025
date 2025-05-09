library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)
library(sharpshootR)
library(viridis)

##################### Load Salvage Count Data from SacPAS
salvage_data <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=no&age=no')

### Subset December to May
wr_salvage<-salvage_data %>% filter(LAD.Race=="Winter") %>% 
  filter(month(Sample.Time) %in% c(12,1,2,3,4,5)) %>% 
  mutate(Year=year(Sample.Time),WY=ifelse(month(Sample.Time)>9,year(Sample.Time)+1,year(Sample.Time)),Month=month(Sample.Time)) %>%
  group_by(WY,Month) %>% summarise(Loss=sum(Loss)) %>% filter(WY>=2009)

wr_salvage$Loss[is.na(wr_salvage$Loss)]<-0

wr_salvage_wy_sum<-salvage_data %>% filter(LAD.Race=="Winter") %>% 
  filter(month(Sample.Time) %in% c(12,1,2,3,4,5)) %>% mutate(Year=year(Sample.Time),WY=ifelse(month(Sample.Time)>9,year(Sample.Time)+1,year(Sample.Time)),Month=month(Sample.Time)) %>%
  group_by(WY) %>% summarise(Loss=sum(Loss)) %>% filter(WY>=2009) %>% rename(WYLoss=Loss)

wr_salvage<-left_join(wr_salvage,wr_salvage_wy_sum)
wr_salvage$Percentage<-wr_salvage$Loss/wr_salvage$WYLoss*100
wr_salvage$WY<-as.factor(wr_salvage$WY)
wr_salvage$Month_abb<-as.factor(month.abb[wr_salvage$Month])
wr_salvage$Month_abb <- ordered(wr_salvage$Month_abb, levels = c("Dec", "Jan", "Feb","Mar","Apr","May"))

#Plot

fig_WR_month<-ggplot(data=wr_salvage, aes(x=Month_abb, y=Percentage, fill=WY)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_viridis(discrete=TRUE, option="cividis")+
  theme(plot.title=element_text(size=28), 
        axis.text.x=element_text(size=21, color="black"), 
        axis.text.y = element_text(size=20, color="black"), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, angle = 90),
        strip.text = element_text(size = 20))+
  labs(y="Percentage of Water Year Loss (%)")


fig_WR_month

#Print figure
tiff(filename=file.path("Salmonids","output","Figure_Winter-run_loss_by_month.tiff"),width=10,height = 6, units = "in",  res=300, compression ="lzw")
fig_WR_month
dev.off()


######################################################################
#pull salvage datasets from SacPAS

sth_salvage_data <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no')

##Format
sth_salvage_data_split <- sth_salvage_data %>%
  mutate(Sample.Time = as.Date(Sample.Time)) %>%
  filter(month(Sample.Time) %in% c(12,1,2,3,4,5,6)) %>% mutate(Year=year(Sample.Time),WY=ifelse(month(Sample.Time)>9,year(Sample.Time)+1,year(Sample.Time)),Month=month(Sample.Time)) %>%
  filter(WY>=2009) %>% filter(!(month(Sample.Time)==6&day(Sample.Time)>15)) %>% mutate(Group=as.factor(ifelse(month(Sample.Time) %in% c(12,1,2,3),"DecMar","AprJun"))) %>%
  group_by(WY,Group) %>% summarise(Loss=sum(Loss))


sth_salvage_data_wy_sum<-sth_salvage_data %>%
  mutate(Sample.Time = as.Date(Sample.Time)) %>%
  filter(month(Sample.Time) %in% c(12,1,2,3,4,5,6)) %>% mutate(Year=year(Sample.Time),WY=ifelse(month(Sample.Time)>9,year(Sample.Time)+1,year(Sample.Time)),Month=month(Sample.Time)) %>%
  filter(WY>=2009) %>% filter(!(month(Sample.Time)==6&day(Sample.Time)>15)) %>% mutate(Group=as.factor(ifelse(month(Sample.Time) %in% c(12,1,2,3),"DecMar","AprJun"))) %>%
  group_by(WY) %>% summarise(WYLoss=sum(Loss))


sth_salvage_data_split<-left_join(sth_salvage_data_split,sth_salvage_data_wy_sum)
sth_salvage_data_split$Percentage<-sth_salvage_data_split$Loss/sth_salvage_data_split$WYLoss*100
sth_salvage_data_split$WY<-as.factor(sth_salvage_data_split$WY)
sth_salvage_data_split$Group <- ordered(sth_salvage_data_split$Group, levels = c("DecMar","AprJun"))

#Plot

figure_sth_loss_group<-ggplot(data=sth_salvage_data_split, aes(x=WY, y=Percentage, fill=Group)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_viridis(discrete=TRUE, option="cividis",labels = c("December-March","April-June"))+
  theme(plot.title=element_text(size=28), 
        axis.text.x=element_text(size=21, color="black"), 
        axis.text.y = element_text(size=20, color="black"), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22, angle = 90),
        strip.text = element_text(size = 20),
        legend.title = element_blank(),legend.position="top",
        legend.text  = element_text(size=21, color="black")
        )+
  labs(x="Water Year",y="Percentage of Water Year Loss (%)")

figure_sth_loss_group

#Print figure
tiff(filename=file.path("Salmonids","output","Figure_Steelhead_Loss_by_Group.tiff"),width=14,height = 10, units = "in",  res=300, compression ="lzw")
figure_sth_loss_group
dev.off()
