############################################################
########### SST Trends by Ileana Callejas and Katie Osborn
############################################################

setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/aqua_sst_rawdata/Long form clean data") # Change to where SST CSV is
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)
library(ggpubr)
library(plotrix)
library(tibbletime)
library(lmerTest)
library(lme4)


###############################
##### STATISTICAL ANALYSIS
###############################
#read in file
c_s_sst <- read.csv(file = 'aqua_sst.csv')

#display internal structure of data file
str(c_s_sst)

#set starting year and apply linear mixed model fit
c_s_sst$year.1 <- c_s_sst$year-2002
m1 <- lmer(temp~year.1+(1|mpa),data=c_s_sst)

summary(m1)

###############################
##### BOXPLOTS OF SEPT DATA
###############################
# READ IN DATA
df <- read_csv("aqua_sst.csv") %>%
  rename(sst="temp") %>%
  mutate(time=ymd(format(time, "%y-%m-%d")))

sept <- df %>%
  filter(month==9)

#write_csv(sept, "Sept_SSTs_only.csv")

# BOXPLOT SEPT SSTs
for (k in unique(sept$mpa)){
  subdata <- subset(sept, mpa == k)
  
  plot <- print(ggplot(subdata, aes(as.factor(year), y=sst))+
                  geom_boxplot(aes(group = year), fill="slateblue", alpha=0.5) +
                  geom_smooth(method = "lm", aes(group=1))+
                  stat_regline_equation() +
                  theme(panel.background = element_rect(fill="white"),
                        axis.text.x = element_text(color="black"),
                        axis.text.y = element_text(color="black"),
                        axis.title.x = element_text(face="bold"),
                        axis.title.y = element_text(face="bold"),
                        plot.title = element_text(hjust = 0.5, face="bold"),
                        panel.border = element_rect(colour = "black", fill=NA, size=1),
                        legend.key=element_blank(),
                        legend.title = element_blank()) +
                  labs(x = "Time", y = "SST (°C)") +
                  ggtitle(paste0(k)))
  
  #ggsave(plot, path = "Sept plots", file=paste0(k,".png"), width=10, height=7, dpi=300)
  
}