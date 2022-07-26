########################################################
########### Kd(490) Classifications by Ileana Callejas
########################################################

setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/aqua_kd_rawdata/Long form clean data") #Change to where kd csv is
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

# READ IN DATA
df <- read_csv("aqua_kd.csv") %>%
  mutate(time=ymd(format(time, "%y-%m-%d")))


# CALCULATE MONTHLY KD(490) AND CLASSIFY TURBIDITY
kd_monthly <- df %>%
  group_by(mpa, year, month) %>%
  summarise(
    n=sum(n),
    mean_kd = mean(kd, na.rm=T),
    sd=sqrt(sum((kd-mean_kd)^2)/(n-1)),
    se=sd/sqrt(n)) %>%
  ungroup()

# CLASSIFY TURBIDITY
kd_cat <-  kd_monthly %>%
  mutate(turb = cut(mean_kd, breaks= c(-Inf, 0.127, 0.3, 0.5, Inf),
                    labels= c("Little/No Stress", "Turbidity Stress", "Turbid Month", "Very Turbid"),
                    right=TRUE)) %>%
  mutate(time=make_datetime(year, month, 15)) %>%
  mutate(time=ymd(format(time, "%y-%m-%d")))

# PALETTE FOR CATEGORIES
pal <- c("Very Turbid" = "red",
         "Turbid Month" = "darkorange", 
         "Turbidity Stress" = "gold", 
         "Little/No Stress" = "forestgreen")

# COUNT FOR MONTHS
counts <- kd_cat %>%
  drop_na() %>%
  group_by(mpa, turb) %>%
  summarise(n=sum(n))

# CREATE CSV IN WIDER FORMAT
wide <- counts %>%
  pivot_wider(names_from = turb, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0))

#write_csv(wide, "kd490monthly_turb_counts_mpa.csv")


# PLOT KD DATA W CLASSIFICATIONS
p <- list()
for (k in unique(kd_cat$mpa)){
  subdata <- subset(kd_cat, mpa == k)
  
  p[[k]] <- print(ggplot(subdata, aes(x=time, y=mean_kd))+
                    geom_point(aes(color=turb)) +
                    theme(panel.background = element_rect(fill="white"),
                          axis.text.x = element_text(color="black"),
                          axis.text.y = element_text(color="black"),
                          axis.title.x = element_text(face="bold"),
                          axis.title.y = element_text(face="bold"),
                          plot.title = element_text(hjust = 0.5, face="bold"),
                          panel.border = element_rect(colour = "black", fill=NA, size=1),
                          legend.key=element_blank(),
                          legend.title = element_blank()) +
                    labs(x = "Time", y = expression(bold("Kd(490)" ~(m^-1)))) +
                    scale_x_date(date_breaks = "years" , date_labels = "%_Y") +
                    scale_color_manual(values = pal) +
                    ggtitle(paste0(k)))
  
  
}
grid <- grid.arrange(grobs=p, ncol=3)
#ggsave("kd_grid.png", grid, dpi=300, width=9000, height=10000 , units="px")
