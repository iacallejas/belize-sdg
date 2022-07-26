####################################################################################################
########### Degree Heating Weeks with 29.7 Threshold by Ileana Callejas adapted from dbcaDHW package
####################################################################################################

setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/aqua_sst_rawdata/Long form clean data") # Change to where CSV files are
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
library(gridExtra)

# READ IN DATA
df <- read_csv("aqua_sst.csv") %>%
  rename(sst="temp") %>%
  mutate(time=ymd(format(time, "%y-%m-%d")))

## SET THRESHOLD
mmmt <- 29.7 # Belize regional threshold

# FUNCTION TO ROLL FUNCTION FOR 12 WEEKS OR 84 DAYS
dhw_calc_12 <- tibbletime::rollify(sum, window = 84)

# CALCULATE THE DHW
dhw <- df %>%
  group_by(mpa) %>%
  dplyr::mutate(hspt = sst - mmmt,
                hsptm = ifelse(hspt >= 1, hspt, 0),
                dhw = dhw_calc_12(hsptm)*(1/7)) %>% #hotspot based on 1 degree
  dplyr::select(-hsptm) %>% #hotspot based on 1 degree
  mutate(risk = cut(dhw,
                    breaks= c(-Inf, 1, 4, 8, Inf),
                    labels= c("Little/No Stress","Temperature Stress", "Bleaching Risk", "Mortality Risk"),
                    right=TRUE))


# CALCULATE DEGREE HEATING WEEK MAXIMUMS
dhwm <- dhw %>%
  dplyr::group_by(mpa) %>%
  dplyr::summarise(dhwm = max(dhw, na.rm = TRUE))


# CALCULATE COUNTS 
counts <- dhw %>%
  drop_na() %>%
  count(mpa,risk,sort=TRUE)

# RISK PER YR PER MPA
yearly <- dhw %>%
  group_by(mpa,year) %>%
  count(risk)

# RISK PER YR ONLY
year_only <- dhw %>%
  group_by(year) %>%
  count(risk)

# CREATE CSV IN WIDER FORMAT
wide <- counts %>%
  pivot_wider(names_from = risk, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0))

#write_csv(wide, "dhw_counts_29.7_mpa.csv")

## palette for plots
pal <- c("Mortality Risk" = "red",
         "Bleaching Risk" = "orange", 
         "Temperature Stress" = "yellow", 
         "Little/No Stress" = "forestgreen")


# PLOTS
for (k in unique(dhw$mpa)){
  subdata <- subset(dhw, mpa == k)
  
  plot <- print(ggplot(na.omit(subdata))+
                  geom_col(aes(x = time, y = dhw/.6, fill = risk)) + 
                  geom_line(aes(x = time, y = na.omit(sst)), color = "blue") +
                  theme(panel.background = element_rect(fill="white"),
                        axis.text.x = element_text(color="black"),
                        axis.text.y = element_text(color="black"),
                        axis.title.x = element_text(face="bold"),
                        axis.title.y = element_text(face="bold"),
                        axis.title.y.left = element_text(color = "blue"),
                        axis.text.y.left = element_text(color = "blue"),
                        plot.title = element_text(hjust = 0.5, face="bold"),
                        panel.border = element_rect(colour = "black", fill=NA, size=1),
                        legend.key=element_blank(),
                        legend.title = element_blank()) +
                  labs(x = "Time") +
                  scale_y_continuous(name="SST (°C)", sec.axis= sec_axis(~. *0.6, name="Degree Heating Week (°C-weeks)"), expand = expansion(mult = c(0, .1))) +
                  scale_x_date(date_breaks = "years" , date_labels = "%_Y") +
                  scale_fill_manual(
                    values = pal,
                    limits = names(pal)
                  ) +
                  ggtitle(paste0(k)))
  

  #ggsave(plot, path = "dhw plots Belize threhold", file=paste0(k,".png"), width=10, height=7, dpi=300)
  
}

p <- list()
for (k in unique(dhw$mpa)){
  subdata <- subset(dhw, mpa == k)
  
  p[[k]] <- print(ggplot(na.omit(subdata))+
                  geom_col(aes(x = time, y = dhw/.6, fill = risk)) + 
                  geom_line(aes(x = time, y = na.omit(sst)), color = "blue") +
                  theme(panel.background = element_rect(fill="white"),
                        axis.text.x = element_text(color="black"),
                        axis.text.y = element_text(color="black"),
                        axis.title.x = element_text(face="bold"),
                        axis.title.y = element_text(face="bold"),
                        axis.title.y.left = element_text(color = "blue"),
                        axis.text.y.left = element_text(color = "blue"),
                        plot.title = element_text(hjust = 0.5, face="bold"),
                        panel.border = element_rect(colour = "black", fill=NA, size=1),
                        legend.key=element_blank(),
                        legend.title = element_blank()) +
                  labs(x = "Time") +
                  scale_y_continuous(name="SST (°C)", sec.axis= sec_axis(~. *0.6, name="Degree Heating Week (°C-weeks)"), expand = expansion(mult = c(0, .1))) +
                  scale_x_date(date_breaks = "years" , date_labels = "%_Y") +
                  scale_fill_manual(
                    values = pal,
                    limits = names(pal)
                  ) +
                  ggtitle(paste0(k)))
  

  
}

# For Figures
grid <- grid.arrange(grobs=p, ncol=4)

ggsave("sst_grid.png", grid, dpi=300, width=13000, height=5000 , units="px")

grid2 <- grid.arrange(grobs=p, ncol=3)
ggsave("sst_grid2.png", grid2, dpi=300, width=9000, height=10000 , units="px")
