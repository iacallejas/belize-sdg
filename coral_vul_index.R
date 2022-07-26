########################################################
########### Coral Vulnerability Index By Ileana Callejas
########################################################

## SET WD
setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/longData") #Set to wherever you have your data

### IMPORT LIBRARIES
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
#library(ggstatsplot)

# READ IN DATA
sst <- read_csv("aqua_sst.csv") %>%
  rename(sst="temp") %>%
  mutate(time=ymd(format(time, "%y-%m-%d"))) %>%
  select(time,mpa,sst)%>%
  unite("id", time:mpa, remove=FALSE)

kd <- read_csv("aqua_kd.csv") %>%
  mutate(time=ymd(format(time, "%y-%m-%d")))%>%
  select(time,mpa,kd)%>%
  unite("id", time:mpa, remove=FALSE)

# MERGE THE DATA AND CLEAN
df <- full_join(kd, sst, by="id") %>%
  drop_na() %>%
  select(-time.y,-mpa.y)%>%
  rename(time="time.x",
         mpa="mpa.x")  %>% 
mutate(year=year(time)) 

index <- df %>%
  mutate(stress=ifelse(kd >= 0.127 & sst >= 29.7, "Stress","No Stress"))

#write_csv(index, "kd_sst_stress_data.csv")

index_counts <- index %>%
  count(mpa, stress, sort=TRUE)

#write_csv(index_counts, "kd_sst_stress_counts_data.csv")

# PLOT
#ggstatsplot::ggscatterstats(data = df, x = kd, y = sst)

# Z-SCORES

# CALCULATE MEAN KD FOR EACH MPA PER YEAR
means <- df %>%
  group_by(year,mpa) %>%
  summarize(mean_kd=mean(kd),
         mean_sst=mean(sst))


# MEAN AND SD OF MPA PER YEAR FOR ALL MPAS
mean_sd_per_mpa <- df %>%
  group_by(year) %>%
  summarize(mean_kd_all=mean(kd),
            mean_sst_all=mean(sst),
            sd_kd_all=sd(kd),
            sd_sst_all=sd(sst))


# Z-SCORE FOR KD PER MPA
merge4zscores <- full_join(means, mean_sd_per_mpa, by="year")

zscores <- merge4zscores %>%
  mutate(zscore_kd=(mean_kd - mean_kd_all)/sd_kd_all,
         zscore_sst=(mean_sst - mean_sst_all)/sd_sst_all)


# MEDIAN Z-SCORES PER MPA
zscore_medians <- zscores %>%
  group_by(mpa) %>%
  summarize(kd_median=median(zscore_kd),
            sst_median=median(zscore_sst),
            sum_zscores=kd_median+sst_median)

#write_csv(zscore_medians, "zscores.csv")

hist(zscore_medians$kd_median)
hist(zscore_medians$sst_median)


# W/o Corozal
zscore_no_corozal <- zscore_medians %>%
  filter(!mpa=="Corozal Bay")

hist(zscore_no_corozal$kd_median)


# ASSIGN VALUES BASED ON Z-SCORE
zscore_index <- zscore_medians %>%
  mutate(sst_index = cut(sst_median,
                    breaks= c(-Inf, -0.4, -0.2, 0, 0.2, 0.4, Inf),
                    labels= c(1,2,3,4,5,6),
                    right=TRUE),
         kd_index = cut(kd_median,
                         breaks= c(-Inf, -0.4, -0.2, 0, 0.2, 0.4, Inf),
                         labels= c(1,2,3,4,5,6),
                         right=TRUE)) %>%
  mutate_at(c('sst_index','kd_index'), as.numeric) %>%
  mutate(coral_index = sst_index+kd_index)

#write_csv(zscore_index, "zscores_index.csv")
