################################################################################
################### COMPILING CSV DATA BY ILEANA CALLEAJS ######################
################################################################################

### SET WD
setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/aqua_kd_rawdata") #Set to wherever you have your data

### IMPORT LIBRARIES
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

#IMPORT CSV FILES AND BIND THEM TOGETHER
files <- list.files(pattern = "aqua_kd490_newname_.*csv")
df_list <- lapply(files, read_csv)
kd <- bind_rows(df_list)

files2 <- list.files(pattern = "aqua_kd490_n_newname_.*csv")
df_list2 <- lapply(files2, read_csv)
kd_n <- bind_rows(df_list2)

# RENAME COLUMN AND PARSE TIME 
# For Kd
kd_df <- kd %>%
  rename(time = "system:time_start")

kd_df$time <- parse_date_time(kd_df$time, c("dmy","mdy"))

# For Pixel counts
kd_n_df <- kd_n %>%
  rename(time = "system:time_start")

kd_n_df$time <- parse_date_time(kd_n_df$time, c("dmy","mdy"))


# RESHAPE DATA TO LONG FORMAT AND CREATE UNIQUE IDENTIFIER
# For Kd
kd_long <- kd_df %>%
  pivot_longer(!time, names_to="mpa", values_to="kd") %>%
  unite("id", time:mpa, remove=FALSE)

# For Pixel counts
kd_n_long <- kd_n_df %>%
  pivot_longer(!time, names_to="mpa", values_to="n") %>%
  unite("id", time:mpa, remove=FALSE)

# JOIN AND ADD TIME DATA
df <- full_join(kd_long, kd_n_long)%>%
  mutate(year=year(time),
         month=month(time),
         doy = yday(time))


# REMOVE NAs AND REPLACE 0 PIXELS WITH 1 FOR PARTIAL PIXELS
df_clean <- df %>%
  drop_na() %>%
  mutate(n = replace(n, n==0, 1)) %>%
  select(-id)

# WRITE CSV FOR ANALYSIS
write_csv(df_clean, "aqua_kd.csv")
