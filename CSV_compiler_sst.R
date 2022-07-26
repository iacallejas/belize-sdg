################################################################################
################### COMPILING CSV DATA BY ILEANA CALLEJAS ######################
################################################################################

### SET WD
setwd("C:/Users/calle/OneDrive/JPL/SDG 14/data/aqua_sst_rawdata") #Set to wherever you have your data

### IMPORT LIBRARIES
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

#IMPORT CSV FILES AND BIND THEM TOGETHER
files <- list.files(pattern = "aqua_sst_newname_.*csv")
df_list <- lapply(files, read_csv)
sst <- bind_rows(df_list)

files2 <- list.files(pattern = "aqua_sst_n_newname_.*csv")
df_list2 <- lapply(files2, read_csv)
sst_n <- bind_rows(df_list2)

# RENAME COLUMN AND PARSE TIME 
# For SST
sst_df <- sst %>%
  rename(time = "system:time_start")

sst_df$time <- parse_date_time(sst_df$time, c("dmy","mdy"))

# For Pixel counts
sst_n_df <- sst_n %>%
  rename(time = "system:time_start")

sst_n_df$time <- parse_date_time(sst_n_df$time, c("dmy","mdy"))

# RESHAPE DATA TO LONG FORMAT AND CREATE UNIQUE IDENTIFIER
# For SST
sst_long <- sst_df %>%
  pivot_longer(!time, names_to="mpa", values_to="temp") %>%
  unite("id", time:mpa, remove=FALSE)

# For Pixel counts
sst_n_long <- sst_n_df %>%
  pivot_longer(!time, names_to="mpa", values_to="n") %>%
  unite("id", time:mpa, remove=FALSE)

# JOIN AND ADD TIME DATA
df <- full_join(sst_long, sst_n_long)%>%
  mutate(year=year(time),
         month=month(time),
         doy = yday(time))


# REMOVE NAs AND REPLACE 0 PIXELS WITH 1 FOR PARTIAL PIXELS
df_clean <- df %>%
  drop_na() %>%
  mutate(n = replace(n, n==0, 1)) %>%
  select(-id)

# WRITE CSV FOR ANALYSIS
write_csv(df_clean, "aqua_sst.csv")

