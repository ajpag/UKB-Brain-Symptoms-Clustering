library(dplyr)
library(readr)

# Purpose: Join brain variables to somatic cluster groups for ANOVA and visualizations

setwd("C:/Users/andre/Documents/NYU/Biobank/Brain_Gastro/post_analysis")
somatic_raw <- read_csv("../../Data/somatic_df_cluster_k4_1yr.csv")
brain_raw <- read_csv("../../Data/brain_56cols_kmeans4_filtered.csv")
output_path <- "../../Data/"
output_filename <- "somatic_brain_kmed4.csv"

# filter to only demographic variables and cluster label
somatic <- somatic_raw %>% select(eid, kmedoid_cluster)
brain <- brain_raw %>% select(-kmeans_cluster_4)

# check records
paste("Somatic participants:", nrow(somatic))
paste("Brain variable participants: ", nrow(brain))

# filter to participants with both brain data and somatic survey responses
somatic_brain <- somatic %>% inner_join(brain, by="eid")

# check records
paste("Somatic and Brain participants: ", nrow(somatic_brain))

# write to csv
write_csv(somatic_brain, file = paste0(output_path, output_filename))
