library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# set paths and filenames
setwd("NYU/Biobank/Brain_Gastro/post_analysis/")
time_windows <- c("6mo", "1yr")
data_files <- paste0("../../Data/somatic_df_cluster_k4_", time_windows, ".csv")
# where to save PDF plot outputs
pdf_path <- "../../Plots/"
pdf_filenames <- paste0("somatic_symptoms_boxplot_kmed4_", time_windows, ".pdf")

#######################
### Create boxplots ###
#######################

for (i in seq_along(data_files)) {
  # load data
  ukb <- read_csv(data_files[i]) %>% 
  mutate(kmedoid_cluster = as.factor(kmedoid_cluster))
  
  # pivot data longer for plotting
  # exclude non-feature columns from pivot
  ukb_long <- ukb %>% 
    pivot_longer(cols = !c("eid", "kmedoid_cluster", "...1", "53-2.0","31-0.0",
                           "center2_time_lapse2", "center2_time_lapse1",
                           "20400-0.0", "21023-0.0"))
  
  # plot
  ukb_long %>% 
    ggplot(aes(x = kmedoid_cluster,
               y = value,
               fill = kmedoid_cluster)) + 
    geom_boxplot() + 
    labs(title = "Somatic Symptoms", 
         subtitle = paste0("Time Window: ", time_windows[i])) +
    theme(legend.position = "bottom") + 
    facet_wrap(~name, scales = "free")
  
  # save to pdf
  ggsave(paste0(pdf_path, pdf_filenames[i]), width = 12, height = 10)
}