library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# set working directory and load data
setwd("C:/Users/andre/Documents/NYU/Biobank/Data")
ukb <- read_csv("brain_56cols_kmeans4_filtered.csv") %>% 
  mutate(kmeans_cluster_4 = as.factor(kmeans_cluster_4))

# pivot features to longer format for plotting and analysis
ukb_long <- ukb %>% 
  pivot_longer(cols = !c("eid", "kmeans_cluster_4"))

###########################################################
### Run ANOVA to test for mean equality across clusters ###
###########################################################

# variable names to loop over
variables <- ukb %>% 
  select(!c(eid, kmeans_cluster_4)) %>% 
  colnames()

# get AOV object for each variable
aov_objects <- list()
# get ANOVA p-value for each variable
p_values <- c()
signif_indicator <- c()
signif_threshold <- 0.05
for (i in seq_along(variables)) {
  ukb_var <- ukb_long %>% 
    filter(name == variables[i])
  aov.model <- aov(value ~ kmeans_cluster_4, 
                           data=ukb_var)
  aov_objects[[i]] <- aov.model
  p_values[i]<- summary(aov.model)[[1]][["Pr(>F)"]][[1]]
  signif_indicator[i] <- ifelse(p_values[i] <= signif_threshold,
                                      1, 0)
}

# name aov objects by variable
names(aov_objects) <- variables

# variables and p-values for ANOVA
p_vals <- data.frame(variable = variables, 
                     p_value = round(p_values, 2),
                     signif_indicator = signif_indicator)

# write results to csv
write_csv(p_vals, file = "brain_var_anova_p_vals.csv")

#####################
### Post-hoc test ###
#####################

# Tukey's method
# Source: https://stats.idre.ucla.edu/r/faq/how-can-i-do-post-hoc-pairwise-comparisons-in-r/

# filter to brain features with significant differences
vars_significant <- p_vals[p_vals$p_value <= signif_threshold, ]
vars_sig <- vars_significant$variable
posthoc_list <- list()
# run pairwise posthoc analysis for each significant variable
for (i in seq_along(vars_sig)) {
  # run Tukey posthoc test
  posthoc_results <- as.data.frame(TukeyHSD(get(vars_sig[i], 
                                                aov_objects))$kmeans_cluster_4)
  # add variable name as column
  posthoc_results$variable <- vars_sig[i]
  # add cluster group comparisons as column
  posthoc_results$cluster_groups <- paste0("'", rownames(posthoc_results), "'")
  # significance indicator
  posthoc_results$is_signif <- if_else(posthoc_results$`p adj` <= signif_threshold, 1, 0)
  posthoc_list[[i]] <- posthoc_results
}

# write results to csv
write_csv(bind_rows(posthoc_list), file = "brain_var_posthoc.csv")

########################
### Plot by variable ###
########################

# plot variables by cluster - boxplots
ukb_long %>%
  ggplot(aes(x = kmeans_cluster_4, 
             y = value,
             fill = kmeans_cluster_4)) + 
  geom_boxplot() + 
  labs(title = "Brain Variables: Average Values") +
  theme(legend.position = "bottom") + 
  facet_wrap(~name, scales = "free")

# save to pdf
ggsave("brain_vars_boxplot.pdf", width = 12, height = 10)

# plot variables by cluster - densities
ukb_long %>%
  ggplot(aes(x = value,
             fill = kmeans_cluster_4)) + 
  geom_density(alpha = 0.3) + 
  labs(title = "Brain Variables: Density") +
  theme(legend.position = "bottom") + 
  facet_wrap(~name, scales = "free")

# save to pdf
ggsave("brain_vars_density.pdf", width = 12, height = 10)
