#Script to Look at Waldschlager et al 2020 dataset

#####Script set up##############################################################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)
library(ggplot2)

#Set working directory
setwd("~/Desktop/Streamflow/Settling Velocity")

#####START HERE WITH MATCHED DATA SET#########################################

######
#Plots for Rouse Numbers Calculate using Mean density assignment in MaTCH

#Load 
df <- read.csv("SAD_Rouse_TRWP_Mean_sens_002.csv")

df  <- df %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") 

# Calculate statistics
stats_mean_flow <- df %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

stats_mean_flow$n <- df %>%
  group_by(Flow.Type) %>%
  count(Morphology)

ggplot(df, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: Mean MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 10th percentile density assignment in MaTCH

df_10 <- read.csv("SAD_Rouse_TRWP_10th_sens_002.csv")

df_10 <- df_10 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_10_flow <- df_10 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

ggplot(df_10, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 10th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density (%)") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 90th percentile density assignment in MaTCH

df_90 <- read.csv("SAD_Rouse_TRWP_90th_sens_002.csv")

df_90 <- df_90 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_90_flow <- df_90 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

ggplot(df_90, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 90th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density (%)") +
  theme_minimal()

########################################################################################
########################################################################################
######

library(tidyr)

#Plots for all three 


#Load 
df_sum <- read.csv("SAD_Rouse_TRWP_short_summary_sens_002.csv")
df_sum <- df_sum %>% 
  filter(Rouse_adj_mean != "NA") %>%
  filter(Rouse_adj_10 != "NA") %>%
  filter(Rouse_adj_90 != "NA") %>% 
  filter(Event != "SF-6")

########################################################################################

# Define the ranges based on Cowger
ranges <- list(
  "surface load" = c(-Inf, -2.5),
  "rising suspended load" = c(-2.5, -0.8),
  "wash load" = c(-0.8, 0.8),
  "settling suspended load" = c(0.8, 2.5),
  "bed load" = c(2.5, 7.5),
  "immobile" = c(7.5, Inf)
)

# Function to calculate the percentage within a range
calc_percentage <- function(x, range) {
  sum(x >= range[1] & x <= range[2]) / length(x) * 100
}


# Apply the function to each scenario and Test.Group
df_percent_Cowger <- df_sum %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj_mean, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj_mean, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj_mean, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj_mean, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj_mean, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj_mean, ranges[["immobile"]]),
    Percent_10_Surface = calc_percentage(Rouse_adj_10, ranges[["surface load"]]),
    Percent_10_RiseSus= calc_percentage(Rouse_adj_10, ranges[["rising suspended load"]]),
    Percent_10_Wash = calc_percentage(Rouse_adj_10, ranges[["wash load"]]),
    Percent_10_SetSus = calc_percentage(Rouse_adj_10, ranges[["settling suspended load"]]),
    Percent_10_Bed = calc_percentage(Rouse_adj_10, ranges[["bed load"]]),
    Percent_10_Immob = calc_percentage(Rouse_adj_10, ranges[["immobile"]]),
    Percent_90_Surface = calc_percentage(Rouse_adj_90, ranges[["surface load"]]),
    Percent_90_RiseSus = calc_percentage(Rouse_adj_90, ranges[["rising suspended load"]]),
    Percent_90_Wash = calc_percentage(Rouse_adj_90, ranges[["wash load"]]),
    Percent_90_SetSus = calc_percentage(Rouse_adj_90, ranges[["settling suspended load"]]),
    Percent_90_Bed = calc_percentage(Rouse_adj_90, ranges[["bed load"]]),
    Percent_90_Immob = calc_percentage(Rouse_adj_90, ranges[["immobile"]]),
  )

########################################################################################


custom_labels <- c(
  "Low Flow" = "Low Flow (n = 453 )",
  "Stormflow" = "Stormflow (n = 1894)"
)

ggplot() + 
  # First scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  
  # Second scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1, labeller = labeller(Flow.Type = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,2.0),breaks = seq(0, 2.0, by = 0.5)) +
  labs(title="SAD Rouse Numbers: Suspected TRWPs (Slope = 0.002)",
       x="Rouse Numbers",
       y="Density (%)") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold") # Increase title size
  ) +
  scale_fill_manual(name = "Particle Density (MaTCH) Assigned", values = c("Mean" = "blue", "10th Percentile" = "red", "90th Percentile" = "green")) +
  geom_text(data = df_percent_Cowger, aes(x = 6, y=1.0, label = paste0("% Washload","\nMean: ", round(Percent_Mean_Wash, 2), "%\n10th: ", round(Percent_10_Wash, 2), "%\n90th: ", round(Percent_90_Wash, 2), "%")), size = 4.5, hjust = 0.5) +
  geom_label(data = df_percent_Cowger, aes(x = 0, y = 2.0, label = "Wash load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 1.65, y = 1.85, label = "Settling \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -1.65, y = 1.85, label = "Rising \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -3.75, y = 2.0, label = "Surface Load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 5, y = 2.0, label = "Bed Load"), size = 3, color = "black", fill = "white", fontface = "bold") 


########################################################################################
########################################################################################

#####Script set up##############################################################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)
library(ggplot2)

#Set working directory
setwd("~/Desktop/Streamflow/Settling Velocity")

#####START HERE WITH MATCHED DATA SET#########################################

######
#Plots for Rouse Numbers Calculate using Mean density assignment in MaTCH

#Load 
df <- read.csv("SAD_Rouse_TRWP_Mean_sens_001.csv")

df  <- df %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") 

# Calculate statistics
stats_mean_flow <- df %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

stats_mean_flow$n <- df %>%
  group_by(Flow.Type) %>%
  count(Morphology)

ggplot(df, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: Mean MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 10th percentile density assignment in MaTCH

df_10 <- read.csv("SAD_Rouse_TRWP_10th_sens_001.csv")

df_10 <- df_10 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_10_flow <- df_10 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

ggplot(df_10, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 10th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density (%)") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 90th percentile density assignment in MaTCH

df_90 <- read.csv("SAD_Rouse_TRWP_90th_sens_001.csv")

df_90 <- df_90 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_90_flow <- df_90 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

ggplot(df_90, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 90th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density (%)") +
  theme_minimal()

########################################################################################
########################################################################################
######

library(tidyr)

#Plots for all three 


#Load 
df_sum <- read.csv("SAD_Rouse_TRWP_short_summary_sens_001.csv")
df_sum <- df_sum %>% 
  filter(Rouse_adj_mean != "NA") %>%
  filter(Rouse_adj_10 != "NA") %>%
  filter(Rouse_adj_90 != "NA") %>% 
  filter(Event != "SF-6")

########################################################################################

# Define the ranges based on Cowger
ranges <- list(
  "surface load" = c(-Inf, -2.5),
  "rising suspended load" = c(-2.5, -0.8),
  "wash load" = c(-0.8, 0.8),
  "settling suspended load" = c(0.8, 2.5),
  "bed load" = c(2.5, 7.5),
  "immobile" = c(7.5, Inf)
)

# Function to calculate the percentage within a range
calc_percentage <- function(x, range) {
  sum(x >= range[1] & x <= range[2]) / length(x) * 100
}


# Apply the function to each scenario and Test.Group
df_percent_Cowger <- df_sum %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj_mean, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj_mean, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj_mean, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj_mean, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj_mean, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj_mean, ranges[["immobile"]]),
    Percent_10_Surface = calc_percentage(Rouse_adj_10, ranges[["surface load"]]),
    Percent_10_RiseSus= calc_percentage(Rouse_adj_10, ranges[["rising suspended load"]]),
    Percent_10_Wash = calc_percentage(Rouse_adj_10, ranges[["wash load"]]),
    Percent_10_SetSus = calc_percentage(Rouse_adj_10, ranges[["settling suspended load"]]),
    Percent_10_Bed = calc_percentage(Rouse_adj_10, ranges[["bed load"]]),
    Percent_10_Immob = calc_percentage(Rouse_adj_10, ranges[["immobile"]]),
    Percent_90_Surface = calc_percentage(Rouse_adj_90, ranges[["surface load"]]),
    Percent_90_RiseSus = calc_percentage(Rouse_adj_90, ranges[["rising suspended load"]]),
    Percent_90_Wash = calc_percentage(Rouse_adj_90, ranges[["wash load"]]),
    Percent_90_SetSus = calc_percentage(Rouse_adj_90, ranges[["settling suspended load"]]),
    Percent_90_Bed = calc_percentage(Rouse_adj_90, ranges[["bed load"]]),
    Percent_90_Immob = calc_percentage(Rouse_adj_90, ranges[["immobile"]]),
  )

########################################################################################


custom_labels <- c(
  "Low Flow" = "Low Flow (n = 453 )",
  "Stormflow" = "Stormflow (n = 1894)"
)

ggplot() + 
  # First scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  
  # Second scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1, labeller = labeller(Flow.Type = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,2.0),breaks = seq(0, 2.0, by = 0.5)) +
  labs(title="SAD Rouse Numbers: Suspected TRWPs (Slope = 0.001)",
       x="Rouse Numbers",
       y="Density (%)") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold") # Increase title size
  ) +
  scale_fill_manual(name = "Particle Density (MaTCH) Assigned", values = c("Mean" = "blue", "10th Percentile" = "red", "90th Percentile" = "green")) +
  geom_text(data = df_percent_Cowger, aes(x = 6, y=1.0, label = paste0("% Washload","\nMean: ", round(Percent_Mean_Wash, 2), "%\n10th: ", round(Percent_10_Wash, 2), "%\n90th: ", round(Percent_90_Wash, 2), "%")), size = 4.5, hjust = 0.5) +
  geom_label(data = df_percent_Cowger, aes(x = 0, y = 2.0, label = "Wash load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 1.65, y = 1.85, label = "Settling \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -1.65, y = 1.85, label = "Rising \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -3.75, y = 2.0, label = "Surface Load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 5, y = 2.0, label = "Bed Load"), size = 3, color = "black", fill = "white", fontface = "bold") 


########################################################################################
########################################################################################

#####Script set up##############################################################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)
library(ggplot2)

#Set working directory
setwd("~/Desktop/Streamflow/Settling Velocity")

#####START HERE WITH MATCHED DATA SET#########################################

######
#Plots for Rouse Numbers Calculate using Mean density assignment in MaTCH

#Load 
df <- read.csv("SAD_Rouse_TRWP_Mean.csv")

df  <- df %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") 

# Calculate statistics
stats_mean_flow <- df %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )

stats_mean_flow$n <- df %>%
  group_by(Flow.Type) %>%
  count(Morphology)

ggplot(df, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: Mean MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 10th percentile density assignment in MaTCH

df_10 <- read.csv("SAD_Rouse_TRWP_10th.csv")

df_10 <- df_10 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_10_flow <- df_10 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )


######
#Plots for Rouse Numbers Calculate using 90th percentile density assignment in MaTCH

df_90 <- read.csv("SAD_Rouse_TRWP_90th.csv")

df_90 <- df_90 %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")


# Calculate statistics
stats_90_flow <- df_90 %>%
  group_by(Flow.Type) %>%
  summarise(
    min= min(Rouse_adj),
    max= max(Rouse_adj),
    mean = mean(Rouse_adj),
    p5 = quantile(Rouse_adj, 0.05),
    q25 = quantile(Rouse_adj, 0.25),
    q50 = median(Rouse_adj),
    q75 = quantile(Rouse_adj, 0.75),
    p95 = quantile(Rouse_adj, 0.95)
  )


########################################################################################
########################################################################################
######

library(tidyr)

#Plots for all three 


#Load 
df_sum <- read.csv("SAD_Rouse_TRWP_short_summary.csv")
df_sum <- df_sum %>% 
  filter(Rouse_adj_mean != "NA") %>%
  filter(Rouse_adj_10 != "NA") %>%
  filter(Rouse_adj_90 != "NA") %>% 
  filter(Event != "SF-6")

########################################################################################

# Define the ranges based on Cowger
ranges <- list(
  "surface load" = c(-Inf, -2.5),
  "rising suspended load" = c(-2.5, -0.8),
  "wash load" = c(-0.8, 0.8),
  "settling suspended load" = c(0.8, 2.5),
  "bed load" = c(2.5, 7.5),
  "immobile" = c(7.5, Inf)
)

# Function to calculate the percentage within a range
calc_percentage <- function(x, range) {
  sum(x >= range[1] & x <= range[2]) / length(x) * 100
}


# Apply the function to each scenario and Test.Group
df_percent_Cowger <- df_sum %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj_mean, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj_mean, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj_mean, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj_mean, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj_mean, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj_mean, ranges[["immobile"]]),
    Percent_10_Surface = calc_percentage(Rouse_adj_10, ranges[["surface load"]]),
    Percent_10_RiseSus= calc_percentage(Rouse_adj_10, ranges[["rising suspended load"]]),
    Percent_10_Wash = calc_percentage(Rouse_adj_10, ranges[["wash load"]]),
    Percent_10_SetSus = calc_percentage(Rouse_adj_10, ranges[["settling suspended load"]]),
    Percent_10_Bed = calc_percentage(Rouse_adj_10, ranges[["bed load"]]),
    Percent_10_Immob = calc_percentage(Rouse_adj_10, ranges[["immobile"]]),
    Percent_90_Surface = calc_percentage(Rouse_adj_90, ranges[["surface load"]]),
    Percent_90_RiseSus = calc_percentage(Rouse_adj_90, ranges[["rising suspended load"]]),
    Percent_90_Wash = calc_percentage(Rouse_adj_90, ranges[["wash load"]]),
    Percent_90_SetSus = calc_percentage(Rouse_adj_90, ranges[["settling suspended load"]]),
    Percent_90_Bed = calc_percentage(Rouse_adj_90, ranges[["bed load"]]),
    Percent_90_Immob = calc_percentage(Rouse_adj_90, ranges[["immobile"]]),
  )

########################################################################################


custom_labels <- c(
  "Low Flow" = "Low Flow (n = 453 )",
  "Stormflow" = "Stormflow (n = 1894)"
)

ggplot() + 
  # First scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  
  # Second scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1, labeller = labeller(Flow.Type = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = c(-1.8, 0.8, -0.6, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,2.0),breaks = seq(0, 2.0, by = 0.5)) +
  labs(title="SAD Rouse Numbers: Suspected TRWPs (Slope = 0.0005)",
       x="Rouse Numbers",
       y="Density (%)") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold") # Increase title size
  ) +
  scale_fill_manual(name = "Particle Density (MaTCH) Assigned", values = c("Mean" = "blue", "10th Percentile" = "red", "90th Percentile" = "green")) +
  geom_text(data = df_percent_Cowger, aes(x = 6, y=1.0, label = paste0("% Washload","\nMean: ", round(Percent_Mean_Wash, 2), "%\n10th: ", round(Percent_10_Wash, 2), "%\n90th: ", round(Percent_90_Wash, 2), "%")), size = 4.5, hjust = 0.5) +
  geom_label(data = df_percent_Cowger, aes(x = 0, y = 2.0, label = "Wash load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 1.65, y = 1.85, label = "Settling \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -1.65, y = 1.85, label = "Rising \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -3.75, y = 2.0, label = "Surface Load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 5, y = 2.0, label = "Bed Load"), size = 3, color = "black", fill = "white", fontface = "bold") 


########################################################################################
########################################################################################