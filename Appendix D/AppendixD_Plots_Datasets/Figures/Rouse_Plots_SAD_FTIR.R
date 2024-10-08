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
df <- read.csv("SAD_Rouse_Mean.csv")

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
count(Flow.Type) 

ggplot(df, aes(x=Rouse_adj, fill=Flow.Type)) + 
  geom_density(alpha=0.7) +
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: Mean MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 10th percentile density assignment in MaTCH

df_10 <- read.csv("SAD_Rouse_10th.csv")

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
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 10th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

######
#Plots for Rouse Numbers Calculate using 90th percentile density assignment in MaTCH

df_90 <- read.csv("SAD_Rouse_90th.csv")

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
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +  # Set x-axis limits and breaks +
  labs(title="SAD Rouse Numbers: 90th percentile MaTCH Density Distributions",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal()

########################################################################################
########################################################################################


library(tidyr)

#Plots for all three 


#Load 
df_sum <- read.csv("SAD_Rouse_FTIR_short_summary.csv")
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

# Reshape data from wide to long format
df_long <- df_sum %>%
  pivot_longer(cols = c(Rouse_adj_mean, Rouse_adj_10, Rouse_adj_90),
               names_to = "Scenario",
               values_to = "Rouse_adj_value")

library(ggplot2)

ggplot(df_long, aes(x=Rouse_adj_value, fill=Scenario)) + 
  geom_density(alpha=0.5) +  # Adjust alpha for transparency
  facet_wrap(~Flow.Type, scales="free_y", ncol = 1) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  labs(title="SAD Rouse Numbers: μFTIR subset",
       x="Rouse Numbers",
       y="Density") +
  theme_minimal() +
  theme(legend.position="bottom") 

###
#OR
###

custom_labels <- c(
  "Low Flow" = "Low Flow (n = 199)",
  "Stormflow" = "Stormflow (n = 314)"
)

ggplot() + 
  # First scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  
  # Second scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1, labeller = labeller(Flow.Type = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,2.0),breaks = seq(0, 2.0, by = 0.5)) +
  labs(title="SAD Rouse Numbers: μFTIR subset (Fibers and Non-Fibers)",
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
  geom_label(data = df_percent_Cowger, aes(x = 0, y = 1.95, label = "Wash load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 1.65, y = 1.8, label = "Settling \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -1.65, y = 1.8, label = "Rising \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -3.75, y = 1.95, label = "Surface Load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 5, y = 1.95, label = "Bed Load"), size = 3, color = "black", fill = "white", fontface = "bold") 



########################################################################################
########################################################################################
####
#Plots by Sample location
####

df <- read.csv("SAD_Rouse_Mean.csv")

df  <- df %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") %>%
  filter(Flow.Type=="Stormflow") 

# Calculate statistics mean
stats_mean_group <- df %>%
  group_by(Test.Group) %>%
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

stats_mean_group$n <- df %>%
  count(Test.Group) 

# Calculate statistics 10th
stats_10_group <- df_10 %>%
  group_by(Test.Group) %>%
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


# Calculate statistics 90th
stats_90_group <- df_90 %>%
  group_by(Test.Group) %>%
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


##Plots

df_sum <- df_sum %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-6") %>% 
filter(Rouse_adj_mean != "NA") %>% 
  filter(Rouse_adj_10 != "NA") %>% 
  filter(Rouse_adj_90 != "NA")

  

custom_labels <- c(
  "Inside Thalweg" = "Thalweg (n=94)",
  "Outside Thalweg_1" = "Nearest Right Bank (n=116)",
  "Outside Thalweg_2" = "Nearest Left Bank (n=104)"
)


ggplot() + 
  # First scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  
  # Second scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  
  facet_wrap(~Test.Group, scales = "free_y", ncol = 1, labeller = labeller(Test.Group = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "gray") +
  
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,4.5),breaks = seq(0, 4.5, by = 0.5))+
  labs(title="SAD Rouse Numbers: μFTIR subset",
       x="Rouse Numbers",
       y="Density") +
  
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_fill_manual(name = "Particle Density (MaTCH) Assigned", values = c("Mean" = "blue", "10th Percentile" = "red", "90th Percentile" = "green"))


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
  group_by(Test.Group) %>%
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


# Create the plot
ggplot() + 
  geom_density(data = df_sum, aes(x = Rouse_adj_mean, fill = "Mean"), alpha = 0.4) +
  geom_density(data = df_sum, aes(x = Rouse_adj_10, fill = "10th Percentile"), alpha = 0.4) +
  geom_density(data = df_sum, aes(x = Rouse_adj_90, fill = "90th Percentile"), alpha = 0.4) +
  facet_wrap(~Test.Group, scales = "fixed", ncol = 1, labeller = labeller(Test.Group = custom_labels)) +
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "darkgray") +
  scale_x_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, by = 2.5)) +
  scale_y_continuous(limits=c(0,2.5),breaks = seq(0, 2.5, by = 0.5))+
  labs(title="SAD Rouse Numbers: Stormflow μFTIR subset (Fibers and Non-Fibers)",
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
  theme(legend.position="bottom") +
  scale_fill_manual(name = "Particle Density (MaTCH) Assigned", values = c("Mean" = "blue", "10th Percentile" = "red", "90th Percentile" = "green")) +
  geom_text(data = df_percent_Cowger, aes(x = 6, y=1.0, label = paste0("% Washload","\nMean: ", round(Percent_Mean_Wash, 2), "%\n10th: ", round(Percent_10_Wash, 2), "%\n90th: ", round(Percent_90_Wash, 2), "%")), size = 4.5, hjust = 0.5) +
geom_label(data = df_percent_Cowger, aes(x = 0, y = 2.3, label = "Wash load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 1.65, y = 2.0, label = "Settling \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -1.65, y = 2, label = "Rising \nSuspended \nLoad"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = -3.75, y = 2.3, label = "Surface Load"), size = 3, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = df_percent_Cowger, aes(x = 5, y = 2.3, label = "Bed Load"), size = 3, color = "black", fill = "white", fontface = "bold") 
###################################

# Define the ranges based on Born
ranges <- list(
  "surface load" = c(-Inf, -1.8),
  "rising suspended load" = c(-1.8, -0.6),
  "wash load" = c(-0.6, 0.8),
  "settling suspended load" = c(0.8, 2.5),
  "bed load" = c(2.5, 7.5),
  "immobile" = c(7.5, Inf)
)

# Function to calculate the percentage within a range
calc_percentage <- function(x, range) {
  sum(x >= range[1] & x <= range[2]) / length(x) * 100
}

df_percent_Born <- df_sum %>%
  group_by(Test.Group) %>%
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

