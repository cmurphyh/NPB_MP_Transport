#Script to Plot
######
#Figure 7: Multiple Rouse Numbers Distributions on one plot all using Mean density assignment in MaTCH
#With Washload Transport Percentages Removed

#####Script set up##############################################################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(cowplot)

#Set working directory
setwd("~/Appendix D/AppendixD_Plots_Datasets/Figures")


#Starting with flow-type plots

#Load & Filter Mean Datasets

#SDC (Fibers and Non-Fibers)
sdc <- read.csv("SDC_Rouse_Mean.csv")
sdc  <- sdc %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")
#SDC (Non-Fibers only)
sdc_nf <- read.csv("SDC_Rouse_Mean.csv")
sdc_nf  <- sdc_nf %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") %>% 
  filter(X1st.Order.Morph!="Fiber")
#SDC (TRWP only)
sdc_trwp <- read.csv("SDC_Rouse_TRWP_Mean.csv")
sdc_trwp  <- sdc_trwp %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") 

#SAD (Fibers and Non-Fibers)
sad <- read.csv("SAD_Rouse_Mean.csv")
sad  <- sad %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6")
#SAD (Non-Fibers only)
sad_nf <- read.csv("SAD_Rouse_Mean.csv")
sad_nf  <- sad_nf %>% 
  filter(Rouse_adj != "NA") %>% 
  filter(Event != "SF-6") %>% 
  filter(X1st.Order.Morph!="Fiber")
#SAD (TRWP only)
sad_trwp <- read.csv("SAD_Rouse_TRWP_Mean.csv")
sad_trwp  <- sad_trwp %>% 
  filter(Rouse_adj != "NA") %>% 
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
sdc_percent_Cowger <- sdc %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to each scenario and Test.Group
sdcnf_percent_Cowger <- sdc_nf %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to sdc trwp
sdctrwp_percent_Cowger <- sdc_trwp %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )


# Apply the function to each scenario and Test.Group
sad_percent_Cowger <- sad %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to each scenario and Test.Group
sadnf_percent_Cowger <- sad_nf %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to sdc trwp
sadtrwp_percent_Cowger <- sad_trwp %>%
  group_by(Flow.Type) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )
########################################################################################

#Plot SDC 

p1 <- ggplot() + 
  
  # Second scenario
  geom_density(data = sdc_nf, aes(x = Rouse_adj, fill = "Non-Fibers"), alpha = 0.4) +
  
  # First scenario
  geom_density(data = sdc, aes(x = Rouse_adj, fill = "Fibers and Non-Fibers"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = sdc_trwp, aes(x = Rouse_adj, fill = "TRWP"), alpha = 0.4) +
  
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "darkgray") +
  
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_y_continuous(limits=c(0,6),breaks = seq(0, 6, by = 1.0)) + 
  xlab(NULL)+
  labs(title="SDC Sourced Microplastics",
       y="Density (%)") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14, face = "bold"),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Increase title size
  ) +
  scale_fill_manual(name = "Dataset", values = c("Fibers and Non-Fibers" = "blue", "Non-Fibers" = "red", "TRWP" = "green")) +
  geom_label(data = sdc_percent_Cowger, aes(x = 0, y = 5.8, label = "Wash Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = 1.65, y = 5.2, label = "Settling \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = -1.65, y = 5.2, label = "Rising \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = -3.75, y = 5.8, label = "Surface Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = 3.75, y = 5.8, label = "Bed Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") 

########################################################################################

#Plot SAD

p2 <- ggplot() + 
  
  # Second scenario
  geom_density(data = sad_nf, aes(x = Rouse_adj, fill = "Non-Fibers"), alpha = 0.4) +
  
  # First scenario
  geom_density(data = sad, aes(x = Rouse_adj, fill = "Fibers and Non-Fibers"), alpha = 0.4) +
  
  
  # Third scenario
  geom_density(data = sad_trwp, aes(x = Rouse_adj, fill = "TRWP"), alpha = 0.4) +
  
  facet_wrap(~Flow.Type, scales = "fixed", ncol = 1) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "darkgray") +
  
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_y_continuous(limits=c(0,3.0),breaks = seq(0, 3.0, by = 1.0)) +
  xlab(NULL)+
  labs(title="SAD Sourced Microplastics",
       y="") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, face = "bold"),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Increase title size
  ) +
  scale_fill_manual(name = "Dataset", values = c("Fibers and Non-Fibers" = "blue", "Non-Fibers" = "red", "TRWP" = "green")) +
  geom_label(data = sad_percent_Cowger, aes(x = 0, y = 2.8, label = "Wash Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = 1.65, y = 2.45, label = "Settling \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = -1.65, y = 2.45, label = "Rising \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = -3.75, y = 2.8, label = "Surface Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = 3.75, y = 2.8, label = "Bed Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") 

#################################################################################
#Now Sample Location plots

#Load & Filter Mean Datasets

#SDC (Fibers and Non-Fibers)
sdc <- read.csv("SDC_Rouse_Mean.csv")
sdc  <- sdc %>%  
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-5") %>%
  filter(Event != "SF-6") %>% 
  filter(Rouse_adj != "NA")
#SDC (Non-Fibers only)
sdc_nf <- read.csv("SDC_Rouse_Mean.csv")
sdc_nf  <- sdc_nf %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-5") %>%
  filter(Event != "SF-6") %>% 
  filter(X1st.Order.Morph!="Fiber") %>% 
  filter(Rouse_adj != "NA")

#SDC (TRWP only)
sdc_trwp <- read.csv("SDC_Rouse_TRWP_Mean.csv")
sdc_trwp  <- sdc_trwp %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-5") %>%
  filter(Event != "SF-6") %>% 
  filter(Rouse_adj != "NA")

#SAD (Fibers and Non-Fibers)
sad <- read.csv("SAD_Rouse_Mean.csv")
sad  <- sad %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-6") %>% 
  filter(Rouse_adj != "NA")
#SAD (Non-Fibers only)
sad_nf <- read.csv("SAD_Rouse_Mean.csv")
sad_nf  <- sad_nf %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-6") %>% 
  filter(X1st.Order.Morph!="Fiber") %>% 
  filter(Rouse_adj != "NA")
#SAD (TRWP only)
sad_trwp <- read.csv("SAD_Rouse_TRWP_Mean.csv")
sad_trwp  <- sad_trwp %>% 
  filter(!is.na(Test.Group)) %>%
  filter(Flow.Type=="Stormflow") %>%
  filter(Event != "SF-6") %>% 
  filter(Rouse_adj != "NA")


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
sdc_percent_Cowger <- sdc %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to each scenario and Test.Group
sdcnf_percent_Cowger <- sdc_nf %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to sdc trwp
sdctrwp_percent_Cowger <- sdc_trwp %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )


# Apply the function to each scenario and Test.Group
sad_percent_Cowger <- sad %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to each scenario and Test.Group
sadnf_percent_Cowger <- sad_nf %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )

# Apply the function to sdc trwp
sadtrwp_percent_Cowger <- sad_trwp %>%
  group_by(Test.Group) %>%
  summarise(
    Percent_Mean_Surface = calc_percentage(Rouse_adj, ranges[["surface load"]]),
    Percent_Mean_RiseSus = calc_percentage(Rouse_adj, ranges[["rising suspended load"]]),
    Percent_Mean_Wash = calc_percentage(Rouse_adj, ranges[["wash load"]]),
    Percent_Mean_SetSus = calc_percentage(Rouse_adj, ranges[["settling suspended load"]]),
    Percent_Mean_Bed = calc_percentage(Rouse_adj, ranges[["bed load"]]),
    Percent_Mean_Immob = calc_percentage(Rouse_adj, ranges[["immobile"]])
  )
########################################################################################

#Plot SDC 

custom_labels <- c(
  "Inside Thalweg" = "Thalweg",
  "Outside Thalweg_1" = "Nearest Right Bank"
)

p3 <- ggplot() + 
  
  # Second scenario
  geom_density(data = sdc_nf, aes(x = Rouse_adj, fill = "Non-Fibers"), alpha = 0.4) +
  
  # First scenario
  geom_density(data = sdc, aes(x = Rouse_adj, fill = "Fibers and Non-Fibers"), alpha = 0.4) +
  
  
  # Third scenario
  geom_density(data = sdc_trwp, aes(x = Rouse_adj, fill = "TRWP"), alpha = 0.4) +
  
  facet_wrap(~Test.Group, scales = "fixed", ncol = 1, labeller = labeller(Test.Group = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "darkgray") +
  
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_y_continuous(limits=c(0.0,6),breaks = seq(0.0, 6.0, by = 1.0)) +
  labs(
    x="Rouse Numbers",
    y="Density (%)") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, face = "bold"),     # Increase x-axis title size
    axis.title.y = element_text(size = 14, face = "bold"),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold") # Increase title size
  ) +
  scale_fill_manual(name = "Dataset", values = c("Fibers and Non-Fibers" = "blue", "Non-Fibers" = "red", "TRWP" = "green")) +
  geom_label(data = sdc_percent_Cowger, aes(x = 0, y = 5.8, label = "Wash Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = 1.65, y = 5.2, label = "Settling \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = -1.65, y = 5.2, label = "Rising \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = -3.75, y = 5.8, label = "Surface Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sdc_percent_Cowger, aes(x = 3.75, y = 5.8, label = "Bed Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") 

########################################################################################

#Plot SAD

custom_labels <- c(
  "Inside Thalweg" = "Thalweg",
  "Outside Thalweg_1" = "Nearest Right Bank",
  "Outside Thalweg_2" = "Nearest Left Bank"
)

p4 <- ggplot() + 
  # Second scenario
  geom_density(data = sad_nf, aes(x = Rouse_adj, fill = "Non-Fibers"), alpha = 0.4) +
  
  
  # First scenario
  geom_density(data = sad, aes(x = Rouse_adj, fill = "Fibers and Non-Fibers"), alpha = 0.4) +
  
  # Third scenario
  geom_density(data = sad_trwp, aes(x = Rouse_adj, fill = "TRWP"), alpha = 0.4) +
  
  facet_wrap(~Test.Group, scales = "fixed", ncol = 1, labeller = labeller(Test.Group = custom_labels)) +
  
  geom_vline(xintercept = c(-2.5, 0.8, -0.8, 2.5, 7.5), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = c(-1.8, -0.6), linetype = "dashed", color = "darkgray") +
  
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 2.5)) +
  scale_y_continuous(limits=c(0,3),breaks = seq(0, 3, by = 1.0)) +
  labs(
    x="Rouse Numbers",
    y="") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14, face = "bold"),     # Increase x-axis title size
    axis.title.y = element_text(size = 14, face = "bold"),     # Increase y-axis title size
    axis.text.x = element_text(size = 12),      # Increase x-axis label size
    axis.text.y = element_text(size = 12),      # Increase y-axis label size
    strip.text = element_text(size = 14),       # Increase facet label size
    plot.title = element_text(size = 16, face = "bold") # Increase title size
  ) +
  scale_fill_manual(name = "Dataset", values = c("Fibers and Non-Fibers" = "blue", "Non-Fibers" = "red", "TRWP" = "green")) +
  geom_label(data = sad_percent_Cowger, aes(x = 0, y = 2.8, label = "Wash Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = 1.65, y = 2.25, label = "Settling \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = -1.65, y = 2.25, label = "Rising \nSuspended \nLoad"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = -3.75, y = 2.8, label = "Surface Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") +
  geom_label(data = sad_percent_Cowger, aes(x = 3.75, y = 2.8, label = "Bed Load"), size = 3.2, color = "black", fill = "white", fontface = "bold") 

p1 <- p1+theme(
  legend.position = "none")
p2 <- p2+theme(
  legend.position = "none")
p3 <- p3+theme(
  legend.position = "none")
p4 <- p4+theme(
  legend.position = "none")

#repeat plot for retaining legend
p5 <- ggplot() + 
  # First scenario
  geom_density(data = sad, aes(x = Rouse_adj, fill = "Fibers and Non-Fibers"), alpha = 0.4) +
  # Second scenario
  geom_density(data = sad_nf, aes(x = Rouse_adj, fill = "Non-Fibers"), alpha = 0.4) +
  # Third scenario
  geom_density(data = sad_trwp, aes(x = Rouse_adj, fill = "TRWP"), alpha = 0.4) +
  facet_wrap(~Test.Group, scales = "fixed", ncol = 1, labeller = labeller(Test.Group = custom_labels)) +
  scale_fill_manual(name = "Dataset", values = c("Fibers and Non-Fibers" = "blue", "Non-Fibers" = "red", "TRWP" = "green"))+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14, face="bold"))  # Increase title size


########################################################################################
########################################################################################

########################################################################################
#Combine Plots for Figure
########################################################################################


# Combine the plots into a 2x2 layout
combined_plot <- (p1 | p2) / (p3 | p4) +  plot_layout(guides = "collect") & theme(
  legend.position = "bottom",
  legend.text = element_text(size = 14),      # Increase legend text size
  legend.title = element_text(size = 14, face="bold"))  # Increase title size

# Print the combined plot
print(combined_plot)

# Save the final combined plot
ggsave("final_combined_plot.png", plot = combined_plot, width = 10, height = 7.5)
