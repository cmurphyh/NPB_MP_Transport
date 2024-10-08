
# Clear the R environment
rm(list=ls())
graphics.off()
cat("\f")

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(grid)
library(tibble)
library(patchwork)
library(cowplot)



# Set Working Directory
setwd("~/Appendix A/conc_comp")

# Load data
SDC_AllEx <- read.csv("SDC_All_Extrap.csv")
SAD_AllEx <- read.csv("SAD_All_Extrap.csv")

#create Non-Fiber only datasets
SDC_AllEx_NF <- SDC_AllEx  %>% 
  filter(X1st.Order.Morph!= "Fiber")
SAD_AllEx_NF <- SAD_AllEx  %>% 
  filter(X1st.Order.Morph!= "Fiber")

#create Fiber only datasets
SDC_AllEx_F <- SDC_AllEx  %>% 
  filter(X1st.Order.Morph!= "Non-Fiber")
SAD_AllEx_F <- SAD_AllEx  %>% 
  filter(X1st.Order.Morph!= "Non-Fiber")


# Helper function to calculate percentages
calculate_percentages <- function(table) {
  apply(table, 2, function(x) {x * 100 / sum(x, na.rm = TRUE)})
}

########################################################################################
#Create SDC Plots
########################################################################################

# San Diego Creek: 1st Order Morphology
table_sdc <- table(SDC_AllEx$X1st.Order.Morph, SDC_AllEx$Event)
table_sdc <- calculate_percentages(table_sdc)
write.csv(table_sdc, file = "1st Order SDC.csv")

# Convert table to data frame for ggplot
df_sdc <- as.data.frame(as.table(table_sdc))
colnames(df_sdc) <- c("Morphology", "Event", "Percentage")

par(mar=c(1,5,2,1))

# Plot San Diego Creek: 1st Order Morphology
p1 <-ggplot(df_sdc, aes(x = Event, y = Percentage, fill = Morphology)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Composition [%]", title = "San Diego Creek", subtitle = "1st Order Morphology", fill = "1st Order 
Morphology") +
  scale_fill_manual(values = c("#88CCEE", "#44AA99")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 50)) +  # Set x-axis limits and breaks +
  theme_minimal() +
  theme(legend.position="right")
###############################################################################################################
# San Diego Creek: Detailed Morphology
table_sdc_morph <- table(SDC_AllEx$Morphology, SDC_AllEx$Event)
table_sdc_morph <- rbind(table_sdc_morph[1:1,], table_sdc_morph[2:2,], 
                         table_sdc_morph[5:5,], table_sdc_morph[7:7,], 
                         table_sdc_morph[4:4,], table_sdc_morph[3:3,], 
                         table_sdc_morph[6:6,])
rownames(table_sdc_morph) <- c("Fiber", "Fiber Bundle", "Fragment", "TRWP", "Foam", "Film", "Sphere")
#SDC all morphologies and Save
table_sdc_morph <- calculate_percentages(table_sdc_morph)
write.csv(table_sdc_morph, file = "Morph_SDC.csv")

# SDC NF morphologies only for plotting
table_sdc_morph_nf <- table_sdc_morph[3:7,]
table_sdc_morph_nf <- calculate_percentages(table_sdc_morph_nf)
write.csv(table_sdc_morph_nf, file = "Morph_SDC_nf.csv")

# Convert Non-Fiber Morphologies Only table to data frame for ggplot
df_sdc_morph_nf <- as.data.frame(as.table(table_sdc_morph_nf))
colnames(df_sdc_morph_nf) <- c("Morphology", "Event", "Percentage")

# Plot San Diego Creek: Non-Fiber Morphologies Only
p2 <- ggplot(df_sdc_morph_nf, aes(x = Event, y = Percentage, fill = Morphology)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]", subtitle = "Morphology (Non-Fibers)", fill = "Non-Fiber 
Morphology") +
  scale_fill_manual(values = c("#CC6677", "#661100", "#DDCC77", "#117733", "#332288")) +
  theme_minimal()  +
  theme(legend.position="right")
###############################################################################################################

#scale_fill_manual(name = "Color-Groups", values = c("Black"="#999999", "Blue" = "#0072B2", "Green" = "#009E73", "White" ="white", "Transparent" ="#CC79A7", "Red" = "#D55E00", "Yellow" = "#F0E442")) +



# San Diego Creek: Color Groups Composition (Non-Fibers)
table_color <- table(SDC_AllEx_NF$Color.Group, SDC_AllEx_NF$Event)
table_color <- calculate_percentages(table_color)
#Re-order SDC Color-Groups and Save
table_color <- rbind(table_color[1:1,], table_color[2:2,], 
                     table_color[3:3,], table_color[6:6,], 
                     table_color[5:5,], table_color[4:4,], 
                     table_color[7:7,])
rownames(table_color) <- c("Black", "Blue", "Green", "White", "Transparent", "Red", "Yellow")
write.csv(table_color, file = "Color_SDC_NF.csv")

# Convert table to data frame for ggplot
df_color <- as.data.frame(as.table(table_color))
colnames(df_color) <- c("Color", "Event", "Percentage")

# Plot San Diego Creek: Color Groups Composition (Non-Fibers)
p3 <- ggplot(df_color, aes(x = Event, y = Percentage, fill = Color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]", subtitle = "Color (Non-Fibers)", fill = "Non-Fiber
Color-Group") +
  scale_fill_manual(values = c("#999999", "#0072B2", "#009E73", "white", "#CC79A7", "#D55E00", "#F0E442")) +
  theme_minimal() +
  theme(legend.position="right")

###############################################################################################################

# San Diego Creek: Color Groups Composition (Fibers)
table_color <- table(SDC_AllEx_F$Color.Group, SDC_AllEx_F$Event)
table_color <- calculate_percentages(table_color)
#Re-order SDC Color-Groups and Save
table_color <- rbind(table_color[1:1,], table_color[2:2,], table_color[4:4,], table_color[5:5,], 
                     table_color[3:3,], table_color[6:6,], 
                     table_color[7:7,])
rownames(table_color) <- c("Black", "Blue", "Red","Transparent","Green", "White", "Yellow")
write.csv(table_color, file = "Color_SDC_F.csv")

# Convert table to data frame for ggplot
df_color <- as.data.frame(as.table(table_color))
colnames(df_color) <- c("Color", "Event", "Percentage")

# Plot San Diego Creek: Color Groups Composition (Non-Fibers)
p4 <- ggplot(df_color, aes(x = Event, y = Percentage, fill = Color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]", subtitle = "Color (Fibers)",fill = "Fiber
Color-Group") +
  scale_fill_manual(values = c("#999999", "#0072B2", "#D55E00", "#CC79A7", "#009E73", "white",   "#F0E442")) +
  theme_minimal() +
  theme(legend.position="right") 



########################################################################################
#Create SAD Plots
########################################################################################

# Santa Ana-Delhi : 1st Order Morphology
table_sad <- table(SAD_AllEx$X1st.Order.Morph, SAD_AllEx$Event)
table_sad <- calculate_percentages(table_sad)
write.csv(table_sad, file = "1st Order SAD.csv")

# Convert table to data frame for ggplot
df_sad <- as.data.frame(as.table(table_sad))
colnames(df_sad) <- c("Morphology", "Event", "Percentage")

par(mar=c(1,5,2,1))

# Plot Santa Ana-Delhi : 1st Order Morphology
p5 <-ggplot(df_sad, aes(x = Event, y = Percentage, fill = Morphology)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Composition [%]", title = "Santa Ana-Delhi", fill = "1st Order 
Morphology") +
  scale_fill_manual(values = c("#88CCEE", "#44AA99")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 50)) +  # Set x-axis limits and breaks +
  theme_minimal() +
  theme(legend.position="right")

###############################################################################################################
# Santa Ana-Delhi : Detailed Morphology
table_sad_morph <- table(SAD_AllEx$Morphology, SAD_AllEx$Event)
table_sad_morph <- rbind(table_sad_morph[1:1,], table_sad_morph[2:2,], 
                         table_sad_morph[5:5,], table_sad_morph[8:8,], 
                         table_sad_morph[4:4,], table_sad_morph[3:3,], 
                         table_sad_morph[7:7,], table_sad_morph[6:6,])
rownames(table_sad_morph) <- c("Fiber", "Fiber Bundle", "Fragment", "TRWP", "Foam", "Film", "Sphere","Pellet")
#SAD all morphologies and Save
table_sad_morph <- calculate_percentages(table_sad_morph)
write.csv(table_sad_morph, file = "Morph_SAD.csv")

# SAD NF morphologies only for plotting
table_sad_morph_nf <- table_sad_morph[3:8,]
table_sad_morph_nf <- calculate_percentages(table_sad_morph_nf)
write.csv(table_sad_morph_nf, file = "Morph_SAD_nf.csv")

# Convert Non-Fiber Morphologies Only table to data frame for ggplot
df_sad_morph_nf <- as.data.frame(as.table(table_sad_morph_nf))
colnames(df_sad_morph_nf) <- c("Morphology", "Event", "Percentage")

# Plot Santa Ana-Delhi : Non-Fiber Morphologies Only
p6 <- ggplot(df_sad_morph_nf, aes(x = Event, y = Percentage, fill = Morphology)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]",fill = "Non-Fiber 
Morphology") +
  scale_fill_manual(values = c("#CC6677", "#661100", "#DDCC77", "#117733", "#332288", "#999933")) +
  theme_minimal() +
  theme(legend.position="right") 
###############################################################################################################

#scale_fill_manual(name = "Color-Groups", values = c("Black"="#999999", "Blue" = "#0072B2", "Green" = "#009E73", "White" ="white", "Transparent" ="#CC79A7", "Red" = "#D55E00", "Yellow" = "#F0E442")) +



# Santa Ana-Delhi : Color Groups Composition (Non-Fibers)
table_color <- table(SAD_AllEx_NF$Color.Group, SAD_AllEx_NF$Event)
table_color <- calculate_percentages(table_color)
#Re-order SAD Color-Groups and Save
table_color <- rbind(table_color[1:1,], table_color[2:2,], 
                     table_color[3:3,], table_color[6:6,], 
                     table_color[5:5,], table_color[4:4,], 
                     table_color[7:7,])
rownames(table_color) <- c("Black", "Blue", "Green", "White", "Transparent", "Red", "Yellow")
write.csv(table_color, file = "Color_SAD_NF.csv")

# Convert table to data frame for ggplot
df_color <- as.data.frame(as.table(table_color))
colnames(df_color) <- c("Color", "Event", "Percentage")

# Plot Santa Ana-Delhi : Color Groups Composition (Non-Fibers)
p7 <- ggplot(df_color, aes(x = Event, y = Percentage, fill = Color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]",fill = "Non-Fiber
Color-Group") +
  scale_fill_manual(values = c("#999999", "#0072B2", "#009E73", "white", "#CC79A7", "#D55E00", "#F0E442")) +
  theme_minimal()+
  theme(legend.position="right") 
###############################################################################################################

# Santa Ana-Delhi : Color Groups Composition (Fibers)
table_color <- table(SAD_AllEx_F$Color.Group, SAD_AllEx_F$Event)
table_color <- calculate_percentages(table_color)
#Re-order SAD Color-Groups and Save
table_color <- rbind(table_color[1:1,], table_color[2:2,], table_color[4:4,], table_color[5:5,], 
                     table_color[3:3,], table_color[6:6,], 
                     table_color[7:7,])
rownames(table_color) <- c("Black", "Blue", "Red","Transparent","Green", "White", "Yellow")
write.csv(table_color, file = "Color_SAD_F.csv")

# Convert table to data frame for ggplot
df_color <- as.data.frame(as.table(table_color))
colnames(df_color) <- c("Color", "Event", "Percentage")

# Plot Santa Ana Delhi: Color Groups Composition (Non-Fibers)
p8 <- ggplot(df_color, aes(x = Event, y = Percentage, fill = Color)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(y = "Composition [%]",fill = "Color-Group") +
  scale_fill_manual(values = c("#999999", "#0072B2", "#D55E00", "#CC79A7", "#009E73", "white",   "#F0E442")) +
  theme_minimal() +
  theme(legend.position="right") 


########################################################################################
#Combine Plots for Figure
########################################################################################
p1
p2
p3
p4
p5
p6
p7
p8


a <- p1+theme(
  axis.title.x = element_blank(),  # Remove y-axis label
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_blank(),      # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2), 
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "none") + 
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 20, 
  fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 10, label = "a", size = 5.5, fontface = "bold")

c <- p2+theme(
axis.title.x = element_blank(),  # Remove y-axis label
legend.text = element_text(size = 12),      # Increase legend text size
legend.title = element_text(size = 14),     # Increase legend title size
axis.title.y = element_text(size = 14),     # Increase y-axis title size
axis.text.x = element_blank(),      # Increase x-axis label size
axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
strip.text = element_text(size = 14),       # Increase facet label size
plot.title = element_text(size = 16, face = "bold"), # Increase title size
plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
panel.grid.major = element_blank(),  # Remove major vertical grid lines
panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
axis.ticks.y = element_line(color = "black"),
  legend.position = "none") +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 12, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 6, label = "c", size = 5.5, fontface = "bold")

e <- p3+theme(
  axis.title.x = element_blank(),  # Remove y-axis label
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_blank(),      # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold", hjust=0.5), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "none") +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 14, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 7, label = "e", size = 5.5, fontface = "bold")

g <- p4+theme(
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.x = element_blank(),     # Increase x-axis title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_text(size = 12, color= "black",angle = 90, hjust = 1),       # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold"), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "none") +
  annotate("rect", xmin = 0.7, xmax = 1.4, ymin = 0, ymax = 14, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 8, label = "g", size = 5.5, fontface = "bold")

b <- p5+theme(
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),
  axis.title.x = element_blank(),  # Remove y-axis label
  axis.title.y = element_blank(),     # Increase y-axis title size
  axis.text.x = element_blank(),      # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2), 
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "right") + 
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 20, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 10, label = "b", size = 5.5, fontface = "bold")

d <- p6 + theme(
  axis.title.x = element_blank(),  # Remove y-axis label
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_blank(),      # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold"), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "right") +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 12, 
  fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 6, label = "d", size = 5.5, fontface = "bold")

f <- p7+theme(
  axis.title.x = element_blank(),  # Remove y-axis label
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_blank(),      # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold", hjust=0.5), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = "none") +
annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 14, 
fill = "white", color = "black") +  # Draw the white box with a black border
annotate("text", x = 1, y = 8, label = "f", size = 5.5, fontface = "bold") 

h <- p8+theme(
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.x = element_blank(),     # Increase x-axis title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_text(size = 12, color= "black",angle = 90, hjust = 1),       # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  strip.text = element_text(size = 14),       # Increase facet label size
  plot.title = element_text(size = 16, face = "bold"), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  panel.grid.major = element_blank(),  # Remove major vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor vertical grid lines
  axis.ticks.y = element_line(color = "black"),
  legend.position = c(1.45,1))+
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0, ymax = 14, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 1, y = 8, label = "h", size = 5.5, fontface = "bold") 

# Create a list of plots to arrange
plots <- list(a, b, c, d, e, f, g, h)

# Define the layout matrix
layout_matrix <- "
ab
cd
ef
gh
"

# Combine plots with specified layout
combined_plot <- wrap_plots(plots, design = layout_matrix) +
  plot_layout(heights = c(0.8, 1.5, 1, 1.1), # Heights of the two rows
              widths = c(3.3, 2.4)) 



# Print the combined plot
print(combined_plot)

# Save the final combined plot
ggsave("figure3_combined.png", plot = combined_plot)

