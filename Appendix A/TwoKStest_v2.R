# Testing for Signifigance between Particle Size Datasets

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

####SET WORKING DIRECTORY
setwd("~/Appendix A/ks2")


library(dplyr)
library(tibble)
###############################
###############################

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

##############################
#1.) Streamflow SDC data 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (NOTE Filter was based on Feret diameters!)
#SDC Lowflow data
SDC_LF <-read.csv("SDC-LF_NF_330_5.csv")  #read in data of all LF sizes
#SDC Stormflow data
SDC_SF <-read.csv("SDC-SF_NF_330_5_Monly.csv")  #read in data excluding SF-5 and SF-6

#Re-filter data to restrict dataset to MinFeret > 330um
SDC_LF <- SDC_LF %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

#Re-filter data to restrict dataset to MinFeret > 330um
SDC_SF <- SDC_SF %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

#Summarize and Save Feret and MinFeret data for testing

sum_LF_a <-summary(SDC_LF$Feret)
LF_a<-SDC_LF$Feret
print("Non-Fiber LF a-axis (Feret) Summary")
sum_LF_a

sum_LF_b <-summary(SDC_LF$MinFeret)
LF_b<-SDC_LF$MinFeret
print("Non-Fiber LF b-axis (MinFeret) Summary")
sum_LF_b

sum_SF_a <-summary(SDC_SF$Feret)
SF_a<-SDC_SF$Feret
print("Non-Fiber SF a-axis (Feret) Summary")
sum_SF_a

sum_SF_b <-summary(SDC_SF$MinFeret)
SF_b<-SDC_SF$MinFeret
print("Non-Fiber SF b-axis (MinFeret) Summary")
sum_SF_b


#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("SDC a-axis CDFs: LF vs SF")
ks.test(LF_a, SF_a)
length(LF_a)
length(SF_a)

#b-axis
print("SDC b-axis CDFs: LF vs SF")
ks.test(LF_b, SF_b)
length(LF_b)
length(SF_b)

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)

# Sort the data
sizes1a <- sort(LF_a)
sizes2a <- sort(SF_a)
sizes1b<- sort(LF_b)
sizes2b <- sort(SF_b)

# Calculate cumulative proportions
cumulative_proportions1a <- seq_along(sizes1a) / length(sizes1a)
cumulative_proportions2a <- seq_along(sizes2a) / length(sizes2a)
cumulative_proportions1b <- seq_along(sizes1b) / length(sizes1b)
cumulative_proportions2b <- seq_along(sizes2b) / length(sizes2b)

# Plot cumulative size distributions Feret (a-axis)
plot(sizes1a, cumulative_proportions1a, type = "l", lwd = 3, lty = 1, col = "#009E73",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SDC CDFs (a-axis): LF vs SF",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizes2a, cumulative_proportions2a, lwd = 3, lty = 1, col = "#CC79A7")

# Add legend
legend("bottomright", legend = c("Low-Flow", "Stormflow"), 
       col = c("#009E73", "#CC79A7"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'a', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.13, p-value = 3.8e-13',col='black', cex=0.9)

# Plot cumulative size distributions Min-Feret (b-axis)
plot(sizes1b, cumulative_proportions1b, type = "l", lwd = 3, lty = 1, col = "#009E73",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SDC CDFs (b-axis): LF vs SF",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizes2b, cumulative_proportions2b, lwd = 3, lty = 1, col = "#CC79A7")

# Add legend
legend("bottomright", legend = c("Low-Flow", "Stormflow"), 
       col = c("#009E73", "#CC79A7"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'b', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.12, p-value = 4.0e-12',col='black', cex=0.9)


##############################
#2.) Streamflow SAD data 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (NOTE Filter was based on Feret diameters!)
#SDC Lowflow data
SAD_LF <-read.csv("SAD-LF_NF_330_5.csv")  #read in data of all LF sizes
#SDC Stormflow data
SAD_SF <-read.csv("SAD-SF_NF_330_5_Monly.csv")  #read in data excluding SF-5 and SF-6

#Re-filter data to restrict dataset to MinFeret > 330um
SAD_LF <- SAD_LF %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

#Re-filter data to restrict dataset to MinFeret > 330um
SAD_SF <- SAD_SF %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")


#Summarize and Save Feret and MinFeret data for testing

sum_LF_a <-summary(SAD_LF$Feret)
LF_a<-SAD_LF$Feret
print("Non-Fiber LF a-axis (Feret) Summary")
sum_LF_a

sum_LF_b <-summary(SAD_LF$MinFeret)
LF_b<-SAD_LF$MinFeret
print("Non-Fiber LF b-axis (MinFeret) Summary")
sum_LF_b

sum_SF_a <-summary(SAD_SF$Feret)
SF_a<-SAD_SF$Feret
print("Non-Fiber SF a-axis (Feret) Summary")
sum_SF_a

sum_SF_b <-summary(SAD_SF$MinFeret)
SF_b<-SAD_SF$MinFeret
print("Non-Fiber SF b-axis (MinFeret) Summary")
sum_SF_b


#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("SAD a-axis CDFs: LF vs SF")
ks.test(LF_a, SF_a)
length(LF_a)
length(SF_a)

#b-axis
print("SAD b-axis CDFs: LF vs SF")
ks.test(LF_b, SF_b)
length(LF_b)
length(SF_b)

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)

# Sort the data
sizes1a <- sort(LF_a)
sizes2a <- sort(SF_a)
sizes1b<- sort(LF_b)
sizes2b <- sort(SF_b)

# Calculate cumulative proportions
cumulative_proportions1a <- seq_along(sizes1a) / length(sizes1a)
cumulative_proportions2a <- seq_along(sizes2a) / length(sizes2a)
cumulative_proportions1b <- seq_along(sizes1b) / length(sizes1b)
cumulative_proportions2b <- seq_along(sizes2b) / length(sizes2b)

# Plot cumulative size distributions Feret (a-axis)
plot(sizes1a, cumulative_proportions1a, type = "l", lwd = 3, lty = 1, col = "#009E73",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SAD CDFs (a-axis): LF vs SF",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizes2a, cumulative_proportions2a, lwd = 3, lty = 1, col = "#CC79A7")

# Add legend
legend("bottomright", legend = c("Low-Flow", "Stormflow"), 
       col = c("#009E73", "#CC79A7"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'c', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.04, p-value = 0.27',col='black', cex=0.9)

# Plot cumulative size distributions Min-Feret (b-axis)
plot(sizes1b, cumulative_proportions1b, type = "l", lwd = 3, lty = 1, col = "#009E73",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SAD CDFs (b-axis): LF vs SF", 
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizes2b, cumulative_proportions2b, lwd = 3, lty = 1, col = "#CC79A7")

# Add legend
legend("bottomright", legend = c("Low-Flow", "Stormflow"), 
       col = c("#009E73", "#CC79A7"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'd', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.10, p-value = 2.6e-05',col='black', cex=0.9)


##############################
#3.) Streamflow Site Compare (All Samples 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (feret diameters)
x_a <- SDC_LF$Feret
x_b <- SDC_LF$MinFeret

y_a <- SAD_LF$Feret
y_b <- SAD_LF$MinFeret

#Summarize and Save Feret and MinFeret data for testing

sum_SDC_LF_a <-summary(x_a)
print("Non-Fiber SDC LF a-axis (Feret) Summary")
sum_SDC_LF_a
length(x_a)

sum_SDC_LF_b <-summary(x_b)
print("Non-Fiber SDC LF b-axis (MinFeret) Summary")
sum_SDC_LF_b
length(x_b)

sum_SAD_LF_a <-summary(y_a)
print("Non-Fiber SAD LF a-axis (Feret) Summary")
sum_SAD_LF_a
length(y_a)

sum_SAD_LF_b <-summary(y_b)
print("Non-Fiber SAD LF b-axis (MinFeret) Summary")
sum_SAD_LF_b
length(y_b)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("Low Flow a-axis CDFs: SDC vs SAD")
ks.test(x_a, y_a)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#b-axis
print("Low Flow b-axis CDFs: SDC vs SAD")
ks.test(x_b, y_b)

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)

# Sort the data
sizesxa <- sort(x_a)
sizesya <- sort(y_a)
sizesxb<- sort(x_b)
sizesyb <- sort(y_b)

# Calculate cumulative proportions
cumulative_proportionsxa <- seq_along(sizesxa) / length(sizesxa)
cumulative_proportionsya <- seq_along(sizesya) / length(sizesya)
cumulative_proportionsxb <- seq_along(sizesxb) / length(sizesxb)
cumulative_proportionsyb <- seq_along(sizesyb) / length(sizesyb)

# Plot cumulative size distributions Feret (a-axis)
plot(sizesxa, cumulative_proportionsxa, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "Low Flow CDFs (a-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesya, cumulative_proportionsya, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'c', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.10, p-value = 0.0013',col='black', cex=0.9)

# Plot cumulative size distributions MinFeret (b-axis)
plot(sizesxb, cumulative_proportionsxb, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "Low Flow CDFs (b-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesyb, cumulative_proportionsyb, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'd', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.063, p-value = 0.083',col='black', cex=0.9)





##############################
#4.) Streamflow Site Compare (All Samples 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (feret diameters)
x_a <- SDC_SF$Feret
x_b <- SDC_SF$MinFeret

y_a <- SAD_SF$Feret
y_b <- SAD_SF$MinFeret

#Summarize and Save Feret and MinFeret data for testing

sum_SDC_SF_a <-summary(x_a)
print("Non-Fiber SDC SF a-axis (Feret) Summary")
sum_SDC_SF_a
length(x_a)

sum_SDC_SF_b <-summary(x_b)
print("Non-Fiber SDC SF b-axis (MinFeret) Summary")
sum_SDC_SF_b
length(x_b)

sum_SAD_SF_a <-summary(y_a)
print("Non-Fiber SAD SF a-axis (Feret) Summary")
sum_SAD_SF_a
length(y_a)

sum_SAD_SF_b <-summary(y_b)
print("Non-Fiber SAD SF b-axis (MinFeret) Summary")
sum_SAD_SF_b
length(y_b)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("Stormflow a-axis CDFs: SDC vs SAD")
ks.test(x_a, y_a)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#b-axis
print("Stormflow b-axis CDFs: SDC vs SAD")
ks.test(x_b, y_b)

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)

# Sort the data
sizesxa <- sort(x_a)
sizesya <- sort(y_a)
sizesxb<- sort(x_b)
sizesyb <- sort(y_b)

# Calculate cumulative proportions
cumulative_proportionsxa <- seq_along(sizesxa) / length(sizesxa)
cumulative_proportionsya <- seq_along(sizesya) / length(sizesya)
cumulative_proportionsxb <- seq_along(sizesxb) / length(sizesxb)
cumulative_proportionsyb <- seq_along(sizesyb) / length(sizesyb)

# Plot cumulative size distributions Feret (a-axis)
plot(sizesxa, cumulative_proportionsxa, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "Stormflow CDFs (a-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesya, cumulative_proportionsya, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'e', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.023, p-value = 0.064',col='black', cex=0.9)

# Plot cumulative size distributions MinFeret (b-axis)
plot(sizesxb, cumulative_proportionsxb, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "Stormflow CDFs (b-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesyb, cumulative_proportionsyb, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'f', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.042, p-value = 1.9e-05',col='black', cex=0.9)




##############################
#4.) Streamflow Site Compare (All Samples 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (feret diameters)
x_a <- c(SDC_LF$Feret,SDC_SF$Feret)
x_b <- c(SDC_LF$MinFeret,SDC_SF$MinFeret)

y_a <- c(SAD_LF$Feret,SAD_SF$Feret)
y_b <- c(SAD_LF$MinFeret,SAD_SF$MinFeret)


#Summarize and Save Feret and MinFeret data for testing

sum_SDC_a <-summary(x_a)
print("Non-Fiber SDC a-axis (Feret) Summary")
sum_SDC_a
length(x_a)

sum_SDC_b <-summary(x_b)
print("Non-Fiber SDC b-axis (MinFeret) Summary")
sum_SDC_b
length(x_b)

sum_SAD_a <-summary(y_a)
print("Non-Fiber SAD a-axis (Feret) Summary")
sum_SAD_a
length(y_a)

sum_SAD_b <-summary(y_b)
print("Non-Fiber SAD b-axis (MinFeret) Summary")
sum_SAD_b
length(y_b)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("All Particles a-axis CDFs: SDC vs SAD")
ks.test(x_a, y_a)

#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#b-axis
print("All Particles b-axis CDFs: SDC vs SAD")
ks.test(x_b, y_b)

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)

# Sort the data
sizesxa <- sort(x_a)
sizesya <- sort(y_a)
sizesxb<- sort(x_b)
sizesyb <- sort(y_b)

# Calculate cumulative proportions
cumulative_proportionsxa <- seq_along(sizesxa) / length(sizesxa)
cumulative_proportionsya <- seq_along(sizesya) / length(sizesya)
cumulative_proportionsxb <- seq_along(sizesxb) / length(sizesxb)
cumulative_proportionsyb <- seq_along(sizesyb) / length(sizesyb)

# Plot cumulative size distributions Feret (a-axis)
plot(sizesxa, cumulative_proportionsxa, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "All Particles CDFs (a-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesya, cumulative_proportionsya, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'a', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.025, p-value = 0.019',col='black', cex=0.9)

# Plot cumulative size distributions MinFeret (b-axis)
plot(sizesxb, cumulative_proportionsxb, type = "l", lwd = 3, lty = 1, col = "#1A85FF",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "All Particles CDFs (b-axis): SDC vs SAD",
     xlim = range(0.3,5), ylim = c(0, 1))
lines(sizesyb, cumulative_proportionsyb, lwd = 3, lty = 1, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0.25, 0.9, 0.75, 1, col='white', border='black')
text(0.5, 0.95, 'b', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3,0.5,'D = 0.036, p-value = 2.0e-04',col='black', cex=0.9)

