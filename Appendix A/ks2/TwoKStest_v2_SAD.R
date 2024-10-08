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
#1.) Streamflow Sample by Sample (sample compare for 330um-5mm)
###############################
#Load Non-Fiber Datasets with data restricted to >330 to 5 mm (feret diameters)

#LF Data
DF1 <-read.csv("SAD-DF-1_NF_330_5.csv") 
DF2 <-read.csv("SAD-DF-2_NF_330_5.csv") 
DF3 <-read.csv("SAD-DF-3_NF_330_5.csv") 
BF4 <-read.csv("SAD-BF-4_NF_330_5.csv") 

#SF Data
SF1 <-read.csv("SAD-SF-1_NF_330_5.csv")
SF2 <-read.csv("SAD-SF-2_NF_330_5.csv")
SF4 <-read.csv("SAD-SF-4_NF_330_5.csv")


#Re-filter data to restrict dataset to MinFeret > 330um
DF1 <- DF1 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

DF2 <- DF2 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

DF3 <- DF3 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

BF4 <- BF4 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

# Load SF data

SF1 <- SF1 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF2 <- SF2 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF4 <- SF4 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")


#Select LF (x) and SF (y) datasets for comparision and summarize data

x <- DF1  # CHANGE DATA HERE
y <- SF1   # CHANGE DATA HERE

  
  #Summarize and Save Feret and MinFeret data for testing
  
sum_x_a <-summary(x$Feret)
x_a<-x$Feret
print("Non-Fiber LF a-axis (Feret) Summary")
sum_x_a

sum_x_b <-summary(x$MinFeret)
x_b<-x$MinFeret
print("Non-Fiber LF b-axis (MinFeret) Summary")
sum_x_b

sum_y_a <-summary(y$Feret)
y_a<-y$Feret
print("Non-Fiber SF a-axis (Feret) Summary")
sum_y_a

sum_y_b <-summary(y$MinFeret)
y_b<-y$MinFeret
print("Non-Fiber SF b-axis (MinFeret) Summary")
sum_y_b


#Testing Size Differences by Flow Type using Two sample Kolmogorov-Smirnov test")
#a-axis
print("SAD a-axis CDFs: LF vs SF")
ks.test(x_a, y_a)
length(x_a)
length(y_a)

#b-axis
print("SAD b-axis CDFs: LF vs SF")
ks.test(x_b, y_b)
length(x_b)
length(y_b)  


#With Monte Carlo
ks.test(x_a, y_a,
        alternative = "two.sided",
        exact = NULL, simulate.p.value=TRUE, B=2000)

ks.test(x_b, y_b,
        alternative = "two.sided",
        exact = NULL, simulate.p.value=TRUE, B=2000)

###############################
###############################
#Multiplots SAD

#Create Plots

#Plot Layout
layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (Feret diameters)

BF4_a<-BF4$Feret

DF1_a<-DF1$Feret
DF2_a<-DF2$Feret
DF3_a<-DF3$Feret

SF1_a<-SF1$Feret
SF2_a<-SF2$Feret
SF4_a<-SF4$Feret
#Create Plots

# Sort the data
sizesBF4_a <- sort(BF4_a)

sizesDF1_a <- sort(DF1_a)
sizesDF2_a <- sort(DF2_a)
sizesDF3_a <- sort(DF3_a)

sizesSF1_a <- sort(SF1_a)
sizesSF2_a <- sort(SF2_a)
sizesSF4_a <- sort(SF4_a)

# Calculate cumulative proportions Low Flow BF
cumulative_proportionsBF4_a <- seq_along(sizesBF4_a) / length(sizesBF4_a)
# Calculate cumulative proportions Low Flow DF
cumulative_proportionsDF1_a <- seq_along(sizesDF1_a) / length(sizesDF1_a)
cumulative_proportionsDF2_a <- seq_along(sizesDF2_a) / length(sizesDF2_a)
cumulative_proportionsDF3_a <- seq_along(sizesDF3_a) / length(sizesDF3_a)
# Calculate cumulative proportions Stormflow
cumulative_proportionsSF1_a <- seq_along(sizesSF1_a) / length(sizesSF1_a)
cumulative_proportionsSF2_a <- seq_along(sizesSF2_a) / length(sizesSF2_a)
cumulative_proportionsSF4_a <- seq_along(sizesSF4_a) / length(sizesSF4_a)
###############################
#SADs
# Plot cumulative size distributions
plot(sizesDF1_a, lwd=3, cumulative_proportionsDF1_a, type = "l", col = "darkgreen",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SAD CDFs (a-axis): SF vs LF Events",
     xlim = range(c(0,5)), ylim = c(0, 1))
lines(sizesDF2_a,lwd=3, cumulative_proportionsDF2_a, col = "green")
lines(sizesDF3_a,lwd=3, cumulative_proportionsDF3_a, col = "lightgreen")
lines(sizesBF4_a,lwd=3, cumulative_proportionsBF4_a, col = "navy")
lines(sizesSF1_a,lwd=3, cumulative_proportionsSF1_a, col = "lightcoral")
lines(sizesSF2_a,lwd=3, cumulative_proportionsSF2_a, col = "lightpink")
lines(sizesSF4_a,lwd=3, cumulative_proportionsSF4_a, col = "magenta")

# Add legend
legend("bottomright", legend = c("DF-1", "DF-2", "DF-3", "BF-4","SF-1","SF-2","SF-4"), 
       col = c("darkgreen","green","lightgreen", "navy", "lightcoral","lightpink","magenta"),lwd = 3, lty = 1)

# Add text to the text box
rect(0, 0.9, 0.5, 1, col='white', border='black')
text(0.25, 0.95, 'a', col='black', font=2, cex=1.5)  # 'font=2' specifies bold

#############################################################################################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (Min feret diameters)

BF4_b<-BF4$MinFeret

DF1_b<-DF1$MinFeret
DF2_b<-DF2$MinFeret
DF3_b<-DF3$MinFeret

SF1_b<-SF1$MinFeret
SF2_b<-SF2$MinFeret
SF4_b<-SF4$MinFeret
#Create Plots

# Sort the data
sizesBF4_b <- sort(BF4_b)

sizesDF1_b <- sort(DF1_b)
sizesDF2_b <- sort(DF2_b)
sizesDF3_b <- sort(DF3_b)

sizesSF1_b <- sort(SF1_b)
sizesSF2_b <- sort(SF2_b)
sizesSF4_b <- sort(SF4_b)

# Calculate cumulative proportions Low Flow BF
cumulative_proportionsBF4_b <- seq_along(sizesBF4_b) / length(sizesBF4_b)
# Calculate cumulative proportions Low Flow DF
cumulative_proportionsDF1_b <- seq_along(sizesDF1_b) / length(sizesDF1_b)
cumulative_proportionsDF2_b <- seq_along(sizesDF2_b) / length(sizesDF2_b)
cumulative_proportionsDF3_b <- seq_along(sizesDF3_b) / length(sizesDF3_b)
# Calculate cumulative proportions Stormflow
cumulative_proportionsSF1_b <- seq_along(sizesSF1_b) / length(sizesSF1_b)
cumulative_proportionsSF2_b <- seq_along(sizesSF2_b) / length(sizesSF2_b)
cumulative_proportionsSF4_b <- seq_along(sizesSF4_b) / length(sizesSF4_b)
###############################
#SADs
# Plot cumulative size distributions
plot(sizesDF1_b, lwd=3, cumulative_proportionsDF1_b, type = "l", col = "darkgreen",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SAD CDFs (b-axis): SF vs LF Events",
     xlim = range(c(0,5)), ylim = c(0, 1))
lines(sizesDF2_b,lwd=3, cumulative_proportionsDF2_b, col = "green")
lines(sizesDF3_b,lwd=3, cumulative_proportionsDF3_b, col = "lightgreen")
lines(sizesBF4_b,lwd=3, cumulative_proportionsBF4_b, col = "navy")
lines(sizesSF1_b,lwd=3, cumulative_proportionsSF1_b, col = "lightcoral")
lines(sizesSF2_b,lwd=3, cumulative_proportionsSF2_b, col = "lightpink")
lines(sizesSF4_b,lwd=3, cumulative_proportionsSF4_b, col = "magenta")

# Add legend
legend("bottomright", legend = c("DF-1", "DF-2", "DF-3", "BF-4","SF-1","SF-2","SF-4"), 
       col = c("darkgreen","green","lightgreen", "navy", "lightcoral","lightpink","magenta"),lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0, 0.9, 0.5, 1, col='white', border='black')
text(0.25, 0.95, 'b', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
