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
DF1 <-read.csv("SDC-DF-1_NF_330_5.csv") 
DF2 <-read.csv("SDC-DF-2_NF_330_5.csv") 
DF3 <-read.csv("SDC-DF-3_NF_330_5.csv") 
BF1 <-read.csv("SDC-BF-1_NF_330_5.csv") 
BF2 <-read.csv("SDC-BF-2_NF_330_5.csv") 
BF3 <-read.csv("SDC-BF-3_NF_330_5.csv") 
BF4 <-read.csv("SDC-BF-4_NF_330_5.csv") 

#SF Data
SF2 <-read.csv("SDC-SF-2_NF_330_5.csv")
SF3 <-read.csv("SDC-SF-3_NF_330_5.csv")
SF4 <-read.csv("SDC-SF-4_NF_330_5.csv")
SF5 <-read.csv("SDC-SF-5_NF_330_5.csv")
SF6 <-read.csv("SDC-SF-6_NF_330_5.csv")

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

BF1 <- BF1 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

BF2 <- BF2 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

BF3 <- BF3 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

BF4 <- BF4 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

# Load SF data

SF2 <- SF2 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF3 <- SF3 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF4 <- SF4 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF5 <- SF5 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

SF6 <- SF6 %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")

#Select LF (x) and SF (y) datasets for comparision and summarize data

x <- DF1  # CHANGE DATA HERE
y <- SF2   # CHANGE DATA HERE


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

BF1_a<-BF1$Feret
BF2_a<-BF2$Feret
BF3_a<-BF3$Feret
BF4_a<-BF4$Feret

DF1_a<-DF1$Feret
DF2_a<-DF2$Feret
DF3_a<-DF3$Feret

SF2_a<-SF2$Feret
SF3_a<-SF3$Feret
SF4_a<-SF4$Feret
SF5_a<-SF5$Feret
SF6_a<-SF6$Feret

#Create Plots

# Sort the data
sizesBF1_a <- sort(BF1_a)
sizesBF2_a <- sort(BF2_a)
sizesBF3_a <- sort(BF3_a)
sizesBF4_a <- sort(BF4_a)

sizesDF1_a <- sort(DF1_a)
sizesDF2_a <- sort(DF2_a)
sizesDF3_a <- sort(DF3_a)

sizesSF2_a <- sort(SF2_a)
sizesSF3_a <- sort(SF3_a)
sizesSF4_a <- sort(SF4_a)
sizesSF5_a <- sort(SF5_a)
sizesSF6_a <- sort(SF6_a)

# Calculate cumulative proportions Low Flow BF
cumulative_proportionsBF1_a <- seq_along(sizesBF1_a) / length(sizesBF1_a)
cumulative_proportionsBF2_a <- seq_along(sizesBF2_a) / length(sizesBF2_a)
cumulative_proportionsBF3_a <- seq_along(sizesBF3_a) / length(sizesBF3_a)
cumulative_proportionsBF4_a <- seq_along(sizesBF4_a) / length(sizesBF4_a)
# Calculate cumulative proportions Low Flow DF
cumulative_proportionsDF1_a <- seq_along(sizesDF1_a) / length(sizesDF1_a)
cumulative_proportionsDF2_a <- seq_along(sizesDF2_a) / length(sizesDF2_a)
cumulative_proportionsDF3_a <- seq_along(sizesDF3_a) / length(sizesDF3_a)
# Calculate cumulative proportions Stormflow
cumulative_proportionsSF2_a <- seq_along(sizesSF2_a) / length(sizesSF2_a)
cumulative_proportionsSF3_a <- seq_along(sizesSF3_a) / length(sizesSF3_a)
cumulative_proportionsSF4_a <- seq_along(sizesSF4_a) / length(sizesSF4_a)
cumulative_proportionsSF5_a <- seq_along(sizesSF5_a) / length(sizesSF5_a)
cumulative_proportionsSF6_a <- seq_along(sizesSF6_a) / length(sizesSF6_a)
###############################
#SDCs
# Plot cumulative size distributions
plot(sizesBF1_a, lwd=3, cumulative_proportionsBF1_a, type = "l", col = "blue",
     xlab = "Particle a-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SDC CDFs (a-axis): SF vs LF Events",
     xlim = range(c(0,5)), ylim = c(0, 1))
lines(sizesBF2_a,lwd=3, cumulative_proportionsBF2_a, col = "cadetblue1")
lines(sizesBF3_a,lwd=3, cumulative_proportionsBF3_a, col = "cadetblue3")
lines(sizesBF4_a,lwd=3, cumulative_proportionsBF4_a, col = "navy")
lines(sizesDF1_a,lwd=3, cumulative_proportionsDF1_a, col = "darkgreen")
lines(sizesDF2_a,lwd=3, cumulative_proportionsDF2_a, col = "green")
lines(sizesDF3_a,lwd=3, cumulative_proportionsDF3_a, col = "lightgreen")
lines(sizesSF2_a,lwd=3, cumulative_proportionsSF2_a, col = "red")
lines(sizesSF3_a,lwd=3, cumulative_proportionsSF3_a, col = "darkred")
lines(sizesSF4_a,lwd=3, cumulative_proportionsSF4_a, col = "magenta")
lines(sizesSF5_a,lwd=3, cumulative_proportionsSF5_a, col = "plum")
lines(sizesSF6_a,lwd=3, cumulative_proportionsSF6_a, col = "lightcoral")
# Add legend
legend("bottomright", legend = c("BF-1","BF-2","BF-3","BF-4", "DF-1","DF-2","DF-3", "SF-2","SF-3","SF-4", "SF-5","SF-6"), 
       col = c("blue","cadetblue1","cadetblue3","navy","darkgreen","green","lightgreen", "red", "darkred","magenta", "plum","lightcoral"),lwd = 3, lty = 1)

# Add text to the text box
rect(0, 0.9, 0.5, 1, col='white', border='black')
text(0.25, 0.95, 'a', col='black', font=2, cex=1.5)  # 'font=2' specifies bold

#############################################################################################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (Min feret diameters)

BF1_b<-BF1$MinFeret
BF2_b<-BF2$MinFeret
BF3_b<-BF3$MinFeret
BF4_b<-BF4$MinFeret

DF1_b<-DF1$MinFeret
DF2_b<-DF2$MinFeret
DF3_b<-DF3$MinFeret

SF2_b<-SF2$MinFeret
SF3_b<-SF3$MinFeret
SF4_b<-SF4$MinFeret
SF5_b<-SF5$MinFeret
SF6_b<-SF6$MinFeret

#Create Plots

# Sort the data
sizesBF1_b <- sort(BF1_b)
sizesBF2_b <- sort(BF2_b)
sizesBF3_b <- sort(BF3_b)
sizesBF4_b <- sort(BF4_b)

sizesDF1_b <- sort(DF1_b)
sizesDF2_b <- sort(DF2_b)
sizesDF3_b <- sort(DF3_b)

sizesSF2_b <- sort(SF2_b)
sizesSF3_b <- sort(SF3_b)
sizesSF4_b <- sort(SF4_b)
sizesSF5_b <- sort(SF5_b)
sizesSF6_b <- sort(SF6_b)

# Calculate cumulative proportions Low Flow BF
cumulative_proportionsBF1_b <- seq_along(sizesBF1_b) / length(sizesBF1_b)
cumulative_proportionsBF2_b <- seq_along(sizesBF2_b) / length(sizesBF2_b)
cumulative_proportionsBF3_b <- seq_along(sizesBF3_b) / length(sizesBF3_b)
cumulative_proportionsBF4_b <- seq_along(sizesBF4_b) / length(sizesBF4_b)
# Calculate cumulative proportions Low Flow DF
cumulative_proportionsDF1_b <- seq_along(sizesDF1_b) / length(sizesDF1_b)
cumulative_proportionsDF2_b <- seq_along(sizesDF2_b) / length(sizesDF2_b)
cumulative_proportionsDF3_b <- seq_along(sizesDF3_b) / length(sizesDF3_b)
# Calculate cumulative proportions Stormflow
cumulative_proportionsSF2_b <- seq_along(sizesSF2_b) / length(sizesSF2_b)
cumulative_proportionsSF3_b <- seq_along(sizesSF3_b) / length(sizesSF3_b)
cumulative_proportionsSF4_b <- seq_along(sizesSF4_b) / length(sizesSF4_b)
cumulative_proportionsSF5_b <- seq_along(sizesSF5_b) / length(sizesSF5_b)
cumulative_proportionsSF6_b <- seq_along(sizesSF6_b) / length(sizesSF6_b)
###############################
#SDCs
# Plot cumulative size distributions
plot(sizesBF1_b, lwd=3, cumulative_proportionsBF1_b, type = "l", col = "blue",
     xlab = "Particle b-axis (mm)", ylab = "Cumulative Proportions", 
     main = "SDC CDFs (b-axis): SF vs LF Events",
     xlim = range(c(0,5)), ylim = c(0, 1))
lines(sizesBF2_b,lwd=3, cumulative_proportionsBF2_b, col = "cadetblue1")
lines(sizesBF3_b,lwd=3, cumulative_proportionsBF3_b, col = "cadetblue3")
lines(sizesBF4_b,lwd=3, cumulative_proportionsBF4_b, col = "navy")
lines(sizesDF1_b,lwd=3, cumulative_proportionsDF1_b, col = "darkgreen")
lines(sizesDF2_b,lwd=3, cumulative_proportionsDF2_b, col = "green")
lines(sizesDF3_b,lwd=3, cumulative_proportionsDF3_b, col = "lightgreen")
lines(sizesSF2_b,lwd=3, cumulative_proportionsSF2_b, col = "red")
lines(sizesSF3_b,lwd=3, cumulative_proportionsSF3_b, col = "darkred")
lines(sizesSF4_b,lwd=3, cumulative_proportionsSF4_b, col = "magenta")
lines(sizesSF5_b,lwd=3, cumulative_proportionsSF5_b, col = "plum")
lines(sizesSF6_b,lwd=3, cumulative_proportionsSF6_b, col = "lightcoral")
# Add legend
legend("bottomright", legend = c("BF-1","BF-2","BF-3","BF-4", "DF-1","DF-2","DF-3", "SF-2","SF-3","SF-4", "SF-5","SF-6"), 
       col = c("blue","cadetblue1","cadetblue3","navy","darkgreen","green","lightgreen", "red", "darkred","magenta", "plum","lightcoral"),lwd = 3, lty = 1)

# Manually add k-s results text to the text box
rect(0, 0.9, 0.5, 1, col='white', border='black')
text(0.25, 0.95, 'b', col='black', font=2, cex=1.5)  # 'font=2' specifies bold

