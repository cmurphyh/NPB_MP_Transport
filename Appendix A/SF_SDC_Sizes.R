###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

##############################################################################################
##############################################################################################
##############################################################################################
#Load SDC LowFlow Non-Fiber Data
####SET WORKING DIRECTORY
setwd("~/Appendix A/hist")
##############################################################################################
##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SDC_SF2_NF_5 <-read.csv("SDC-SF-2_NF_5.csv")  #read in database as csv
head(SDC_SF2_NF_5)
class(SDC_SF2_NF_5)
#check data is between 250um and 5 mm
summary(SDC_SF2_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SDC_SF2_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SDC SF-2:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SDC_SF2_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,61),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.250,col="red",lwd=2, lty="dotted", 
       text(1.4,62,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-2 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SDC_SF2_NF_5[,16] # calling up column for Feret Values Only
class(data_points) #check Numeric
# declaring the break points
break_points <- seq(0.25, 5, by=0.25) #Bin sizes
# transforming the data
data_transform <- cut(data_points, break_points,
                      right=FALSE)
# creating the frequency table
freq_table <- table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)
# calculating cumulative frequency
cumulative_freq = c(0, cumsum(freq_table))
print("Cumulative Frequency")
print(cumulative_freq)
# plotting the data to check
plot(break_points, cumulative_freq,
     xlab="Feret Diameter",
     ylab="Cumulative Frequency")
# creating line graph
lines(break_points, cumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (250 um)",col = "red"))

#Creating a Relative Frequency Plot from the Cumulative Frequency Table
n <- length(data_points) #calculate the number of data points (particles)
# calculating relative cumulative frequency
relcumulative_freq = c(0, (100*cumsum(freq_table)/n)) # divide by number of particles
print("RelCumulative Frequency")
print(relcumulative_freq)
# plotting the data
par(mar=c(4,5,2,2))
plot(break_points, relcumulative_freq,xaxp=c(0.25,5.25,20), las=2,
     xlab="Feret Diameter [mm]",
     ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)",
     main ="")
# creating line graph
lines(break_points, relcumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1.35,100,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-2 Sample: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#Load SDC LowFlow Non-Fiber Data
####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data/Sizes")
##############################################################################################
##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SDC_SF3_NF_5 <-read.csv("SDC-SF-3_NF_5.csv")  #read in database as csv
head(SDC_SF3_NF_5)
class(SDC_SF3_NF_5)
#check data is between 250um and 5 mm
summary(SDC_SF3_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SDC_SF3_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SDC SF-3:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SDC_SF3_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,61),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.250,col="red",lwd=2, lty="dotted", 
       text(1.4,62,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-3 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SDC_SF3_NF_5[,16] # calling up column for Feret Values Only
class(data_points) #check Numeric
# declaring the break points
break_points <- seq(0.25, 5, by=0.25) #Bin sizes
# transforming the data
data_transform <- cut(data_points, break_points,
                      right=FALSE)
# creating the frequency table
freq_table <- table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)
# calculating cumulative frequency
cumulative_freq = c(0, cumsum(freq_table))
print("Cumulative Frequency")
print(cumulative_freq)
# plotting the data to check
plot(break_points, cumulative_freq,
     xlab="Feret Diameter",
     ylab="Cumulative Frequency")
# creating line graph
lines(break_points, cumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (250 um)",col = "red"))

#Creating a Relative Frequency Plot from the Cumulative Frequency Table
n <- length(data_points) #calculate the number of data points (particles)
# calculating relative cumulative frequency
relcumulative_freq = c(0, (100*cumsum(freq_table)/n)) # divide by number of particles
print("RelCumulative Frequency")
print(relcumulative_freq)
# plotting the data
par(mar=c(4,5,2,2))
plot(break_points, relcumulative_freq,xaxp=c(0.25,5.25,20), las=2,
     xlab="Feret Diameter [mm]",
     ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)",
     main ="")
# creating line graph
lines(break_points, relcumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1.35,100,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-3 Sample: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#Load SDC LowFlow Non-Fiber Data
####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data/Sizes")
##############################################################################################
##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SDC_SF4_NF_5 <-read.csv("SDC-SF-4_NF_5.csv")  #read in database as csv
head(SDC_SF4_NF_5)
class(SDC_SF4_NF_5)
#check data is between 250um and 5 mm
summary(SDC_SF4_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SDC_SF4_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SDC SF-4:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SDC_SF4_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,61),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.250,col="red",lwd=2, lty="dotted", 
       text(1.4,62,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-4 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SDC_SF4_NF_5[,16] # calling up column for Feret Values Only
class(data_points) #check Numeric
# declaring the break points
break_points <- seq(0.25, 5, by=0.25) #Bin sizes
# transforming the data
data_transform <- cut(data_points, break_points,
                      right=FALSE)
# creating the frequency table
freq_table <- table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)
# calculating cumulative frequency
cumulative_freq = c(0, cumsum(freq_table))
print("Cumulative Frequency")
print(cumulative_freq)
# plotting the data to check
plot(break_points, cumulative_freq,
     xlab="Feret Diameter",
     ylab="Cumulative Frequency")
# creating line graph
lines(break_points, cumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (250 um)",col = "red"))

#Creating a Relative Frequency Plot from the Cumulative Frequency Table
n <- length(data_points) #calculate the number of data points (particles)
# calculating relative cumulative frequency
relcumulative_freq = c(0, (100*cumsum(freq_table)/n)) # divide by number of particles
print("RelCumulative Frequency")
print(relcumulative_freq)
# plotting the data
par(mar=c(4,5,2,2))
plot(break_points, relcumulative_freq,xaxp=c(0.25,5.25,20), las=2,
     xlab="Feret Diameter [mm]",
     ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)",
     main ="")
# creating line graph
lines(break_points, relcumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1.35,100,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-4 Sample: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#Load SDC LowFlow Non-Fiber Data
####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data/Sizes")
##############################################################################################
##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SDC_SF5_NF_5 <-read.csv("SDC-SF-5_NF_5.csv")  #read in database as csv
head(SDC_SF5_NF_5)
class(SDC_SF5_NF_5)
#check data is between 250um and 5 mm
summary(SDC_SF5_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SDC_SF5_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SDC SF-5:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SDC_SF5_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,61),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.250,col="red",lwd=2, lty="dotted", 
       text(1.4,62,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-5 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SDC_SF5_NF_5[,16] # calling up column for Feret Values Only
class(data_points) #check Numeric
# declaring the break points
break_points <- seq(0.25, 5, by=0.25) #Bin sizes
# transforming the data
data_transform <- cut(data_points, break_points,
                      right=FALSE)
# creating the frequency table
freq_table <- table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)
# calculating cumulative frequency
cumulative_freq = c(0, cumsum(freq_table))
print("Cumulative Frequency")
print(cumulative_freq)
# plotting the data to check
plot(break_points, cumulative_freq,
     xlab="Feret Diameter",
     ylab="Cumulative Frequency")
# creating line graph
lines(break_points, cumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (250 um)",col = "red"))

#Creating a Relative Frequency Plot from the Cumulative Frequency Table
n <- length(data_points) #calculate the number of data points (particles)
# calculating relative cumulative frequency
relcumulative_freq = c(0, (100*cumsum(freq_table)/n)) # divide by number of particles
print("RelCumulative Frequency")
print(relcumulative_freq)
# plotting the data
par(mar=c(4,5,2,2))
plot(break_points, relcumulative_freq,xaxp=c(0.25,5.25,20), las=2,
     xlab="Feret Diameter [mm]",
     ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)",
     main ="")
# creating line graph
lines(break_points, relcumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1.35,100,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-5 Sample: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################


#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SDC_SF6_NF_5 <-read.csv("SDC-SF-6_NF_5.csv")  #read in database as csv
head(SDC_SF6_NF_5)
class(SDC_SF6_NF_5)
#check data is between 250um and 5 mm
summary(SDC_SF6_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SDC_SF6_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SDC SF-6:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SDC_SF6_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,61),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.250,col="red",lwd=2, lty="dotted", 
       text(1.4,62,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-6 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SDC_SF6_NF_5[,16] # calling up column for Feret Values Only
class(data_points) #check Numeric
# declaring the break points
break_points <- seq(0.25, 5, by=0.25) #Bin sizes
# transforming the data
data_transform <- cut(data_points, break_points,
                      right=FALSE)
# creating the frequency table
freq_table <- table(data_transform)
# printing the frequency table
print("Frequency Table")
print(freq_table)
# calculating cumulative frequency
cumulative_freq = c(0, cumsum(freq_table))
print("Cumulative Frequency")
print(cumulative_freq)
# plotting the data to check
plot(break_points, cumulative_freq,
     xlab="Feret Diameter",
     ylab="Cumulative Frequency")
# creating line graph
lines(break_points, cumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (250 um)",col = "red"))

#Creating a Relative Frequency Plot from the Cumulative Frequency Table
n <- length(data_points) #calculate the number of data points (particles)
# calculating relative cumulative frequency
relcumulative_freq = c(0, (100*cumsum(freq_table)/n)) # divide by number of particles
print("RelCumulative Frequency")
print(relcumulative_freq)
# plotting the data
par(mar=c(4,5,2,2))
plot(break_points, relcumulative_freq,xaxp=c(0.25,5.25,20), las=2,
     xlab="Feret Diameter [mm]",
     ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)",
     main ="")
# creating line graph
lines(break_points, relcumulative_freq)
abline(v=0.250,col="red", lty="dotted", lwd=2, text(1.35,100,"net mesh size (250 um)",col = "red"))
title(main ="SDC SF-6 Sample: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################