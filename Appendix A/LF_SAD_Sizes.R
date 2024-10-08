##############################################################################################
##############################################################################################
##############################################################################################
#Load SAD LowFlow Non-Fiber Data
####SET WORKING DIRECTORY
setwd("~/Appendix A/hist")
##############################################################################################
##############################################################################################
##############################################################################################
#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
LF_SAD_NF_5 <-read.csv("SAD_LF_NF_5.csv")  #read in database as csv
head(LF_SAD_NF_5)
class(LF_SAD_NF_5)
#check data is between 250um and 5 mm
summary(LF_SAD_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(LF_SAD_NF_5$Feret, ylim = c(0,400), xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
#title(main ="SAD Dry Flow Samples:
#     Size of Non-Fiber Suspected Microplastics",line=1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- LF_SAD_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,60),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.330,col="red",lwd=2, lty="dotted", 
       text(1.45,60,"net mesh size (330 um)",col = "red"))
#title(main ="SAD Dry Flow Samples: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- LF_SAD_NF_5[,16] # calling up column for Feret Values Only
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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (330 um)",col = "red"))

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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1.45,100,"net mesh size (330 um)",col = "red"))
#title(main ="SAD Dry Flow Samples: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SAD_DF1_NF_5 <-read.csv("SAD-DF-1_NF_5.csv")  #read in database as csv
head(SAD_DF1_NF_5)
class(SAD_DF1_NF_5)
#check data is between 250um and 5 mm
summary(SAD_DF1_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SAD_DF1_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SAD DF-1:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SAD_DF1_NF_5$Feret

summary(Feret) #check correct data is loaded

hist_info <- hist(Feret, plot = FALSE)         # Store output of hist function
hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100 # sum(hist_info$counts) is the same as n
par(mar=c(4,5,2,2))
plot(hist_info, freq = FALSE,
     col= 'light blue', xlab = 'Feret Diameter [mm]', ylab= "Relative Cumulative Frequency 
     (% Non-Fiber particles)", main="",
     las=1, cex.axis=1, cex.lab=1, ylim=c(0,62),
     xlim = c(0,5), xaxp=c(0,5,10))  # Plot histogram with percentages
abline(v=0.330,col="red",lwd=2, lty="dotted", 
       text(1.5,62,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-1 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SAD_DF1_NF_5[,16] # calling up column for Feret Values Only
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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (330 um)",col = "red"))

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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1.45,100,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-1 Samples: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SAD_DF2_NF_5 <-read.csv("SAD-DF-2_NF_5.csv")  #read in database as csv
head(SAD_DF2_NF_5)
class(SAD_DF2_NF_5)
#check data is between 250um and 5 mm
summary(SAD_DF2_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SAD_DF2_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SAD DF-2:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SAD_DF2_NF_5$Feret

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
abline(v=0.330,col="red",lwd=2, lty="dotted", 
       text(1.5,62,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-2 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SAD_DF2_NF_5[,16] # calling up column for Feret Values Only
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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (330 um)",col = "red"))

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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1.45,100,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-2 Samples: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SAD_DF3_NF_5 <-read.csv("SAD-DF-3_NF_5.csv")  #read in database as csv
head(SAD_DF3_NF_5)
class(SAD_DF3_NF_5)
#check data is between 250um and 5 mm
summary(SAD_DF3_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SAD_DF3_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SAD DF-3:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SAD_DF3_NF_5$Feret

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
abline(v=0.330,col="red",lwd=2, lty="dotted", 
       text(1.5,62,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-3 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SAD_DF3_NF_5[,16] # calling up column for Feret Values Only
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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (330 um)",col = "red"))

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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1.45,100,"net mesh size (330 um)",col = "red"))
title(main ="SAD DF-3 Samples: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################

#Load subtidal Non-Fiber Data with data restricted to 250um to 5 mm (feret diameters)
SAD_BF4_NF_5 <-read.csv("SAD-BF-4_NF_5.csv")  #read in database as csv
head(SAD_BF4_NF_5)
class(SAD_BF4_NF_5)
#check data is between 250um and 5 mm
summary(SAD_BF4_NF_5$Feret)


#Plot histogram of frequencies (all intervals)
hist(SAD_BF4_NF_5$Feret, xlab = 'Feret Diameter [mm]', xlim = c(0,5), breaks= seq(0,5,0.25), xaxp=c(0.25,5.25,20),
     col= 'light blue', main = " ",cex.lab=1.1, cex.axis=1.1, cex.main=1.2,las=1)
title(main ="SAD BF-4:
 Size of Non-Fiber Suspected Microplastics",line=-1, cex.lab=1)

##############################################################################################
#Plot Histogram with Realtive Percent on Y axis
Feret <- SAD_BF4_NF_5$Feret

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
abline(v=0.330,col="red",lwd=2, lty="dotted", 
       text(1.5,62,"net mesh size (330 um)",col = "red"))
title(main ="SAD BF-4 Sample: Size Distribution (Non-Fibers Only)",line=0.5, cex.lab=1)
##############################################################################################
#Creating a Cumulative Frequency Plot (all intervals) --> do this first to use in relative frequency
# declaring data points
data_points <- SAD_BF4_NF_5[,16] # calling up column for Feret Values Only
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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1,0,"net mesh size (330 um)",col = "red"))

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
abline(v=0.330,col="red", lty="dotted", lwd=2, text(1.45,100,"net mesh size (330 um)",col = "red"))
title(main ="SAD BF-4 Samples: Size Distribution",line=1, cex.lab=1)
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
