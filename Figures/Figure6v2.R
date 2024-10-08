###Clear the R environment
rm(list=ls());graphics.off();cat("\f");



####SET WORKING DIRECTORY
setwd("~/Appendix A/ks2")

layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout.matrix
layout(mat = layout.matrix,
       heights = c(1, 1, 1.1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

layout.show(2)


##############################
#A.) Streamflow Site Compare (All Samples 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >330 to 5 mm (feret diameters)

# Load one dataset
x <-read.csv("SDC-LF_NF_330_5.csv")  #
x <- x %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")
summary(x$Feret)
x<-x$Feret
length(x)
# Load other dataset
y <-read.csv("SAD-LF_NF_330_5.csv")  #read in data of all LF sizes
y <- y %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")
summary(y$Feret)
y<-y$Feret
length(y)
#Testing Size Difference by Flow Type using Two sample Kolmogorov-Smirnov test")
ks.test(x, y)

#Create Plots

# Sort the data
sizes1 <- sort(x)
sizes2 <- sort(y)

# Calculate cumulative proportions
cumulative_proportions1 <- seq_along(sizes1) / length(sizes1)
cumulative_proportions2 <- seq_along(sizes2) / length(sizes2)

cumulative_proportions1<- cumulative_proportions1*100
cumulative_proportions2<- cumulative_proportions2*100

# Plot cumulative size distributions
plot(sizes1, cumulative_proportions1, type = "l", lty = 1, lwd=3, col = "#1A85FF",
     xlab = "Particle a-axis(mm)", ylab = "Percent Smaller",  
     main = "Low Flow CDFs (a-axis): SDC vs SAD",
     xlim = range(c(sizes1, sizes2)), ylim = c(0, 100))
lines(sizes2, cumulative_proportions2, lty = 1, lwd=3, col = "#D41159")

# Add legend
legend("bottomright", legend = c("SDC", "SAD"), 
       col = c("#1A85FF", "#D41159"), lty = 1, lwd=3)

# Add text to the text box
rect(0.25, 90, 0.75, 100, col='white', border='black')
text(0.5, 95, 'a', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3.1,50,'D = 0.10, p-value < 0.0013',col='black', cex=0.9)

##############################
#B.) Streamflow (All Samples 330um-5mm measured)
###############################
#Load Non-Fiber Data with data restricted to >250 to 5 mm (feret diameters)
SDC_LF <-read.csv("SDC-LF_NF_330_5.csv")  #read in data of all LF sizes
SDC_LF <- SDC_LF  %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")
summary(SDC_LF$Feret)
LF<-SDC_LF$Feret

# Load SF data
SDC_SF <-read.csv("SDC-SF_NF_330_5_Monly.csv")  #read in data excluding SF-5 and SF-6
SDC_SF <- SDC_SF %>% 
  filter(MinFeret > 0.330) %>% 
  filter(MinFeret != "NA")
summary(SDC_SF$Feret)
SF<-SDC_SF$Feret

#Testing Size Difference by Flow Type using Two sample Kolmogorov-Smirnov test")
ks.test(LF, SF)
length(LF)
length(SF)

#Create Plot

# Sort the data
sizes1 <- sort(LF)
sizes2 <- sort(SF)

# Calculate cumulative proportions
cumulative_proportions1 <- seq_along(sizes1) / length(sizes1)
cumulative_proportions2 <- seq_along(sizes2) / length(sizes2)

cumulative_proportions1<- cumulative_proportions1*100
cumulative_proportions2<- cumulative_proportions2*100

# Plot cumulative size distributions
plot(sizes1, cumulative_proportions1, type = "l", lwd = 3, lty = 1, col = "#009E73",
     xlab = "Particle a-axis(mm)", ylab = "Percent Smaller", 
     main = "SDC CDFs (a-axis): LF vs SF",
     xlim = range(c(sizes1, sizes2)), ylim = c(0, 100))
lines(sizes2, cumulative_proportions2, lwd = 3, lty = 1, col = "#CC79A7")

# Add legend
legend("bottomright", legend = c("Low Flow", "Stormflow"), 
       col = c("#009E73", "#CC79A7"), lwd = 3, lty = 1)

# Add text to the text box
rect(0.25, 90, 0.75, 100, col='white', border='black')
text(0.5, 95, 'b', col='black', font=2, cex=1.5)  # 'font=2' specifies bold
text(3.1,50,'D = 0.13, p-value < 3.8e-13',col='black', cex=0.9)