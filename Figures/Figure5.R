### Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#### SET WORKING DIRECTORY
setwd("~/Figures")

################
# Sample data for two sets
data <- data.frame(
  Category = c("1/18/2021", "1/23/2021", "1/28-29/2021", "2/13/2021", "3/3/2021", "3/10/2021", "3/15/2021", "4/16/2021", "6/21/2021", "7/19/2021", "9/1-2/2021", "2/21/2022", "3/28/2022"),
  Label = c("BF-1","SF-1","SF-2","BF-2","SF-3","SF-4","SF-5","BF-3","DF-1","DF-2", "DF-3", "BF-4","SF-6"),
  SDC = c(4.2,NA , 3407, 4.6, 1405, 2633, 32, 12, 2.3, 4.6, 5.2, 3.6, 1302),
  SDC_F = c(2.1,NA , 1544, 2.3, 422, 898, 15, 3.9, 0.70, 1.7, 2.7, 2.1, 615),
  SDC_NF = c(2.1,NA , 1863, 2.3, 983, 1735, 16, 8.5, 1.6, 2.8, 2.5, 1.5, 687),
  SDC_TRWP = c(0.393,NA , 650, 0.227, 19.0, 405, 2.03, 0.778, 0.354, 0.299, 0.605, 0.208, 55.3),
  SAD = c(NA, 1179, 10366,NA , NA, 19424, NA, NA, 10, 18, 40, 16, 25228),
  SAD_F = c(NA, 388, 3289,NA , NA, 5648, NA, NA, 3.3, 4.0, 21, 11, 14007),
  SAD_NF = c(NA, 791, 7077,NA , NA, 13776, NA, NA, 6.5, 14, 19, 5.2, 11221),
  SAD_TRWP = c(NA, 66.1, 2448,NA , NA, 4921, NA, NA, 2.02, 3.50, 12.3, 0.95, 4503)
)

# Create a bar plot with logarithmic y-axis scale
par(mar=c(6,6,2,2))  # Increase left margin space
barplot(
  height = t(data[, c("SDC", "SAD")]),
  beside = TRUE,
  #names.arg = data$Category,
  las=2,
  ylim = c(0.1,100000),
  col = c("#1A85FF", "#D41159"),
  log = "y",
  axes = FALSE  # Suppress default axis labels
)

# Add customized y-axis labels rotated horizontally
axis(2, at=c(0.1, 1, 10, 100, 1000, 10000, 100000), 
     labels=c(expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)),
     las=1, cex.axis=1)  # Rotate y-axis labels and adjust text size

# Re-add x-axis and rotate x-axis labels
axis(1, at=seq(2, by=3, length.out=length(data$Category)), labels=data$Category, las=2, cex.axis=1)

# Add grid lines
abline(v=c(0.5,3.5,6.5,9.5,12.5,15.5,18.5,21.5,24.5,27.5,30.5,33.5,36.5,39.5), col="grey")

# Add custom labels on top of bars
text(c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38), 70000, c("BF-1", "SF-1", "SF-2", "BF-2", "SF-3", "SF-4", "SF-5", "BF-3", "DF-1", "DF-2", "DF-3", "BF-4", "SF-6"))

# Add legend
legend("topleft", inset = c(0.02, 0.08), legend = c("SDC", "SAD"), fill = c("#1A85FF", "#D41159"))

# Add title with y-axis label using expression for units
title(ylab=expression("Concentration (All Particles) [n/m"^3*"]"), line=2.75, cex.lab=1)
