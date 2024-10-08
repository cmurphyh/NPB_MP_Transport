###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data/Sizes/conc_comp")

##################################################################################
###SDC <- LOAD SAN DIEGO CREEK DATA
##################################################################################
SDC_AllEx <-read.csv("SDC_All_Extrap.csv")  #read in database as csv 
head(SDC_AllEx )

##################################################################################
###SAD <- LOAD SANTA ANA DELHI CHANNEL DATA
##################################################################################
SAD_AllEx <-read.csv("SAD_All_Extrap.csv")  #read in database as csv 
head(SAD_AllEx)

##################################################################################
#Plots of Sample Composition by Morphology
##################################################################################
# SDC Color-Groups
SDC <- table(SDC_AllEx$Color.Group,SDC_AllEx$Event) # create table of values for plotting
SDC

SDC <- apply(SDC, 2, function(x){x*100/sum(x,na.rm=T)}) # turn table counts to percents
SDC
SDC_df <-data.frame(t(SDC))

# SAD Color-Groups
SAD <- table(SAD_AllEx$Color.Group,SAD_AllEx$Event) # create table of values for plotting
SAD


SAD <- apply(SAD, 2, function(x){x*100/sum(x,na.rm=T)}) # turn table counts to percents
SAD
SAD_df <-data.frame(t(SAD))

# Compare color-groups %'s

# Compare Black %'s in each sample

# Sample data
SDC_col <-SDC_df$Black
SAD_col <-SAD_df$Black

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Black")


######################################################

# Compare Blues %'s in each sample

# Sample data
SDC_col <-SDC_df$Blue
SAD_col <-SAD_df$Blue

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Blue")


######################################################

# Compare Green %'s in each sample

# Sample data
SDC_col <-SDC_df$Green
SAD_col <-SAD_df$Green

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Green")


######################################################

# Compare Red %'s in each sample

# Sample data
SDC_col <-SDC_df$Red
SAD_col <-SAD_df$Red

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Red")



######################################################

# Compare Transparent %'s in each sample

# Sample data
SDC_col <-SDC_df$Transparent
SAD_col <-SAD_df$Transparent

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Transparent")


######################################################

# Compare White %'s in each sample

# Sample data
SDC_col <-SDC_df$White
SAD_col <-SAD_df$White

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}


summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("White")



######################################################

# Compare Yellow %'s in each sample

# Sample data
SDC_col <-SDC_df$Yellow
SAD_col <-SAD_df$Yellow

#SDC_col <- c([PASTE from table row]
#)
#SAD_col <- c([PASTE from table row]
#)

SDC_col <- c(0.0000000,
             1.226994,
             1.666667,
             0.3436426,
             0.9259259,
             4.467354,
             0.000000,
             1.337029,
             3.300796,
             0.7054963,
             0.4950495,
             1.118881)
SAD_col <- c(3.278689,
             2.578797,
             1.377410,
             1.167728,
             1.596311,
             1.973251,
             2.652106,
             2.398190)

sdc<-SDC_col
sad <-SAD_col

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sdc , sad)

# Check the normality and equal variances assumptions
if (shapiro_sdc$p.value < 0.05 || shapiro_sad$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc, sad, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc, sad, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc, sad)
  print("Assumptions are met.")
  print(t_test_result)
}


summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="% of sample")

# Print the result
print("Yellow")

