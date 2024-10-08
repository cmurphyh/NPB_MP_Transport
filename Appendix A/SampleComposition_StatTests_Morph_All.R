###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

####SET WORKING DIRECTORY
setwd("~/Appendix A/conc_comp")

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
# SDC Morphologies
SDC <- table(SDC_AllEx$Morphology,SDC_AllEx$Event) # create table of values for plotting
SDC

SDC <- apply(SDC, 2, function(x){x*100/sum(x,na.rm=T)}) # turn table counts to percents
SDC
SDC_df <-data.frame(t(SDC))

# SAD Morphologies
SAD <- table(SAD_AllEx$Morphology,SAD_AllEx$Event) # create table of values for plotting
SAD

SAD <- apply(SAD, 2, function(x){x*100/sum(x,na.rm=T)}) # turn table counts to percents
SAD
SAD_df <-data.frame(t(SAD))

# Compare morphology %'s

# Compare fibers %'s in each sample
# Sample data
SDC_morph <-SDC_df$fiber
SAD_morph <-SAD_df$fiber

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Fiber")

######################################################

# Compare fiber bundle %'s in each sample
# Sample data
SDC_morph <-SDC_df$fiber.bundle
SAD_morph <-SAD_df$fiber.bundle

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Fiber Bundle")

######################################################

# Compare film %'s in each sample
# Sample data
SDC_morph <-SDC_df$film
SAD_morph <-SAD_df$film

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Film")

######################################################

# Compare foam %'s in each sample
# Sample data
SDC_morph <-SDC_df$foam
SAD_morph <-SAD_df$foam

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Foam")

######################################################

# Compare fragment %'s in each sample
# Sample data
SDC_morph <-SDC_df$fragment
SAD_morph <-SAD_df$fragment

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Fragment")

######################################################

# Compare Sphere %'s in each sample
# Sample data
SDC_morph <-SDC_df$sphere
SAD_morph <-SAD_df$sphere

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("Sphere")

######################################################

# Compare TRWP %'s in each sample
# Sample data
SDC_morph <-SDC_df$TRWP
SAD_morph <-SAD_df$TRWP

#SDC_morph <- c([PASTE from table row]
#)
#SAD_morph <- c([PASTE from table row]
#)

sdc<-SDC_morph
sad <-SAD_morph

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
print("TRWP")

######################################################