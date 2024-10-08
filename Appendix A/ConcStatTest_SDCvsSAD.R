# Testing Signifigance between SAD and SDC datasets
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

##############################
#1.) Low Flow Data (All Particles)
###############################
# SAD data [n/m3]
sad <- c(15.8, 9.8, 17.6, 40.1)

# SDC data [n/m3]
sdc <- c(4.2, 4.6, 12.4, 3.6, 2.3, 4.6, 5.2)

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sad, sdc)

# Check the normality and equal variances assumptions
if (shapiro_sad$p.value < 0.05 || shapiro_sdc$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad, sdc, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad, sdc, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad, sdc)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Low Flow Data (All Particles)")

##############################
##############################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
##############################
#1.) Low Flow Data (NonFiber Particles)
###############################
# SAD data Non-Fiber [n/m3]
sad <- c(5.2, 6.5, 13.6, 18.6)

# SDC data [Non-Fiber [n/m3]
sdc <- c(2.1, 2.3, 8.5, 1.5, 1.6,2.8,2.5)

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sad, sdc)

# Check the normality and equal variances assumptions
if (shapiro_sad$p.value < 0.05 || shapiro_sdc$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad, sdc, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad, sdc, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad, sdc)
  print("Assumptions are met.")
  print(t_test_result)
}


summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Low Flow Data (Non-Fiber Particles)")

##############################
##############################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
##############################
#1.) Storm Flow Data (All Particles)
###############################
# SAD data [n/m3]
sad <- c(1179.5, 10365.9, 19424.2, 25228.3)

# SDC data [n/m3]
sdc <- c(3407.2, 1405.4, 2632.8, 31.6, 1302.4)

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sad, sdc)

# Check the normality and equal variances assumptions
if (shapiro_sad$p.value < 0.05 || shapiro_sdc$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad, sdc, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad, sdc, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad, sdc)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Storm Flow Data (All Particles)")

##############################
##############################
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
##############################
#1.) Storm Flow Data (Non-Fiber Particles)
###############################
# SAD data Non-Fiber [n/m3]
sad <- c(791.2, 7077.3, 13775.8, 11221.5)

# SDC data Non-Fiber [n/m3]
sdc <- c(1863.3, 983.3, 1737.4,16.4, 687.3)

# Perform Shapiro-Wilk test for normality
shapiro_sad <- shapiro.test(sad)
shapiro_sdc <- shapiro.test(sdc)

# Perform variance test for equal variances
var_test <- var.test(sad, sdc)

# Check the normality and equal variances assumptions
if (shapiro_sad$p.value < 0.05 || shapiro_sdc$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad, sdc, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad, sdc, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad, sdc)
  print("Assumptions are met.")
  print(t_test_result)
}

summary(sdc)
summary(sad)
par(mar=c(2.5,4,1,1))
boxplot(sdc,sad,
        col = c("steelblue", "plum"),
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Storm Flow Data (Non-Fiber Particles)")
