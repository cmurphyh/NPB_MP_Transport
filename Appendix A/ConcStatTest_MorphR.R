# Testing Signifigance between SAD and SDC datasets
###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data/Sizes/conc_comp")
##################################################################################
###SDC <- LOAD SAN DIEGO CREEK DATA
##################################################################################
SDC_summary <-data.frame(read.csv("summary_sdc.csv"))  #read in database as csv 
head(SDC_summary )

##############################
#1.) SDC Flow Data (All Particles)
###############################
# SDC data [n/m3]
sdc_lf <- SDC_summary[1:7,]
sdc_lf
sdc_sf <- SDC_summary[8:12,]
sdc_sf

# SDC  onc TRWP data [n/m3]
sdc_lf_total <- sdc_lf$conc_Total
sdc_lf_total 

sdc_sf_total <- sdc_sf$conc_Total
sdc_sf_total 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sdc_lf_total)
shapiro_sf <- shapiro.test(sdc_sf_total)

# Perform variance test for equal variances
var_test <- var.test(sdc_lf_total , sdc_sf_total)

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc_lf_total, sdc_sf_total, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc_lf_total, sdc_sf_total, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc_lf_total, sdc_sf_total)
  print("Assumptions are met.")
  print(t_test_result)
}

##############################
#2.) SDC Flow Data (F)
###############################
# SDC data [n/m3]
sdc_lf <- SDC_summary[1:7,]
sdc_lf
sdc_sf <- SDC_summary[8:12,]
sdc_sf

# SDC  onc F data [n/m3]
sdc_lf_F <- sdc_lf$conc_Fiber
sdc_lf_F 

sdc_sf_F <- sdc_sf$conc_Fiber
sdc_sf_F 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sdc_lf_F)
shapiro_sf <- shapiro.test(sdc_sf_F)

# Perform variance test for equal variances
var_test <- var.test(sdc_lf_F , sdc_sf_F )

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc_lf_F, sdc_sf_F, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc_lf_F, sdc_sf_F, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc_lf_F, sdc_sf_F)
  print("Assumptions are met.")
  print(t_test_result)
}


##############################
#3.) SDC Flow Data (NF)
###############################
# SDC data [n/m3]
sdc_lf <- SDC_summary[1:7,]
sdc_lf
sdc_sf <- SDC_summary[8:12,]
sdc_sf

# SDC  onc NF data [n/m3]
sdc_lf_NF <- sdc_lf$conc_NonFiber
sdc_lf_NF 

sdc_sf_NF <- sdc_sf$conc_NonFiber
sdc_sf_NF 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sdc_lf_NF)
shapiro_sf <- shapiro.test(sdc_sf_NF)

# Perform variance test for equal variances
var_test <- var.test(sdc_lf_NF , sdc_sf_NF )

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc_lf_NF, sdc_sf_NF, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc_lf_NF, sdc_sf_NF, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc_lf_NF, sdc_sf_NF)
  print("Assumptions are met.")
  print(t_test_result)
}


##############################
#4.) SDC Flow Data (TRWP)
###############################
# SDC data [n/m3]
sdc_lf <- SDC_summary[1:7,]
sdc_lf
sdc_sf <- SDC_summary[8:12,]
sdc_sf

# SDC  onc TRWP data [n/m3]
sdc_lf_TRWP <- sdc_lf$conc_TRWP
sdc_lf_TRWP 

sdc_sf_TRWP <- sdc_sf$conc_TRWP
sdc_sf_TRWP 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sdc_lf_TRWP)
shapiro_sf <- shapiro.test(sdc_sf_TRWP)

# Perform variance test for equal variances
var_test <- var.test(sdc_lf_TRWP , sdc_sf_TRWP )

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sdc_lf_TRWP, sdc_sf_TRWP, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sdc_lf_TRWP, sdc_sf_TRWP, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sdc_lf_TRWP, sdc_sf_TRWP)
  print("Assumptions are met.")
  print(t_test_result)
}

##################################################################################
###SAD <- LOAD SANTA ANA DELHI Dataset
##################################################################################
SAD_summary <-data.frame(read.csv("summary_sad.csv"))  #read in database as csv 
head(SAD_summary )

##############################
#1.) SAD Flow Data (All Particles) Conc  data [n/m3]
###############################
# SAD
sad_lf <- SAD_summary[1:4,]
sad_lf
sad_sf <- SAD_summary[5:8,]
sad_sf

# SAD 
sad_lf_total <- sad_lf$conc_Total
sad_lf_total 

sad_sf_total <- sad_sf$conc_Total
sad_sf_total 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sad_lf_total)
shapiro_sf <- shapiro.test(sad_sf_total)

# Perform variance test for equal variances
var_test <- var.test(sad_lf_total , sad_sf_total)

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad_lf_total, sad_sf_total, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad_lf_total, sad_sf_total, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad_lf_total, sad_sf_total)
  print("Assumptions are met.")
  print(t_test_result)
}

##############################
#1.) SAD Flow Data (TRWP Particles)
###############################
# SAD data [n/m3]
sad_lf <- SAD_summary[1:4,]
sad_lf
sad_sf <- SAD_summary[5:8,]
sad_sf

# SAD  onc TRWP data [n/m3]
sad_lf_TRWP <- sad_lf$conc_TRWP
sad_lf_TRWP 

sad_sf_TRWP <- sad_sf$conc_TRWP
sad_sf_TRWP 

# Perform Shapiro-Wilk test for normality
shapiro_lf <- shapiro.test(sad_lf_TRWP)
shapiro_sf <- shapiro.test(sad_sf_TRWP)

# Perform variance test for equal variances
var_test <- var.test(sad_lf_TRWP , sad_sf_TRWP)

# Check the normality and equal variances assumptions
if (shapiro_lf$p.value < 0.05 || shapiro_sf$p.value < 0.05) {
  # If data are not normally distributed, you might consider non-parametric tests
  wilcox_result <- wilcox.test(sad_lf_TRWP, sad_sf_TRWP, exact = FALSE)
  print("Data are not normally distributed.")
  print(wilcox_result)
} else if (var_test$p.value < 0.05) {
  # If equal variances assumption is violated, you might consider Welch's t-test
  t_test_result <- t.test(sad_lf_TRWP, sad_sf_TRWP, var.equal = FALSE)
  print("Variances are not equal.")
  print(t_test_result)
} else {
  # If assumptions are met, perform the t-test
  t_test_result <- t.test(sad_lf_TRWP, sad_sf_TRWP)
  print("Assumptions are met.")
  print(t_test_result)
}

##################################################################################
###SAD vs SDC 
##################################################################################
####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data")

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
#Load Date
SAD_summary <-data.frame(read.csv("summary_sad.csv"))  #read in database as csv 
head(SAD_summary )
SDC_summary <-data.frame(read.csv("summary_sdc.csv"))  #read in database as csv 
head(SDC_summary )

##############################
#1.) SAD vs SDC All Flow Data (TRWP Particles)
###############################

# SDC 
sdc<- SDC_summary$conc_TRWP
sdc

# SAD  
sad<- SAD_summary$conc_TRWP
sad

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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("SAD vs SDC All Flow Data (TRWP Particles)")

###############################

#All Particles Concentrations  [n/m3]

# SDC 
sdc<- SDC_summary$conc_Total
sdc

# SAD 
sad<- SAD_summary$conc_Total
sad <- sad[1:8]
sad

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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("All Particles Concentrations [n/m3]")

###############################

#Non-Fibers Conc Non-Fiber [n/m3]

# SDC  
sdc<- SDC_summary$conc_NonFiber
sdc

# SAD  
sad<- SAD_summary$conc_NonFiber
sad <- sad[1:8]
sad

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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Non-Fibers Conc Non-Fiber [n/m3]")

###############################
#Fibers Conc Fiber [n/m3]

# SDC 
sdc<- SDC_summary$conc_Fiber
sdc

# SAD  
sad<- SAD_summary$conc_Fiber
sad <- sad[1:8]
sad
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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("Fibers Conc Fiber [n/m3]")


####SET WORKING DIRECTORY
setwd("~/Desktop/Streamflow/Current Data")

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
#Load Date
SAD_summary <-data.frame(read.csv("summary_sad.csv"))  #read in database as csv 
head(SAD_summary )
SDC_summary <-data.frame(read.csv("summary_sdc.csv"))  #read in database as csv 
head(SDC_summary )

##############################
#1.) SAD vs SDC Low Flow Data (TRWP Particles)
###############################

# SDC 
sdc<- SDC_summary$conc_TRWP
sdc
sdc <- sdc[1:7]
sdc
length(sdc)
# SAD  
sad<- SAD_summary$conc_TRWP
sad
sad <- sad[1:4]
sad
length(sad)
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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("SAD vs SDC Low Flow Data (TRWP Particles)")

##############################
#1.) SAD vs SDC StormFlow Data (TRWP Particles)
###############################

# SDC 
sdc<- SDC_summary$conc_TRWP
sdc
sdc <- sdc[8:12]
sdc
# SAD  
sad<- SAD_summary$conc_TRWP
sad
sad <- sad[5:8]
sad
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
        names=c("SDC", "SAD"), ylab="")
title(ylab=expression("Concentration [n/m"^3*"]"),line=2)

# Print the result
print("SAD vs SDC StormFlow Data (TRWP Particles)")

