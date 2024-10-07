#Script to Calculate Rising and Settling Velocities of Particles
#Cmurp025@ucr.edu
#version: 9/10/2024
#Updates from previous: Edits to annotation only

#####Set up##############################################################

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)

###############################################################################
#Notes: 
#(1) Each row of data is a separate particle with at least one dimension measured,
#polymer identified, and morphology listed. 
#(2) The data file can be run through the MaTCH tool (Hapich et al., 2024) to add
#particle densities and any missing dimensions (projected width and height).
#(3) The tool can also harmonize morphology classifications (if needed)
#(4) Equations for Fiber and Non-Fiber Settling/Rising Velocities are from 
#Waldschlager et al., 2019 (equations 10-13).
#(5) Rouse Number equation is from Cowger et al., 2021
#(6) constants: 9.8 m/s (gravity),  999.81 m/s2 (m/s2) and 998 kg/m3 (water density)

###############################################################################
#####Start here with the MaTCHED dataset##########################################

#Set working directory
setwd("~/Desktop/Papers/Streamflow Composition Paper/Supplemental Materials/AppendixD")

#Load data into R
data <- read.csv("example_matched.csv") #CHANGE TO YOUR INPUT FILE

####data clean up #############################################################
#Make sure dimensional data is in a numeric format

#Length
data$Length..microns. <- as.numeric(data$Length..microns.)

#Width
#Here we measured the particle width and therefore we use our 
#measured value over the projected values from MaTCH
data$Width..microns. <- as.numeric(data$Width..microns.)

#Height
#Assuming value is from MaTCH: assign which projected Height to use
#options from outputs in our data (min=10%, max=90%, mean)
data$Height..microns. <- as.numeric(data$Projected.Height..microns.) #mean

#check formats are numeric
class(data$Length..microns.)
class (data$Width..microns.)
class (data$Height..microns.)

#######################################################################

#convert dimensions [um] to [m] and assign as a, b and c axis
data$a <-data$Length..microns./1E6
data$b <-data$Width..microns./1E6
data$c <-data$Height..microns./1E6

###functions needed to add variables to data table for calculations

# Function for calculating the Corey Shape Factor (CSF) [unitless]
assign_CSF <- function(a, b, c) {
  c / sqrt(a*b)
}

#Add CSF results to data table
data$CSF <- mapply(assign_CSF, data$a, data$b, data$c)

#Function for calculating the Equivalent Diameter (Deqi) [m]
#(Note that Non-Fibers and Fibers are treated differently; fibers Deqi = c) 
#Equations 7 & 8 of Waldschlager et al., 2019
assign_Deqi <- function(X1st.Order.Morph, a, b, c) {
  if (X1st.Order.Morph== "Non-Fiber") {
    return((a*b*c)^(1/3))
  } else if (X1st.Order.Morph == "Fiber") {
    return(c)
  } else {
    return(NA)
  }
}
#Add Deqi result to data table
data$Deqi<- mapply(assign_Deqi, data$X1st.Order.Morph, data$a, data$b, data$c)


## Load Constants for calculations
g = 9.81 # m/s2
v = 1.00E-6 #m2/s 
data$water.density = 998 #kg/m3

#Selection and Conversion of Particle Density

##Convert particle density from [mg/um3] to [kg/m3] by mutiplying by 1e+12

#ACTIVATE WHICH DENSITY TO USE from MaTCH (mean, 5th, 10th, 90th, 95th)

# Mean of distribution
data$particle.density <- data$Density..mg.microns3.*1000000000000 # Mean

# 5th percentile
#data$particle.density <- data$X5..Density..mg.microns3.*1000000000000

# 10th percentile
#data$particle.density <- data$X10..Density..mg.microns3.*1000000000000

#90th percentile
#data$particle.density <- data$X90..Density..mg.microns3.*1000000000000

# 95th percentile
#data$particle.density <- data$X95..Density..mg.microns3.*1000000000000

#Calculate relative density (delta) and add to data table
#Note that abs excludes direction of movement; Will add back in at the end
data$delta = abs(data$water.density - data$particle.density)/data$water.density 

##Calculating Dimensionless diameter (D*) [Unitless] and add to data table 
#[Note: Equation (2) in Waldschlager et al 2019 is missing the square of viscosity.]
data$D_dimless <- data$Deqi*(((data$delta*g)/(v^2))^(1/3)) 


#Filtering data to remove any particles not meeting analysis criteria
#dataset specific
data <- data %>% 
  filter(OS..Y.N. =="Y") %>% #only particles analysed in Open Specy
  filter(plastic_or_not=="plastic") %>% #only those matching to plastic polymers
  filter(max_cor_new >= 0.7) %>% # setting a match correlation threshold
  filter(!is.na(Length..microns.)) %>% # removing rows without dimension data
  filter(!is.na(Density..mg.microns3.)) #removing rows without denisty data

################################################################################
#Option to save new data sets before beginning calculations, clear and reload
################################################################################
#write.csv(data, "sdc_master_calc_ready.csv")

###Clear the R environment
#rm(list=ls());graphics.off();cat("\f");

#Load Calc Ready data
#data <- read.csv("sdc_master_calc_ready.csv")

#Reload Constants for calculations
#g = 9.81 # m/s2
#v = 1.00E-6 #m2/s 
#data$water.density = 998 #kg/m3
################################################################################
################################################################################

#Function to assign Powers Roundness (P) Values 
#Can skip if P values for each particle are already included in the dataset
#We used Median values from Wald et al. 2020 SI dataset of 100 particles
assign_Powers <- function(Morphology..Raw.Data.) {
  if (Morphology..Raw.Data.== "fragment") {
    return(4.6)
  } else if (Morphology..Raw.Data.== "foam") {
    return(5.0)
  } else if (Morphology..Raw.Data.== "pellet") {
    return(5.7)
  }  else if (Morphology..Raw.Data.== "film") {
    return(4.35)
  }    else if (Morphology..Raw.Data.== "sphere") {
    return(6.0)
  }    else if (Morphology..Raw.Data.== "TRWP") {
    return(4.6)
  }  else {
    return(NA)
  }
}

#Add P value results to data table
data$P<- mapply(assign_Powers, data$Morphology..Raw.Data.)

#####Functions to calculate Re, Cd, and W ######################################

# Function to calculate Re (depends on W)
calculate_Re <- function(W, Deqi, v) {
  return(W * Deqi / v)
}

# Function to calculate Cd (depends on Re)
#Using equations 10-13 of Waldschlager et al 2019
calculate_Cd <- function(Re, CSF, X1st.Order.Morph, particle.density, P, water.density) {
  if (X1st.Order.Morph == "Non-Fiber" & particle.density > water.density) {
    return(3 / (CSF * (Re^(1/3))))
  } else if (X1st.Order.Morph == "Non-Fiber" & particle.density < water.density) {
    return(((20 / Re) + (10 / sqrt(Re)) + sqrt(1.195 - CSF)) * ((6 / P)^(1 - CSF)))
  } else if (X1st.Order.Morph == "Fiber" & particle.density > water.density) {
    return((4.7 / sqrt(Re)) +  sqrt(CSF))
  } else if (X1st.Order.Morph == "Fiber" & particle.density < water.density) {
    return((10 / sqrt(Re)) +  sqrt(CSF))
  } else {
    return(NA)
  }
}

# Function to calculate W (depends on Cd)
calculate_W <- function(Cd, Deqi, delta, g) {
  return(sqrt((4/3) * (Deqi / Cd) * delta * g))
}  

### Iterative approach to calculating Settling velocity#########################

#Set up Iteration for a dataset
data <- data.frame(data) # data frame to calculate for
n <- nrow(data) # number of rows
max_iterations <- 100
data$W <-0.02 #add initial guess for W to data frame to start calculation


### Iterative calculation for Re, Cd, and W ###################################
#(1) Starting with provided W alculates Re, Cd and W for each row of the dataset
#(2) Then starts over again with the new W
#(3) Iterates the calculation X # of times before stopping
# For our data it only takes ~23 to converge with test data)

for (iter in 1:max_iterations) {
  for (i in 1:n) {
    data$Re[i] <- calculate_Re(data$W[i], data$Deqi[i], v)
    data$Cd[i] <- calculate_Cd(data$Re[i], data$CSF[i], data$X1st.Order.Morph[i], data$particle.density[i], data$P[i], data$water.density[i])
    data$W[i] <- calculate_W(data$Cd[i], data$Deqi[i], data$delta[i], g)
  }
}


#print the final iteration results
print(data)

#Adjust the final setting velocities for direction based on relative density
data$W_adj <- ifelse(data$particle.density < data$water.density, -1 * data$W, data$W) 

################################################################################
#Option to save before beginning Rouse calculations

#Write results to file
write.csv(data, "TestData_Settling.csv") #NAME FILE YOUR FILE

################################################################################
#Continue to calculate Rouse numbers (see Cowger et al. 2021)

#Check the depth is numeric for calc
class(data$Depth..m.)
data$Depth..m.<- as.numeric(data$Depth..m.)

#Assign slopes water or channel bed slopes 
data$slope <- 0.0005
#This could be based on field or geospatial measurements or other available data
#Here we used geospatial data from:
#https://ocs.ocpublicworks.com/sites/ocpwocs/files/2023-07/OCPW%20Geospatial%20Data%20Details.pdf
# SDC Michelson to below Campus Drive slope: 0.002 =(16.55-1.69)/6842 ( 1m DEM on Arc Pro)
# SAD SA Ave to Mesa Drive slope: 0.0005 =(8.62-6.58)/3885 ( 1m DEM on Arc Pro)

#Calculate shear velocity
#Note this assumes channel is much wider than deep
data$shear_velocity <- sqrt(g*data$Depth..m.*data$slope) 

#Calculate Rouse numbers
# k= 0.4 and B=1; (see Cowger et al. 2021)
data$Rouse <- data$W_adj /(0.4*1*data$shear_velocity) 

#Write results to file
write.csv(data, "TestData_Rouse.csv") #NAME FILE YOUR FILE

