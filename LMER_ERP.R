# Select data to run LMER on Cz, visual condition, all bins

Visual_N400 <- subset(YA_All_ERP_data, cond== "visual" & Electrode == "Cz",select=participant:Amplitude) 

# Filter out artifacts and innacurate responses
Visual_N400 <- subset(Visual_N400, Artifact ==0 & acc == 1,select=participant:Amplitude)

#Convert Bin to factor

Visual_N400$Bin = factor(Visual_N400$Bin)

#Analyses on every bin

library(lme4)
lmerlist = list();
for(i in levels(Visual_N400$Bin)) {
  newlmer = lmer(Amplitude ~ type + (1|participant) + (1|Item_ID), data=Visual_N400)
  lmerlist = append(lmerlist, newlmer)
}

# Put t-values in a list

val = list();
for (l in lmerlist) {
  val = append(val, l$fixed$coeff$typeword)
}

