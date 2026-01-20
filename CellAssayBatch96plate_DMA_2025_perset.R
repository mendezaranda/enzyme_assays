######################################################################################################
########## Batch analysis script for cell assay data                                   ###############
########## Daniel Mendez                                                           ###############
######################################################################################################

############ Step1: set working directory and load packages
rm(list=ls())

getwd()
setwd("K:/Birds/CA1/clean_data") # where your data is
#setwd("K:/Birds/CA1/LCT") #

library(plater)
library(plyr)
library(ggplot2)
library(data.table) 
library(plotrix)
library(gridExtra)
library(bindrcpp)
library(stringr)
library(dplyr)


############ Step 2: read data and check
# csv. file format for plater is 
# Top left cell of each layout is the name
# The rest of the top row of each layout is the column numbers (1:12 for a 96-well plate)
# The rest of the left column is the row names (A:H for a 96-well plate)
# Note that Microsoft Excel will sometimes include cells that appear to be blank in the .csv files it
# produces, so the files may have spurious columns or rows outside of the plate, causing errors. To
# solve this problem, copy and paste just the cells within the plate to a fresh worksheet and save it.

## Specify file paths

#FullSet4_90 <- "Set4_90_clean.csv"
Full20250725_1_90 <- "20250725_1_90_clean.csv" ### Set 1 CA21 100 SUC
Full20250725_2_90 <- "20250725_2_90_clean.csv"  ### Set 1 CA21 100 SUC
Full20250731_1_90 <- "20250731_1_90_clean.csv"   ### Set 4 CA22
Full20250731_2_90 <- "20250731_2_90_clean.csv"   ### Set 4 CA22
Full20250801_1_90 <- "20250801_1_90_clean.csv"  ### Set 5 CA23
Full20250801_2_90 <- "20250801_2_90_clean.csv"  ### Set 5 CA23
Full20250807_1_90 <- "20250807_1_90_clean.csv"  ### Set 2 CA24
Full20250807_2_90 <- "20250807_2_90_clean.csv"  ### Set 2 CA24
Full20250808_1_90 <- "20250808_1_90_clean.csv"  ### Set 6 CA25
Full20250808_2_90 <- "20250808_2_90_clean.csv"  ### Set 6 CA25
Full20250822_1_90 <- "20250822_1_90_clean.csv"  ### Set 3 CA28
Full20250828_1_90 <- "20250828_1_90_clean.csv"  ### Set 1 CA29 50 SUC
Full20250828_2_90 <- "20250828_2_90_clean.csv"  ### Set 1 CA29 50 SUC
Full20250829_1_90 <- "20250829_1_90_clean.csv"  ### Set 2b CA30
Full20250829_2_90 <- "20250829_2_90_clean.csv"  ### Set 2b CA30
Full20250904_1_90 <- "20250904_1_90_clean.csv"  ### Set 6b CA31a
Full20250904_2_90 <- "20250904_2_90_clean.csv"  ### Set 3 CA31b
Full20250905_1_90 <- "20250905_1_90_clean.csv"  ### Set 1 CA32 25 SUC
Full20250905_2_90 <- "20250905_2_90_clean.csv"  ### Set 1 CA32 25 SUC
Full20250911_1_90 <- "20250911_1_90_clean.csv"  ### Set 3 CA33
Full20250911_2_90 <- "20250911_2_90_clean.csv"  ### Set 3 CA33

Full20250925_1_90 <- "20250925_1_90_clean.csv"  ### Set 5 CA34
Full20250925_2_90 <- "20250925_2_90_clean.csv"  ### Set 5 CA34
Full20250925_3_90 <- "20250925_3_90_clean.csv"  ### Set 1 CA35 25 SUC
Full20250925_4_90 <- "20250925_4_90_clean.csv"  ### Set 1 CA35 50 SUC
Full20250926_3_90 <- "20250926_3_90_clean.csv"  ### Mixed Set 4, 2b CA36B

#### Replace all Set4 by whichever set you are analyzing

########## Read plates -------------------------------------------------------------
## Read plate-shape files with read_plates from plater  
Set4_90<- read_plates(
   # file = c(Full20250725_1_90,Full20250725_2_90,Full20250828_1_90,Full20250828_2_90,Full20250905_1_90,Full20250905_2_90), ## Set 1
   # plate_names = c("Full20250725_1_90","Full20250725_2_90","Full20250828_1_90","Full20250828_2_90","Full20250905_1_90","Full20250905_2_90"), ## Set 1
   # file = c(Full20250725_1_90,Full20250725_2_90), ## Set 1 100mM
  #  plate_names = c("Full20250725_1_90","Full20250725_2_90"), ## Set 1 100mM
  
    # file = c(Full20250828_1_90,Full20250828_2_90,Full20250925_4_90), ## Set 1 50mM
    # plate_names = c("Full20250828_1_90","Full20250828_2_90", "Full20250925_4_90"), ## Set 1 50mM
    #file = c(Full20250905_1_90,Full20250905_2_90,Full20250925_3_90), ## Set 1 25 mM
    #plate_names = c("Full20250905_1_90","Full20250905_2_90", "Full20250925_3_90" ), ## Set 1 25 mM
    
    # file = c(Full20250807_1_90,Full20250807_2_90), ## Set 2
    # plate_names = c("Full20250807_1_90","Full20250807_2_90"), ## Set 2
    # 
    # file = c(Full20250829_1_90,Full20250829_2_90,Full20250926_3_90), ## Set 2b
    # plate_names = c("Full20250829_1_90","Full20250829_2_90","Full20250926_3_90"), ## Set 2b
    # 
    # file = c(Full20250822_1_90,Full20250911_1_90,Full20250911_2_90,Full20250904_2_90), ## Set 3
    # plate_names = c("Full20250822_1_90","Full20250911_1_90","Full20250911_2_90","Full20250904_2_90"), ## Set 3
    # file = c(Full20250911_1_90,Full20250904_2_90), ## Set 3 red
    # plate_names = c("Full20250911_1_90","Full20250904_2_90"), ## Set 3 red
    # file = c(Full20250822_1_90,Full20250911_1_90,Full20250904_2_90), ## Set 3 red2
    # plate_names = c("Full20250822_1_90","Full20250911_1_90","Full20250904_2_90"), ## Set 3 red2
    # file = c(Full20250822_1_90,Full20250911_1_90), ## Set 3 red3
    # plate_names = c("Full20250822_1_90","Full20250911_1_90"), ## Set 3 red3
    # file = c(Full20250911_2_90,Full20250911_1_90), ## Set 3 red4
    # plate_names = c("Full20250911_2_90","Full20250911_1_90"), ## Set 3 red4
    # file = c(Full20250822_1_90,Full20250911_2_90), ## Set 3 50mm
    # plate_names = c("Full20250822_1_90","Full20250911_2_90"), ## Set 3 50mm
  
     file = c(Full20250731_1_90,Full20250731_2_90,Full20250904_1_90, Full20250926_3_90), ## Set 4
     plate_names = c("Full20250731_1_90","Full20250731_2_90","Full20250904_1_90","Full20250926_3_90"), ## Set 4
    # 
    # file = c(Full20250801_1_90,Full20250801_2_90,Full20250925_1_90,Full20250925_2_90), ## Set 5
    # plate_names = c("Full20250801_1_90","Full20250801_2_90", "Full20250925_1_90", "Full20250925_2_90"), ### Set 5
    # file = c(Full20250801_1_90,Full20250801_2_90), ## Set 5b
    # plate_names = c("Full20250801_1_90","Full20250801_2_90"), ### Set 5b    
    # 
    # file = c(Full20250808_1_90,Full20250808_2_90,Full20250904_1_90), ### Set 6
    # plate_names = c("Full20250808_1_90","Full20250808_2_90","Full20250904_1_90"), ## Set 6
    # 
    well_ids_column = "Wells") # optional

########## Analyses and plots ------------------------------------------------------


str(Set4_90)
head(Set4_90)

#colnames(Set4_90)=c("Plate","Wells","Batch","Enzyme", "Ligand", "AUC")

## save organized data to new csv file
write.csv(Set4_90, "Full_Set4_90DM.csv")


## View plate-shape data if you would like to double check
view_plate(
  data = Set4_90[which(Set4_90$Plate == "FullSet4_90"),], 
  well_ids_column = "Wells", 
  columns_to_display = c("Ligand", "AUC")
)


############ Step 3: summary statistics and t-test

DTSet4_90= as.data.table(Set4_90)

# Create a new data table with summary statistics by trasfection number and ligand 
DTSumSet4_90<- dcast(DTSet4_90[, ],
              Enzyme+Ligand~., fun = list(length,mean,sd,std.error), value.var = "AUC")

# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DTSet4_90$Ligand))
EnzlabSet4_90<- unique(DTSet4_90$Enzyme)
DTSumSet4_90[, ':='(p.val = 0, p_adjusted = 0)]

write.csv(DTSumSet4_90, "FullSet4_90_clean.csv")

############ Step 4: plots

plotAllFull <- ggplot(DTSumSet4_90, aes(x = Enzyme, y = AUC_mean, 
                                             fill=factor(ifelse(Enzyme=="SC","canary",
                                                                ifelse(Enzyme=="SV","starling","Normal"))))) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) + 
  facet_grid(~Ligand)+
  #scale_fill_manual(name = "Enzyme", values=c("darkorange","grey50","royalblue")) +
  coord_cartesian(ylim = c(0,1000000)) +
  geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=.1)

plotAllFull

ggsave("AllLigands_Set4_90.pdf", plotAllFull, width = 600, height = 210, units = "mm")


###################################     MALTOSE     ##############################

DTMaltFull <- DTSumSet4_90[Ligand == "Maltose"]
# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DTSet4_90$Ligand))
EnzlabFull <- as.factor (unique(DTSumSet4_90$Enzyme))
DTMaltFull[, ':='(p.val = 0, p_adjusted = 0)]


for (j in 1:length(EnzlabFull)) {
  
  # do t.test p value by each Enzyme and each ligand 
  for (i in 1:length(ligands)){ 
  DTMaltFull[Enzyme == EnzlabFull[j],
        p.val := DTSet4_90[, t.test(AUC[Enzyme == EnzlabFull[j]],AUC[Enzyme == "UT"])]$p.value]
  # UT is my untransfected control, to which I am comparing all my other Enzymes
  
  # Adjust p values for each Enzyme
    #DTMaltFull[,p_adjusted:=round(p.adjust(DTMaltFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]
    DTMaltFull[,p_adjusted:=round(p.adjust(DTMaltFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]
    #DTMaltFull[Enzyme == EnzlabFull[j] , 
        #p_adjusted := round(p.adjust(p.val, method = p.adjust.methods, n = length(p.val)),6)]
     #   p_adjusted:=round(p.adjust(DTMaltFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]
  }
}
#DTMaltFull[,p_adjusted:=round(p.adjust(DTMaltFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]

write.csv(DTMaltFull, "MaltSet4_90_clean.csv")

############ Step 4: plots

# Create an empty list for the plot of each Enzyme
#plotsMaltFull <- list()

#for (j in 1:length(EnzlabFull)) {

#plotMaltFull <- ggplot(DTMaltFull, aes(x = Enzyme, y = AUC_mean)) +
 # geom_bar(stat= "identity", fill ="firebrick4") +
 # scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) + 
#  coord_cartesian(ylim = c(0,25000)) +
 # geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=.1) +
  #ggtitle(paste0("Maltose Set4_90"))  

#}
#plotMaltFull

maltose_data <- DTSet4_90[Ligand == "Maltose"]
#maltose_data <- DTSet4_90l %>%
#  filter(Ligand == "Maltose")

plotMaltFull <- ggplot(DTMaltFull, aes(x = Enzyme, y = AUC_mean, 
                                       fill = factor(ifelse(Enzyme == "SC", "canary",
                                                            ifelse(Enzyme == "SV", "starling", "Normal"))))) +
  geom_bar(stat = "identity", show.legend = FALSE, position = position_dodge(width = 0.9)) +  # Use position_dodge for bars
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  scale_fill_manual(
    name = "Enzyme",
    values = c("canary" = "darkorange", "starling" = "royalblue", "Normal" = "grey50"),
    drop = TRUE          # <-- ensures unused categories are dropped
  ) +
  coord_cartesian(ylim = c(0, 1000000)) +
  geom_errorbar(aes(ymin = AUC_mean - AUC_std.error, ymax = AUC_mean + AUC_std.error), width = 0.1) +
 # geom_point(data = maltose_data, aes(x = Enzyme, y = AUC), color = "grey20", size = 0.8, 
#             position = position_dodge(width = 0.9), show.legend = FALSE) +  # Center points using position_dodge
#             position = position_jitter(width = 0.2, height = 0), show.legend = FALSE) + # Scatter points
  geom_point(data = maltose_data,
             aes(x = Enzyme, y = AUC, color = Batch),   # <-- highlight Batch
             size = 1.5,
             position = position_jitter(width = 0.2, height = 0),   # Scatter points
             show.legend = TRUE) +
  ggtitle(paste0("Maltose Set4_90")) + 
  guides(fill = "none") # remove Enzyme label

plotMaltFull

ggsave("Maltose_Set4_90.pdf", plotMaltFull, width = 400, height = 210, units = "mm")




##############################     SUCROSE   ############################

DTSucFull <- DTSumSet4_90[Ligand == "Sucrose"]
# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DTSet4_90$Ligand))
EnzlabFull <- unique(DTSumSet4_90$Enzyme)
DTSucFull[, ':='(p.val = 0, p_adjusted = 0)]

for (j in 1:length(EnzlabFull)) {
  
  # do t.test p value by each Enzyme and each ligand 
  for (i in 1:length(ligands)){ 
  DTSucFull[Enzyme == EnzlabFull[j],
             p.val := DTSet4_90[, t.test(AUC[Enzyme == EnzlabFull[j]],AUC[Enzyme == "UT"])]$p.value]
    # UT is my untransfected control, to which I am comparing all my other Enzymes
  
  # Adjust p values for each Enzyme
 # DTSucFull[Enzyme == EnzlabFull[j] , 
            #p_adjusted := round(p.adjust(p.val, method = p.adjust.methods, n = length(p.val)),6)]
            #p_adjusted := round(p.adjust(p.val, 'bonferroni', n = length(p.val)),6)]  # here is where I have the issue, I tried changing the one line above for this one
  }
}
#DTSucFull[,p_adjusted := round(p.adjust(DTSucFull[,p.val], method = p.adjust.methods = "bonferroni", n = length(p.val)),6)]
          #p_adjusted := round(p.adjust(p.val, 'bonferroni', n = length(p.val)),6)]  # here is where I have the issue, I tried changing the one line above for this one
          
DTSucFull[,p_adjusted:=round(p.adjust(DTSucFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]

#check if 1 from comparingUT to UT should be included for adjustment
write.csv(DTSucFull, "SucSet4_90_clean.csv")

############ Step 4: plots

# Create an empty list for the plot of each Enzyme
plotsSucFull <- list()
sucrose_data <- DTSet4_90[Ligand == "Sucrose"]

plotSucFull <- ggplot(DTSucFull, aes(x = Enzyme, y = AUC_mean, 
                                     fill=factor(ifelse(Enzyme=="SC","canary",
                                                        ifelse(Enzyme=="SV","starling","Normal"))))) +
  geom_bar(stat= "identity", show.legend = FALSE, position = position_dodge(width = 0.9)) +
  #scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  scale_fill_manual(
    name = "Enzyme",
    values = c("canary" = "darkorange", "starling" = "royalblue", "Normal" = "red"),
    drop = TRUE          # <-- ensures unused categories are dropped
  ) +
  coord_cartesian(ylim = c(0,1000000)) +
  geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=0.1, position = position_dodge(width = 0.9)) +
#  geom_point(data = sucrose_data, aes(x = Enzyme, y = AUC), color = "grey20", size = 0.8, 
#                        position = position_dodge(width = 0.9), show.legend = FALSE) +  # Add data points 
#              position = position_jitter(width = 0.2, height = 0), show.legend = FALSE) +
  geom_point(data = sucrose_data,
             aes(x = Enzyme, y = AUC, color = Batch),   # <-- highlight Batch
             size = 1.5,
             position = position_jitter(width = 0.2, height = 0),   # Scatter points
             show.legend = TRUE) +
  ggtitle(paste0("Sucrose Set4_90")) + 
  guides(fill = "none") # remove Enzyme label

plotSucFull
  
  ggsave("Sucrose_Set4_90.pdf", plotSucFull, width = 400, height = 210, units = "mm")

