######################################################################################################
########## Batch analysis script for cell assay data                                   ###############
########## Daniel Mendez                                                           ###############
######################################################################################################

############ Step1: set working directory and load packages
rm(list=ls())

getwd()
setwd("C:/Users/dmendez/Documents/Birds/CA1") # where your data is

library(plater)
library(plyr)
library(ggplot2)
library(data.table) 
library(plotrix)
library(gridExtra)
library(bindrcpp)
library(stringr)


############ Step 2: read data and check
# csv. file format for plater is 
# Top left cell of each layout is the name
# The rest of the top row of each layout is the column numbers (1:12 for a 96-well plate)
# The rest of the left column is the row names (A:H for a 96-well plate)
# Note that Microsoft Excel will sometimes include cells that appear to be blank in the .csv files it
# produces, so the files may have spurious columns or rows outside of the plate, causing errors. To
# solve this problem, copy and paste just the cells within the plate to a fresh worksheet and save it.

## Specify file paths

Full20240408_120 <- "20240408_120.csv"
#Full20231130_1_120 <- "20231130_1_120.csv"
#Full20231130_2_120 <- "20231130_2_120.csv"

## Check if file format is readable
check_plater_format(Full20240408_120)
#check_plater_format(Full20231130_2_120)

## Read plate-shape files with read_plates from plater  
data20240408_120<- read_plates(
    #file = c(Full20231130_1_120,Full20231130_2_120),
    #file = c(Full20220126_1_120,Full20220126_2_120,Full20220126_3_120),
    file = c(Full20240408_120),
    plate_names = c("Full20240408_120"),
    #plate_names = c("Full20231130_1_120 ","Full20231130_2_120"),
    #plate_names = c("Full20220126_1_120 ","Full20220126_2_120","Full20220126_3_120"),
    well_ids_column = "Wells") # optional

str(data20240408_120)
head(data20240408_120)

#colnames(data20240408_120)=c("Plate","Wells","Batch","Enzyme", "Ligand", "AUC")

## save organized data to new csv file
write.csv(data20240408_120, "Full_20240408_120DM.csv")


## View plate-shape data if you would like to double check
view_plate(
  data = data20240408_120[which(data20240408_120$Plate == "Full20240408_120"),], 
  well_ids_column = "Wells", 
  columns_to_display = c("Ligand", "AUC")
)


############ Step 3: summary statistics and t-test

DT20240408_120= as.data.table(data20240408_120)

# Create a new data table with summary statistics by trasfection number and ligand 
DTSum20240408_120<- dcast(DT20240408_120[, ],
              Enzyme+Ligand~., fun = list(length,mean,sd,std.error), value.var = "AUC")

#DTsucFull <- DTSum20240408_120[Ligand == "Sucrose"]
#DT_CM <- DTsucFull[Enzyme == "CM"]

# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DT20240408_120$Ligand))
Enzlab20240408_120<- unique(DT20240408_120$Enzyme)
DTSum20240408_120[, ':='(p.val = 0, p_adjusted = 0)]

write.csv(DTSum20240408_120, "Full20240408_120.csv")


############ Step 4: plots

plotAllFull <- ggplot(DTSum20240408_120, aes(x = Enzyme, y = AUC_mean, 
                                             fill=factor(ifelse(Enzyme=="SC","canary",
                                                                ifelse(Enzyme=="SV","starling","Normal"))))) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) + 
  facet_grid(~Ligand)+
  #scale_fill_manual(name = "Enzyme", values=c("darkorange","grey50","royalblue")) +
  coord_cartesian(ylim = c(0,500000)) +
  geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=.1)

plotAllFull

ggsave("AllLigands_20240408_120.pdf", plotAllFull, width = 600, height = 210, units = "mm")

###########################  Separate data by substrates ##############################
  
###################################################     MALTOSE     #####################


DTMaltFull <- DTSum20240408_120[Ligand == "Maltose"]
# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DT20240408_120$Ligand))
EnzlabFull <- as.factor (unique(DTSum20240408_120$Enzyme))
DTMaltFull[, ':='(p.val = 0, p_adjusted = 0)]


for (j in 1:length(EnzlabFull)) {
  
  # do t.test p value by each Enzyme and each ligand 
  for (i in 1:length(ligands)){ 
  DTMaltFull[Enzyme == EnzlabFull[j],
        p.val := DT20240408_120[, t.test(AUC[Enzyme == EnzlabFull[j]],AUC[Enzyme == "UT"])]$p.value]
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

write.csv(DTMaltFull, "Malt20240408_120.csv")

############ Step 4: plots

# Create an empty list for the plot of each Enzyme
plotsMaltFull <- list()

plotMaltFull <- ggplot(DTMaltFull, aes(x = Enzyme, y = AUC_mean, 
                                     fill=factor(ifelse(Enzyme=="SC","canary",
                                                        ifelse(Enzyme=="SV","starling","Normal"))))) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  #scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  scale_fill_manual(name = "Enzyme", values=c("darkorange","grey50","royalblue")) +
  coord_cartesian(ylim = c(0,250000)) +
  geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=.1) +
  ggtitle(paste0("Maltose 20240408_120"))  

#}
plotMaltFull

ggsave("Maltose_20240408_120.pdf", plotMaltFull, width = 240, height = 210, units = "mm")


##############################     SUCROSE   ############################

DTSucFull <- DTSum20240408_120[Ligand == "Sucrose"]
# Create variables containing all unique ligands and Enzyme IDs
ligands <- as.factor(unique(DT20240408_120$Ligand))
EnzlabFull <- unique(DTSum20240408_120$Enzyme)
DTSucFull[, ':='(p.val = 0, p_adjusted = 0)]

for (j in 1:length(EnzlabFull)) {
  
  # do t.test p value by each Enzyme and each ligand 
  for (i in 1:length(ligands)){ 
  DTSucFull[Enzyme == EnzlabFull[j],
             p.val := DT20240408_120[, t.test(AUC[Enzyme == EnzlabFull[j]],AUC[Enzyme == "UT"])]$p.value]
    
  }
}

DTSucFull[,p_adjusted:=round(p.adjust(DTSucFull[,p.val],method = 'bonferroni', n=length(p.val)),6)]

#check if 1 from comparing UT to UT should be included for adjustment
write.csv(DTSucFull, "Suc20240408_120.csv")

############ Step 4: plots

# Create an empty list for the plot of each Enzyme
plotsSucFull <- list()

plotSucFull <- ggplot(DTSucFull, aes(x = Enzyme, y = AUC_mean, 
                                     fill=factor(ifelse(Enzyme=="SC","canary",
                                                        ifelse(Enzyme=="SV","starling","Normal"))))) +
  geom_bar(stat= "identity", show.legend = FALSE) +

  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
 # Enzyme = "10.2 SC", fill = "orange" +
  scale_fill_manual(name = "Enzyme", values=c("darkorange","grey50","royalblue")) +
  coord_cartesian(ylim = c(0,700000)) +
  geom_errorbar(aes(ymin=AUC_mean-AUC_std.error, ymax=AUC_mean+AUC_std.error), width=.1) +
  ggtitle(paste0("Sucrose 20240408_120"))  

plotSucFull

ggsave("Sucrose_20240408_120.pdf", plotSucFull, width = 240, height = 210, units = "mm")
