################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

###############
## Inventory ##
###############

# pgrdata_Inventory
{
  pgrdata_Inventory <- pgrdata[pgrdata$StudentStaff == "Student",  
                               grep(pattern="^Inventory", x=colnames(pgrdata))]
  
  head(pgrdata_Inventory)
  pgrdata_Inventory <- data.frame(stack(pgrdata_Inventory))$values
  pgrdata_Inventory <- pgrdata_Inventory[!is.na(pgrdata_Inventory)]
  
  ## Nb of responses
  length(pgrdata_Inventory)
  
}

# pgrdata_staff_Inventory
{
  pgrdata_staff_Inventory <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                     grep(pattern="^Inventory", x=colnames(pgrdata))]
  
  head(pgrdata_staff_Inventory)
  pgrdata_staff_Inventory <- data.frame(stack(pgrdata_staff_Inventory))$values
  pgrdata_staff_Inventory <- pgrdata_staff_Inventory[!is.na(pgrdata_staff_Inventory)]
  
  ## Nb of responses
  length(pgrdata_staff_Inventory)
  
}
