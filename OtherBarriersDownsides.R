################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

# source("FormatPGRdata.R")
require('reshape2') # for making pivot tables

# pgrdata_OtherBarriers -----

pgrdata_OtherBarriers <- pgrdata[pgrdata$StudentStaff == "Student",  
                                 c(grep("Div", colnames(pgrdata)), grep(pattern="^OtherBarriers", x=colnames(pgrdata)))]
head(pgrdata_OtherBarriers)

pgrdata_OtherBarriers <- pgrdata_OtherBarriers[rowSums(!is.na(pgrdata_OtherBarriers)) > 1, ]

## Nb of responses
pgrdata_OtherBarriers %>% summarise(across (everything(), ~sum(!is.na(.))))

pgrdata_OtherBarriers <-  data.frame(lapply(pgrdata_OtherBarriers, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}), stringsAsFactors=FALSE)

## check if respondents wrote something like same as previous answer.....
pgrdata_OtherBarriers[unique(c(
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OtherBarriers$OtherBarriers_RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]


## categorise barriers
pgrdata_OtherBarriers$OtherBarriers_OA_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_Data_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_Code_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_Materials_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_Preprint_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_Prereg_recode <- NA
pgrdata_OtherBarriers$OtherBarriers_RegRep_recode <- NA

### OA
pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, "INDUSTRY")] <- 'Resource not owned'
pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANCIAL|PAY|CHARGES"))] <- 'Financial cost'
pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, "JOURNAL QUALITY")] <- 'Lower quality'

pgrdata_OtherBarriers$OtherBarriers_OA_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_OA) & is.na(pgrdata_OtherBarriers$OtherBarriers_OA_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_OA[!is.na(pgrdata_OtherBarriers$OtherBarriers_OA_recode) & pgrdata_OtherBarriers$OtherBarriers_OA_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_OA_recode)

## Data
pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA")] <- 'Ethical concerns'
pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "AUTHORITY|INDUSTRY")] <- 'Resource not owned'
pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "OEUVERS")] <- 'Resource not always digital'

pgrdata_OtherBarriers$OtherBarriers_Data_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_Data) & is.na(pgrdata_OtherBarriers$OtherBarriers_Data_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_Data[!is.na(pgrdata_OtherBarriers$OtherBarriers_Data_recode) & pgrdata_OtherBarriers$OtherBarriers_Data_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_Data_recode)

## Code
pgrdata_OtherBarriers$OtherBarriers_Code_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Code, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OtherBarriers$OtherBarriers_Code_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Code, "TIME")] <- 'Time investment'

pgrdata_OtherBarriers$OtherBarriers_Code_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_Code) & is.na(pgrdata_OtherBarriers$OtherBarriers_Code_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_Code[!is.na(pgrdata_OtherBarriers$OtherBarriers_Code) & pgrdata_OtherBarriers$OtherBarriers_Code_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_Code_recode)

## Materials
pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "LIBRARIES")] <- 'Resource not always digital'

pgrdata_OtherBarriers$OtherBarriers_Materials_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_Materials) & is.na(pgrdata_OtherBarriers$OtherBarriers_Materials_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_Materials[!is.na(pgrdata_OtherBarriers$OtherBarriers_Materials) & pgrdata_OtherBarriers$OtherBarriers_Materials_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_Materials_recode)

## Preprint
pgrdata_OtherBarriers$OtherBarriers_Preprint_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Preprint, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OtherBarriers$OtherBarriers_Preprint_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Preprint, "PEER REVIEW")] <- 'Lack of peer review'

pgrdata_OtherBarriers$OtherBarriers_Preprint_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_Preprint) & is.na(pgrdata_OtherBarriers$OtherBarriers_Preprint_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_Preprint[!is.na(pgrdata_OtherBarriers$OtherBarriers_Preprint) & pgrdata_OtherBarriers$OtherBarriers_Preprint_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_Preprint_recode)

## Preregistration
pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "PILOT")] <- 'Lack of funding for pilot studies'
pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "DISCIPLINE")] <- 'Not applicable to all disciplines'

pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_Prereg) & is.na(pgrdata_OtherBarriers$OtherBarriers_Prereg_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_Prereg[!is.na(pgrdata_OtherBarriers$OtherBarriers_Prereg) & pgrdata_OtherBarriers$OtherBarriers_Prereg_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_Prereg_recode)


## Registered Report
pgrdata_OtherBarriers$OtherBarriers_RegRep_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_RegRep, "MESSY")] <- 'Impedes flexibility in protocols'

pgrdata_OtherBarriers$OtherBarriers_RegRep_recode[!is.na(pgrdata_OtherBarriers$OtherBarriers_RegRep) & is.na(pgrdata_OtherBarriers$OtherBarriers_RegRep_recode)] <- 'Not categorised'
pgrdata_OtherBarriers$OtherBarriers_RegRep[!is.na(pgrdata_OtherBarriers$OtherBarriers_RegRep) & pgrdata_OtherBarriers$OtherBarriers_RegRep_recode == 'Not categorised']

table(pgrdata_OtherBarriers$OtherBarriers_RegRep_recode)

  
# pgrdata_staff_OtherBarriers -----
pgrdata_staff_OtherBarriers <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                       c(grep("Div", colnames(pgrdata)), grep(pattern="^OtherBarriers", x=colnames(pgrdata)))]
head(pgrdata_staff_OtherBarriers)

pgrdata_staff_OtherBarriers <- pgrdata_staff_OtherBarriers[rowSums(!is.na(pgrdata_staff_OtherBarriers)) > 1, ]

## Nb of responses
pgrdata_staff_OtherBarriers %>% summarise(across (everything(), ~sum(!is.na(.))))


# pgrdata_WhatDownsides -----

pgrdata_WhatDownsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                                 c(grep("Div", colnames(pgrdata)), grep(pattern="^WhatDownsides", x=colnames(pgrdata)))]
head(pgrdata_WhatDownsides)

pgrdata_WhatDownsides <- pgrdata_WhatDownsides[rowSums(!is.na(pgrdata_WhatDownsides)) > 1, ]

pgrdata_WhatDownsides <-  data.frame(lapply(pgrdata_WhatDownsides, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}),  stringsAsFactors=FALSE)



## check if respondents wrote something like 'same as previous answer'.....
Cells_to_fillup_manually <- pgrdata_WhatDownsides[unique(c(
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]

### line 6
pgrdata_WhatDownsides$WhatDownsides_Data[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & pgrdata_WhatDownsides$WhatDownsides_Data == 'SAME AS OPEN ACCESS PUBLICATION'] <- pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & pgrdata_WhatDownsides$WhatDownsides_Data == 'SAME AS OPEN ACCESS PUBLICATION']
pgrdata_WhatDownsides$WhatDownsides_Materials[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == 'SAME AS OPEN ACCESS PUBLICATION'] <- pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == 'SAME AS OPEN ACCESS PUBLICATION']

### line 9
pgrdata_WhatDownsides$WhatDownsides_Data[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & pgrdata_WhatDownsides$WhatDownsides_Data == '^'] <- pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & pgrdata_WhatDownsides$WhatDownsides_Data == '^']
pgrdata_WhatDownsides$WhatDownsides_Code[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code) & pgrdata_WhatDownsides$WhatDownsides_Code == '^'] <- pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code) & pgrdata_WhatDownsides$WhatDownsides_Code == '^']
pgrdata_WhatDownsides$WhatDownsides_Materials[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == '^'] <- pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == '^']

### line 52
pgrdata_WhatDownsides$WhatDownsides_Materials[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == 'AS FOR DATA. '] <- pgrdata_WhatDownsides$WhatDownsides_Data[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials == 'AS FOR DATA. ']

### line 53
pgrdata_WhatDownsides$WhatDownsides_RegRep[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep) & pgrdata_WhatDownsides$WhatDownsides_RegRep == 'SAME AS FOR PREREGISTRATION.'] <- pgrdata_WhatDownsides$WhatDownsides_Prereg[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep) & pgrdata_WhatDownsides$WhatDownsides_RegRep == 'SAME AS FOR PREREGISTRATION.']



## Nb of responses
pgrdata_WhatDownsides %>% summarise(across (everything(), ~sum(!is.na(.))))


## categorise downsides
pgrdata_WhatDownsides$WhatDownsides_OA_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Data_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Data_recode2 <- NA
pgrdata_WhatDownsides$WhatDownsides_Code_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Materials_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2 <- NA
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode <- NA
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2 <- NA
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3 <- NA

### OA
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "HARM*|INAPPROPRIATE")] <- 'No control over validity of reuse, misrepresentation, misuse'
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANCIAL|PAY|CHARGES")] <- 'Ficancial cost' # including inequalities of access to publishing between institutions
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "JOURNAL INCOME|PRODUCTION")] <- 'Loss of journal income' # need to find other means of journal production'
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "QUALITY|RIGOR*")] <- 'Lowers quality' # reduce quality of peer review if journal paid
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "OPTIONS")] <- 'Fewer (prestigious) journal options'
pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "OPINION")] <- 'Ethical concerns'

pgrdata_WhatDownsides$WhatDownsides_OA_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_OA) & is.na(pgrdata_WhatDownsides$WhatDownsides_OA_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_OA[!is.na(pgrdata_WhatDownsides$WhatDownsides_OA) & pgrdata_WhatDownsides$WhatDownsides_OA_recode == 'Not categorised']

table(pgrdata_WhatDownsides$WhatDownsides_OA_recode)
WhatDownsides_OA_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_OA),c('WhatDownsides_OA','WhatDownsides_OA_recode')]

## Data
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "PIPPED|STEALING|SCOOPED")] <- 'Fear of scooping'
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY")] <- 'Ethical or security concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
pgrdata_WhatDownsides$WhatDownsides_Data_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "CONTINUITY")] <- 'Challenges around continuity of ownership (e.g. for longitudinal dataset)'
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "FIRST TO PUBLISH")] <- 'Lowers quality by increasing \'first to publish\' pressure' 
pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "HARMFUL|MALICIOUS|MISUSE")] <- 'No control over validity of reuse, misrepresentation, misuse'

pgrdata_WhatDownsides$WhatDownsides_Data_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & is.na(pgrdata_WhatDownsides$WhatDownsides_Data_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_Data[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data) & pgrdata_WhatDownsides$WhatDownsides_Data_recode == 'Not categorised']

table(c(pgrdata_WhatDownsides$WhatDownsides_Data_recode, pgrdata_WhatDownsides$WhatDownsides_Data_recode2))
WhatDownsides_Data_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data),c('WhatDownsides_Data','WhatDownsides_Data_recode','WhatDownsides_Data_recode2')]  #not recoded: "Academics are too protective over their work"


## Code
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "POOR QUALITY|REVIEWED AND TESTED")] <- 'Lowers quality by propagating unreviewed and untested code'
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "LONG TIME")] <- 'Time investment'
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY")] <- 'Ethical and safety concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "FIRST TO PUBLISH")] <- 'Lowers quality by increasing \'first to publish\' pressure' 
pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'

pgrdata_WhatDownsides$WhatDownsides_Code_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code) & is.na(pgrdata_WhatDownsides$WhatDownsides_Code_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_Code[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code) & pgrdata_WhatDownsides$WhatDownsides_Code_recode == 'Not categorised']

WhatDownsides_Code_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code),c('WhatDownsides_Code','WhatDownsides_Code_recode')]  

table(pgrdata_WhatDownsides$WhatDownsides_Code_recode)

## Materials
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|RADIOACTIVE")] <- 'Ethical and safety concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "FIRST TO PUBLISH")] <- 'Lowers quality by increasing \'first to publish\' pressure' 
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "PIPPED|STEALING|SCOOPED|PUBLISH PAPERS FAST")] <- 'Fear of scooping' # leading to loss of career prospect
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "RETURNED")] <- 'Resource not owned' # (e.g. archelogical artifacts)
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "PLATFORMS")] <- 'Unusable if not from open source platforms' # 

pgrdata_WhatDownsides$WhatDownsides_Materials_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & is.na(pgrdata_WhatDownsides$WhatDownsides_Materials_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_Materials[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials) & pgrdata_WhatDownsides$WhatDownsides_Materials_recode == 'Not categorised']

WhatDownsides_Materials_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials),c('WhatDownsides_Materials','WhatDownsides_Materials_recode')]  

table(pgrdata_WhatDownsides$WhatDownsides_Materials_recode) 

## Preprint
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "TIME REQUIRED")] <- 'Time investment'
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "PUBLICATION OF FULL PAPER|PREVENT PUBLICATION|TOP JOURNALS")] <- 'Prevents or complicates formal publishing'
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "PEER REVIEW*|UNRELIABLE|ISSUES|PEER-REVIEW*|ERRO*|POOR QUALITY|SCHOLARS")] <- 'Misleading due to lack of peer review' # (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "BLIND PEER REVIEW*")] <- 'Interferes with blind peer reviewing' # to overwrite the simple 'peer review' above
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "FIRST TO PUBLISH")] <- 'Lowers quality by increasing \'first to publish\' pressure' 
pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "FASTER")] <- 'Fear of scooping' 

pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Preprint) & is.na(pgrdata_WhatDownsides$WhatDownsides_Preprint_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_Preprint[!is.na(pgrdata_WhatDownsides$WhatDownsides_Preprint) & pgrdata_WhatDownsides$WhatDownsides_Preprint_recode == 'Not categorised']

WhatDownsides_Preprint_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Preprint),c('WhatDownsides_Preprint','WhatDownsides_Preprint_recode')] 

table(pgrdata_WhatDownsides$WhatDownsides_Preprint_recode)

## Preregistration
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE")] <- 'Fear of scooping' 
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH")] <- 'Time Investment' 
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "NO EXPERIMENTATION|NOT RELEVANT")] <- 'Not relevant for all fields' # (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "NULL FINDINGS")] <- 'Needs corresponding increase in respect for null findings' 
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|ADAPT TO UNFORESEEN")] <- 'Impedes flexibility in protocols'

pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg) & is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_Prereg[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg) & pgrdata_WhatDownsides$WhatDownsides_Prereg_recode == 'Not categorised']
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2) & pgrdata_WhatDownsides$WhatDownsides_Prereg_recode == 'Not categorised'] <- pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2) & pgrdata_WhatDownsides$WhatDownsides_Prereg_recode == 'Not categorised']
pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2) & pgrdata_WhatDownsides$WhatDownsides_Prereg_recode == pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2] <- NA

WhatDownsides_Prereg_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg),c('WhatDownsides_Prereg','WhatDownsides_Prereg_recode', 'WhatDownsides_Prereg_recode2')]  
table(c(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode,pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2))


## Registered Report
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE|POACHING|COPY METHOD")] <- 'Fear of scooping' 
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN")] <- 'Impedes flexibility in protocols' # 'the understanding of data (e.g. longitudinal data) or the developement of hypothesis for an experiment are evolving, and this process is valuable, it should not be forced into a preregistration'
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|SLOWS")] <- 'Time Investment' 
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "TIMESCALE")] <- 'Challenging timescale for ECR under short term contracts' 
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' #including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "EXPLORATORY|BLUE SKIES")] <- 'Impede progress of field through exploratory analyses especially if made mandatory' 

pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep) & is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode)] <- 'Not categorised'
pgrdata_WhatDownsides$WhatDownsides_RegRep[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == 'Not categorised']
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == 'Not categorised'] <- pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == 'Not categorised']
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2] <- NA
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == 'Not categorised'] <- pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == 'Not categorised']
pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3) & pgrdata_WhatDownsides$WhatDownsides_RegRep_recode == pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3] <- NA

WhatDownsides_RegRep_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep),c('WhatDownsides_RegRep','WhatDownsides_RegRep_recode', 'WhatDownsides_RegRep_recode2','WhatDownsides_RegRep_recode3')]  #not recoded: "Academics are too protective over their work"

table(c(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode, pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2, pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3))



# pgrdata_staff_WhatDownsides -----

pgrdata_staff_WhatDownsides <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                       c(grep("Div", colnames(pgrdata)), grep(pattern="^WhatDownsides", x=colnames(pgrdata)))]
head(pgrdata_staff_WhatDownsides)

pgrdata_staff_WhatDownsides <- pgrdata_staff_WhatDownsides[rowSums(!is.na(pgrdata_staff_WhatDownsides)) > 1, ]

## Nb of responses
pgrdata_staff_WhatDownsides %>% summarise(across (everything(), ~sum(!is.na(.))))




# making other barriers table -----

pgrdata_OtherBarriers

colnameswithrecode <- colnames(pgrdata_OtherBarriers[,grep(pattern=".*recode", x=colnames(pgrdata_OtherBarriers))])

pgrdata_OtherBarriers_long_values <- pivot_longer(pgrdata_OtherBarriers[,!colnames(pgrdata_OtherBarriers) %in% colnameswithrecode], -Div, values_to = "Value", names_to = "Measure")
pgrdata_OtherBarriers_long_recode <- pivot_longer(pgrdata_OtherBarriers[,colnames(pgrdata_OtherBarriers) %in% colnameswithrecode],  colnameswithrecode, values_to = "Recode", names_to = "Measure")
a <- cbind(pgrdata_OtherBarriers_long_values, pgrdata_OtherBarriers_long_recode[,c('Recode')])
a <- a[!is.na(a["Value"]),]


a$Measure[a$Measure == 'OtherBarriers_OA'] <- "Open Access" 
a$Measure[a$Measure == 'OtherBarriers_Data'] <- "Open Data" 
a$Measure[a$Measure == 'OtherBarriers_Code'] <- "Open Code" 
a$Measure[a$Measure == 'OtherBarriers_Materials'] <- "Open Materials" 
a$Measure[a$Measure == 'OtherBarriers_Preprint'] <- "Preprint" 
a$Measure[a$Measure == 'OtherBarriers_Prereg'] <- "Preregistration"
a$Measure[a$Measure == 'OtherBarriers_RegRep'] <- "Registered Report"
a$Measure<- factor(a$Measure, levels = Measures)

b <- a %>% group_by(Measure, Recode) %>% summarise(count = n()) 
c <- dcast(b, Recode ~ Measure, value.var = "count") # from reshape2
c$Total <- rowSums(c[,-1], na.rm=TRUE)

d <- c[with(c, order(-c$Total,
                -c$`Open Access`,
                -c$`Open Data`,
                -c$`Open Code`,
                -c$`Open Materials`,
                -c$Preprint,
                -c$Preregistration,
                -c$`Registered Report`)),]

d[is.na(d)] <- '-'
e <- d[d$Recode != 'Not categorised',1:8]
colnames(e)[colnames(e) == 'Recode'] <- ''
rownames(e) <- NULL
pivot_table_OtherBarriers <- e
pivot_table_OtherBarriers %>% knitr::kable()

