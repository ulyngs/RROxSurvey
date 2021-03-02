################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
##                 Format data                ##
################################################

rm(list = ls())

# Packages
{
  library(tidyverse)
  library(here)
  library(RColorBrewer)
  
  # brewer.pal(n = 11, name = "RdYlBu")
  # display.brewer.pal(n = 11, name = 'RdYlBu')
  # display.brewer.pal(n = 11, name = 'RdBu')
  # brewer.pal(n = 11, name = "RdBu")
  # brewer.pal(n = 9, name = "Blues")
}

# load data
{
  pgrdata <- read.csv(here("Data/RealData_20210302-0945.csv"), stringsAsFactors=FALSE)
  # simdata <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE)
  surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
  targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE)

}

# items to judge
Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')
Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')
Trainings <- c('Open Access', 'Data Management Plan', 'FAIR Data','Ethics','Open Code', 'Open Materials', 'Licences', 'Preprint', 'Preregistration')
Supports <- c('Seminars', 'Mentoring', 'Coaching', 'Support Networks', 'Online Resources')
Criteria <- c('Number of publications','Prestige of publication outlet','Quality of publications', 'Authorship role', 'Citations', 'Grant support', 
              'Impact','Teaching', 'Supervision, mentoring', 'Service to the profession','Citizenship','National and/or international reputation',
              'Collaboration network','Open research practices')
Criteria_short <- c("PubNub","PubPrestige","PubQual","Authorship","Citation","Grant","Impact", "Teaching","Supervision","Service","Citizenship",
                    "Reputation","Collaboration","OpenResearch")

# format pgrdata
{
  ## change column names according to survey index
  pgrdata <- pgrdata[-c(1, 2), -which(names(pgrdata) %in% c("DistributionChannel","UserLanguage"))] 
  colnames(pgrdata) <- surveyindex$VariableName[surveyindex$QuestionCode == colnames(pgrdata)] 
  rm(surveyindex)
  pgrdata[pgrdata == ""] <- NA
  
  ## Subset pgrdata to records with filled out Consent, Affiliation, and Role: the 3 mandatory questions
  nrow(pgrdata)
  pgrdata <- subset(pgrdata[!is.na(pgrdata$Consent) & !is.na(pgrdata$DivCol) & !is.na(pgrdata$Role),] )
  nrow(pgrdata)
  
  ## Consent T/F
  pgrdata$Consent <- pgrdata$Consent == "I consent to participating in the survey and to processing of the information I provide as outlined above."
  
  # Affiliation
  {
    ## College TRUE FALSE
    pgrdata$College <- pgrdata$DivCol == "College-only staff"
    table(pgrdata$College)
    
    ## Division
    pgrdata$Div <- pgrdata$DivCol
    pgrdata$Div[pgrdata$Div == "College-only staff"] <- pgrdata$ColDiv[pgrdata$Div == "College-only staff"]
    
    pgrdata$Div[pgrdata$Div == "Social Sciences Division"] <- "SSD"
    pgrdata$Div[pgrdata$Div == "Humanities Division"] <- "Hum"
    pgrdata$Div[pgrdata$Div == "Department for Continuing Education"] <- "ContEd"
    pgrdata$Div[pgrdata$Div == "Mathematical, Physical, and Life Sciences Division"] <- "MPLS"
    pgrdata$Div[pgrdata$Div == "Medical Sciences Division"] <- "MSD"
    pgrdata$Div[pgrdata$Div == "Gardens, Libraries and Museums"] <- "GLAM"
    table(pgrdata$Div)
    
    ## Department
    pgrdata$Dept <- pgrdata$Dept1 
    pgrdata$Dept[!is.na(pgrdata$Dept2)] <- pgrdata$Dept2[!is.na(pgrdata$Dept2)]
    pgrdata$Dept[!is.na(pgrdata$Dept3)] <- pgrdata$Dept3[!is.na(pgrdata$Dept3)]
    pgrdata$Dept[!is.na(pgrdata$Dept4)] <- pgrdata$Dept4[!is.na(pgrdata$Dept4)]
    pgrdata$Dept[!is.na(pgrdata$Dept5)] <- pgrdata$Dept5[!is.na(pgrdata$Dept5)]
    pgrdata$Dept[!is.na(pgrdata$Dept6)] <- pgrdata$Dept6[!is.na(pgrdata$Dept6)]
    pgrdata$Dept[!is.na(pgrdata$Dept7)] <- pgrdata$Dept7[!is.na(pgrdata$Dept7)]
    pgrdata$Dept[!is.na(pgrdata$Dept8)] <- pgrdata$Dept8[!is.na(pgrdata$Dept8)]
    pgrdata$Dept[!is.na(pgrdata$Dept9)] <- pgrdata$Dept9[!is.na(pgrdata$Dept9)]
    pgrdata$Dept[!is.na(pgrdata$Dept10)] <- pgrdata$Dept10[!is.na(pgrdata$Dept10)]
    table(pgrdata$Dept)
    
    ## Other Department
    table(pgrdata$OtherDept)
    pgrdata[!is.na(pgrdata$OtherDept),c('OtherDept', 'Dept', 'Div')]
    
    ### recoding of Dept for otherdept actually in the list
    pgrdata$Dept[!is.na(pgrdata$OtherDept) & (pgrdata$OtherDept == "Wellcome Centre for Human Genetics" |
                                                pgrdata$OtherDept == "Experimental Medicine"|
                                                pgrdata$OtherDept == "Nuffield Department of Experimental Medicine")] <- "Nuffield Department of Clinical Medicine"
    
    pgrdata$Dept[!is.na(pgrdata$OtherDept) & str_detect(pgrdata$OtherDept, "Oxford Internet Institute")] <- "Oxford Internet Institute"
    
    pgrdata[!is.na(pgrdata$Dept) & pgrdata$Dept == "Other",]
    
    # clean up long longer useful column
    unwanted_colnames <- c("DivCol", "ColDiv", names(pgrdata[, grep(pattern="Dept[0-9]+", colnames(pgrdata))]))
    pgrdata <- pgrdata[, -which(names(pgrdata) %in% unwanted_colnames)] 
    rm(unwanted_colnames)
  }
  
  # Role
  {
    table(pgrdata$Role) 
    pgrdata$StudentStaff <- pgrdata$Role == "Student on a postgraduate research programme"
    pgrdata$StudentStaff[pgrdata$StudentStaff == TRUE] <- "Student"
    pgrdata$StudentStaff[pgrdata$StudentStaff == FALSE] <- "Staff"
  }
  
  # Years of Experience
  pgrdata$Duration <- as.numeric(pgrdata$Duration)
  
}

head(pgrdata, 2)
