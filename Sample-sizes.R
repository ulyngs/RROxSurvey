################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

# Numbers of responses
{
  ## total number of respondents with Consent, Affiliation, and Role
  nrow(pgrdata) 
  
  ## total number of respondents with Consent, Affiliation, and Role, who submitted the survey 
  nrow(pgrdata[pgrdata$Finished == 'True',]) 
  
  ## number of responses for each items
  colSums(!is.na(pgrdata))
  
  ## Representativeness per Division
  targetnumbers <- merge(targetnumbers, pgrdata[pgrdata$StudentStaff == 'Student',] %>% group_by(Div) %>% summarise (StudentRecorded = n()), by = "Div", all.x = TRUE)
  targetnumbers <- merge(targetnumbers, pgrdata[pgrdata$StudentStaff == 'Staff',] %>% group_by(Div) %>% summarise (StaffRecorded = n()), by = "Div", all.x = TRUE)
  targetnumbers <- merge(targetnumbers, data.frame(targetnumbers %>% group_by(Div) %>% summarise(RepStudent2019 = StudentRecorded/StudentTotal2019*100,
                                                                                                 RepStudent2021 = StudentRecorded/StudentTotal2021*100,
                                                                                                 RepStaff = StaffRecorded/StaffTotal*100)))
  mean(targetnumbers$RepStudent2021, na.rm=TRUE) 
  
  ## number of responses per Dpt
  
  data.frame(pgrdata[pgrdata$StudentStaff == "Student",] %>% group_by(Div, Dept) %>% summarise(n = n()))
  
  ## survey duration in minutes
  summary(as.numeric(pgrdata$SurveyDur[pgrdata$Finished == "True" & pgrdata$StudentStaff == "Student"]))/60
  
  ## Experience Duration
  summary(pgrdata$Duration[pgrdata$StudentStaff == "Student"])
  data.frame(pgrdata[pgrdata$StudentStaff == "Student",] %>% group_by(Div) %>% summarise(minDuration = min(Duration, na.rm=TRUE),
                                                                                         medDuration = median(Duration, na.rm=TRUE),
                                                                                         meanDuration = mean(Duration, na.rm=TRUE),
                                                                                         maxDuration = max(Duration, na.rm=TRUE),
                                                                                         n = n(),
                                                                                         NADuration = sum(is.na(Duration))))
  
}


# sample sizes of students per Division

Consent_Affiliation_Role_ss <- pgrdata[pgrdata$StudentStaff == "Student", ] %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())

pgrdata_Awareness <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c( grep("Div", colnames(pgrdata)), grep(pattern="^Awareness", x=colnames(pgrdata)))]
Awareness_ss <- pgrdata_Awareness[rowSums(!is.na(pgrdata_Awareness)) > 1, ] %>% group_by(Div) %>% summarise (Awareness=n())

pgrdata_Effect <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c( grep("Div", colnames(pgrdata)), grep(pattern="^Effect", x=colnames(pgrdata)))]
Effect_ss <- pgrdata_Effect[rowSums(!is.na(pgrdata_Effect)) > 1, ] %>% group_by(Div) %>% summarise (Effect=n())

pgrdata_Barriers <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c( grep("Div", colnames(pgrdata)), grep(pattern="^Barriers", x=colnames(pgrdata)))]
Barriers_ss <- pgrdata_Barriers[rowSums(!is.na(pgrdata_Barriers)) > 1, ] %>% group_by(Div) %>% summarise (Barriers=n())

pgrdata_Downsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                            c( grep("Div", colnames(pgrdata)), grep(pattern="^Downsides", x=colnames(pgrdata)))]
Downsides_ss <- pgrdata_Downsides[rowSums(!is.na(pgrdata_Downsides)) > 1, ] %>% group_by(Div) %>% summarise (Downsides=n())

pgrdata_CurrentRecruitment <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c( grep("Div", colnames(pgrdata)), grep(pattern="^CurrentRecruitment", x=colnames(pgrdata)))]
CurrentRecruitment_ss <- pgrdata_CurrentRecruitment[rowSums(!is.na(pgrdata_CurrentRecruitment)) > 1, ] %>% group_by(Div) %>% summarise (CurrentRecruitment=n())

pgrdata_FutureRecruitment <- pgrdata[pgrdata$StudentStaff == "Student",  
                                       c( grep("Div", colnames(pgrdata)), grep(pattern="^FutureRecruitment", x=colnames(pgrdata)))]
FutureRecruitment_ss <- pgrdata_FutureRecruitment[rowSums(!is.na(pgrdata_FutureRecruitment)) > 1, ] %>% group_by(Div) %>% summarise (FutureRecruitment=n())

pgrdata_Training <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c( grep("Div", colnames(pgrdata)), grep(pattern="^Training", x=colnames(pgrdata)))]
Training_ss <- pgrdata_Training[rowSums(!is.na(pgrdata_Training)) > 1, ] %>% group_by(Div) %>% summarise (Training=n())

pgrdata_Support <- pgrdata[pgrdata$StudentStaff == "Student",  
                            c( grep("Div", colnames(pgrdata)), grep(pattern="^Support", x=colnames(pgrdata)))]
Support_ss <- pgrdata_Support[rowSums(!is.na(pgrdata_Support)) > 1, ] %>% group_by(Div) %>% summarise (Support=n())

ss <- merge(merge(merge(merge(merge(merge(merge(merge(
  Consent_Affiliation_Role_ss,
  Awareness_ss, all = TRUE),
  Effect_ss, all = TRUE),
  Barriers_ss, all = TRUE),
  Downsides_ss, all = TRUE),
  CurrentRecruitment_ss, all = TRUE),
  FutureRecruitment_ss, all = TRUE),
 Training_ss, all = TRUE),
 Support_ss, all = TRUE)

ss <- merge(ss, targetnumbers[,c('Div', 'StudentTotal2021')], all.x=TRUE)
ss <- rbind(ss, c("Total", colSums(ss[,-1])))
ss[,2:ncol(ss)] <- sapply(ss[,2:ncol(ss)], as.integer)
ss$PercRepresentativeness <- round(as.numeric(ss$Consent_Affiliation_Role)*100/as.numeric(ss$StudentTotal2021),2)
ss$TotalDrop <- apply(ss[,2:10], 1, max) - apply(ss[,2:10], 1, min)
ss$PercDrop <- round(ss$TotalDrop/apply(ss[,2:10], 1, max)*100,2)
str(ss)

library(data.table)
sst <- transpose(ss)
rownames(sst) <- colnames(ss)
colnames(sst) <- NULL
sst







