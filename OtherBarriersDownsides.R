################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

source("FormatPGRdata.R")

# pgrdata_OtherBarriers
{
  pgrdata_OtherBarriers <- pgrdata[pgrdata$StudentStaff == "Student",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^OtherBarriers", x=colnames(pgrdata)))]
  head(pgrdata_OtherBarriers)
  
  pgrdata_OtherBarriers <- pgrdata_OtherBarriers[rowSums(!is.na(pgrdata_OtherBarriers)) > 1, ]
  
  ## Nb of responses
  pgrdata_OtherBarriers %>% summarise(across (everything(), ~sum(!is.na(.))))
  
  ## categorise barriers
  pgrdata_OtherBarriers$OtherBarriers_OA_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_Data_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_Code_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_Materials_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_Preprint_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_Prereg_recode <- NA
  pgrdata_OtherBarriers$OtherBarriers_RegRep_recode <- NA
  
  ### OA
  pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, "industry")] <- 'industry collaboration policies'
  pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, c("expensive|Expensive|fee|fees|Cost|Costs|cost|costs|costly|money|expensive|funding|funds|financial|Financial|pay|charges"))] <- 'ficancial cost'
  pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, "journal quality")] <- 'OA are lower quality journals'
  
  table(pgrdata_OtherBarriers$OtherBarriers_OA_recode)
  
  ## Data
  pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "industry")] <- 'industry collaboration policies'
  pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "managing data")] <- 'data management difficult and lack convention for metadata'
  pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "anonymising|complex governance requirements|sensitive|privacy")] <- 'sensitive data'
  pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "I do not have the authority to share")] <- 'data not owned'
  pgrdata_OtherBarriers$OtherBarriers_Data_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Data, "scannes oeuvers")] <- 'data not always digital'
  
  table(pgrdata_OtherBarriers$OtherBarriers_Data_recode)
  
  ## Code
  pgrdata_OtherBarriers$OtherBarriers_Code_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Code, "sharing research ideas")] <- 'fear of scooping'
  pgrdata_OtherBarriers$OtherBarriers_Code_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Code, "time")] <- 'time investment'
  
  table(pgrdata_OtherBarriers$OtherBarriers_Code_recode)
  
  ## Materials
  pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "sharing research ideas")] <- 'fear of scooping'
  pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "managing data")] <- 'materials management difficult and lack convention for metadata'
  pgrdata_OtherBarriers$OtherBarriers_Materials_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Materials, "libraries were not accessible")] <- 'materials not always digital'
  
  table(pgrdata_OtherBarriers$OtherBarriers_Materials_recode)
  
  ## Preprint
  pgrdata_OtherBarriers$OtherBarriers_Preprint_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Preprint, "sharing research ideas")] <- 'fear of scooping'
  
  table(pgrdata_OtherBarriers$OtherBarriers_Preprint_recode)
  
  ## Preregistration
  pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "sharing research ideas")] <- 'fear of scooping'
  pgrdata_OtherBarriers$OtherBarriers_Prereg_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_Prereg, "funding to run a pilot study")] <- 'lack of funding for pilot studies'
  
  
  table(pgrdata_OtherBarriers$OtherBarriers_Prereg_recode)
  
  
  ## Registered Report
  pgrdata_OtherBarriers$OtherBarriers_RegRep_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_RegRep, "inherently messy data")] <- 'challenge inherited from the the difficulties associated with data management'
  
  table(pgrdata_OtherBarriers$OtherBarriers_RegRep_recode)
  
  
  
}

# pgrdata_staff_OtherBarriers
{
  pgrdata_staff_OtherBarriers <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                         c(grep("Div", colnames(pgrdata)), grep(pattern="^OtherBarriers", x=colnames(pgrdata)))]
  head(pgrdata_staff_OtherBarriers)
  
  pgrdata_staff_OtherBarriers <- pgrdata_staff_OtherBarriers[rowSums(!is.na(pgrdata_staff_OtherBarriers)) > 1, ]
  
  ## Nb of responses
  pgrdata_staff_OtherBarriers %>% summarise(across (everything(), ~sum(!is.na(.))))
  
}


# pgrdata_WhatDownsides
{
  pgrdata_WhatDownsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^WhatDownsides", x=colnames(pgrdata)))]
  head(pgrdata_WhatDownsides)
  
  pgrdata_WhatDownsides <- pgrdata_WhatDownsides[rowSums(!is.na(pgrdata_WhatDownsides)) > 1, ]
  
  ## Nb of responses
  pgrdata_WhatDownsides %>% summarise(across (everything(), ~sum(!is.na(.))))
  
  
  ## categorise downsides
  pgrdata_WhatDownsides$WhatDownsides_OA_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Data_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Code_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode <- NA
  
  ### OA
  #### more like barriers?
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "harmful|Inappropriate")] <- 'harmful or inappropriate application or interpretation of the research'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "fee|fees|Cost|Costs|cost|costs|costly|money|expensive|Expensive|funding|funds|financial|Financial|pay|charges")] <- 'ficancial cost (including inequalities of access to publishing between institutions'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "duplication of research|plagiarism|commercialisation|Patenting|lack of approval or payment")] <- 'Intellectual property concerns (including plagiarism, duplication of research, scooping, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
  #### actual downsides?
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "journal income|funding to produce the journals|funding the production")] <- 'loss of journal income, need to find other means of journal production' # this needs to overwrite category 'financial costs to the author'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "may reduce the quality of peer review|less rigorous")] <- 'may reduce quality of peer review, or OA articles found to be less rigorous' # this needs to overwrite category 'financial costs to the author'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "limits publication options")] <- 'fewer (prestigious) journal options'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "express their opinion")] <- 'If all information is public, people would be less able to express their opinion'
  
  table(pgrdata_WhatDownsides$WhatDownsides_OA_recode)
  WhatDownsides_OA_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_OA),c('WhatDownsides_OA','WhatDownsides_OA_recode')]
  
  ## Data
  #### more like barriers?
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "pipped|stealing|duplication of research|plagiarism|commercialisation|Patenting|proper credit|lack of approval or payment|copyright|copyrights|Commercial sensitivity")] <- 'Intellectual property concerns (including plagiarism, duplication of research, scooping, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "anonymize|anonymising|complex governance requirements|sensitive|Sensitive|privacy|tricky|data protection|ethical|data security|identify the individuals|military safety")] <- 'sensitive data (human participants, archeological site, endengered animal/plant species, military information'
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "more work|Increased workload|overhead in time|burden")] <- 'time investment (more work, not valued for career, significant burden for qualitative researchers)'
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "good metadata")] <- 'need good metadata to ensure validy of reuse' # to pool with bad interpretation of research?
  pgrdata_WhatDownsides$WhatDownsides_Data_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "continuity of ownership")] <- 'challenges around continuity of ownership (e.g. for longitudinal dataset)'
  #### actual downsides?  
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "first to publish")] <- 'Increases \'first to publish\' pressure which may affect the work quality' 
  pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "harmful|Inappropriate|Same as Open Access publication|maliciously")] <- 'harmful or inappropriate application or interpretation of the research'
  
  WhatDownsides_Data_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Data),c('WhatDownsides_Data','WhatDownsides_Data_recode','WhatDownsides_Data_recode2')]  #not recoded: "Academics are too protective over their work"
  
  table(c(pgrdata_WhatDownsides$WhatDownsides_Data_recode, pgrdata_WhatDownsides$WhatDownsides_Data_recode2))
  
  ## Code
  #### more like barriers?
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "poor quality|reviewed and tested")] <- 'dissemination of poor quality code or need for thorough review and testing (which isn\'t incentivise)'
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "long time")] <- 'time investment'
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "duplication of research|plagiarism|commercialisation|Patenting|proper credit|lack of approval or payment|copyright|copyrights|Commercial sensitivity|not be acknowledged|stealing")] <- 'Intellectual property concerns (including plagiarism, duplication of research, scooping, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "anonymize|anonymising|complex governance requirements|sensitive|Sensitive|privacy|tricky|data protection|ethical|data security|identify the individuals|military safety")] <- 'sensitive data (human participants, archeological site, endengered animal/plant species, military information'
  #### actual downsides?  
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "first to publish")] <- 'Increases \'first to publish\' pressure which may affect the work quality' 
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "might not understand the code|understanding nuance")] <- 'no control over correctness of reuse'
  pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "harmful|Inappropriate|Same as Open Access publication|maliciously")] <- 'harmful or inappropriate application or interpretation of the research'
  
  
  WhatDownsides_Code_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Code),c('WhatDownsides_Code','WhatDownsides_Code_recode')]  #not recoded: "Academics are too protective over their work"
  
  table(pgrdata_WhatDownsides$WhatDownsides_Code_recode)
  
  ## Materials
  #### more like barriers?
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "anonymize|anonymising|complex governance requirements|sensitive|Sensitive|privacy|tricky|data protection|ethical|data security|identify the individuals|military safety")] <- 'sensitive data (human participants, archeological site, endengered animal/plant species, military information'
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "duplication of research|plagiarism|commercialisation|Patenting|proper credit|lack of approval or payment|copyright|copyrights|Commercial sensitivity|not be acknowledged|stealing")] <- 'Intellectual property concerns (including plagiarism, duplication of research, scooping, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
  #### actual downsides?  
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "first to publish")] <- 'Increases \'first to publish\' pressure which may affect the work quality' 
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "publish papers fast on it than myself|being pipped")] <- 'others to publish with material shared faster, loss of career prospects' 
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "harmful|Inappropriate|Same as Open Access|maliciously")] <- 'harmful or inappropriate application or interpretation of the research'
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "radioactive")] <- 'health hazard if shared/stored inappropriately (e.g. radioactive materials)'
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "need to be returned")] <- 'materials not owned (e.g. archelogical artifacts)'
  
  WhatDownsides_Materials_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Materials),c('WhatDownsides_Materials','WhatDownsides_Materials_recode')]  #not recoded: "Academics are too protective over their work"
  
  table(pgrdata_WhatDownsides$WhatDownsides_Materials_recode) 
  
  ## Preprint
  #### more like barriers?
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "time required")] <- 'time investment'
  #### actual downsides?  
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "interfere with blind peer reviewing|blind peer review")] <- 'interfere with blind peer reviewing'
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "make publication of full paper|prevent publication|Top journals")] <- 'make formal publishing more difficult'
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "Lack of peer review|lack of peer-review|Lack pf peer review|unreliable|still contain issues|not peer reviewed|not yet peer-reviewed|not yet gone through peer review|not undergone peer review|non-peer-reviewed|not peer-reviewed|before they are peer reviewed|hasn't been peer-reviewed|hadn't been through peer review|un-peer reviewed research|full of errors|considered as valid peer-reviewed research|Poor quality research|have not seen peer-review|has yet to benefit from peer review|erroneous")] <- 'lack peer review which can mislead consumers of research (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "first to publish")] <- 'Increases \'first to publish\' pressure which may affect the work quality'
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "follow up research faster")] <- 'others to publish with preprint shared faster, loss of career prospects' 
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "scholars being attributed with ideas that do not precisely reflect their most fully formed ideas")] <- 'scholars being attributed with ideas that do not precisely reflect their most fully formed ideas' 
  
  WhatDownsides_Preprint_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Preprint),c('WhatDownsides_Preprint','WhatDownsides_Preprint_recode')]  #not recoded: "Academics are too protective over their work"
  
  table(pgrdata_WhatDownsides$WhatDownsides_Preprint_recode)
  
  ## Preregistration
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "analysed later in context of evolving understanding|hypotheses change")] <- 'the understanding of data (e.g. longitudinal data) or the developement of hypothesis for an experiment are evolving, and this process is valuable, it should not be forced into a preregistration'
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "we do care about the results more than the method")] <- 'the result (e.g. whether something works, is beneficial, detrimental) matters more than the methods'
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "publish before you|steal")] <- 'others to publish with preregistration shared faster, loss of career prospects' 
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "Using up more time|slow down the project|time for research")] <- 'requires time (that could be spent on conducting experiments)' 
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "little to no experimentation")] <- 'Not relevant for the field (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "impeding exploratory analyses|stopping blue skies research|report an unanticipated finding|exploratory research")] <- 'Impede progress of field through exploratory analyses especially if made mandatory' 
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "problem if the practice became the norm without any corresponding increase in respect for null findings")] <- 'problem if the practice became the norm without any corresponding increase in respect for null findings' 
  
  WhatDownsides_Prereg_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_Prereg),c('WhatDownsides_Prereg','WhatDownsides_Prereg_recode', 'WhatDownsides_Prereg_recode2')]  #not recoded: "Academics are too protective over their work"
  
  table(c(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode,pgrdata_WhatDownsides$WhatDownsides_Prereg_recode2))
  
  
  ## Registered Report
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "publish before you|steal|poaching|copy methods or study objectives")] <- 'others to publish with registered report shared faster, loss of career prospects' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "prevents you from updating protocols based on new insights|adapt to unforeseen challenges")] <- 'prevents you from updating protocols based on new insights' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "Using up more time|slow down the project|time for research|slows the research down")] <- 'requires time (that could be spent on conducting experiments)' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "timescale")] <- 'challenging timescale for ECR under short term contracts' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "analysed later in context of evolving understanding|hypotheses change")] <- 'the understanding of data (e.g. longitudinal data) or the developement of hypothesis for an experiment are evolving, and this process is valuable, it should not be forced into a preregistration'
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "IP and accreditation|duplication of research|plagiarism|commercialisation|Patenting|proper credit|lack of approval or payment|copyright|copyrights|Commercial sensitivity|not be acknowledged|stealing")] <- 'Intellectual property concerns (including plagiarism, duplication of research, scooping, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "impeding exploratory analyses|stopping blue skies research|report an unanticipated finding")] <- 'Impede progress of field through exploratory analyses especially if made mandatory' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "impeding exploratory analyses|stopping blue skies research|report an unanticipated finding")] <- 'Impede progress of field through exploratory analyses especially if made mandatory' 
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "Peer review improves the quality")] <- 'lack peer review which can mislead consumers of research (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
  
  WhatDownsides_RegRep_recode <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_RegRep),c('WhatDownsides_RegRep','WhatDownsides_RegRep_recode', 'WhatDownsides_RegRep_recode2','WhatDownsides_RegRep_recode3')]  #not recoded: "Academics are too protective over their work"
  
  table(c(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode, pgrdata_WhatDownsides$WhatDownsides_RegRep_recode2, pgrdata_WhatDownsides$WhatDownsides_RegRep_recode3))
  
}

# pgrdata_staff_WhatDownsides
{
  pgrdata_staff_WhatDownsides <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                         c(grep("Div", colnames(pgrdata)), grep(pattern="^WhatDownsides", x=colnames(pgrdata)))]
  head(pgrdata_staff_WhatDownsides)
  
  pgrdata_staff_WhatDownsides <- pgrdata_staff_WhatDownsides[rowSums(!is.na(pgrdata_staff_WhatDownsides)) > 1, ]
  
  ## Nb of responses
  pgrdata_staff_WhatDownsides %>% summarise(across (everything(), ~sum(!is.na(.))))
  
}



##############################################
## merging OtherBarriers and What Downsides ##
##############################################

OtherBarriers_and_WhatDownsides <- merge(data.frame(IndID = rownames(pgrdata_OtherBarriers),pgrdata_OtherBarriers ), data.frame(IndID = rownames(pgrdata_WhatDownsides),pgrdata_WhatDownsides), by = 'IndID', all =TRUE)

### merge the columns Div into one

for (i in 1:nrow(OtherBarriers_and_WhatDownsides))
{
  if (!is.na(OtherBarriers_and_WhatDownsides$Div.x[i]) & !is.na(OtherBarriers_and_WhatDownsides$Div.y[i]))
  {OtherBarriers_and_WhatDownsides$Div[i] <- OtherBarriers_and_WhatDownsides$Div.x[i]}
  if (is.na(OtherBarriers_and_WhatDownsides$Div.x[i]))
  {OtherBarriers_and_WhatDownsides$Div[i] <- OtherBarriers_and_WhatDownsides$Div.y[i]
  }
  if(is.na(OtherBarriers_and_WhatDownsides$Div.y[i]))
  {OtherBarriers_and_WhatDownsides$Div[i] <- OtherBarriers_and_WhatDownsides$Div.x[i]}
}

OtherBarriers_and_WhatDownsides <- OtherBarriers_and_WhatDownsides[, -which(names(OtherBarriers_and_WhatDownsides) %in% c('Div.x', 'Div.y'))] 

### rewrite people's answer when they write something like same as previous answer.....
OtherBarriers_and_WhatDownsides_tofillup <- OtherBarriers_and_WhatDownsides[unique(c(
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_Data, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_Code, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_Materials, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_Preprint, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_Prereg, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$OtherBarriers_RegRep, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_OA, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_Data, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_Code, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_Materials, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_Preprint, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_Prereg, "As for|as for|As in|as in|Same as |same as|\\^")),
  which(str_detect(OtherBarriers_and_WhatDownsides$WhatDownsides_RegRep, "As for|as for|As in|as in|Same as|same as|\\^")))) ,]

OtherBarriers_and_WhatDownsides[OtherBarriers_and_WhatDownsides$IndID %in% c(33, 45, 247, 248),] 


OtherBarriers_and_WhatDownsides
colnameswithrecode <- colnames(OtherBarriers_and_WhatDownsides[,grep(pattern=".*recode", x=colnames(OtherBarriers_and_WhatDownsides))])
colnameswithrecode2 <- colnames(OtherBarriers_and_WhatDownsides[,grep(pattern=".*recode$", x=colnames(OtherBarriers_and_WhatDownsides))])
colnameswithrecode3 <- colnames(OtherBarriers_and_WhatDownsides[,grep(pattern=".*recode.$", x=colnames(OtherBarriers_and_WhatDownsides))])


Long_OtherBarriers_and_WhatDownsides <- pivot_longer(OtherBarriers_and_WhatDownsides[, !colnames(OtherBarriers_and_WhatDownsides) %in% colnameswithrecode], -c(IndID, Div), values_to = "Value", names_to = "Measure")
#Long_OtherBarriers_and_WhatDownsides <- Long_OtherBarriers_and_WhatDownsides[!is.na(Long_OtherBarriers_and_WhatDownsides$Value),]
Long_OtherBarriers_and_WhatDownsides_recode <- pivot_longer(OtherBarriers_and_WhatDownsides[, colnames(OtherBarriers_and_WhatDownsides) %in% c(colnameswithrecode2, 'IndID','Div')], -c(IndID, Div), values_to = "Recode", names_to = "Measure")
Long_OtherBarriers_and_WhatDownsides_recode3 <- pivot_longer(OtherBarriers_and_WhatDownsides[, colnames(OtherBarriers_and_WhatDownsides) %in% c(colnameswithrecode3, 'IndID','Div')], -c(IndID, Div), values_to = "Value", names_to = "Measure")
Long_OtherBarriers_and_WhatDownsides_recode3 <- Long_OtherBarriers_and_WhatDownsides_recode3[!is.na(Long_OtherBarriers_and_WhatDownsides_recode3[4]),]

LongWide_OtherBarriers_and_WhatDownsides <- cbind(Long_OtherBarriers_and_WhatDownsides, Long_OtherBarriers_and_WhatDownsides_recode[,c('Recode')])
LongWide_OtherBarriers_and_WhatDownsides <- LongWide_OtherBarriers_and_WhatDownsides[!is.na(LongWide_OtherBarriers_and_WhatDownsides[4]),]
write.csv(LongWide_OtherBarriers_and_WhatDownsides, file = "List_OtherBarriers_and_WhatDownsides.csv")

OtherBarriers_and_WhatDownsides_OA <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_OA','WhatDownsides_OA', 'OtherBarriers_OA_recode','WhatDownsides_OA_recode')]
OtherBarriers_and_WhatDownsides_Data <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_Data','WhatDownsides_Data', 'OtherBarriers_Data_recode','WhatDownsides_Data_recode','WhatDownsides_Data_recode2' )]
OtherBarriers_and_WhatDownsides_Code <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_Code','WhatDownsides_Code', 'OtherBarriers_Code_recode','WhatDownsides_Code_recode' )]
OtherBarriers_and_WhatDownsides_Materials <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_Materials','WhatDownsides_Materials', 'OtherBarriers_Materials_recode','WhatDownsides_Materials_recode' )]
OtherBarriers_and_WhatDownsides_Preprint <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_Preprint','WhatDownsides_Preprint', 'OtherBarriers_Preprint_recode','WhatDownsides_Preprint_recode' )]
OtherBarriers_and_WhatDownsides_Prereg <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_Prereg','WhatDownsides_Prereg', 'OtherBarriers_Prereg_recode','WhatDownsides_Prereg_recode','WhatDownsides_Prereg_recode2' )]
OtherBarriers_and_WhatDownsides_RegRep <- OtherBarriers_and_WhatDownsides[rowSums(!is.na(OtherBarriers_and_WhatDownsides)) > 1,c('IndID','OtherBarriers_RegRep','WhatDownsides_RegRep', 'OtherBarriers_RegRep_recode','WhatDownsides_RegRep_recode','WhatDownsides_RegRep_recode2','WhatDownsides_RegRep_recode3' )]
