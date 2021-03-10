################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

{
  pgrdata_Support <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c(grep("Div", colnames(pgrdata)), grep(pattern="^Support", x=colnames(pgrdata)))]
  head(pgrdata_Support)
  
  pgrdata_Support_Seminars <- pgrdata_Support[!is.na(pgrdata_Support$Support_Seminar),] %>% 
    group_by(Div, Support_Seminar) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Seminars$ID <- paste(paste(pgrdata_Support_Seminars$Div, "Seminars", sep="_"), pgrdata_Support_Seminars$Support_Seminar, sep ="_")
  head(pgrdata_Support_Seminars)
  
  pgrdata_Support_Mentoring <- pgrdata_Support[!is.na(pgrdata_Support$Support_Mentoring),] %>% 
    group_by(Div, Support_Mentoring) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Mentoring$ID <- paste(paste(pgrdata_Support_Mentoring$Div, "Mentoring", sep="_"), pgrdata_Support_Mentoring$Support_Mentoring, sep ="_")
  head(pgrdata_Support_Mentoring)
  
  pgrdata_Support_Coaching <- pgrdata_Support[!is.na(pgrdata_Support$Support_Coaching),] %>% 
    group_by(Div, Support_Coaching) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Coaching$ID <- paste(paste(pgrdata_Support_Coaching$Div, "Coaching", sep="_"), pgrdata_Support_Coaching$Support_Coaching, sep ="_")
  head(pgrdata_Support_Coaching)
  
  pgrdata_Support_Network <- pgrdata_Support[!is.na(pgrdata_Support$Support_Network),] %>% 
    group_by(Div, Support_Network) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Network$ID <- paste(paste(pgrdata_Support_Network$Div, "Support Networks", sep="_"), pgrdata_Support_Network$Support_Network, sep ="_")
  head(pgrdata_Support_Network)
  
  pgrdata_Support_Resources <- pgrdata_Support[!is.na(pgrdata_Support$Support_Resources),] %>% 
    group_by(Div, Support_Resources) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Resources$ID <- paste(paste(pgrdata_Support_Resources$Div, "Online Resources", sep="_"), pgrdata_Support_Resources$Support_Resources, sep ="_")
  head(pgrdata_Support_Resources)
  
  
  prgdata_Support_Freq <- data.frame(rbind(pgrdata_Support_Seminars[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Mentoring[, c('ID', 'n', 'perc')], 
                                           pgrdata_Support_Coaching[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Network[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Resources[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Support_Seminars,pgrdata_Support_Mentoring, pgrdata_Support_Coaching, pgrdata_Support_Network, pgrdata_Support_Resources)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Support$Div), each = 20) # 5 divisions 4 answers
  LabelIndiv <- rep(Supports, each = 4, times = 5) # 5 Support
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Support$Support_Seminar[!is.na(pgrdata_Support$Support_Seminar)]), times= 25) # 4 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Support_Freq <- merge(dataframe, prgdata_Support_Freq, by = "ID", all.x = TRUE)
  prgdata_Support_Freq <- prgdata_Support_Freq[, names(prgdata_Support_Freq) != "ID"]
 # rm(dataframe, prgdata_Support_Freq)
}
  
prgdata_Support_Freq


  # select subdataset
  pgrdata_Support <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c(grep("Div", colnames(pgrdata)), grep(pattern="^Support", x=colnames(pgrdata)))]
  head(pgrdata_Support)
  
  ## to move to formatpgrdata
  Supports
  Support_columns <- c(expr(Support_Seminar), expr(Support_Mentoring), expr(Support_Coaching), expr(Support_Network),expr(Support_Resources))
  Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")
  Divisions <- c("SSD", "Hum", "ContEd","MPLS", "MSD")
  
  # create skeleton of all possible answers
  
  create_skeleton <- function(question, Divisions, answers, columns){
    Div <- rep(Divisions, each = length(Divisions)*length(answers)) # 5 divisions 4 answers
    LabelIndiv <- rep(question, each = length(answers), times = length(columns)) # 5 Support
    Indiv <-paste(Div, LabelIndiv, sep ="_") 
    Answer <- rep(answers, times= length(Divisions)*length(columns)) # 4 possible answers
    ID <- paste(Indiv, Answer, sep="_") 
    skeleton <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  }
  
  skeleton <- create_skeleton(Supports, Divisions, Support_answers, Support_columns)
    

 # summarise items                                         
  
summarise_item <-  function(data, item, name_item){
       data2 <-  data[!is.na(data[,as.character(item)]),]  %>%  
                    group_by(Div,{{item}}) %>%
                    summarise (n = n()) %>% 
                    mutate(perc = n / sum(n) * 100 ) 
     data2$ID = paste(paste(data2$Div, name_item, sep="_"), unlist(data2[,as.character(item)]), sep ="_")
 return(data2[,c('ID', 'n', 'perc')])
  }

 
summaryitems <- list(length(Supports))
for (i in 1:length(Supports)) {
  summaryitems[[i]] <-  summarise_item(pgrdata_Support,Support_columns[[i]],Supports[i])
}

summaryitems <- data.frame(do.call(rbind, summaryitems))

# merge summary items to skeleton
data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Support_columns)

data











