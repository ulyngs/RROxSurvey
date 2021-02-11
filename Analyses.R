################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

## keeping track of decisions
#### I have not considered non pgr entries
#### if one item not scored in a grid, I remove those NAs for calculation of percentage for each responses for that item.
#Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')

rm(list = ls())

# Packages
{
library(dplyr)
library(here)
library(ggplot2)
library(RColorBrewer)
}

# load data
{
pgrdata <- read.csv(here("Data/RealData_20210210-1144.csv"), stringsAsFactors=FALSE)
# simdata <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE)
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE)
Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')
}

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

table(pgrdata$OtherDept)


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
targetnumbers <- merge(targetnumbers, data.frame(targetnumbers %>% group_by(Div) %>% summarise(RepStudent = StudentRecorded/StudentTotal*100,
                                                                                                RepStaff = StaffRecorded/StaffTotal*100)))
mean(targetnumbers$RepStudent, na.rm=TRUE) 
}

########

# pgrdata_Awareness
{
pgrdata_Awareness <- pgrdata[pgrdata$StudentStaff == "Student",  
                               c(grep("Div", colnames(pgrdata)), grep(pattern="^Awareness", x=colnames(pgrdata)))]
head(pgrdata_Awareness)


pgrdata_Awareness_OA <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_OA),] %>% 
  group_by(Div, Awareness_OA) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_OA$ID <- paste(paste(pgrdata_Awareness_OA$Div, "Open Access", sep="_"), pgrdata_Awareness_OA$Awareness_OA, sep ="_")
head(pgrdata_Awareness_OA)

pgrdata_Awareness_Data <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_Data),] %>% 
  group_by(Div, Awareness_Data) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_Data$ID <- paste(paste(pgrdata_Awareness_Data$Div, "Open Data", sep="_"), pgrdata_Awareness_Data$Awareness_Data, sep ="_")
head(pgrdata_Awareness_Data)

pgrdata_Awareness_Code <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_Code),] %>% 
  group_by(Div, Awareness_Code) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_Code$ID <- paste(paste(pgrdata_Awareness_Code$Div, "Open Code", sep="_"), pgrdata_Awareness_Code$Awareness_Code, sep ="_")
head(pgrdata_Awareness_Code)

pgrdata_Awareness_Materials <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_Materials),] %>% 
  group_by(Div, Awareness_Materials) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_Materials$ID <- paste(paste(pgrdata_Awareness_Materials$Div, "Open Materials", sep="_"), pgrdata_Awareness_Materials$Awareness_Materials, sep ="_")
head(pgrdata_Awareness_Materials)

pgrdata_Awareness_Preprint <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_Preprint),] %>% 
  group_by(Div, Awareness_Preprint) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_Preprint$ID <- paste(paste(pgrdata_Awareness_Preprint$Div, "Preprint", sep="_"), pgrdata_Awareness_Preprint$Awareness_Preprint, sep ="_")
head(pgrdata_Awareness_Preprint)

pgrdata_Awareness_Prereg <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_Prereg),] %>% 
                                  group_by(Div, Awareness_Prereg) %>%
                                  summarise (n = n()) %>% 
                                  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_Prereg$ID <- paste(paste(pgrdata_Awareness_Prereg$Div, "Preregistration", sep="_"), pgrdata_Awareness_Prereg$Awareness_Prereg, sep ="_")
head(pgrdata_Awareness_Prereg)

pgrdata_Awareness_RegRep <- pgrdata_Awareness[!is.na(pgrdata_Awareness$Awareness_RegRep),] %>% 
                                  group_by(Div, Awareness_RegRep) %>%
                                  summarise (n = n()) %>% 
                                  mutate(perc = n / sum(n) * 100 )
pgrdata_Awareness_RegRep$ID <- paste(paste(pgrdata_Awareness_RegRep$Div, "Registered Report", sep="_"), pgrdata_Awareness_RegRep$Awareness_RegRep, sep ="_")
head(pgrdata_Awareness_RegRep)

prgdata_Awareness_Freq <- data.frame(rbind(pgrdata_Awareness_OA[, c('ID', 'n', 'perc')],
                                             pgrdata_Awareness_Data[, c('ID', 'n', 'perc')], 
                                             pgrdata_Awareness_Code[, c('ID', 'n', 'perc')],
                                             pgrdata_Awareness_Materials[, c('ID', 'n', 'perc')],
                                             pgrdata_Awareness_Preprint[, c('ID', 'n', 'perc')],
                                             pgrdata_Awareness_Prereg[, c('ID', 'n', 'perc')],
                                             pgrdata_Awareness_RegRep[, c('ID', 'n', 'perc')]))

rm(pgrdata_Awareness_OA,pgrdata_Awareness_Data,pgrdata_Awareness_Code,pgrdata_Awareness_Materials,pgrdata_Awareness_Preprint,pgrdata_Awareness_Prereg, pgrdata_Awareness_RegRep)

# create structure with temp variables
Div <- rep(unique(pgrdata_Awareness$Div), each = 35) # 5 divisions
LabelIndiv <- rep(Measures, each = 5, times = 5) # 7 measures
Indiv <-paste(Div, LabelIndiv, sep ="_") 
Answer <- rep(unique(pgrdata_Awareness$Awareness_OA[!is.na(pgrdata_Awareness$Awareness_OA)]), times= 35) # 5 possible answers - remove 'NA'/ blank answers / skipped items
ID <- paste(Indiv, Answer, sep="_")
  
dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
rm(Div, LabelIndiv, Indiv, Answer, ID)

prgdata_Awareness_Freq <- merge(dataframe, prgdata_Awareness_Freq, by = "ID", all.x = TRUE)
pgrdata_Awareness <- prgdata_Awareness_Freq[, names(prgdata_Awareness_Freq) != "ID"]
rm(dataframe, prgdata_Awareness_Freq)
}

head(pgrdata_Awareness)
nrow(pgrdata_Awareness) # need to be 175
str(pgrdata_Awareness)

# plot pgrdata_Awareness
{
## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
data <- pgrdata_Awareness
str(data)

# Set a number of 'empty bar' to add at the end of each Div
empty_bar <- 2
nObsType <- nlevels(data$Answer)
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(Div, Indiv)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
rm(to_add)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$LabelIndiv <- sapply(strsplit(as.character(label_data$Indiv),"\\_"), `[`, 2)
  
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(Div) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]





## Make the plot
pgrdata_Awareness_plot <- ggplot(data) +      
  
  ### Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware / not sure if applicable",  "Not applicable" ))), stat="identity", alpha=0.5) +

  ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  ### Add text showing the value of each lines
  ggplot2::annotate("text", x = rep(max(data$id)-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0", "25", "50", "75", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  scale_fill_manual(values = c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                    breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
  
  ylim(-100,150) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
   # plot.margin = unit(rep(1,4), "cm") 
  ) +
  coord_polar() +
  
  ### Add labels on top of each bar
 geom_text(data=label_data, aes(x=id, y=105, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  ### Add base line information
 geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
 geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +

  ### Add title in the middle
  ggplot2::annotate("text", x = 0, y = -90, label = "Awareness" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
    
pgrdata_Awareness_plot

## Save at png
ggsave(pgrdata_Awareness_plot, file=here("Figures/pgrdata_Awareness.png"), width=10, height=8)

## clean up
rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle, pgrdata_Awareness_plot)

}

#######

# pgrdata_Effect
{
  pgrdata_Effect <- pgrdata[pgrdata$StudentStaff == "Student",  
                               c(grep("Div", colnames(pgrdata)), grep(pattern="^Effect", x=colnames(pgrdata)))]
  head(pgrdata_Effect)
  
  
  pgrdata_Effect_OA <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_OA),] %>% 
    group_by(Div, Effect_OA) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_OA$ID <- paste(paste(pgrdata_Effect_OA$Div, "Open Access", sep="_"), pgrdata_Effect_OA$Effect_OA, sep ="_")
  head(pgrdata_Effect_OA)
  
  pgrdata_Effect_Data <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_Data),] %>% 
    group_by(Div, Effect_Data) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_Data$ID <- paste(paste(pgrdata_Effect_Data$Div, "Open Data", sep="_"), pgrdata_Effect_Data$Effect_Data, sep ="_")
  head(pgrdata_Effect_Data)
  
  pgrdata_Effect_Code <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_Code),] %>% 
    group_by(Div, Effect_Code) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_Code$ID <- paste(paste(pgrdata_Effect_Code$Div, "Open Code", sep="_"), pgrdata_Effect_Code$Effect_Code, sep ="_")
  head(pgrdata_Effect_Code)
  
  pgrdata_Effect_Materials <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_Materials),] %>% 
    group_by(Div, Effect_Materials) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_Materials$ID <- paste(paste(pgrdata_Effect_Materials$Div, "Open Materials", sep="_"), pgrdata_Effect_Materials$Effect_Materials, sep ="_")
  head(pgrdata_Effect_Materials)
  
  pgrdata_Effect_Preprint <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_Preprint),] %>% 
    group_by(Div, Effect_Preprint) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_Preprint$ID <- paste(paste(pgrdata_Effect_Preprint$Div, "Preprint", sep="_"), pgrdata_Effect_Preprint$Effect_Preprint, sep ="_")
  head(pgrdata_Effect_Preprint)
  
  pgrdata_Effect_Prereg <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_Prereg),] %>% 
    group_by(Div, Effect_Prereg) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_Prereg$ID <- paste(paste(pgrdata_Effect_Prereg$Div, "Preregistration", sep="_"), pgrdata_Effect_Prereg$Effect_Prereg, sep ="_")
  head(pgrdata_Effect_Prereg)
  
  pgrdata_Effect_RegRep <- pgrdata_Effect[!is.na(pgrdata_Effect$Effect_RegRep),] %>% 
    group_by(Div, Effect_RegRep) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Effect_RegRep$ID <- paste(paste(pgrdata_Effect_RegRep$Div, "Registered Report", sep="_"), pgrdata_Effect_RegRep$Effect_RegRep, sep ="_")
  head(pgrdata_Effect_RegRep)
  
  prgdata_Effect_Freq <- data.frame(rbind(pgrdata_Effect_OA[, c('ID', 'n', 'perc')],
                                             pgrdata_Effect_Data[, c('ID', 'n', 'perc')], 
                                             pgrdata_Effect_Code[, c('ID', 'n', 'perc')],
                                             pgrdata_Effect_Materials[, c('ID', 'n', 'perc')],
                                             pgrdata_Effect_Preprint[, c('ID', 'n', 'perc')],
                                             pgrdata_Effect_Prereg[, c('ID', 'n', 'perc')],
                                             pgrdata_Effect_RegRep[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Effect_OA,pgrdata_Effect_Data,pgrdata_Effect_Code,pgrdata_Effect_Materials,pgrdata_Effect_Preprint,pgrdata_Effect_Prereg, pgrdata_Effect_RegRep)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Effect$Div), each = 35) # 5 divisions 5 answers
  LabelIndiv <- rep(Measures, each = 5, times = 5) # 7 measures
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
      ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Effect$Effect_RegRep[!is.na(pgrdata_Effect$Effect_RegRep)]), times= 35) # 5 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Effect_Freq <- merge(dataframe, prgdata_Effect_Freq, by = "ID", all.x = TRUE)
  pgrdata_Effect <- prgdata_Effect_Freq[, names(prgdata_Effect_Freq) != "ID"]
  rm(dataframe, prgdata_Effect_Freq)
}

# plot pgrdata_Effect
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Effect
  str(data)
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, Indiv)
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  rm(to_add)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  label_data$LabelIndiv <- sapply(strsplit(as.character(label_data$Indiv),"\\_"), `[`, 2)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(Div) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  
  
  
  ## Make the plot
  pgrdata_Effect_plot <- ggplot(data) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Beneficial", "Neutral(neither detrimental nor beneficial)", "Detrimental",  "Not sure",  "Not applicable" ))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-0.3, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines
    ggplot2::annotate("text", x = rep(max(data$id)-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0", "25", "50", "75", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    scale_fill_manual(values = c("black", "#666666", "#D95F02", "#E6AB02", "#1B9E77"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not applicable", "Not sure", "Detrimental", "Neutral(neither detrimental nor beneficial)", "Beneficial"),
                      labels =c("Not applicable", "Not sure", "Detrimental", "Neutral (neither detrimental nor beneficial)", "Beneficial")
                      , drop = FALSE)+
    
    ylim(-100,150) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
      # plot.margin = unit(rep(1,4), "cm") 
    ) +
    coord_polar() +
    
    ### Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=105, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -90, label = "Effect" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Effect_plot
  
  ## Save at png
  ggsave(pgrdata_Effect_plot, file=here("Figures/pgrdata_Effect.png"), width=10, height=8)
  
  ## clean up
 rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle, pgrdata_Effect_plot)
  
}



