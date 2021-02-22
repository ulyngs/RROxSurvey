################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

## keeping track of decisions
#### I have not considered non pgr entries
#### if one item not scored in a grid, I remove those NAs for calculation of percentage for each responses for that item. (i.e. like if items were independants)
#### for Barriers: select items if at least one answer is not NA to calculate percentages (like if items were independents)


rm(list = ls())

# Packages
{
library(tidyverse)
library(here)
library(RColorBrewer)
  
  brewer.pal(n = 11, name = "RdYlBu")
  display.brewer.pal(n = 11, name = 'RdYlBu')
  display.brewer.pal(n = 11, name = 'RdBu')
  brewer.pal(n = 11, name = "RdBu")
}

# load data
{
pgrdata <- read.csv(here("Data/RealData_20210218-1058.csv"), stringsAsFactors=FALSE)
# simdata <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE)
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE)

Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')
Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')
Trainings <- c('Open Access', 'Data Management Plan', 'FAIR Data','Ethics','Open Code', 'Open Materials', 'Licences', 'Preprint', 'Preregistration')
Supports <- c('Seminars', 'Mentoring', 'Coaching', 'Support Networks', 'Online Resources')

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

###############
## Awareness ##
###############

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
data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
str(data)

# Set a number of 'empty bar' to add at the end of each Div
empty_bar <- 2
nObsType <- nlevels(data$Answer)
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType) # this is x in the plot - determines the order of the bars
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
  geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  ### Add text showing the value of each lines max(data$id-0.1)
  ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
  
  #scale_fill_manual(values = c("black", "#B2182B", "#F4A582", "#F7F7F7", "#053061"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
    #                breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
  
  #scale_fill_manual(values = c("black", "#D73027", "#FDAE61", "#FFFFBF", "#313695"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
     #               breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
 
   scale_fill_manual(values = c("black", "#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                    breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
  
  #scale_fill_manual(values = c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
   #                 breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
  
  scale_x_discrete(expand = c(0, 0)) + # remove padding
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
# ggsave(pgrdata_Awareness_plot, file=here("Figures/pgrdata_Awareness.png"), width=10, height=8)

## clean up
rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)

}

############
## Effect ##
############

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
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
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
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    ### Add text showing the value of each lines max(data$id-0.1)
   ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
   
   scale_fill_manual(values = c("black", "#666666", "#D95F02", "#E6AB02", "#1B9E77"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                       breaks=c("Not applicable", "Not sure", "Detrimental", "Neutral(neither detrimental nor beneficial)", "Beneficial"),
                       labels =c("Not applicable", "Not sure", "Detrimental", "Neutral (neither detrimental nor beneficial)", "Beneficial")
                       , drop = FALSE)+
    
    scale_x_discrete(expand = c(0, 0)) +
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
  # ggsave(pgrdata_Effect_plot, file=here("Figures/pgrdata_Effect.png"), width=10, height=8)
  
  ## clean up
 rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

##############
## Barriers ##
##############

# pgrdata_Barriers
{
pgrdata_Barriers <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c(grep("Div", colnames(pgrdata)), grep(pattern="^Barriers", x=colnames(pgrdata)))]
head(pgrdata_Barriers)
nrow(pgrdata_Barriers)

Barriers_short <- c("Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None", "NotSure","NA")
Barriers <- c("Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None", "Not sure","Not applicable")

## pgrdata_Barriers_OA
{
pgrdata_Barriers_OA <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_OA*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_OA)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_OA <- pgrdata_Barriers_OA[rowSums(!is.na(pgrdata_Barriers_OA)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_OA <- pgrdata_Barriers_OA %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_OA_Freq <- 
  
  (pgrdata_Barriers_OA %>% 
    group_by(Div) %>% 
    summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_OA_Freq <- merge(pgrdata_Barriers_OA_Freq, NbRespondents_Barriers_OA, by = 'Div', all.x = TRUE)
pgrdata_Barriers_OA_Freq$perc <- pgrdata_Barriers_OA_Freq$n/pgrdata_Barriers_OA_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_OA)

pgrdata_Barriers_OA <- pgrdata_Barriers_OA_Freq
rm(pgrdata_Barriers_OA_Freq)                     
                   
pgrdata_Barriers_OA$ID <- paste(pgrdata_Barriers_OA$Div, pgrdata_Barriers_OA$Barriers, sep="_")
}

head(pgrdata_Barriers_OA)


## pgrdata_Barriers_Data 
{
pgrdata_Barriers_Data <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_Data*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_Data)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_Data <- pgrdata_Barriers_Data[rowSums(!is.na(pgrdata_Barriers_Data)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_Data <- pgrdata_Barriers_Data %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_Data_Freq <- 
  
  (pgrdata_Barriers_Data %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_Data_Freq <- merge(pgrdata_Barriers_Data_Freq, NbRespondents_Barriers_Data, by = 'Div', all.x = TRUE)
pgrdata_Barriers_Data_Freq$perc <- pgrdata_Barriers_Data_Freq$n/pgrdata_Barriers_Data_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_Data)

pgrdata_Barriers_Data <- pgrdata_Barriers_Data_Freq
rm(pgrdata_Barriers_Data_Freq)                     

pgrdata_Barriers_Data$ID <- paste(pgrdata_Barriers_Data$Div, pgrdata_Barriers_Data$Barriers, sep="_")
}

head(pgrdata_Barriers_Data)
  

## pgrdata_Barriers_Code 
{
pgrdata_Barriers_Code <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_Code*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_Code)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_Code <- pgrdata_Barriers_Code[rowSums(!is.na(pgrdata_Barriers_Code)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_Code <- pgrdata_Barriers_Code %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_Code_Freq <- 
  
  (pgrdata_Barriers_Code %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_Code_Freq <- merge(pgrdata_Barriers_Code_Freq, NbRespondents_Barriers_Code, by = 'Div', all.x = TRUE)
pgrdata_Barriers_Code_Freq$perc <- pgrdata_Barriers_Code_Freq$n/pgrdata_Barriers_Code_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_Code)

pgrdata_Barriers_Code <- pgrdata_Barriers_Code_Freq
rm(pgrdata_Barriers_Code_Freq)                     

pgrdata_Barriers_Code$ID <- paste(pgrdata_Barriers_Code$Div, pgrdata_Barriers_Code$Barriers, sep="_")
}

head(pgrdata_Barriers_Code)


## pgrdata_Barriers_Materials 
{
pgrdata_Barriers_Materials <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_Materials*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_Materials)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_Materials <- pgrdata_Barriers_Materials[rowSums(!is.na(pgrdata_Barriers_Materials)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_Materials <- pgrdata_Barriers_Materials %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_Materials_Freq <- 
  
  (pgrdata_Barriers_Materials %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_Materials_Freq <- merge(pgrdata_Barriers_Materials_Freq, NbRespondents_Barriers_Materials, by = 'Div', all.x = TRUE)
pgrdata_Barriers_Materials_Freq$perc <- pgrdata_Barriers_Materials_Freq$n/pgrdata_Barriers_Materials_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_Materials)

pgrdata_Barriers_Materials <- pgrdata_Barriers_Materials_Freq
rm(pgrdata_Barriers_Materials_Freq)                     

pgrdata_Barriers_Materials$ID <- paste(pgrdata_Barriers_Materials$Div, pgrdata_Barriers_Materials$Barriers, sep="_")
}

head(pgrdata_Barriers_Materials)


## pgrdata_Barriers_Preprint 
{
pgrdata_Barriers_Preprint <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_Preprint*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_Preprint)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_Preprint <- pgrdata_Barriers_Preprint[rowSums(!is.na(pgrdata_Barriers_Preprint)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_Preprint <- pgrdata_Barriers_Preprint %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_Preprint_Freq <- 
  
  (pgrdata_Barriers_Preprint %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_Preprint_Freq <- merge(pgrdata_Barriers_Preprint_Freq, NbRespondents_Barriers_Preprint, by = 'Div', all.x = TRUE)
pgrdata_Barriers_Preprint_Freq$perc <- pgrdata_Barriers_Preprint_Freq$n/pgrdata_Barriers_Preprint_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_Preprint)
   
pgrdata_Barriers_Preprint <- pgrdata_Barriers_Preprint_Freq
rm(pgrdata_Barriers_Preprint_Freq)                     

pgrdata_Barriers_Preprint$ID <- paste(pgrdata_Barriers_Preprint$Div, pgrdata_Barriers_Preprint$Barriers, sep="_")
}

head(pgrdata_Barriers_Preprint)
  

## pgrdata_Barriers_Prereg 
{
pgrdata_Barriers_Prereg <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_Prereg*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_Prereg)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_Prereg <- pgrdata_Barriers_Prereg[rowSums(!is.na(pgrdata_Barriers_Prereg)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_Prereg <- pgrdata_Barriers_Prereg %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_Prereg_Freq <- 
  
  (pgrdata_Barriers_Prereg %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_Prereg_Freq <- merge(pgrdata_Barriers_Prereg_Freq, NbRespondents_Barriers_Prereg, by = 'Div', all.x = TRUE)
pgrdata_Barriers_Prereg_Freq$perc <- pgrdata_Barriers_Prereg_Freq$n/pgrdata_Barriers_Prereg_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_Prereg)

pgrdata_Barriers_Prereg <- pgrdata_Barriers_Prereg_Freq
rm(pgrdata_Barriers_Prereg_Freq)                     

pgrdata_Barriers_Prereg$ID <- paste(pgrdata_Barriers_Prereg$Div, pgrdata_Barriers_Prereg$Barriers, sep="_")
}

head(pgrdata_Barriers_Prereg) 
  

## pgrdata_Barriers_RegRep 
{
pgrdata_Barriers_RegRep <- pgrdata_Barriers[, c(grep("Div", colnames(pgrdata_Barriers)), grep(pattern="Barriers_RegRep*", x=colnames(pgrdata_Barriers)))]
nrow(pgrdata_Barriers_RegRep)

#### remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
pgrdata_Barriers_RegRep <- pgrdata_Barriers_RegRep[rowSums(!is.na(pgrdata_Barriers_RegRep)) > 1, ]

#### number of scores
##### number of time the item was scored
NbRespondents_Barriers_RegRep <- pgrdata_Barriers_RegRep %>% group_by(Div) %>% summarise(NbRespondents = n())

##### counts of each answer
pgrdata_Barriers_RegRep_Freq <- 
  
  (pgrdata_Barriers_RegRep %>% 
     group_by(Div) %>% 
     summarise(across (everything(), ~sum(!is.na(.)))) ) %>% 
  
  pivot_longer(!Div, names_to = "Barriers" , values_to = "n")

##### caluclate percentages of respondent having selected (non mutually exclusively) each answer
pgrdata_Barriers_RegRep_Freq <- merge(pgrdata_Barriers_RegRep_Freq, NbRespondents_Barriers_RegRep, by = 'Div', all.x = TRUE)
pgrdata_Barriers_RegRep_Freq$perc <- pgrdata_Barriers_RegRep_Freq$n/pgrdata_Barriers_RegRep_Freq$NbRespondents * 100
rm(NbRespondents_Barriers_RegRep)

pgrdata_Barriers_RegRep <- pgrdata_Barriers_RegRep_Freq
rm(pgrdata_Barriers_RegRep_Freq)                     

pgrdata_Barriers_RegRep$ID <- paste(pgrdata_Barriers_RegRep$Div, pgrdata_Barriers_RegRep$Barriers, sep="_")
}

head(pgrdata_Barriers_RegRep)


## Combine all
{
prgdata_Barriers_Freq <- data.frame(rbind(pgrdata_Barriers_OA[, c('ID', 'n', 'perc')],
                                          pgrdata_Barriers_Data[, c('ID', 'n', 'perc')], 
                                          pgrdata_Barriers_Code[, c('ID', 'n', 'perc')],
                                          pgrdata_Barriers_Materials[, c('ID', 'n', 'perc')],
                                          pgrdata_Barriers_Preprint[, c('ID', 'n', 'perc')],
                                          pgrdata_Barriers_Prereg[, c('ID', 'n', 'perc')],
                                          pgrdata_Barriers_RegRep[, c('ID', 'n', 'perc')]))
  
rm(pgrdata_Barriers_OA,pgrdata_Barriers_Data,pgrdata_Barriers_Code,pgrdata_Barriers_Materials,pgrdata_Barriers_Preprint,pgrdata_Barriers_Prereg, pgrdata_Barriers_RegRep)
  
### create structure with temp variables (needed for the legend in case some values were not represented)
Div <- rep(sort(unique(pgrdata_Barriers$Div)), each = 63) # 5 divisions 9 answers 7 measures
LabelIndiv <- rep(Measures, each = 9, times = 5) # 7 measures
LabelIndivShort <- rep(Measures_short, each = 9, times = 5) # 7 measures
Indiv <-paste(Div , paste( "Barriers", LabelIndivShort, sep = "_"), sep ="_") 
Answer <- rep(Barriers_short, times= 35)
ID <- paste(Indiv, Answer, sep="_")
sortingID <-  seq(1:length(ID))

dataframe <- data.frame(sortingID, ID, Indiv, Div, LabelIndiv, Answer)

rm(Div, LabelIndiv, Indiv, Answer, ID, sortingID, LabelIndivShort)

prgdata_Barriers_Freq <- merge(dataframe, prgdata_Barriers_Freq, by = "ID", all.x = TRUE)
pgrdata_Barriers <- prgdata_Barriers_Freq[order(prgdata_Barriers_Freq$sortingID), names(prgdata_Barriers_Freq) != "ID"]
rm(dataframe, prgdata_Barriers_Freq)
}
}

head(pgrdata_Barriers)

# plot pgrdata_Barriers
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Barriers
  str(data)
  head(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv)  # this will determine order of the bars
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  rm(to_add)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  label_data <- merge(label_data, unique(data[,c('Indiv', 'LabelIndiv')]), all.x= TRUE, by = 'Indiv')

  
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
  pgrdata_Barriers_plot <- ggplot(data) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("None", "Other", "Policy", "Incentives", "Norms" , "Training", "Infrastructure", "NotSure", "NA"))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +

  
   scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    
    scale_x_discrete(expand = c(0, 0)) + # remove padding
    ylim(-100,max(label_data$tot, na.rm=T)+30) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title=element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
      # plot.margin = unit(rep(1,4), "cm") 
    ) +
    coord_polar() +
    
    ### Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+10, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -90, label = "Barriers" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Barriers_plot
  
  ## Save at png
 # ggsave(pgrdata_Barriers_plot, file=here("Figures/pgrdata_Barriers.png"), width=10, height=8)
  
  ## clean up
 rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

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
pgrdata_OtherBarriers$OtherBarriers_OA_recode[str_detect(pgrdata_OtherBarriers$OtherBarriers_OA, c("fee|fees|cost|costs|costly|money|expensive|funding|funds|financial|pay|charges"))] <- 'ficancial cost'
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

###############
## Downsides ##
###############

# pgrdata_Downsides
{
pgrdata_Downsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                            c(grep("Div", colnames(pgrdata)), grep(pattern="^Downsides", x=colnames(pgrdata)))]
head(pgrdata_Downsides)

pgrdata_Downsides_OA <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_OA),] %>% 
  group_by(Div, Downsides_OA) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_OA$ID <- paste(paste(pgrdata_Downsides_OA$Div, "Open Access", sep="_"), pgrdata_Downsides_OA$Downsides_OA, sep ="_")
head(pgrdata_Downsides_OA)

pgrdata_Downsides_Data <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Data),] %>% 
  group_by(Div, Downsides_Data) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_Data$ID <- paste(paste(pgrdata_Downsides_Data$Div, "Open Data", sep="_"), pgrdata_Downsides_Data$Downsides_Data, sep ="_")
head(pgrdata_Downsides_Data)

pgrdata_Downsides_Code <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Code),] %>% 
  group_by(Div, Downsides_Code) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_Code$ID <- paste(paste(pgrdata_Downsides_Code$Div, "Open Code", sep="_"), pgrdata_Downsides_Code$Downsides_Code, sep ="_")
head(pgrdata_Downsides_Code)

pgrdata_Downsides_Materials <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Materials),] %>% 
  group_by(Div, Downsides_Materials) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_Materials$ID <- paste(paste(pgrdata_Downsides_Materials$Div, "Open Materials", sep="_"), pgrdata_Downsides_Materials$Downsides_Materials, sep ="_")
head(pgrdata_Downsides_Materials)

pgrdata_Downsides_Preprint <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Preprint),] %>% 
  group_by(Div, Downsides_Preprint) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_Preprint$ID <- paste(paste(pgrdata_Downsides_Preprint$Div, "Preprint", sep="_"), pgrdata_Downsides_Preprint$Downsides_Preprint, sep ="_")
head(pgrdata_Downsides_Preprint)

pgrdata_Downsides_Prereg <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Prereg),] %>% 
  group_by(Div, Downsides_Prereg) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_Prereg$ID <- paste(paste(pgrdata_Downsides_Prereg$Div, "Preregistration", sep="_"), pgrdata_Downsides_Prereg$Downsides_Prereg, sep ="_")
head(pgrdata_Downsides_Prereg)

pgrdata_Downsides_RegRep <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_RegRep),] %>% 
  group_by(Div, Downsides_RegRep) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
pgrdata_Downsides_RegRep$ID <- paste(paste(pgrdata_Downsides_RegRep$Div, "Registered Report", sep="_"), pgrdata_Downsides_RegRep$Downsides_RegRep, sep ="_")
head(pgrdata_Downsides_RegRep)

prgdata_Downsides_Freq <- data.frame(rbind(pgrdata_Downsides_OA[, c('ID', 'n', 'perc')],
                                        pgrdata_Downsides_Data[, c('ID', 'n', 'perc')], 
                                        pgrdata_Downsides_Code[, c('ID', 'n', 'perc')],
                                        pgrdata_Downsides_Materials[, c('ID', 'n', 'perc')],
                                        pgrdata_Downsides_Preprint[, c('ID', 'n', 'perc')],
                                        pgrdata_Downsides_Prereg[, c('ID', 'n', 'perc')],
                                        pgrdata_Downsides_RegRep[, c('ID', 'n', 'perc')]))

rm(pgrdata_Downsides_OA,pgrdata_Downsides_Data,pgrdata_Downsides_Code,pgrdata_Downsides_Materials,pgrdata_Downsides_Preprint,pgrdata_Downsides_Prereg, pgrdata_Downsides_RegRep)

# create structure with temp variables
Div <- rep(unique(pgrdata_Downsides$Div), each = 28) # 5 divisions 4 answers
LabelIndiv <- rep(Measures, each = 4, times = 5) # 7 measures
Indiv <-paste(Div, LabelIndiv, sep ="_") 
## need to pick an item where all responses are represented
Answer <- rep(unique(pgrdata_Downsides$Downsides_RegRep[!is.na(pgrdata_Downsides$Downsides_RegRep)]), times= 35) # 5 possible answers - remove 'NA'/ blank answers / skipped items
ID <- paste(Indiv, Answer, sep="_")

dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
rm(Div, LabelIndiv, Indiv, Answer, ID)

prgdata_Downsides_Freq <- merge(dataframe, prgdata_Downsides_Freq, by = "ID", all.x = TRUE)
pgrdata_Downsides <- prgdata_Downsides_Freq[, names(prgdata_Downsides_Freq) != "ID"]
rm(dataframe, prgdata_Downsides_Freq)
}

# plot pgrdata_Downsides
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Downsides
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
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
  pgrdata_Downsides_plot <- ggplot(data) +      #  c("Not applicable", "Not sure",  "Yes",  "No" )
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("No", "Yes",  "Not sure",  "Not applicable" ))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    scale_fill_manual(values = c("black", "#666666", "#D95F02", "#1B9E77"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not applicable", "Not sure", "Yes", "No"),
                      labels =c("Not applicable", "Not sure", "Yes", "No")
                      , drop = FALSE)+
    

    scale_x_discrete(expand = c(0, 0)) +
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
    ggplot2::annotate("text", x = 0, y = -90, label = "Downsides" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Downsides_plot
  
  ## Save at png
  # ggsave(pgrdata_Downsides_plot, file=here("Figures/pgrdata_Downsides.png"), width=10, height=8)
  
  ## clean up
  rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# pgrdata_WhatDownsides
{
  pgrdata_WhatDownsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^WhatDownsides", x=colnames(pgrdata)))]
  head(pgrdata_WhatDownsides)
  
  pgrdata_WhatDownsides <- pgrdata_WhatDownsides[rowSums(!is.na(pgrdata_WhatDownsides)) > 1, ]
   
  ## Nb of responses
  pgrdata_WhatDownsides %>% summarise(across (everything(), ~sum(!is.na(.))))
  
  
  ## categorise barriers
  pgrdata_WhatDownsides$WhatDownsides_OA_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Data_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Code_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Materials_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Preprint_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_Prereg_recode <- NA
  pgrdata_WhatDownsides$WhatDownsides_RegRep_recode <- NA
  
  ### OA
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "harmful|Inappropriate")] <- 'harmful or inappropriate application or interpretation of the research'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, c("fee|fees|cost|costs|costly|money|expensive|Expensive|funding|funds|financial|pay|charges"))] <- 'ficancial cost (including inequalities of access to publishing between institutions'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "commercialisation|Patenting")] <- 'Intellectual property concerns'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "journal income|funding to produce the journals|funding the production")] <- 'loss of journal income, need to find other means of journal production' # this needs to overwrite category 'financial costs to the author'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "may reduce the quality of peer review|less rigorous")] <- 'may reduce quality of peer review, or OA articles found to be less rigorous' # this needs to overwrite category 'financial costs to the author'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "duplication of research|plagiarism")] <- 'concerns about duplication of research or plagiarism'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "limits publication options")] <- 'fewer (prestigious) journal options'
  pgrdata_WhatDownsides$WhatDownsides_OA_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_OA, "express their opinion")] <- 'If all information is public, people would be less able to express their opinion'
 
  table(pgrdata_WhatDownsides$WhatDownsides_OA_recode)
  #x <-  pgrdata_WhatDownsides[!is.na(pgrdata_WhatDownsides$WhatDownsides_OA),c('WhatDownsides_OA','WhatDownsides_OA_recode')]
  
  # ## Data
  # pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "industry")] <- 'industry collaboration policies'
  # pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "managing data")] <- 'data management difficult and lack convention for metadata'
  # pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "anonymising|complex governance requirements|sensitive|privacy")] <- 'sensitive data'
  # pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "I do not have the authority to share")] <- 'data not owned'
  # pgrdata_WhatDownsides$WhatDownsides_Data_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Data, "scannes oeuvers")] <- 'data not always digital'
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_Data_recode)
  # 
  # ## Code
  # pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "sharing research ideas")] <- 'fear of scooping'
  # pgrdata_WhatDownsides$WhatDownsides_Code_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Code, "time")] <- 'time investment'
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_Code_recode)
  # 
  # ## Materials
  # pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "sharing research ideas")] <- 'fear of scooping'
  # pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "managing data")] <- 'materials management difficult and lack convention for metadata'
  # pgrdata_WhatDownsides$WhatDownsides_Materials_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Materials, "libraries were not accessible")] <- 'materials not always digital'
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_Materials_recode)
  # 
  # ## Preprint
  # pgrdata_WhatDownsides$WhatDownsides_Preprint_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Preprint, "sharing research ideas")] <- 'fear of scooping'
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_Preprint_recode)
  # 
  # ## Preregistration
  # pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "sharing research ideas")] <- 'fear of scooping'
  # pgrdata_WhatDownsides$WhatDownsides_Prereg_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_Prereg, "funding to run a pilot study")] <- 'lack of funding for pilot studies'
  # 
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_Prereg_recode)
  # 
  # 
  # ## Registered Report
  # pgrdata_WhatDownsides$WhatDownsides_RegRep_recode[str_detect(pgrdata_WhatDownsides$WhatDownsides_RegRep, "inherently messy data")] <- 'challenge inherited from the the difficulties associated with data management'
  # 
  # table(pgrdata_WhatDownsides$WhatDownsides_RegRep_recode)
  
  
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


##############
## Training ##
##############

# pgrdata_Training
{
  pgrdata_Training <- pgrdata[pgrdata$StudentStaff == "Student",  
                               c(grep("Div", colnames(pgrdata)), grep(pattern="^Training", x=colnames(pgrdata)))]
  head(pgrdata_Training)
  
  pgrdata_Training_OA <- pgrdata_Training[!is.na(pgrdata_Training$Training_OA),] %>% 
    group_by(Div, Training_OA) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_OA$ID <- paste(paste(pgrdata_Training_OA$Div, "Open Access", sep="_"), pgrdata_Training_OA$Training_OA, sep ="_")
  head(pgrdata_Training_OA)
  
  pgrdata_Training_DMP <- pgrdata_Training[!is.na(pgrdata_Training$Training_DMP),] %>% 
    group_by(Div, Training_DMP) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_DMP$ID <- paste(paste(pgrdata_Training_DMP$Div, "Data Management Plan", sep="_"), pgrdata_Training_DMP$Training_DMP, sep ="_")
  head(pgrdata_Training_DMP)
  
  pgrdata_Training_FAIR <- pgrdata_Training[!is.na(pgrdata_Training$Training_FAIR),] %>% 
    group_by(Div, Training_FAIR) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_FAIR$ID <- paste(paste(pgrdata_Training_FAIR$Div, "FAIR Data", sep="_"), pgrdata_Training_FAIR$Training_FAIR, sep ="_")
  head(pgrdata_Training_FAIR)
  
  pgrdata_Training_Ethics <- pgrdata_Training[!is.na(pgrdata_Training$Training_Ethics),] %>% 
    group_by(Div, Training_Ethics) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Ethics$ID <- paste(paste(pgrdata_Training_Ethics$Div, "Ethics", sep="_"), pgrdata_Training_Ethics$Training_Ethics, sep ="_")
  head(pgrdata_Training_Ethics)
  
  pgrdata_Training_Code <- pgrdata_Training[!is.na(pgrdata_Training$Training_Code),] %>% 
    group_by(Div, Training_Code) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Code$ID <- paste(paste(pgrdata_Training_Code$Div, "Open Code", sep="_"), pgrdata_Training_Code$Training_Code, sep ="_")
  head(pgrdata_Training_Code)
  
  pgrdata_Training_Materials <- pgrdata_Training[!is.na(pgrdata_Training$Training_Materials),] %>% 
    group_by(Div, Training_Materials) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Materials$ID <- paste(paste(pgrdata_Training_Materials$Div, "Open Materials", sep="_"), pgrdata_Training_Materials$Training_Materials, sep ="_")
  head(pgrdata_Training_Materials)
  
  pgrdata_Training_Licences <- pgrdata_Training[!is.na(pgrdata_Training$Training_Licences),] %>% 
    group_by(Div, Training_Licences) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Licences$ID <- paste(paste(pgrdata_Training_Licences$Div, "Licences", sep="_"), pgrdata_Training_Licences$Training_Licences, sep ="_")
  head(pgrdata_Training_Licences)
  
  pgrdata_Training_Prereg <- pgrdata_Training[!is.na(pgrdata_Training$Training_Prereg),] %>% 
    group_by(Div, Training_Prereg) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Prereg$ID <- paste(paste(pgrdata_Training_Prereg$Div, "Preregistration", sep="_"), pgrdata_Training_Prereg$Training_Prereg, sep ="_")
  head(pgrdata_Training_Prereg)
  
  pgrdata_Training_Preprint <- pgrdata_Training[!is.na(pgrdata_Training$Training_Preprint),] %>% 
    group_by(Div, Training_Preprint) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Preprint$ID <- paste(paste(pgrdata_Training_Preprint$Div, "Preprint", sep="_"), pgrdata_Training_Preprint$Training_Preprint, sep ="_")
  head(pgrdata_Training_Preprint)
  
  pgrdata_Training_Recruitement <- pgrdata_Training[!is.na(pgrdata_Training$Training_Recruitement),] %>% 
    group_by(Div, Training_Recruitement) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Recruitement$ID <- paste(paste(pgrdata_Training_Recruitement$Div, "Recruitement", sep="_"), pgrdata_Training_Recruitement$Training_Recruitement, sep ="_")
  head(pgrdata_Training_Recruitement)
  

  
  
  
  prgdata_Training_Freq <- data.frame(rbind(pgrdata_Training_OA[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_DMP[, c('ID', 'n', 'perc')], 
                                             pgrdata_Training_FAIR[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Ethics[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Code[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Materials[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Licences[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Preprint[, c('ID', 'n', 'perc')],
                                             pgrdata_Training_Prereg[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Training_OA,pgrdata_Training_DMP, pgrdata_Training_FAIR, pgrdata_Training_Ethics, pgrdata_Training_Code,pgrdata_Training_Materials,pgrdata_Training_Preprint,pgrdata_Training_Prereg, pgrdata_Training_Licences)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Training$Div), each = 54) # 5 divisions 6 answers
  LabelIndiv <- rep(Trainings, each = 6, times = 5) # 9 training
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Training$Training_OA[!is.na(pgrdata_Training$Training_OA)]), times= 45) # 6 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Training_Freq <- merge(dataframe, prgdata_Training_Freq, by = "ID", all.x = TRUE)
  pgrdata_Training <- prgdata_Training_Freq[, names(prgdata_Training_Freq) != "ID"]
  rm(dataframe, prgdata_Training_Freq)
}

# plot pgrdata_Training
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Training
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Trainings) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
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
  pgrdata_Training_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" ))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    scale_fill_manual(values = c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C'), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training"), 
                      labels =c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training"), 
                      drop = FALSE)+
    
    #scale_fill_manual(values = c("black", "#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
      #                breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
    
    
    scale_x_discrete(expand = c(0, 0)) +
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
    ggplot2::annotate("text", x = 0, y = -90, label = "Training" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Training_plot
  
  ## Save at png
  # ggsave(pgrdata_Training_plot, file=here("Figures/pgrdata_Training.png"), width=10, height=8)
  
  ## clean up
  rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# pgrdata_OtherTraining
{
pgrdata_OtherTraining <- pgrdata[pgrdata$StudentStaff == "Student",  
                            c(grep("Div", colnames(pgrdata)), grep(pattern="^Training_Other", x=colnames(pgrdata)))]

head(pgrdata_OtherTraining)


pgrdata_OtherTraining <- pgrdata_OtherTraining[rowSums(!is.na(pgrdata_OtherTraining)) > 1, ]
colnames(pgrdata_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
pgrdata_OtherTraining <- rbind(pgrdata_OtherTraining[,c(1,2,3)], pgrdata_OtherTraining[,c(1,4,5)], pgrdata_OtherTraining[, c(1,6,7)])
pgrdata_OtherTraining <- pgrdata_OtherTraining[!is.na(pgrdata_OtherTraining$Training_Other),] 

  ## Nb of responses
  nrow(pgrdata_OtherTraining)
  
}

# pgrdata_OtherTraining
{
  pgrdata_Staff_OtherTraining <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^Training_Other", x=colnames(pgrdata)))]
  
  head(pgrdata_Staff_OtherTraining)
  
  
  pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[rowSums(!is.na(pgrdata_Staff_OtherTraining)) > 1, ]
  colnames(pgrdata_Staff_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
  pgrdata_Staff_OtherTraining <- rbind(pgrdata_Staff_OtherTraining[,c(1,2,3)], pgrdata_Staff_OtherTraining[,c(1,4,5)], pgrdata_Staff_OtherTraining[, c(1,6,7)])
  pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[!is.na(pgrdata_Staff_OtherTraining$Training_Other),] 
  
  ## Nb of responses
  nrow(pgrdata_Staff_OtherTraining)
  
}


##############
## Support ##
##############

# pgrdata_Support
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
  pgrdata_Support <- prgdata_Support_Freq[, names(prgdata_Support_Freq) != "ID"]
  rm(dataframe, prgdata_Support_Freq)
}

# plot pgrdata_Support
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Support
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Supports) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
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
  pgrdata_Support_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Essential", "Useful", "Not useful", "Not sure"))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    scale_fill_manual(values = c("black", "#ABDDA4", "#FFFFBF", '#D7191C'), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not sure", "Not useful", "Useful", "Essential"), 
                      labels =c("Not sure", "Not useful", "Useful", "Essential"), 
                      drop = FALSE)+
    
    
    scale_x_discrete(expand = c(0, 0)) +
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
    ggplot2::annotate("text", x = 0, y = -90, label = "Support" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Support_plot
  
  ## Save at png
  # ggsave(pgrdata_Support_plot, file=here("Figures/pgrdata_Support.png"), width=10, height=8)
  
  ## clean up
  rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# pgrdata_OtherSupport
{
  pgrdata_OtherSupport <- pgrdata[pgrdata$StudentStaff == "Student",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^Support_Other", x=colnames(pgrdata)))]
  
  head(pgrdata_OtherSupport)
  
  
  pgrdata_OtherSupport <- pgrdata_OtherSupport[rowSums(!is.na(pgrdata_OtherSupport)) > 1, ]
  colnames(pgrdata_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
  pgrdata_OtherSupport <- rbind(pgrdata_OtherSupport[,c(1,2,3)], pgrdata_OtherSupport[,c(1,4,5)], pgrdata_OtherSupport[, c(1,6,7)])
  pgrdata_OtherSupport <- pgrdata_OtherSupport[!is.na(pgrdata_OtherSupport$Support_Other),] 
  
  ## Nb of responses
  nrow(pgrdata_OtherSupport)
  
}

# pgrdata_OtherSupport
{
  pgrdata_Staff_OtherSupport <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                         c(grep("Div", colnames(pgrdata)), grep(pattern="^Support_Other", x=colnames(pgrdata)))]
  
  head(pgrdata_Staff_OtherSupport)
  
  
  pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[rowSums(!is.na(pgrdata_Staff_OtherSupport)) > 1, ]
  colnames(pgrdata_Staff_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
  pgrdata_Staff_OtherSupport <- rbind(pgrdata_Staff_OtherSupport[,c(1,2,3)], pgrdata_Staff_OtherSupport[,c(1,4,5)], pgrdata_Staff_OtherSupport[, c(1,6,7)])
  pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[!is.na(pgrdata_Staff_OtherSupport$Support_Other),] 
  
  ## Nb of responses
  nrow(pgrdata_Staff_OtherSupport)
  
}


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
