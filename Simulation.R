#####################################
## Simulation of data in Qualtrics ##
#####################################

# Packages
{
library(dplyr)
library(here)
library(ggplot2)
library(RColorBrewer)
}

# load data
{
#simdata <- read_csv(here("Data/SimulatedData_20201214.csv"))
#surveyindex <- read_csv(here("Data/SurveyIndex.csv"))
#targetnumbers <- read_csv(here("Data/TargetNumbers.csv"))

realdata <- read.csv(here("Data/RealData_20210210-1144.csv"), stringsAsFactors=FALSE)
simdata <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE)
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE)

head(realdata)
head(simdata)
head(surveyindex, 10)
head(targetnumbers)
}

# SIM DATA
{

# format simdata
{x <- data.frame(1,"" , 5)
simdata <- simdata[-c(1, 2), -which(names(simdata) %in% c("DistributionChannel","UserLanguage"))] 
colnames(simdata) <- surveyindex$VariableName[surveyindex$QuestionCode == colnames(simdata)] 
head(simdata)

# Subset simdata to records with filled out Consent, Affiliation, and Role
nrow(simdata)
simdata <- subset(simdata[!is.na(simdata$Consent) & !is.na(simdata$DivCol) & !is.na(simdata$Role),] )
nrow(simdata)
}

# Consent
{
simdata$Consent <- simdata$Consent == "I consent to participating in the survey and to processing of the information I provide as outlined above."
}

# Affiliation
{## College TRUE FALSE
simdata$College <- simdata$DivCol == "College-only staff"
table(simdata$College)

## Division
simdata$Div <- simdata$DivCol
simdata$Div[simdata$Div == "College-only staff"] <- simdata$ColDiv[simdata$Div == "College-only staff"]
table(simdata$Div)
table(simdata$Div[simdata$College == FALSE])

## Department
simdata$Dept <- simdata$Dept1 
simdata$Dept[!is.na(simdata$Dept2)] <- simdata$Dept2[!is.na(simdata$Dept2)]
simdata$Dept[!is.na(simdata$Dept3)] <- simdata$Dept3[!is.na(simdata$Dept3)]
simdata$Dept[!is.na(simdata$Dept4)] <- simdata$Dept4[!is.na(simdata$Dept4)]
simdata$Dept[!is.na(simdata$Dept5)] <- simdata$Dept5[!is.na(simdata$Dept5)]
simdata$Dept[!is.na(simdata$Dept6)] <- simdata$Dept6[!is.na(simdata$Dept6)]
simdata$Dept[!is.na(simdata$Dept7)] <- simdata$Dept7[!is.na(simdata$Dept7)]
simdata$Dept[!is.na(simdata$Dept8)] <- simdata$Dept8[!is.na(simdata$Dept8)]
simdata$Dept[!is.na(simdata$Dept9)] <- simdata$Dept9[!is.na(simdata$Dept9)]
simdata$Dept[!is.na(simdata$Dept10)] <- simdata$Dept10[!is.na(simdata$Dept10)]
table(simdata$Dept)

table(simdata$OtherDept)


# clean up non useful column
unwanted_colnames <- c("DivCol", "ColDiv", names(simdata[, grep(pattern="Dept[0-9]+", colnames(simdata))]))
simdata <- simdata[, -which(names(simdata) %in% unwanted_colnames)] 

}

# Role
{
table(simdata$Role) 
simdata$StudentStaff <- simdata$Role == "Student on a postgraduate research programme"
simdata$StudentStaff[simdata$StudentStaff == TRUE] <- "Student"
simdata$StudentStaff[simdata$StudentStaff == FALSE] <- "Staff"
}

# Years of Experience
simdata$Duration <- as.numeric(simdata$Duration)

# Representativeness
StaffRecorded <- data.frame(table(simdata$Div[simdata$StudentStaff == "Staff"]))
colnames(StaffRecorded) <- c("Div", "StaffRecorded")
StudentRecorded <- data.frame(table(simdata$Div[simdata$StudentStaff == "Student"]))
colnames(StudentRecorded) <- c("Div", "StudentRecorded")

targetnumbers <- merge(targetnumbers, StaffRecorded, by = "Div")
targetnumbers <- merge(targetnumbers, StudentRecorded, by = "Div")

RepresentativenessTotal <- targetnumbers %>% group_by(Div) %>% summarise(RepStudent =StudentRecorded/StudentTotal,
                                              RepStaff =StaffRecorded/StaffTotal)

# Awareness
## All
pie(table(simdata$Awareness_OA), main = "OA")
pie(table(simdata$Awareness_Data), main = "Data")

## Per Div
table(simdata$Awareness_OA, simdata$Div )
chisq.test(table(simdata$Awareness_OA, simdata$Div))

## Per Role
barplot(table(as.factor(simdata$Awareness_OA), as.factor(simdata$Role) ))

## Per years of experience
table(simdata$Awareness_OA, simdata$Duration )
plot(as.factor(simdata$Awareness_OA) ~ simdata$Duration)


# Effect
pie(table(simdata$Effect_OA), main = "OA")
pie(table(simdata$Effect_Data), main = "Data")

names(simdata)
}

########################################################################################

# format realdata
{
  realdata <- realdata[-c(1, 2), -which(names(realdata) %in% c("DistributionChannel","UserLanguage"))] 
  colnames(realdata) <- surveyindex$VariableName[surveyindex$QuestionCode == colnames(realdata)] 
  realdata[realdata == ""] <- NA ## added
  head(realdata)
  
  # Subset realdata to records with filled out Consent, Affiliation, and Role
  nrow(realdata)
  realdata <- subset(realdata[!is.na(realdata$Consent) & !is.na(realdata$DivCol) & !is.na(realdata$Role),] )
  nrow(realdata)
}

# Consent
{
  realdata$Consent <- realdata$Consent == "I consent to participating in the survey and to processing of the information I provide as outlined above."
}

# Affiliation
{## College TRUE FALSE
  realdata$College <- realdata$DivCol == "College-only staff"
  table(realdata$College)
  
  ## Division
  realdata$Div <- realdata$DivCol
  realdata$Div[realdata$Div == "College-only staff"] <- realdata$ColDiv[realdata$Div == "College-only staff"]
  table(realdata$Div)
  table(realdata$Div[realdata$College == FALSE])
  
  realdata$Div[realdata$Div == "Social Sciences Division"] <- "SSD"
  realdata$Div[realdata$Div == "Humanities Division"] <- "Hum"
  realdata$Div[realdata$Div == "Department for Continuing Education"] <- "ContEd"
  realdata$Div[realdata$Div == "Mathematical, Physical, and Life Sciences Division"] <- "MPLS"
  realdata$Div[realdata$Div == "Medical Sciences Division"] <- "MSD"
  
  
  ## Department
  realdata$Dept <- realdata$Dept1 
  realdata$Dept[!is.na(realdata$Dept2)] <- realdata$Dept2[!is.na(realdata$Dept2)]
  realdata$Dept[!is.na(realdata$Dept3)] <- realdata$Dept3[!is.na(realdata$Dept3)]
  realdata$Dept[!is.na(realdata$Dept4)] <- realdata$Dept4[!is.na(realdata$Dept4)]
  realdata$Dept[!is.na(realdata$Dept5)] <- realdata$Dept5[!is.na(realdata$Dept5)]
  realdata$Dept[!is.na(realdata$Dept6)] <- realdata$Dept6[!is.na(realdata$Dept6)]
  realdata$Dept[!is.na(realdata$Dept7)] <- realdata$Dept7[!is.na(realdata$Dept7)]
  realdata$Dept[!is.na(realdata$Dept8)] <- realdata$Dept8[!is.na(realdata$Dept8)]
  realdata$Dept[!is.na(realdata$Dept9)] <- realdata$Dept9[!is.na(realdata$Dept9)]
  realdata$Dept[!is.na(realdata$Dept10)] <- realdata$Dept10[!is.na(realdata$Dept10)]
  table(realdata$Dept)
  
  table(realdata$OtherDept)
  
  
  # clean up non useful column
  unwanted_colnames <- c("DivCol", "ColDiv", names(realdata[, grep(pattern="Dept[0-9]+", colnames(realdata))]))
  realdata <- realdata[, -which(names(realdata) %in% unwanted_colnames)] 
  
}

# Role
{
  table(realdata$Role) 
  realdata$StudentStaff <- realdata$Role == "Student on a postgraduate research programme"
  realdata$StudentStaff[realdata$StudentStaff == TRUE] <- "Student"
  realdata$StudentStaff[realdata$StudentStaff == FALSE] <- "Staff"
}

# Years of Experience
realdata$Duration <- as.numeric(realdata$Duration)

# Representativeness
StaffRecorded <- data.frame(table(realdata$Div[realdata$StudentStaff == "Staff"]))
colnames(StaffRecorded) <- c("Div", "StaffRecorded")
StudentRecorded <- data.frame(table(realdata$Div[realdata$StudentStaff == "Student"]))
colnames(StudentRecorded) <- c("Div", "StudentRecorded")

targetnumbers <- merge(targetnumbers, StaffRecorded, by = "Div", all = TRUE)## added
targetnumbers <- merge(targetnumbers, StudentRecorded, by = "Div", all = TRUE)## added

RepresentativenessTotal <- targetnumbers %>% group_by(Div) %>% summarise(RepStudent =StudentRecorded/StudentTotal*100,
                                                                         RepStaff =StaffRecorded/StaffTotal*100)
mean(RepresentativenessTotal$RepStudent, na.rm=TRUE) ## added

# Awareness
# ## All
# par(mfrow = c(3, 3))
# pie(table(realdata$Awareness_OA), main = "OA")
# pie(table(realdata$Awareness_Data), main = "Data")
# pie(table(realdata$Awareness_Code), main = "Code")
# pie(table(realdata$Awareness_Materials), main = "Materials")
# pie(table(realdata$Awareness_Preprint), main = "Preprints")
# pie(table(realdata$Awareness_Prereg), main = "Preregistration")
# pie(table(realdata$Awareness_RegRep), main = "Registered Report")
# 
# barplot(table(realdata$Awareness_OA), main = "OA")
# 
# 
# ## Per Div
# table(realdata$Awareness_OA, realdata$Div )
# chisq.test(table(realdata$Awareness_OA, realdata$Div))
# 
# ## Per Role
# barplot(table(as.factor(realdata$Awareness_OA), as.factor(realdata$Role) ))
# 
# ## Per years of experience
# table(realdata$Awareness_OA, realdata$Duration )
# plot(as.factor(realdata$Awareness_OA) ~ realdata$Duration)
# 
# 
# # Effect
# pie(table(realdata$Effect_OA), main = "OA")
# pie(table(realdata$Effect_Data), main = "Data")
# 
# names(realdata)
# 
# 




## realdata_Awareness


realdata_Awareness <- realdata[realdata$StudentStaff == "Student",  
                               c(grep("Div", colnames(realdata)), grep(pattern="^Awareness", x=colnames(realdata)))]


head(realdata_Awareness)


realdata_Awareness_OA <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_OA),] %>% 
  group_by(Div, Awareness_OA) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_OA$ID <- paste(paste(realdata_Awareness_OA$Div, "Open Access", sep="_"), realdata_Awareness_OA$Awareness_OA, sep ="_")
head(realdata_Awareness_OA)


realdata_Awareness_Data <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_Data),] %>% 
  group_by(Div, Awareness_Data) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_Data$ID <- paste(paste(realdata_Awareness_Data$Div, "Open Data", sep="_"), realdata_Awareness_Data$Awareness_Data, sep ="_")
head(realdata_Awareness_Data)


realdata_Awareness_Code <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_Code),] %>% 
  group_by(Div, Awareness_Code) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_Code$ID <- paste(paste(realdata_Awareness_Code$Div, "Open Code", sep="_"), realdata_Awareness_Code$Awareness_Code, sep ="_")
head(realdata_Awareness_Code)


realdata_Awareness_Materials <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_Materials),] %>% 
  group_by(Div, Awareness_Materials) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_Materials$ID <- paste(paste(realdata_Awareness_Materials$Div, "Open Materials", sep="_"), realdata_Awareness_Materials$Awareness_Materials, sep ="_")
head(realdata_Awareness_Materials)


realdata_Awareness_Preprint <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_Preprint),] %>% 
  group_by(Div, Awareness_Preprint) %>%
  summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_Preprint$ID <- paste(paste(realdata_Awareness_Preprint$Div, "Preprint", sep="_"), realdata_Awareness_Preprint$Awareness_Preprint, sep ="_")
head(realdata_Awareness_Preprint)

realdata_Awareness_Prereg <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_Prereg),] %>% 
                                  group_by(Div, Awareness_Prereg) %>%
                                  summarise (n = n()) %>% 
                                  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_Prereg$ID <- paste(paste(realdata_Awareness_Prereg$Div, "Preregistration", sep="_"), realdata_Awareness_Prereg$Awareness_Prereg, sep ="_")
head(realdata_Awareness_Prereg)



realdata_Awareness_RegRep <- realdata_Awareness[!is.na(realdata_Awareness$Awareness_RegRep),] %>% 
                                  group_by(Div, Awareness_RegRep) %>%
                                  summarise (n = n()) %>% 
                                  mutate(perc = n / sum(n) * 100 )
realdata_Awareness_RegRep$ID <- paste(paste(realdata_Awareness_RegRep$Div, "Registered Report", sep="_"), realdata_Awareness_RegRep$Awareness_RegRep, sep ="_")
head(realdata_Awareness_RegRep)



real_data_Awareness_Freq <- data.frame(rbind(realdata_Awareness_OA[, c('ID', 'n', 'perc')],
                                             realdata_Awareness_Data[, c('ID', 'n', 'perc')], 
                                             realdata_Awareness_Code[, c('ID', 'n', 'perc')],
                                             realdata_Awareness_Materials[, c('ID', 'n', 'perc')],
                                             realdata_Awareness_Preprint[, c('ID', 'n', 'perc')],
                                             realdata_Awareness_Prereg[, c('ID', 'n', 'perc')],
                                             realdata_Awareness_RegRep[, c('ID', 'n', 'perc')]))


#Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')
Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')


Div <- rep(unique(realdata_Awareness$Div), each = 35)
LabelMeasures <- rep(Measures, each = 5, times = 5)
Indiv <-paste(Div, LabelMeasures, sep ="_") 
Values <- rep(unique(realdata_Awareness$Awareness_OA[!is.na(realdata_Awareness$Awareness_OA)]), times= 35)
ID <- paste(Indiv, Values, sep="_")
  
dataframe <- data.frame(ID, Indiv, Div,  LabelMeasures,Values)
head(dataframe)
nrow(dataframe)


realdata_Awareness_Fig <- merge(dataframe, real_data_Awareness_Freq, by = "ID", all.x = TRUE)

head(realdata_Awareness_Fig)



#data[] <- lapply(realdata_Awareness_Fig, as.character) # turns everything to character instead of factors https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
data <- realdata_Awareness_Fig
colnames(data) <- c('ID', 'individual', 'group', 'LabelMeasures', 'observation', 'rawN', 'value') # individual group observation value = Indiv Div Values n
head(data)
str(data)

data$observation <- as.character(data$observation)
data$observation[data$observation == "Not applicable"] <- "5 Not Applicable"
data$observation[data$observation == "Not aware / not sure if applicable"] <- "4 Not aware / not sure if applicable"
data$observation[data$observation == "Aware only"] <- "3 Aware only"
data$observation[data$observation == "Accessing / using only"] <- "2 Accessing / using only"
data$observation[data$observation == "Practicing myself"] <- "1 Practicing myself"
data$observation <- as.factor(data$observation)


############################################# https://www.r-graph-gallery.com/299-circular-stacked-barplot.html


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value, na.rm=TRUE))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$LabelMeasures <- sapply(strsplit(as.character(label_data$individual),"\\_"), `[`, 2)
  
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]





# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + # changes x to 0-44 = nb of bars including empty ones
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 44.7, y = 100, xend = 44.9, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 44.7, y = 75, xend = 44.9, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 44.7, y = 50, xend = 44.9, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 44.7, y = 25, xend = 44.9, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 44.7, y = 0, xend = 44.9, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each lines
  ggplot2::annotate("text", x = rep(max(data$id)-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0", "25", "50", "75", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  #scale_fill_viridis(discrete=TRUE, direction = -1) +
  scale_fill_manual(values = c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("5 Not Applicable", "4 Not aware / not sure if applicable", "3 Aware only", "2 Accessing / using only", "1 Practicing myself"),
                      labels=c("Not Applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+

  ylim(-100,150) +
  theme_minimal() +
  theme(
    legend.title=element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
   # plot.margin = unit(rep(1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
 geom_text(data=label_data, aes(x=id, y=105, label=LabelMeasures, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
 geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
 geom_text(data=base_data, aes(x = title, y = -20, label=group), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +

# Add title in the middle
ggplot2::annotate("text", x = 0, y = -90, label = "Awareness" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  

p

