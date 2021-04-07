################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################


rm(list = ls())
source("FormatPGRdata.R")

Measures
Trainings <- c('Open Access', 'Data Management Plan', 'FAIR Data','Ethics','Open Code', 'Open Materials', 'Licences', 'Preprint', 'Preregistration', 'Recruitement')
Training_columns <- c(expr(Training_OA), expr(Training_DMP), expr(Training_FAIR), expr(Training_Ethics),
                      expr(Training_Code),expr(Training_Materials),expr(Training_Licences), 
                       expr(Training_Preprint), expr(Training_Prereg), expr(Training_Recruitement))
Training_answers <- c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" )
answers_colors <- c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C')
title_plot <- 'Training'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_Training <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c(grep("Div", colnames(pgrdata)), grep(pattern="^Training", x=colnames(pgrdata)))]
head(pgrdata_Training)

## create skeleton of all possible answers
skeleton <- create_skeleton(Trainings, Divisions, Training_answers, Training_columns)

## summarise items    
summaryitems <- bind_summaries_items(Trainings, pgrdata_Training, Training_columns)

## merge summary items to skeleton
pgrdata_Training <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Training_columns)


# plot per Division -----
pgrdata_Training_plot <- circular_plot_function(pgrdata_Training, Measures, Training_answers, title_plot, answers_colors)

## ggsave(pgrdata_Training_plot, file=here("Figures/pgrdata_Training2.png"), width=10, height=8)

# regroup data split per Division for overall plot -----
All_pgrdata_Training <- regroup_all_data(pgrdata_Training)

# plot regrouped data  -----
All_pgrdata_Training_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Training, Training_answers, answers_colors)
## ggsave(All_pgrdata_Support_plot, file=here("Figures/All_pgrdata_Supportfunctions.png"), width=10, height=8)



# pgrdata_OtherTraining -----
pgrdata_OtherTraining <- pgrdata[pgrdata$StudentStaff == "Student",  
                                 c(grep("Div", colnames(pgrdata)), grep(pattern="^Training_Other", x=colnames(pgrdata)))]

head(pgrdata_OtherTraining)


pgrdata_OtherTraining <- pgrdata_OtherTraining[rowSums(!is.na(pgrdata_OtherTraining)) > 1, ]
colnames(pgrdata_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
pgrdata_OtherTraining <- rbind(pgrdata_OtherTraining[,c(1,2,3)], pgrdata_OtherTraining[,c(1,4,5)], pgrdata_OtherTraining[, c(1,6,7)])
pgrdata_OtherTraining <- pgrdata_OtherTraining[!is.na(pgrdata_OtherTraining$Training_Other),] 
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, "superv")] <- 'how to supervise/mentor/teach/collaborate'
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, "employable skill")] <- 'How to ensure that you have acquired employable skill'
pgrdata_OtherTraining$Training_Other_recode[pgrdata_OtherTraining$Training_Other == 'How to negotiate with publishers about book publications'] <- 'How to negotiate with publishers about book publications'
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, 'diversity|Diverse|Social Justice')] <- 'How to foster diversity and social justice'


pgrdata_OtherTraining[,c('Div','Training_Other_score','Training_Other_recode')]

Training_answers <- c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" )
pgrdata_OtherTraining$Training_Other_score <- factor(pgrdata_OtherTraining$Training_Other_score , levels = Training_answers)

xtab_OtherTraining <- pgrdata_OtherTraining %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(xtab_OtherTraining)[1] <- "" 


## Nb of responses
nrow(pgrdata_OtherTraining)
  

# pgrdata_Staff_OtherTraining -----
pgrdata_Staff_OtherTraining <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                       c(grep("Div", colnames(pgrdata)), grep(pattern="^Training_Other", x=colnames(pgrdata)))]

head(pgrdata_Staff_OtherTraining)


pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[rowSums(!is.na(pgrdata_Staff_OtherTraining)) > 1, ]
colnames(pgrdata_Staff_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
pgrdata_Staff_OtherTraining <- rbind(pgrdata_Staff_OtherTraining[,c(1,2,3)], pgrdata_Staff_OtherTraining[,c(1,4,5)], pgrdata_Staff_OtherTraining[, c(1,6,7)])
pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[!is.na(pgrdata_Staff_OtherTraining$Training_Other),] 

## Nb of responses
nrow(pgrdata_Staff_OtherTraining)




