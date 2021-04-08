################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

#rm(list = ls())
#source("FormatPGRdata.R")

Criteria
Criteria_columns <- c(expr(CurrentRecruitment_PubNub), expr(CurrentRecruitment_PubPrestige), expr(CurrentRecruitment_PubQual), expr(CurrentRecruitment_Authorship),expr(CurrentRecruitment_Citation),
                      expr(CurrentRecruitment_Grant),expr(CurrentRecruitment_Impact),expr(CurrentRecruitment_Teaching),expr(CurrentRecruitment_Supervision),expr(CurrentRecruitment_Service),
                      expr(CurrentRecruitment_Citizenship),expr(CurrentRecruitment_Reputation),expr(CurrentRecruitment_Collaboration),expr(CurrentRecruitment_OpenResearch))
Criteria_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
answers_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')
title_plot <- 'Current 
recruitement criteria'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_CurrentCriteria <- pgrdata[pgrdata$StudentStaff == "Student",  
                           c(grep("Div", colnames(pgrdata)), grep(pattern="^CurrentRecruitment", x=colnames(pgrdata)))]
head(pgrdata_CurrentCriteria)


## create skeleton of all possible answers
skeleton <- create_skeleton(Criteria, Divisions, Criteria_answers, Criteria_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Criteria, pgrdata_CurrentCriteria, Criteria_columns)

## merge summary items to skeleton
pgrdata_CurrentCriteria <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Criteria_columns)


# plot per Division -----
pgrdata_CurrentCriteria_plot <- circular_plot_function(pgrdata_CurrentCriteria, Criteria, Criteria_answers, title_plot, answers_colors)
pgrdata_CurrentCriteria_plot

## Save at png
# ggsave(pgrdata_Support_plot, file=here("Figures/pgrdata_Support.png"), width=10, height=8)



# regroup data split per Division for overall plot -----
All_pgrdata_CurrentCriteria <- regroup_all_data(pgrdata_CurrentCriteria)

# plot regrouped data  -----
All_pgrdata_CurrentCriteria_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_CurrentCriteria, Criteria, Criteria_answers, answers_colors)
## ggsave(All_pgrdata_CurrentCriteria_plot, file=here("Figures/All_pgrdata_CurrentCriteria_plot.png"), width=10, height=8)


# pgrdata_OtherCurrentCriteria -----
pgrdata_OtherCurrentRecruitment <- pgrdata[pgrdata$StudentStaff == "Student",  
                                c(grep("Div", colnames(pgrdata)), grep(pattern="^CurrentRecruitment_Other", x=colnames(pgrdata)))]

head(pgrdata_OtherCurrentRecruitment)


pgrdata_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment[rowSums(!is.na(pgrdata_OtherCurrentRecruitment)) > 1, ]
colnames(pgrdata_OtherCurrentRecruitment) <- c('Div', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other','CurrentRecruitment_Other_score', 'CurrentRecruitment_Other')
pgrdata_OtherCurrentRecruitment <- rbind(pgrdata_OtherCurrentRecruitment[,c(1,2,3)], pgrdata_OtherCurrentRecruitment[,c(1,4,5)], pgrdata_OtherCurrentRecruitment[, c(1,6,7)])
pgrdata_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment[!is.na(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other),] 

pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. academic references, people from the hiring department'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement and social media presence'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Diversity')] <- 'Diversity'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Leading major fieldwork projects')] <- 'Leading major fieldwork projects'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'underserved')] <- 'Having expertise underserved in the department'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Academic Background')] <- 'Academic Background (not sure what is meant here)'


pgrdata_OtherCurrentRecruitment[,c('Div','CurrentRecruitment_Other_score','CurrentRecruitment_Other_recode')]
table(pgrdata_OtherCurrentRecruitment[,c('CurrentRecruitment_Other_recode','CurrentRecruitment_Other_score')])




pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_score <- factor(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_score , levels = Criteria_answers)


xtab_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherCurrentRecruitment)[1] <- "" 



## Nb of responses
nrow(pgrdata_OtherCurrentRecruitment)



# pgrdata_Staff_OtherCurrentRecruitment  -----
pgrdata_Staff_OtherCurrentRecruitment <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                      c(grep("Div", colnames(pgrdata)), grep(pattern="^CurrentRecruitment_Other", x=colnames(pgrdata)))]

head(pgrdata_Staff_OtherCurrentRecruitment)


pgrdata_Staff_OtherCurrentRecruitment <- pgrdata_Staff_OtherCurrentRecruitment[rowSums(!is.na(pgrdata_Staff_OtherCurrentRecruitment)) > 1, ]
colnames(pgrdata_Staff_OtherCurrentRecruitment) <- c('Div', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other_recode', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other_recode','CurrentRecruitment_Other_score', 'CurrentRecruitment_Other_recode')
pgrdata_Staff_OtherCurrentRecruitment <- rbind(pgrdata_Staff_OtherCurrentRecruitment[,c(1,2,3)], pgrdata_Staff_OtherCurrentRecruitment[,c(1,4,5)], pgrdata_Staff_OtherCurrentRecruitment[, c(1,6,7)])
pgrdata_Staff_OtherCurrentRecruitment <- pgrdata_Staff_OtherCurrentRecruitment[!is.na(pgrdata_Staff_OtherCurrentRecruitment$CurrentRecruitment_Other_recode),] 
pgrdata_Staff_OtherCurrentRecruitment

## Nb of responses
nrow(pgrdata_Staff_OtherCurrentRecruitment)
















