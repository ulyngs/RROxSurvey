################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

Criteria
Criteria_columns <- c(expr(FutureRecruitment_PubNub), expr(FutureRecruitment_PubPrestige), expr(FutureRecruitment_PubQual), expr(FutureRecruitment_Authorship),expr(FutureRecruitment_Citation),
                      expr(FutureRecruitment_Grant),expr(FutureRecruitment_Impact),expr(FutureRecruitment_Teaching),expr(FutureRecruitment_Supervision),expr(FutureRecruitment_Service),
                      expr(FutureRecruitment_Citizenship),expr(FutureRecruitment_Reputation),expr(FutureRecruitment_Collaboration),expr(FutureRecruitment_OpenResearch))
Criteria_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
answers_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')
title_plot <- 'Future recruitement criteria'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_FutureCriteria <- pgrdata[pgrdata$StudentStaff == "Student",  
                                   c(grep("Div", colnames(pgrdata)), grep(pattern="^FutureRecruitment", x=colnames(pgrdata)))]
head(pgrdata_FutureCriteria)
names(pgrdata_FutureCriteria)

## create skeleton of all possible answers
skeleton <- create_skeleton(Criteria, Divisions, Criteria_answers, Criteria_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Criteria, pgrdata_FutureCriteria, Criteria_columns)

## merge summary items to skeleton
data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Criteria_columns)


# plot per Division -----
pgrdata_FutureCriteria_plot <- circular_plot_function(data, Criteria, Criteria_answers, title_plot, answers_colors)
pgrdata_FutureCriteria_plot

## Save at png
# ggsave(pgrdata_Support_plot, file=here("Figures/pgrdata_Support.png"), width=10, height=8)



# plot for all pgrdata_Support  -----
All_pgrdata_FutureCriteria <- data[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
All_pgrdata_FutureCriteria <- All_pgrdata_FutureCriteria  %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )

All_pgrdata_FutureCriteria$LabelIndiv <- factor(All_pgrdata_FutureCriteria$LabelIndiv, levels = Criteria) # this will determine order of the bars
str(All_pgrdata_FutureCriteria)

All_pgrdata_FutureCriteria_plot <- ggplot(All_pgrdata_FutureCriteria) +      
  
  ### Add the stacked bar
  geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                 level = Criteria_answers)),
           stat="identity", alpha=0.5) +
  
  scale_fill_manual(values = rev(answers_colors), 
                    breaks=Criteria_answers, 
                    labels =Criteria_answers, 
                    drop = FALSE)+
  
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90),
    legend.title=element_blank())

All_pgrdata_FutureCriteria_plot


















# pgrdata_OtherSupport -----
pgrdata_OtherFutureRecruitment <- pgrdata[pgrdata$StudentStaff == "Student",  
                                           c(grep("Div", colnames(pgrdata)), grep(pattern="^FutureRecruitment_Other", x=colnames(pgrdata)))]

head(pgrdata_OtherFutureRecruitment)


pgrdata_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment[rowSums(!is.na(pgrdata_OtherFutureRecruitment)) > 1, ]
colnames(pgrdata_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other','FutureRecruitment_Other_score', 'FutureRecruitment_Other')
pgrdata_OtherFutureRecruitment <- rbind(pgrdata_OtherFutureRecruitment[,c(1,2,3)], pgrdata_OtherFutureRecruitment[,c(1,4,5)], pgrdata_OtherFutureRecruitment[, c(1,6,7)])
pgrdata_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment[!is.na(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other),] 

pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. people from the hiring department)'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Diversity|Social Justice')] <- 'Diversity'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'fieldwork projects')] <- 'Leading major fieldwork projects'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Innovation|Originality')] <- 'Originality of research'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Communication skills')] <- 'Communication skills (explaining their research)'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'medical impact')] <- 'Medical impact'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Availability of teaching in previous career')] <- 'Availability of teaching in previous career'


pgrdata_OtherFutureRecruitment[,c('Div','FutureRecruitment_Other_score','FutureRecruitment_Other_recode')]

pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_score <- factor(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_score , levels = Criteria_answers)


xtab_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment %>% 
  tabyl(FutureRecruitment_Other_recode, FutureRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherFutureRecruitment)[1] <- "" 




# pgrdata_Staff_OtherFutureRecruitment  -----
pgrdata_Staff_OtherFutureRecruitment <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                                 c(grep("Div", colnames(pgrdata)), grep(pattern="^FutureRecruitment_Other", x=colnames(pgrdata)))]

head(pgrdata_Staff_OtherFutureRecruitment)


pgrdata_Staff_OtherFutureRecruitment <- pgrdata_Staff_OtherFutureRecruitment[rowSums(!is.na(pgrdata_Staff_OtherFutureRecruitment)) > 1, ]
colnames(pgrdata_Staff_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other_recode', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other_recode','FutureRecruitment_Other_score', 'FutureRecruitment_Other_recode')
pgrdata_Staff_OtherFutureRecruitment <- rbind(pgrdata_Staff_OtherFutureRecruitment[,c(1,2,3)], pgrdata_Staff_OtherFutureRecruitment[,c(1,4,5)], pgrdata_Staff_OtherFutureRecruitment[, c(1,6,7)])
pgrdata_Staff_OtherFutureRecruitment <- pgrdata_Staff_OtherFutureRecruitment[!is.na(pgrdata_Staff_OtherFutureRecruitment$FutureRecruitment_Other_recode),] 
pgrdata_Staff_OtherFutureRecruitment

## Nb of responses
nrow(pgrdata_Staff_OtherFutureRecruitment)
















