################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")


Supports
Support_columns <- c(expr(Support_Seminar), expr(Support_Mentoring), expr(Support_Coaching), expr(Support_Network),expr(Support_Resources))
Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")
answers_colors <- c("black", "#666666", "#6BAED6", '#08519C')
title_plot <- 'Support'



# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_Support <- pgrdata[pgrdata$StudentStaff == "Student",  
                           c(grep("Div", colnames(pgrdata)), grep(pattern="^Support", x=colnames(pgrdata)))]
head(pgrdata_Support)

## create skeleton of all possible answers
skeleton <- create_skeleton(Supports, Divisions, Support_answers, Support_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Supports, pgrdata_Support, Support_columns)

## merge summary items to skeleton
data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Support_columns)

# plot per Division -----
pgrdata_Support_plot <- circular_plot_function(data, Supports, Support_answers, title_plot, answers_colors)

## Save as png
## ggsave(pgrdata_Support_plot, file=here("Figures/pgrdata_Supportfunctions.png"), width=10, height=8)


# regroup data split per Division for overall plot -----
All_pgrdata_Support <- regroup_all_data(data)

# plot regrouped data  -----
All_pgrdata_Support_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Support, Support_answers, answers_colors)
## ggsave(All_pgrdata_Support_plot, file=here("Figures/All_pgrdata_Supportfunctions.png"), width=10, height=8)


# pgrdata_OtherSupport -----
pgrdata_OtherSupport <- pgrdata[pgrdata$StudentStaff == "Student",  
                                c(grep("Div", colnames(pgrdata)), grep(pattern="^Support_Other", x=colnames(pgrdata)))]

head(pgrdata_OtherSupport)


pgrdata_OtherSupport <- pgrdata_OtherSupport[rowSums(!is.na(pgrdata_OtherSupport)) > 1, ]
colnames(pgrdata_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
pgrdata_OtherSupport <- rbind(pgrdata_OtherSupport[,c(1,2,3)], pgrdata_OtherSupport[,c(1,4,5)], pgrdata_OtherSupport[, c(1,6,7)])
pgrdata_OtherSupport <- pgrdata_OtherSupport[!is.na(pgrdata_OtherSupport$Support_Other),] 
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'Funding')] <- 'Funding'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'find funding')] <- 'Information on how to find funding for publishing in hybrid journal'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'incentives|recruitment practices|university')] <- 'University endorsement, recruitement criteria, and  policies'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'Publishing|publishing')] <- 'Training on how to publish transparent research'

pgrdata_OtherSupport[,c('Div','Support_Other_recode')]

pgrdata_OtherSupport$Support_Other_score <- factor(pgrdata_OtherSupport$Support_Other_score , levels = Support_answers)

xtab_OtherSupport <- pgrdata_OtherSupport %>% 
  tabyl(Support_Other_recode, Support_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Essential)
names(xtab_OtherSupport)[1] <- "" 


## Nb of responses
nrow(pgrdata_OtherSupport)


 
# pgrdata_Staff_OtherSupport  -----
pgrdata_Staff_OtherSupport <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                      c(grep("Div", colnames(pgrdata)), grep(pattern="^Support_Other", x=colnames(pgrdata)))]

head(pgrdata_Staff_OtherSupport)


pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[rowSums(!is.na(pgrdata_Staff_OtherSupport)) > 1, ]
colnames(pgrdata_Staff_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
pgrdata_Staff_OtherSupport <- rbind(pgrdata_Staff_OtherSupport[,c(1,2,3)], pgrdata_Staff_OtherSupport[,c(1,4,5)], pgrdata_Staff_OtherSupport[, c(1,6,7)])
pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[!is.na(pgrdata_Staff_OtherSupport$Support_Other),] 

## Nb of responses
nrow(pgrdata_Staff_OtherSupport)

