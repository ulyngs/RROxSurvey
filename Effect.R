################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

Measures
Effect_columns <- c(expr(Effect_OA), expr(Effect_Data), expr(Effect_Code), expr(Effect_Materials),expr(Effect_Preprint),expr(Effect_Prereg),expr(Effect_RegRep))
Effect_answers <- c("Beneficial", "Neutral(neither detrimental nor beneficial)", "Detrimental","Not sure", "Not applicable")
answers_colors <- c("black", "#666666", "#D95F02", "#E6AB02", "#1B9E77")
title_plot <- 'Effect'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_Effect <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c(grep("Div", colnames(pgrdata)), grep(pattern="^Effect", x=colnames(pgrdata)))]
head(pgrdata_Effect)

## create skeleton of all possible answers
skeleton <- create_skeleton(Measures, Divisions, Effect_answers, Effect_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Measures, pgrdata_Effect, Effect_columns)

## merge summary items to skeleton
data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Effect_columns)


# plot per Division -----
pgrdata_Effect_plot <- circular_plot_function(data, Measures, Effect_answers, title_plot, answers_colors)

## ggsave(pgrdata_Effect_plot, file=here("Figures/pgrdata_Effect2.png"), width=10, height=8)

# regroup data split per Division for overall plot -----
All_pgrdata_Effect <- regroup_all_data(data)

# plot regrouped data  -----
All_pgrdata_Effect_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Effect, Effect_answers, answers_colors)
## ggsave(All_pgrdata_Support_plot, file=here("Figures/All_pgrdata_Supportfunctions.png"), width=10, height=8)
