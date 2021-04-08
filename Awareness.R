################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

#rm(list = ls())
#source("FormatPGRdata.R")

Measures
Awareness_columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
Awareness_answers <- c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware / not sure if applicable",  "Not applicable" )
answers_colors <- rev(c("#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C", "black"))
title_plot <- 'Awareness'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_Awareness <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c(grep("Div", colnames(pgrdata)), grep(pattern="^Awareness", x=colnames(pgrdata)))]
head(pgrdata_Awareness)

## create skeleton of all possible answers
skeleton <- create_skeleton(Measures, Divisions, Awareness_answers, Awareness_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Measures, pgrdata_Awareness, Awareness_columns)

## merge summary items to skeleton
pgrdata_Awareness <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Awareness_columns)


# plot per Division -----
pgrdata_Awareness_plot <- circular_plot_function(pgrdata_Awareness, Measures, Awareness_answers, title_plot, answers_colors)
pgrdata_Awareness_plot
## ggsave(pgrdata_Awareness_plot, file=here("Figures/pgrdata_Awareness2.png"), width=10, height=8)

# regroup data split per Division for overall plot -----
All_pgrdata_Awareness <- regroup_all_data(pgrdata_Awareness)

# plot regrouped data  -----
All_pgrdata_Awareness_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Awareness, Measures, Awareness_answers, answers_colors)
All_pgrdata_Awareness_plot
## ggsave(All_pgrdata_Support_plot, file=here("Figures/All_pgrdata_Supportfunctions.png"), width=10, height=8)
