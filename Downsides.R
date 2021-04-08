################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

#rm(list = ls())
#source("FormatPGRdata.R")

Measures
Downsides_columns <- c(expr(Downsides_OA), expr(Downsides_Data), expr(Downsides_Code), expr(Downsides_Materials),expr(Downsides_Preprint),expr(Downsides_Prereg),expr(Downsides_RegRep))
Downsides_answers <- c("No", "Yes",  "Not sure",  "Not applicable" )
answers_colors <- c("black", "#666666", "#D95F02", "#1B9E77")
title_plot <- 'Any 
downsides'


# create datadet for plotting per Divisions -----
## select subdataset 
pgrdata_Downsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                          c(grep("Div", colnames(pgrdata)), grep(pattern="^Downsides", x=colnames(pgrdata)))]
head(pgrdata_Downsides)

## create skeleton of all possible answers
skeleton <- create_skeleton(Measures, Divisions, Downsides_answers, Downsides_columns)

## summarise items                                         
summaryitems <- bind_summaries_items(Measures, pgrdata_Downsides, Downsides_columns)

## merge summary items to skeleton
pgrdata_Downsides <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE)
rm(skeleton, summaryitems, Downsides_columns)


# plot per Division -----
pgrdata_Downsides_plot <- circular_plot_function(pgrdata_Downsides, Measures, Downsides_answers, title_plot, answers_colors)
pgrdata_Downsides_plot
## ggsave(pgrdata_Downsides_plot, file=here("Figures/pgrdata_Downsides.png"), width=10, height=8)

# regroup data split per Division for overall plot -----
All_pgrdata_Downsides <- regroup_all_data(pgrdata_Downsides)

# plot regrouped data  -----
All_pgrdata_Downsides_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Downsides, Measures, Downsides_answers, answers_colors)
All_pgrdata_Downsides_plot
## ggsave(All_pgrdata_Downsides_plot, file=here("Figures/All_pgrdata_Downsides.png"), width=10, height=8)
