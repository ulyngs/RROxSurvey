source("FormatPGRdata.R")
source("Awareness.R")
source("Training.R")
source("Effect.R")
source("Downsides.R")
source("Barriers.R")
source("CurrentRecruitment.R")
source("FutureRecruitment.R")

require(gridExtra)
require(patchwork)


Fig1 <- grid.arrange(pgrdata_Awareness_plot, pgrdata_Training_plot, ncol=2)
Fig1

Fig2 <- grid.arrange(pgrdata_Effect_plot, pgrdata_Downsides_plot, ncol=2)
Fig2

Fig3 <- All_pgrdata_Barriers_plot
Fig3


stacked_barplot_CurrentCriteria <- function(All_data, Question, answers, answers_colors){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", alpha=0.5) +
    
    scale_y_continuous(labels=function(perc) paste0(perc,"%")) +
  
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE)+
    ggtitle('Perceived current recruitement criteria')+
    
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_blank()     )
  
}  
All_pgrdata_CurrentCriteria_plot_to_merge <- stacked_barplot_CurrentCriteria(All_pgrdata_CurrentCriteria,Criteria,Criteria_answers,answers_colors)

stacked_barplot_FutureCriteria <- function(All_data, Question, answers, answers_colors){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", alpha=0.5) +
    scale_y_continuous(labels=function(perc) paste0(perc,"%")) +
    
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE)+
    
    ggtitle('Desired recruitement criteria')+
    
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
     axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 67.5, hjust = 1, size = 12))
  
}  
All_pgrdata_FutureCriteria_plot_to_merge <- stacked_barplot_FutureCriteria(All_pgrdata_FutureCriteria,Criteria,Criteria_answers,answers_colors)

combined <- All_pgrdata_CurrentCriteria_plot_to_merge +  All_pgrdata_FutureCriteria_plot_to_merge & theme(legend.position="right",  legend.title=element_blank(), legend.text=element_text(size=9))
Fig4 <- combined + plot_layout(nrow = 2, guide = "collect")
Fig4
