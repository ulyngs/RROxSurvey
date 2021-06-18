################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

#rm(list = ls())
#source("FormatPGRdata.R")

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
  data$Answer <- as.factor(data$Answer)
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
  base_data2 <- base_data
  base_data2$title[base_data$Div == 'SSD'] <- 39
    
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
      legend.position = "bottom",
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
    geom_text(data=base_data2, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), vjust=c(0.5,0.5,0.5,0.5, 1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -90, label = "Barriers" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Barriers_plot
  
  ## Save at png
  # ggsave(pgrdata_Barriers_plot, file=here("Figures/pgrdata_Barriers.png"), width=10, height=8)
  
  ## clean up
  #rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# plot for all pgrdata_Barriers
{
  
  All_pgrdata_Barriers <- pgrdata_Barriers[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_pgrdata_Barriers$NbRespondents <- rep(c(sum(NbRespondents_Barriers_OA$NbRespondents),
                                              sum( NbRespondents_Barriers_Data$NbRespondents),
                                              sum(NbRespondents_Barriers_Code$NbRespondents),
                                              sum(NbRespondents_Barriers_Materials$NbRespondents),
                                              sum(NbRespondents_Barriers_Preprint$NbRespondents),
                                              sum(NbRespondents_Barriers_Prereg$NbRespondents),
                                              sum( NbRespondents_Barriers_RegRep$NbRespondents)), each = 9)
  All_pgrdata_Barriers <- All_pgrdata_Barriers  %>% group_by(LabelIndiv) %>% mutate(perc = n / NbRespondents * 100 )
  
  All_pgrdata_Barriers$LabelIndiv <- factor(All_pgrdata_Barriers$LabelIndiv, levels = Measures) # this will determine order of the bars
  str(All_pgrdata_Barriers)
  
 # All_pgrdata_Barriers2 <- regroup_all_data(pgrdata_Barriers)
  
  
  
  
 # Barriers <- c("Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None", "Not sure","Not applicable")
  #answers_barriers <- c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
 # answers_colors <- c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186")
 # stacked_barplot_on_regrouped_data(All_pgrdata_Barriers, Barriers, answers_barriers, answers_colors)
  
  
  All_pgrdata_Barriers_plot <- ggplot(All_pgrdata_Barriers) +      
    
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, level = c("None", "Other", "Policy", "Incentives", "Norms" , "Training", "Infrastructure", "NotSure", "NA"))), stat="identity", alpha=0.5) +
    

    scale_fill_manual(values = rev(c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186")), 
                      breaks=rev(c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")),
                      labels =rev(c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"))
                      , drop = FALSE)+
    
    scale_y_continuous(labels=function(perc) paste0(perc,"%")) +
    
    theme_minimal() +
    ggtitle("Barriers") +
    theme(
      plot.title = element_text(hjust=0.5, size = 14, face = "bold"),
      legend.position = "right",
      legend.text=element_text(size=11),
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 12),
      legend.title=element_blank())
  
  All_pgrdata_Barriers_plot
  
}