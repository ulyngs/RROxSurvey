################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

##############
## Training ##
##############

# pgrdata_Training
{
  pgrdata_Training <- pgrdata[pgrdata$StudentStaff == "Student",  
                              c(grep("Div", colnames(pgrdata)), grep(pattern="^Training", x=colnames(pgrdata)))]
  head(pgrdata_Training)
  
  pgrdata_Training_OA <- pgrdata_Training[!is.na(pgrdata_Training$Training_OA),] %>% 
    group_by(Div, Training_OA) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_OA$ID <- paste(paste(pgrdata_Training_OA$Div, "Open Access", sep="_"), pgrdata_Training_OA$Training_OA, sep ="_")
  head(pgrdata_Training_OA)
  
  pgrdata_Training_DMP <- pgrdata_Training[!is.na(pgrdata_Training$Training_DMP),] %>% 
    group_by(Div, Training_DMP) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_DMP$ID <- paste(paste(pgrdata_Training_DMP$Div, "Data Management Plan", sep="_"), pgrdata_Training_DMP$Training_DMP, sep ="_")
  head(pgrdata_Training_DMP)
  
  pgrdata_Training_FAIR <- pgrdata_Training[!is.na(pgrdata_Training$Training_FAIR),] %>% 
    group_by(Div, Training_FAIR) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_FAIR$ID <- paste(paste(pgrdata_Training_FAIR$Div, "FAIR Data", sep="_"), pgrdata_Training_FAIR$Training_FAIR, sep ="_")
  head(pgrdata_Training_FAIR)
  
  pgrdata_Training_Ethics <- pgrdata_Training[!is.na(pgrdata_Training$Training_Ethics),] %>% 
    group_by(Div, Training_Ethics) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Ethics$ID <- paste(paste(pgrdata_Training_Ethics$Div, "Ethics", sep="_"), pgrdata_Training_Ethics$Training_Ethics, sep ="_")
  head(pgrdata_Training_Ethics)
  
  pgrdata_Training_Code <- pgrdata_Training[!is.na(pgrdata_Training$Training_Code),] %>% 
    group_by(Div, Training_Code) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Code$ID <- paste(paste(pgrdata_Training_Code$Div, "Open Code", sep="_"), pgrdata_Training_Code$Training_Code, sep ="_")
  head(pgrdata_Training_Code)
  
  pgrdata_Training_Materials <- pgrdata_Training[!is.na(pgrdata_Training$Training_Materials),] %>% 
    group_by(Div, Training_Materials) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Materials$ID <- paste(paste(pgrdata_Training_Materials$Div, "Open Materials", sep="_"), pgrdata_Training_Materials$Training_Materials, sep ="_")
  head(pgrdata_Training_Materials)
  
  pgrdata_Training_Licences <- pgrdata_Training[!is.na(pgrdata_Training$Training_Licences),] %>% 
    group_by(Div, Training_Licences) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Licences$ID <- paste(paste(pgrdata_Training_Licences$Div, "Licences", sep="_"), pgrdata_Training_Licences$Training_Licences, sep ="_")
  head(pgrdata_Training_Licences)
  
  pgrdata_Training_Prereg <- pgrdata_Training[!is.na(pgrdata_Training$Training_Prereg),] %>% 
    group_by(Div, Training_Prereg) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Prereg$ID <- paste(paste(pgrdata_Training_Prereg$Div, "Preregistration", sep="_"), pgrdata_Training_Prereg$Training_Prereg, sep ="_")
  head(pgrdata_Training_Prereg)
  
  pgrdata_Training_Preprint <- pgrdata_Training[!is.na(pgrdata_Training$Training_Preprint),] %>% 
    group_by(Div, Training_Preprint) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Preprint$ID <- paste(paste(pgrdata_Training_Preprint$Div, "Preprint", sep="_"), pgrdata_Training_Preprint$Training_Preprint, sep ="_")
  head(pgrdata_Training_Preprint)
  
  pgrdata_Training_Recruitement <- pgrdata_Training[!is.na(pgrdata_Training$Training_Recruitement),] %>% 
    group_by(Div, Training_Recruitement) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Training_Recruitement$ID <- paste(paste(pgrdata_Training_Recruitement$Div, "Recruitement", sep="_"), pgrdata_Training_Recruitement$Training_Recruitement, sep ="_")
  head(pgrdata_Training_Recruitement)
  
  
  
  
  
  prgdata_Training_Freq <- data.frame(rbind(pgrdata_Training_OA[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_DMP[, c('ID', 'n', 'perc')], 
                                            pgrdata_Training_FAIR[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Ethics[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Code[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Materials[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Licences[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Preprint[, c('ID', 'n', 'perc')],
                                            pgrdata_Training_Prereg[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Training_OA,pgrdata_Training_DMP, pgrdata_Training_FAIR, pgrdata_Training_Ethics, pgrdata_Training_Code,pgrdata_Training_Materials,pgrdata_Training_Preprint,pgrdata_Training_Prereg, pgrdata_Training_Licences)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Training$Div), each = 54) # 5 divisions 6 answers
  LabelIndiv <- rep(Trainings, each = 6, times = 5) # 9 training
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Training$Training_OA[!is.na(pgrdata_Training$Training_OA)]), times= 45) # 6 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Training_Freq <- merge(dataframe, prgdata_Training_Freq, by = "ID", all.x = TRUE)
  pgrdata_Training <- prgdata_Training_Freq[, names(prgdata_Training_Freq) != "ID"]
  rm(dataframe, prgdata_Training_Freq)
}

# plot pgrdata_Training
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Training
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Trainings) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  rm(to_add)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  label_data$LabelIndiv <- sapply(strsplit(as.character(label_data$Indiv),"\\_"), `[`, 2)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(Div) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  
  
  
  ## Make the plot
  pgrdata_Training_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" ))), stat="identity", alpha=0.5) +
    
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
    
    scale_fill_manual(values = c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C'), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training"), 
                      labels =c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training"), 
                      drop = FALSE)+
    
    #scale_fill_manual(values = c("black", "#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
    #                breaks=c("Not applicable", "Not aware / not sure if applicable", "Aware only", "Accessing / using only", "Practicing myself"))+
    
    
    scale_x_discrete(expand = c(0, 0)) +
    ylim(-100,150) +
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
    geom_text(data=label_data, aes(x=id, y=105, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -90, label = "Training" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Training_plot
  
  ## Save at png
  # ggsave(pgrdata_Training_plot, file=here("Figures/pgrdata_Training.png"), width=10, height=8)
  
  ## clean up
  #rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# pgrdata_OtherTraining
{
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
  

  pgrdata_OtherTraining[,c('Div','Training_Other_recode')]
  
  ## Nb of responses
  nrow(pgrdata_OtherTraining)
  
}

# pgrdata_Staff_OtherTraining
{
  pgrdata_Staff_OtherTraining <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                         c(grep("Div", colnames(pgrdata)), grep(pattern="^Training_Other", x=colnames(pgrdata)))]
  
  head(pgrdata_Staff_OtherTraining)
  
  
  pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[rowSums(!is.na(pgrdata_Staff_OtherTraining)) > 1, ]
  colnames(pgrdata_Staff_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
  pgrdata_Staff_OtherTraining <- rbind(pgrdata_Staff_OtherTraining[,c(1,2,3)], pgrdata_Staff_OtherTraining[,c(1,4,5)], pgrdata_Staff_OtherTraining[, c(1,6,7)])
  pgrdata_Staff_OtherTraining <- pgrdata_Staff_OtherTraining[!is.na(pgrdata_Staff_OtherTraining$Training_Other),] 
  
  ## Nb of responses
  nrow(pgrdata_Staff_OtherTraining)
  
}


# plot for all pgrdata_Training
{
  All_pgrdata_Training <- pgrdata_Training[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_pgrdata_Training <- All_pgrdata_Training  %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )
  
  All_pgrdata_Training$LabelIndiv <- factor(All_pgrdata_Training$LabelIndiv, levels = Trainings) # this will determine order of the bars
  str(All_pgrdata_Training)
  
  All_pgrdata_Training_plot <- ggplot(All_pgrdata_Training) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" ))),
             stat="identity", alpha=0.5) +
    
    scale_fill_manual(values = rev(c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C')), 
                      breaks=rev(c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training")), 
                      labels =rev(c("Not applicable", "Not sure", "No guidance needed", "No guidance wanted", "Written guidance only",  "Written guidance and workshop-led training")), 
                      drop = FALSE)+
    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.title=element_blank())
  
  All_pgrdata_Training_plot
  
}

