################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

##############
## Support ##
##############

# pgrdata_Support
{
  pgrdata_Support <- pgrdata[pgrdata$StudentStaff == "Student",  
                             c(grep("Div", colnames(pgrdata)), grep(pattern="^Support", x=colnames(pgrdata)))]
  head(pgrdata_Support)
  
  pgrdata_Support_Seminars <- pgrdata_Support[!is.na(pgrdata_Support$Support_Seminar),] %>% 
    group_by(Div, Support_Seminar) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Seminars$ID <- paste(paste(pgrdata_Support_Seminars$Div, "Seminars", sep="_"), pgrdata_Support_Seminars$Support_Seminar, sep ="_")
  head(pgrdata_Support_Seminars)
  
  pgrdata_Support_Mentoring <- pgrdata_Support[!is.na(pgrdata_Support$Support_Mentoring),] %>% 
    group_by(Div, Support_Mentoring) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Mentoring$ID <- paste(paste(pgrdata_Support_Mentoring$Div, "Mentoring", sep="_"), pgrdata_Support_Mentoring$Support_Mentoring, sep ="_")
  head(pgrdata_Support_Mentoring)
  
  pgrdata_Support_Coaching <- pgrdata_Support[!is.na(pgrdata_Support$Support_Coaching),] %>% 
    group_by(Div, Support_Coaching) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Coaching$ID <- paste(paste(pgrdata_Support_Coaching$Div, "Coaching", sep="_"), pgrdata_Support_Coaching$Support_Coaching, sep ="_")
  head(pgrdata_Support_Coaching)
  
  pgrdata_Support_Network <- pgrdata_Support[!is.na(pgrdata_Support$Support_Network),] %>% 
    group_by(Div, Support_Network) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Network$ID <- paste(paste(pgrdata_Support_Network$Div, "Support Networks", sep="_"), pgrdata_Support_Network$Support_Network, sep ="_")
  head(pgrdata_Support_Network)
  
  pgrdata_Support_Resources <- pgrdata_Support[!is.na(pgrdata_Support$Support_Resources),] %>% 
    group_by(Div, Support_Resources) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Support_Resources$ID <- paste(paste(pgrdata_Support_Resources$Div, "Online Resources", sep="_"), pgrdata_Support_Resources$Support_Resources, sep ="_")
  head(pgrdata_Support_Resources)
  
  
  prgdata_Support_Freq <- data.frame(rbind(pgrdata_Support_Seminars[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Mentoring[, c('ID', 'n', 'perc')], 
                                           pgrdata_Support_Coaching[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Network[, c('ID', 'n', 'perc')],
                                           pgrdata_Support_Resources[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Support_Seminars,pgrdata_Support_Mentoring, pgrdata_Support_Coaching, pgrdata_Support_Network, pgrdata_Support_Resources)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Support$Div), each = 20) # 5 divisions 4 answers
  LabelIndiv <- rep(Supports, each = 4, times = 5) # 5 Support
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Support$Support_Seminar[!is.na(pgrdata_Support$Support_Seminar)]), times= 25) # 4 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Support_Freq <- merge(dataframe, prgdata_Support_Freq, by = "ID", all.x = TRUE)
  pgrdata_Support <- prgdata_Support_Freq[, names(prgdata_Support_Freq) != "ID"]
  rm(dataframe, prgdata_Support_Freq)
}

# plot pgrdata_Support
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Support
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Supports) # this will determine order of the bars
  
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
  pgrdata_Support_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("Essential", "Useful", "Not sure", "Not useful"))), stat="identity", alpha=0.5) +
    
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
    
    scale_fill_manual(values = c("black", "#666666", "#6BAED6", '#08519C'), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not useful", "Not sure", "Useful", "Essential"), 
                      labels =c("Not useful", "Not sure", "Useful", "Essential"), 
                      drop = FALSE)+
    
    
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
    ggplot2::annotate("text", x = 0, y = -90, label = "Support" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  #  pgrdata_Support_plot
  
  ## Save at png
  # ggsave(pgrdata_Support_plot, file=here("Figures/pgrdata_Support.png"), width=10, height=8)
  
  ## clean up
  # rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# pgrdata_OtherSupport
{
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
  
  ## Nb of responses
  nrow(pgrdata_OtherSupport)
  
}

# pgrdata_Staff_OtherSupport
{
  pgrdata_Staff_OtherSupport <- pgrdata[pgrdata$StudentStaff == "Staff",  
                                        c(grep("Div", colnames(pgrdata)), grep(pattern="^Support_Other", x=colnames(pgrdata)))]
  
  head(pgrdata_Staff_OtherSupport)
  
  
  pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[rowSums(!is.na(pgrdata_Staff_OtherSupport)) > 1, ]
  colnames(pgrdata_Staff_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
  pgrdata_Staff_OtherSupport <- rbind(pgrdata_Staff_OtherSupport[,c(1,2,3)], pgrdata_Staff_OtherSupport[,c(1,4,5)], pgrdata_Staff_OtherSupport[, c(1,6,7)])
  pgrdata_Staff_OtherSupport <- pgrdata_Staff_OtherSupport[!is.na(pgrdata_Staff_OtherSupport$Support_Other),] 
  
  ## Nb of responses
  nrow(pgrdata_Staff_OtherSupport)
  
}

# plot for all pgrdata_Support
{
  All_pgrdata_Support <- pgrdata_Support[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_pgrdata_Support <- All_pgrdata_Support  %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )
  
  All_pgrdata_Support$LabelIndiv <- factor(All_pgrdata_Support$LabelIndiv, levels = Supports) # this will determine order of the bars
  str(All_pgrdata_Support)
  
  All_pgrdata_Support_plot <- ggplot(All_pgrdata_Support) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = c("Essential", "Useful",  "Not sure","Not useful"))),
             stat="identity", alpha=0.5) +
    
    scale_fill_manual(values = rev(c("black", "#666666", "#6BAED6", '#08519C')), 
                      breaks=rev(c("Not useful", "Not sure", "Useful", "Essential")), 
                      labels =rev(c("Not useful", "Not sure", "Useful", "Essential")), 
                      drop = FALSE)+
    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.title=element_blank())
  
  All_pgrdata_Support_plot
  
}
