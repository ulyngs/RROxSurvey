################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

rm(list = ls())
source("FormatPGRdata.R")

###############
## Downsides ##
###############

# pgrdata_Downsides
{
  pgrdata_Downsides <- pgrdata[pgrdata$StudentStaff == "Student",  
                               c(grep("Div", colnames(pgrdata)), grep(pattern="^Downsides", x=colnames(pgrdata)))]
  head(pgrdata_Downsides)
  
  pgrdata_Downsides_OA <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_OA),] %>% 
    group_by(Div, Downsides_OA) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_OA$ID <- paste(paste(pgrdata_Downsides_OA$Div, "Open Access", sep="_"), pgrdata_Downsides_OA$Downsides_OA, sep ="_")
  head(pgrdata_Downsides_OA)
  
  pgrdata_Downsides_Data <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Data),] %>% 
    group_by(Div, Downsides_Data) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_Data$ID <- paste(paste(pgrdata_Downsides_Data$Div, "Open Data", sep="_"), pgrdata_Downsides_Data$Downsides_Data, sep ="_")
  head(pgrdata_Downsides_Data)
  
  pgrdata_Downsides_Code <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Code),] %>% 
    group_by(Div, Downsides_Code) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_Code$ID <- paste(paste(pgrdata_Downsides_Code$Div, "Open Code", sep="_"), pgrdata_Downsides_Code$Downsides_Code, sep ="_")
  head(pgrdata_Downsides_Code)
  
  pgrdata_Downsides_Materials <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Materials),] %>% 
    group_by(Div, Downsides_Materials) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_Materials$ID <- paste(paste(pgrdata_Downsides_Materials$Div, "Open Materials", sep="_"), pgrdata_Downsides_Materials$Downsides_Materials, sep ="_")
  head(pgrdata_Downsides_Materials)
  
  pgrdata_Downsides_Preprint <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Preprint),] %>% 
    group_by(Div, Downsides_Preprint) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_Preprint$ID <- paste(paste(pgrdata_Downsides_Preprint$Div, "Preprint", sep="_"), pgrdata_Downsides_Preprint$Downsides_Preprint, sep ="_")
  head(pgrdata_Downsides_Preprint)
  
  pgrdata_Downsides_Prereg <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_Prereg),] %>% 
    group_by(Div, Downsides_Prereg) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_Prereg$ID <- paste(paste(pgrdata_Downsides_Prereg$Div, "Preregistration", sep="_"), pgrdata_Downsides_Prereg$Downsides_Prereg, sep ="_")
  head(pgrdata_Downsides_Prereg)
  
  pgrdata_Downsides_RegRep <- pgrdata_Downsides[!is.na(pgrdata_Downsides$Downsides_RegRep),] %>% 
    group_by(Div, Downsides_RegRep) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 )
  pgrdata_Downsides_RegRep$ID <- paste(paste(pgrdata_Downsides_RegRep$Div, "Registered Report", sep="_"), pgrdata_Downsides_RegRep$Downsides_RegRep, sep ="_")
  head(pgrdata_Downsides_RegRep)
  
  prgdata_Downsides_Freq <- data.frame(rbind(pgrdata_Downsides_OA[, c('ID', 'n', 'perc')],
                                             pgrdata_Downsides_Data[, c('ID', 'n', 'perc')], 
                                             pgrdata_Downsides_Code[, c('ID', 'n', 'perc')],
                                             pgrdata_Downsides_Materials[, c('ID', 'n', 'perc')],
                                             pgrdata_Downsides_Preprint[, c('ID', 'n', 'perc')],
                                             pgrdata_Downsides_Prereg[, c('ID', 'n', 'perc')],
                                             pgrdata_Downsides_RegRep[, c('ID', 'n', 'perc')]))
  
  rm(pgrdata_Downsides_OA,pgrdata_Downsides_Data,pgrdata_Downsides_Code,pgrdata_Downsides_Materials,pgrdata_Downsides_Preprint,pgrdata_Downsides_Prereg, pgrdata_Downsides_RegRep)
  
  # create structure with temp variables
  Div <- rep(unique(pgrdata_Downsides$Div), each = 28) # 5 divisions 4 answers
  LabelIndiv <- rep(Measures, each = 4, times = 5) # 7 measures
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  ## need to pick an item where all responses are represented
  Answer <- rep(unique(pgrdata_Downsides$Downsides_RegRep[!is.na(pgrdata_Downsides$Downsides_RegRep)]), times= 35) # 5 possible answers - remove 'NA'/ blank answers / skipped items
  ID <- paste(Indiv, Answer, sep="_")
  
  dataframe <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  rm(Div, LabelIndiv, Indiv, Answer, ID)
  
  prgdata_Downsides_Freq <- merge(dataframe, prgdata_Downsides_Freq, by = "ID", all.x = TRUE)
  pgrdata_Downsides <- prgdata_Downsides_Freq[, names(prgdata_Downsides_Freq) != "ID"]
  rm(dataframe, prgdata_Downsides_Freq)
}

# plot pgrdata_Downsides
{
  ## https://www.r-graph-gallery.com/299-circular-stacked-barplot.html
  data <- pgrdata_Downsides
  str(data)
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
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
  pgrdata_Downsides_plot <- ggplot(data) +      #  c("Not applicable", "Not sure",  "Yes",  "No" )
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("No", "Yes",  "Not sure",  "Not applicable" ))), stat="identity", alpha=0.5) +
    
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
    
    scale_fill_manual(values = c("black", "#666666", "#D95F02", "#1B9E77"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("Not applicable", "Not sure", "Yes", "No"),
                      labels =c("Not applicable", "Not sure", "Yes", "No")
                      , drop = FALSE)+
    
    
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
    ggplot2::annotate("text", x = 0, y = -90, label = "Downsides" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
  pgrdata_Downsides_plot
  
  ## Save at png
  # ggsave(pgrdata_Downsides_plot, file=here("Figures/pgrdata_Downsides.png"), width=10, height=8)
  
  ## clean up
  # rm(data, base_data, grid_data, label_data, number_of_bar, nObsType, empty_bar, angle)
  
}

# plot for all pgrdata_Downsides
{
  All_pgrdata_Downsides <- pgrdata_Downsides[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_pgrdata_Downsides <- All_pgrdata_Downsides  %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )
  
  All_pgrdata_Downsides$LabelIndiv <- factor(All_pgrdata_Downsides$LabelIndiv, levels = Measures) # this will determine order of the bars
  str(All_pgrdata_Downsides)
  
  All_pgrdata_Downsides_plot <- ggplot(All_pgrdata_Downsides) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   c("No", "Yes",  "Not sure",  "Not applicable" )
                                                   )), 
             stat="identity", alpha=0.5) +
    
    scale_fill_manual(values = rev(c("black", "#666666", "#D95F02", "#1B9E77")), 
                      breaks=rev(c("Not applicable", "Not sure", "Yes", "No")),
                      labels =rev(c("Not applicable", "Not sure", "Yes", "No"))
                      , drop = FALSE)+

    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank())
  
  All_pgrdata_Downsides_plot
  
}
