################################################
##    RROx survey: Open research at Oxford    ##
##        Functions and parameters            ##
################################################

# items to judges or categories

Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')
Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')

Criteria <- c('Number of publications','Prestige of publication outlet','Quality of publications', 'Authorship role', 'Citations', 'Grant support', 
              'Impact','Teaching', 'Supervision, mentoring', 'Service to the profession','Citizenship','Reputation',
              'Collaboration network','Open research practices')
Criteria_short <- c("PubNub","PubPrestige","PubQual","Authorship","Citation","Grant","Impact", "Teaching","Supervision","Service","Citizenship",
                    "Reputation","Collaboration","OpenResearch")

Divisions <- c("SSD", "Hum", "ContEd","MPLS", "MSD")

# functions

create_skeleton <- function(Question, Divisions, answers, columns){
  Div <- rep(Divisions, each = length(columns)*length(answers)) 
  LabelIndiv <- rep(Question, each = length(answers), times = length(Divisions)) 
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  Answer <- rep(answers, times= length(Divisions)*length(columns)) 
  ID <- paste(Indiv, Answer, sep="_") 
  skeleton <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
}

summarise_item <-  function(data, item, name_item){
  data2 <-  data[!is.na(data[,as.character(item)]),]  %>%  
    group_by(Div,{{item}}) %>%
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 ) 
  data2$ID = paste(paste(data2$Div, name_item, sep="_"), unlist(data2[,as.character(item)]), sep ="_")
  return(data2[,c('ID', 'n', 'perc')])
}

bind_summaries_items <- function(Question, data, columns){
  summaryitems <- vector(mode= "list", length = length(Question))
  for (i in 1:length(Question)) {
    summaryitems[[i]] <-  summarise_item(data,columns[[i]],Question[i])
  }
  summaryitems <- data.frame(do.call(rbind, summaryitems))
  return(summaryitems)
}

circular_plot_function <- function(data, Question, answers, title_plot, answers_colors) {

  name_data_argument <- deparse(substitute(data)) # get the name of the dataset to apply if statement below
  
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Question) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  data$Answer <- as.factor(data$Answer)
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
  
if (name_data_argument == 'pgrdata_Awareness'){
base_data$title[base_data$Div == 'SSD'] <- 39} #for awareness plot

if (name_data_argument == 'pgrdata_Training'){
base_data$title[base_data$Div == 'SSD'] <- 51} #for training plot
  
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  
  
  
  ## Make the plot
  pgrdata_Support_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = answers)), stat="identity", alpha=0.5) +
    
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
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="dimgrey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    scale_fill_manual(values = rev(answers_colors), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks = answers, 
                      labels = answers, 
                      drop = FALSE)+

    scale_x_discrete(expand = c(0, 0)) +
    ylim(-70,150) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text=element_text(size=13),
      legend.title=element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border=element_blank(), axis.ticks.length = unit(0, "mm")
    ) +
    
   # guides(fill=guide_legend(nrow=2,byrow=FALSE))+
    
    coord_polar() +
    
    ### Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=105, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4.3, angle= label_data$angle, inherit.aes = FALSE ) + 
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8,  size=4, fontface="bold", inherit.aes = FALSE) + 
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -60, label = title_plot , color="black", size=5.5 , angle=0, fontface="bold", hjust=0.5) 
  
  
}

regroup_all_data <- function(splitdata){
  All_data <- splitdata[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_data <- All_data %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )
  return(All_data)
  }
  
stacked_barplot_on_regrouped_data <- function(All_data, Question, answers, answers_colors){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
  ### Add the stacked bar
  geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                 level = answers)),
           stat="identity", alpha=0.5) +
  
  scale_fill_manual(values = rev(answers_colors), 
                    breaks=answers, 
                    labels =answers, 
                    drop = FALSE)+
  
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90),
    legend.title=element_blank())
  
}  
