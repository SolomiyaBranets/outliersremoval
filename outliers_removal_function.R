remove_outliers <- function(input_data,
                            target_variable,
                            cut_point = 3,
                            lr = 5,
                            lr_perc = NA,
                            group_by = c(),
                            log = F,
                            rm_neg = F) {
  
  require(data.table, quietly = T)
  require(dplyr, quietly = T)
  
  data <- input_data %>% mutate(Outlier = 0)
  
  if (log == TRUE) {
    data$help_variable <- log(data[, target_variable])
  } else {
    data$help_variable <- data[, target_variable]
  }
  
  data <- data.table(data)
  
  
  
  if (length(group_by) >  0) {
    #if groups exist
    data_splited <- split(data, by = group_by)
    data <- data.frame()
    
    for (i in names(data_splited)) {
      subset <- data_splited[[i]]
      
      if (!is.na(lr_perc)) {
        lr <- (dim(subset)[1] * lr_perc / 100) + 1
      } else{
        lr <- lr + 1
      }
      
      subset <- subset[order(help_variable)]
      subset[1:lr, Diff := abs(help_variable / shift(help_variable, 1L, type =
                                                       "lead"))]
      
      for (row in (lr-1):1){
        if (subset[row, "Diff"] > cut_point){subset[1:row, "Outlier"] <- 1}
      }
      
      subset <- subset[order(-help_variable)]
      subset[1:lr, Diff := abs(help_variable / shift(help_variable, 1L, type =
                                                       "lead"))]
      for (row in (lr-1):1){
        if (subset[row, "Diff"] > cut_point){subset[1:row, "Outlier"] <- 1}
      }
      data <- rbind(data, subset)
    }
    
    if (rm_neg == TRUE) {
      data <- data.frame(data)
      data[data[, target_variable] < 0, "Outlier"] <- 1
    }
    
  } else {
    #groups don't exist
   
    if (!is.na(lr_perc)) {
      lr <- (dim(data)[1] * lr_perc / 100) + 1
    } else{
      lr <- lr + 1
    }
    
    data <- data[order(help_variable)]
    data[1:lr, Diff := abs(help_variable / shift(help_variable, 1L, type =
                                                   "lead"))]
    for (row in (lr-1):1){
      if (data[row, "Diff"] > cut_point){data[1:row, "Outlier"] <- 1}
    }
    
    data <- data[order(-help_variable)]
    data[1:lr, Diff := abs(help_variable / shift(help_variable, 1L, type =
                                                   "lead"))]
    for (row in (lr-1):1){
      if (data[row, "Diff"] > cut_point){data[1:row, "Outlier"] <- 1}
    }
    
    if (rm_neg == TRUE) {
      data <- data.frame(data)
      data[data[,target_variable] < 0, "Outlier"] <- 1
    }
  }
  return(select(data,-Diff,-help_variable) %>% arrange(desc(Outlier)))
}


  #example----------
  
  #   check <- remove_outliers(input_data = profit_data, 
  #                  target_variable = 'profit',
  #                  lr_perc = 1,
  #                  cut_point = 2,
  #                  group_by = 'country')



