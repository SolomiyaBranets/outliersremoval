
#Function for outliers removal creates an additional variable "Outlier" and insers value 1 if the value is extreme, 0 otherwise

#The way how outliers are removed:
# N lowest and highest values of each segment is checked
# by how many percent this value is different from the previous one calculated
# if difference exceeds cut point -  this and all the rest rowsare marked as outliers


#input objects:

#input_data - the whole data frame (for example input_data = profit_data)
#target_variable - the name of the column that contains outliers (for example target_variable = "profit")
#cut_point (by default = 3) - if the value is n times higher that the previous one, it will be marked as an outlier
#lr(by default = 5) - how many smallest and largest values will be checked
#lr_perc - can be used instead of lr, if the user want to specify not the number but persentage (f.e lr_perc = 1 means to check 1% of smallest and largest values)
#group_by - subsets data into groups and checks for outliers separately (f.e. group_by = c("country", "segment"))
#log (by default = False) - if TRUE, the target variable will be logarithmised before further processing (N.B. log(0) = INF)
#rm_neg (by default = False) if TRUE, all negative values will be marked as outliers

#Possible issues:
# N/0 = Inf
# -N/0 = -Inf
#log(0) = Inf
# log(-N) = NaN (Not-a-Number)


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



