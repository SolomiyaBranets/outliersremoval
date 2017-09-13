# outliersremoval

Function for outliers removal creates an additional variable "Outlier" and insers value 1 if the value is extreme, 0 otherwise

The way how outliers are removed:
 N lowest and highest values of each segment is checked
 by how many percent this value is different from the previous one calculated
 if difference exceeds cut point -  this and all the rest rowsare marked as outliers


input objects:

input_data - the whole data frame (for example input_data = profit_data)

target_variable - the name of the column that contains outliers (for example target_variable = "profit")

cut_point (by default = 3) - if the value is n times higher that the previous one, it will be marked as an outlier

lr(by default = 5) - how many smallest and largest values will be checked

lr_perc - can be used instead of lr, if the user want to specify not the number but persentage (f.e lr_perc = 1 means to check 1% of smallest and largest values)

group_by - subsets data into groups and checks for outliers separately (f.e. group_by = c("country", "segment"))

log (by default = False) - if TRUE, the target variable will be logarithmised before further processing (N.B. log(0) = INF)

rm_neg (by default = False) if TRUE, all negative values will be marked as outliers


Possible issues:

 N/0 = Inf
 
 -N/0 = -Inf
 
 log(0) = Inf
 
 log(-N) = NaN (Not-a-Number)
 
