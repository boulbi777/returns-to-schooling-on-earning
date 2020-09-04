var_significativity <- function(data_column,plot=TRUE){
  #' var_significativity function
  #' 
  #' @description This function allow you to study the significativity of one variable, in the context of our study
  #' you must use our data basis, which contains the column "logsal"
  #' 
  #' @param data_column vector of column's values of the data frame
  
  model = lm(data$logsal~data_column)
  print(summary(model))
  if (plot) {
    plot(data_column, data$logsal, ylab="logsal")
    abline(reg=model, col='blue')
  }
}
