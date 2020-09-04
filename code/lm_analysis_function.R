lm_analysis = function(data, model, p, summary=TRUE, obs_analysis=TRUE, var_analysis=TRUE, residuals_analysis=TRUE){
   #' lm_analysis function
   #' 
   #' @description This function gives the useful tools to interpret a linear model 
   #' 
   #' @param data the data to be analysed
   #' @param model an lm object (doesn't work with any other object)
   #' @param p the number of explanatory variables
   #' @param summary if TRUE, show the summary of model
   #' @param obs_analysis if TRUE, return a plot of outliers and several data frame of the outliers, the data without the outliers, the influential observations and the leverage observations
   #' @param var_analysis if TRUE, return the correlation matrix and the VIF
   #' @param residuals_analysis if TRUE, return a histogramm of residuals, a QQ-plot residuals VS N(0,1), a plot of student residuals vs adjusted variables, results of BP test and the robust covariance matrix (with White method)
  
  
  n = nrow(data)
  results = list()
  
  if (summary) {
    print("summary :")
    print(summary(model))
    results_summ = list(summary=summary(model))
    results=c(results, results_summ)}
  
  
  if (obs_analysis){
    #Outliers
    res_stud = rstudent(model)
    pred = model$fitted.values
    
    # alpha = 1% because 10.000 observations (--> many)
    plot(res_stud, xlab="Observation", ylab="T-residuals", main="Outliers with a 1% level", sub="Graph 1/4")
    abline(h = qt(0.9995, n-p-1), col = "red")
    abline(h = -qt(0.9995, n-p-1), col = "red")
    #outliers :
    outliers = data[abs(res_stud) >= qt(0.9995, n-p-1),]
    data_without_outliers = data[abs(res_stud) <= qt(0.9995, n-p-1),]
    
    
    #influential observations
    d_cook = cooks.distance(model)
    influential_obs = data[d_cook>qf(0.9995, p,n-p-1),]
    
    
    #Leverage effect
    h_ii = hatvalues(model)
    leverage_obs = data[h_ii>3*(p+1)/n,]
    
    results_obs = list(outliers=outliers, data_wtht_out=data_without_outliers, influential_obs=influential_obs, leverage_obs=leverage_obs)
    results = c(results, results_obs)
  }
  
  if (var_analysis){
    corr = cor(data[,c("age","age","for.","for_p","for_m")])
    print("corr :")
    print(corr)
    VIF = vif(model)
    print("VIF :")
    print(VIF)
    results_var = list(VIF=VIF,correlation=corr)
    results=c(results, results_var)
  }
  
  if (residuals_analysis){
    #Residuals normality
    hist(model$residuals, breaks = 40, col='blue', xlab="Residuals", main="Histogram of residuals", sub="Graph 2/4")
    qqplot(model$residuals, rnorm(10000,0,1), xlab="Residuals", ylab="N(0,1)", main="QQ-plot : residuals VS N(0,1)", sub="Graph 3/4")
    abline(a=0,b=1, col="red")
    
    
    #Residuals study
    res_stud = rstudent(model)
    pred = model$fitted.values
    plot(pred, res_stud, xlab = "Adjusted variable", ylab = "T-residuals", main="Student residuals", sub="Graph 4/4") 
    
    #breush pagan test : (H0) No heteroskedasticity
    bp_test = bptest(model)
    print("Heteroskedasticity test: ")
    print("(H0) No heteroskedasticity")
    print(bp_test)
    
    #White method
    robust_cov = vcovHC(model, type = "HC")
    print("Robust standard error :")
    rob_std = sqrt(diag(robust_cov))
    print(rob_std)
    
    results_residuals = list(bp_test=bp_test, rob_std=rob_std)
    results = c(results, results_residuals)
  }
  return(results)
}


lm_analysis_graph = function(data, model, p, outliers=FALSE, histogram=FALSE, QQ_plot=FALSE, student_res=FALSE){
  #' lm_analysis_graph
  #' 
  #' @description this function allows to return the graph given by lm_analysis
  #' 
  #' @param data the data to be analysed
  #' @param model an lm object
  #' @param p the number of explanatory variables
  #' @param outliers if TRUE, return the outliers graph
  #' @param histogram if TRUE, return the histogram of residuals
  #' @param QQ_plot if TRUE, return the QQ-plot residuals VS N(0,1)
  #' @param Student_res if TRUE, return a plot of student residuals vs adjusted variables
  
  n = nrow(data)
  if(outliers){
    res_stud = rstudent(model)
    plot(res_stud, xlab="Observation", ylab="T-residuals", main="Outliers with a 1% level")
    abline(h = qt(0.9995, n-p-1), col = "red")
    abline(h = -qt(0.9995, n-p-1), col = "red")}
  
  if(histogram){hist(model$residuals, breaks = 40, col='blue', xlab="Residuals", main="Histogram of residuals")}
  
  if(QQ_plot){
    qqplot(model$residuals, rnorm(10000,0,1), xlab="Residuals", ylab="N(0,1)", main="QQ-plot : residuals VS N(0,1)")
    abline(a=0,b=1, col="red")}
  
  if(student_res){
    res_stud = rstudent(model)
    pred = model$fitted.values
    plot(pred, res_stud, xlab = "Adjusted variable", ylab = "T-residuals", main="Student residuals")}
}
