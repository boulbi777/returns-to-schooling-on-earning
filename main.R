#Loading librairies ------

source("code/libraries.R")

#Importing Data -----
data = read.sas7bdat("data/projet4.sas7bdat")


#Exploration and preparation of data -----

source("code/data_preparation.R")


#Study of the significativity of our variables -----

source("code/var_significativity_function.R")

source("code/study_var_significativity.R")

#Analysis of several regressions -----

source("code/lm_analysis_function.R")


# Regression 1 : with the model given and all the observations ------

source("code/model1.R")

#data2 is data without the outliers identified in the first model

data2 = mod1_results$data_wtht_out


# Regression 2 : to study the interaction between female and for., without outliers : -----

source("code/model2_interaction.R")

# Regression 3 : instrumental variables ------

source("code/model3_IV.R")


