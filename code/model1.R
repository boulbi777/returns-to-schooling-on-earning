mod1 = lm(logsal~age + age2 + female + for. + reg1 + reg2 + reg3 + reg4, data=data)

mod1_results = lm_analysis(data,mod1,p=8)


print("Model with White method robust covariance :")
mod1_rob = commarobust(mod1, se_type="HC0") 

print(summary(mod1_rob))


#Interesting graphics (for report):
#lm_analysis_graph(data2, mod1, p=8, outliers=TRUE)
#lm_analysis_graph(data2, mod1, p=8, student_res=TRUE)
