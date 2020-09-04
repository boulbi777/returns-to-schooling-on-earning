mod2 = lm(logsal~age + age2 + female + for. + reg1 + reg2 + reg3 + reg4 +female*for., data=data2)

mod2_results = lm_analysis(data2,mod2,p=9)

print("Model with White method robust covariance :")
mod2_rob = commarobust(mod2, se_type="HC0")

print(summary(mod2_rob))
