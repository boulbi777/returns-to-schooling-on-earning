mod3 = ivreg(logsal~age + age2 + female + for. + reg1 + reg2 + reg3 + reg4 + female*for.|for_p*female + for_m*female + age + age2 + female + reg1 + reg2 + reg3 + reg4, data=data2)


print(summary(mod3, vcov = sandwich, diagnostics = TRUE))

print("Model with White method robust covariance :")
mod3_rob = iv_robust(logsal~age + age2 + female + for. + reg1 + reg2 + reg3 + reg4 + female*for.|for_p*female + for_m*female + age + age2 + female + reg1 + reg2 + reg3 + reg4, data=data2, se_type = "HC0")
print(summary(mod3_rob))


## 2 steps method :
mod3_1 = lm(for.~age + age2 + female  + reg1 + reg2 + reg3 + reg4 + female*for_m + female*for_p, data=data2)
print("First stage of 2SLS :")
print(summary(mod3_1))

for_hat = mod3_1$fitted.values
mod3_2 = lm(logsal~age + age2 + female + for_hat + reg1 + reg2 + reg3 + reg4 +female*for_hat, data=data2)

lm_analysis(data2, mod3_2, p=9)
