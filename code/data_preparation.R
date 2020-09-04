summary(data)
plot(data$logsal, log(data$sal)) 
#We notice that logsal is different from log(sal) for some observations

#deletion of the "bad" logsal
data = subset(data, select=-c(logsal))
#creation of the "good" logsal
data$logsal = log(data$sal)
plot(data$logsal, log(data$sal), xlab="logsal variable", ylab="log(sal)") 

#renaming of "sex" in "female"
data$female = data$sex

#Drop unnecessary columns
data= subset(data, select=-c(seed1, sexe, u,reg,sex))