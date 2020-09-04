#significativity test : 15-20%

print("age :")
var_significativity(data$age) #ok 

print("age2 :")
var_significativity(data$age2) #ok

print("female :")
var_significativity(data$female) #ok

print("for. :")
var_significativity(data$for.) #ok

print("reg1 :")
var_significativity(data$reg1, FALSE) #ok

print("reg2 :")
var_significativity(data$reg2, FALSE) #not ok 

print("reg3 :")
var_significativity(data$reg3, FALSE) #not really ok 

print("reg4 :")
var_significativity(data$reg4, FALSE) #ok