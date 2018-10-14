library(readr)
library(mice)

file<-read.csv("responses.csv")
View(file)
#names(file)
phobias<-file[,64:73]
#View(phobias)
health_habits<-file[,74:76]

#View(health_habits)
spending<-file[,134:140]
View(spending)

#Imputing Phobias
phobia_imp<-mice(phobias,m=5,maxit=50,meth='pmm',seed=500)
imputed_phobias<-complete(phobia_imp,2)
#View(imputed_phobias)
#summary(phobia_imp)

#Imputing spending habits
spending_imp<-mice(spending,m=5,maxit=50,meth='pmm',seed=500)
imputed_spending<-complete(spending_imp,2)
#View(imputed_spending)
#summary(spending_imp)

#To check if all the NAs have been imputed
# for (i in 1:10)
# {
#   print(sum(is.na(phobias[i])))
# }
# for (i in 1:10)
# {
# 
#   print(sum(is.na(phobia_imp[i])))
# }
# for (i in 1:7)
# {
#   print(sum(is.na(spending[i])))
# }
# for (i in 1:7)
# {
#   print(sum(is.na(spending_imp[i])))
# }

distinct_smoking<-unique(health_habits$Smoking, incomparables = FALSE)
distinct_smoking

distinct_alcohol<-unique(health_habits$Alcohol, incomparables = FALSE)
distinct_alcohol

health_habits_copy<-health_habits
#Since Smoking and Alcohol have categorical values, we have converted them into numerical values based on their levels
#Converting into numerical values will ensure easier imputation
#For Smoking:
# never smoked-1
# tried smoking-2
# former smoker-4
# current smoker-5
smoking_transf<-transform(health_habits_copy, Smoking = factor(Smoking,levels = c("never smoked","tried smoking","former smoker","current smoker"),labels = c(1,2,4,5)))
#For Alcohol:
# never-1
# social drinker-3
# drink a lot-5

alcohol_transf<-transform(smoking_transf, Alcohol= factor(Alcohol,levels = c("never", "social drinker", "drink a lot"),labels = c(1,3,5)))
View(alcohol_transf)

#Imputing health habits
health_habits_imp<-mice(alcohol_transf,m=5,maxit=50,meth='pmm',seed=500)
imputed_health_habits <- complete(health_habits_imp,2)
#summary(health_habits_imp)
#class(health_habits_imp)
#To check if NAs have been replaced
# for (i in 1:3)
# {
#   print(sum(is.na(health_habits[i])))
# }
# for (i in 1:3)
# {
# 
#   print(sum(is.na(health_habits_imp[i])))
# }
