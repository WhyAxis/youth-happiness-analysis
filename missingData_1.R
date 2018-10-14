library(mice)

df <- read.csv("responses.csv")

personalityTraits <- df[,c(77:133)]
demographics <- df[,c(141:150)]
# We extract the rows which have missing values.
personalityTraitsMissing <- personalityTraits[rowSums(is.na(personalityTraits)) > 0,]
demographicsMissing <- demographics[rowSums(is.na(demographics)) > 0,]
# There are 144 observations with missing values,the values are absent at random.
demMissingPattern <- md.pattern(demographicsMissing,plot = TRUE)
pTMissingPattern <- md.pattern(personalityTraitsMissing,plot = TRUE)
# As you can observe, the md.patten plot shows us which rows have how many missing values and in how may observations.
# There is no sort of correlation we observe in these patterns.

# From the pattern plot of Missing Demographic Data the Columns Height and Weight both are missing for 10 observations.
# We also notice the most no of missing datas are in the columns height and weight.

# From the pattern plot of Missing Personality Trait Data we infer that,as the questions start getting personal,a few 
# people start avoiding them.There is no particular pattern to the missing values,they are missing at random.

# Since the pattern found is only for 10 observations among 1010 observations we can still say the its a case of MCAR.
# Looking at missing values as a whole.
dfMissing <- df[rowSums(is.na(df))>3,]
md.pattern(dfMissing,plot=TRUE)

# Before proceeding with imputations we need to encode the categorical variables into numeric factors.
table(is.na(personalityTraits$Internet.usage))
table(is.na(personalityTraits$Punctuality))
table(is.na(personalityTraits$Lying))
print(demMissingPattern)
# We observe that none of the categorical variables in personalityTraits have any missing values.Same with Demographic Data.
tempData <- mice(personalityTraits,m=5,maxit=5,meth='pmm',seed=500)
personalityComplete <- complete(tempData,1)
# verifying omittion of NA values
table(is.na(personalityComplete))

tempData <- mice(demographics,m=5,maxit=5,meth='pmm',seed=500)
demographicsComplete <- complete(tempData,1)
# verifying omittion of NA values
table(is.na(demographicsComplete))


#Generating separate dataframes for music , movie prefernces and hobbies.
music <- df[,1:19]
movie <- df[,20:31]
hobbies <-df[,32:63]

#We find out the rows which have missing values and analyse the pattern
musicMissing <- music[!complete.cases(music),] #There are 79 observations with missing values.
movieMissing <- movie[!complete.cases(movie),] #There are 36 observations with missing values.
hobbiesMissing <- hobbies[!complete.cases(hobbies),] #There are 124 observations with missing values.

musicMissingPattern <- md.pattern(musicMissing)
#There are not more than 8 missing values for each column.
hobiesMissingPattern <- md.pattern(hobbiesMissing)
#The maximum number of missing values is 15 for the column Passive sport.
movieMissingPattern <- md.pattern(movieMissing)
#Column Documentary has maximum number of missing values.

#From the above plots it can be observed that there is no pattern in missing values and hence can be assumed that data is missing at random.

#Imputing missing data in music with predictive mean
imputedData <- mice(music, m=3, maxit = 5, method = 'pmm', seed = 500)
musicComplete <-complete(imputedData,1) 

#imputing missing data in movies with predictive mean
imputedData <- mice(movie, m=3, maxit = 5, method = 'pmm', seed = 500)
movieComplete <-complete(imputedData,1) 

#imputing missing data in hobbies with predictive mean
imputedData <- mice(hobbies, m=3, maxit = 5, method = 'pmm', seed = 500)
hobbiesComplete <-complete(imputedData,1) 

#Verifying removal of NA values
sum(is.na(musicComplete))
sum(is.na(movieComplete))
sum(is.na(hobbiesComplete))

phobias <- df[,64:73]
healthHabits <- df[,74:76]
spending <- df[,134:140]

#Imputing Phobias
imputedData <- mice(phobias,m=5,maxit=5,meth='pmm',seed=500)
phobiasComplete <- complete(imputedData,2)

#Imputing spending habits
imputedData <-mice(spending,m=5,maxit=5,meth='pmm',seed=500)
spendingComplete <- complete(imputedData,2)

distinctSmoking <- unique(healthHabits$Smoking, incomparables = FALSE)
distinctAlcohol <- unique(healthHabits$Alcohol, incomparables = FALSE)
healthHabitsCopy <- healthHabits
#Since Smoking and Alcohol have categorical values, we have converted them into numerical values based on their levels.
#Converting into numerical values will ensure easier imputation

#For Smoking:
# never smoked-1
# tried smoking-2
# former smoker-4
# current smoker-5
smokingTransf <- transform(healthHabitsCopy, Smoking = factor(Smoking,levels = c("never smoked","tried smoking","former smoker","current smoker"),labels = c(1,2,4,5)))

#For Alcohol:
# never-1
# social drinker-3
# drink a lot-5
alcoholTransf <- transform(smokingTransf, Alcohol= factor(Alcohol,levels = c("never", "social drinker", "drink a lot"),labels = c(1,3,5)))

#Imputing health habits
healthHabitsImp <- mice(alcoholTransf,m=5,maxit=5,meth='pmm',seed=500)
healthHabitsComplete <- complete(healthHabitsImp,2)

finalImputedDataFrame <- cbind(demographicsComplete, personalityComplete, phobiasComplete, hobbiesComplete, healthHabitsComplete, spendingComplete, musicComplete, movieComplete )
write.csv(finalImputedDataFrame,file = "imputedResponses.csv")
