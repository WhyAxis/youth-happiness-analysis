library(mice)

df <- read.csv("~/Downloads/DA/young-people-survey/youth-happiness-analysis/responses.csv")

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

# Before proceeding with imputations we need to encode the categorical variables into numeruc factors.
tempData <- mice(personalityTraits,m=5,maxit=50,meth='pmm',seed=500)
personalityComplete <- complete(tempData,1)
# verifying omittion of NA values
table(is.na(personalityComplete))

tempData <- mice(demographics,m=5,maxit=50,meth='pmm',seed=500)
demographicsComplete <- complete(tempData,1)
# verifying omittion of NA values
table(is.na(demographicsComplete))

