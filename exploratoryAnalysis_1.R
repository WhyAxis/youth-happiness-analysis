library(corrplot)
library(dplyr)
library(ggplot2)

# setwd("~/Downloads/DA/young-people-survey/youth-happiness-analysis/")
df <- read.csv("imputedResponses.csv",na.strings=c(""," ","NA"))
colnames <- read.csv("columns.csv")
# Following are the columns we chose as parameters to judge how happy pr sad a person is.
happinessFactors <- c("Hypochondria","Loneliness","Dreams","Number.of.friends","Mood.swings",
                      "Getting.angry","Life.struggles","Happiness.in.life","Energy.levels","Personality")
happinessSadness <- df[happinessFactors]
correlationMatrix = cor(happinessSadness,use="na.or.complete")
correlationMatrix
plot.new()
plot1 = corrplot.mixed(correlationMatrix, lower.col = "black", number.cex = .7,tl.pos = "l",
                      title="Correlation Plot",mar=c(0,0,1,0),tl.cex = 1/par("cex"))
covarianceMatrix = cov(happinessSadness, use="na.or.complete")
covarianceMatrix

# We consider the above mentoned variables as the factors of happiness and sadness.From their correlation 
# plot we can infer that none of the variables under study are very highly correlated.So,we use these ten 
# factors across sections of our dataset to find the variables which effect these factors the most and in 
# the end effect the happiness / sadness of people.

# The first ten columns show th demographics
# As we studied in prior kernels,considering bmi instead if height and weight as separate metrics is better.
# We have established that the height is in cms and weight in kgs.

bmi = function(height,weight){
  return(weight/(height/100)^2)
}
BMI = bmi(df$Height,df$Weight)
demographics <- cbind(df[,c(1,2,5:10)],BMI)

# Hapiness factors across Gender which is a categorical form of data.
freqGender <- table(demographics$Gender)
# We notice the bias is not much,hence we can proceed with the analysis.
workingData <- cbind(demographics,happinessSadness)
summary(workingData)
# We observe that the sum of no of males and females does not come up to 1010 even though they are no NA values.
# Hence we drop those entries from our dataset for which we have empty strings.
workingData <- na.omit(workingData)
summary(workingData)

femaleData <- workingData[workingData$Gender == "female",]
maleData <- workingData[workingData$Gender == "male",]
i = 1
dev.off()
par(mfrow=c(2,5))
for (factorV in happinessFactors){
    plot(density(femaleData[[factorV]]),col = "blue")
    lines(density(maleData[[factorV]]),col = "red")
        
}
dev.off()
par(mfrow=c(2,5))
for (factorV in happinessFactors){
counts <- table(workingData[[factorV]],workingData$Age)
barplot(counts, main= paste("Distribution by" ,factorV ,"and Age"),
        xlab=factorV, col=c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff")
        )
legend("topright",legend = rownames(counts),fill = c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff") ,ncol = 1,cex=0.4)
}

dev.off()
hist(workingData$BMI,col = "blue",breaks = 100,xlim = c(12,60))
workingData$BMI[workingData$BMI <= 18.5] = 1
workingData$BMI[workingData$BMI > 18.5 & workingData$BMI <= 20] = 2
workingData$BMI[workingData$BMI > 20 & workingData$BMI <= 25] = 3
workingData$BMI[workingData$BMI > 25 & workingData$BMI <= 30] = 4
workingData$BMI[workingData$BMI > 30] = 5
dev.off()
par(mfrow=c(2,5))
for (factorV in happinessFactors){
  counts <- table(workingData[[factorV]],workingData$BMI)
  barplot(counts, main= paste("Distribution by" ,factorV ,"and Health Factor"),
          xlab=factorV, col=c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff"))
  legend("topright",legend = rownames(counts),fill = c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff") ,ncol = 1,cex=0.4)
}



#Inverting the values of Sadness Factors and performing PCA
modify <- function(x) 5-x

modifiedHappinessSadness <- data.frame(happinessSadness[,c('Dreams','Number.of.friends','Happiness.in.life','Energy.levels','Personality')], lapply(happinessSadness[,c('Hypochondria','Loneliness','Mood.swings','Getting.angry','Life.struggles')], modify) )
pcaHappinessSadness <- prcomp(modifiedHappinessSadness)
pcaHappinessSadness = as.data.frame(pcaHappinessSadness$x[,1])

category <- vector(length = 1010)
modifiedHappinessSadness = cbind(modifiedHappinessSadness,pcaHappinessSadness,category)
colnames(modifiedHappinessSadness)[11] <- "pcaHappinessSadness"
modifiedHappinessSadness$category[modifiedHappinessSadness$pcaHappinessSadness<0] = "FALSE"
modifiedHappinessSadness$category[modifiedHappinessSadness$pcaHappinessSadness>0] = "TRUE"