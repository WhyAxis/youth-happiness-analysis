library(corrplot)
library(dplyr)
library(ggplot2)
library(DescTools)
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

dev.off()
par(mfrow=c(2,5))
for (factorV in happinessFactors){
  hist(df[[factorV]])
  lines(density(df[[factorV]]),col = "red")
}
# We consider the above mentoned variables as the factors of happiness and sadness.From their correlation 
# plot we can infer that none of the variables under study are very highly correlated.So,we use these ten 
# factors across sections of our dataset to find the variables which effect these factors the most and in 
# the end effect the happiness / sadness of people.


# Since analysing for ten factors ,we bottle it down to the principal components that indicate happiness
# We invert those which indicate sadness and perform PCA to obtain the Principal Component which shows if 
# a person is happy or not.
# Inverting the values of Sadness Factors and performing PCA
modify <- function(x) 5-x

modifiedHappinessSadness <- data.frame(happinessSadness[,c('Dreams','Number.of.friends','Happiness.in.life','Energy.levels','Personality')], lapply(happinessSadness[,c('Hypochondria','Loneliness','Mood.swings','Getting.angry','Life.struggles')], modify) )
pcaHappinessSadness <- prcomp(modifiedHappinessSadness)
pcaHappinessSadness = as.data.frame(pcaHappinessSadness$x[,1])

Happy <- vector(length = 1010)
modifiedHappinessSadness = cbind(modifiedHappinessSadness,pcaHappinessSadness,Happy)
colnames(modifiedHappinessSadness)[11] <- "pcaHappinessSadness"
modifiedHappinessSadness$Happy[modifiedHappinessSadness$pcaHappinessSadness<0] = "FALSE"
modifiedHappinessSadness$Happy[modifiedHappinessSadness$pcaHappinessSadness>0] = "TRUE"
df$Happy <- modifiedHappinessSadness$Happy
# The first ten columns show th demographics

dev.off()
par(mfrow=c(2,5),mar=c(2,2,2,2))
for (factorV in happinessFactors){
  hist(happinessSadness[[factorV]],breaks = c(0,1,2,3,4,5),freq = FALSE,col="#3399FF",main="",mgp=c(1,0,0),xlab=factorV)
}

# The first ten columns show the demographics

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
workingData <- cbind(demographics,happinessSadness,df$Happy)
summary(workingData)
# We observe that the sum of no of males and females does not come up to 1010 even though they are no NA values.
# Hence we drop those entries from our dataset for which we have empty strings.
workingData <- na.omit(workingData)
summary(workingData)

femaleData <- workingData[workingData$Gender == "female",]
maleData <- workingData[workingData$Gender == "male",]
i = 1
dev.off()
par(mfrow=c(2,5), mar=c(1,1,1,1))
for (factorV in happinessFactors){
    plot(density(femaleData[[factorV]]),col = "blue")
    lines(density(maleData[[factorV]]),col = "red")
        
}
dev.off()
happinessCount <- table(workingData$`df$Happy`,workingData$Gender)
barplot(happinessCount, main= paste("Happy vs Gender"), col=c("red","blue"))
legend("topright",legend = rownames(happinessCount),fill = c("red","blue") ,ncol = 1,cex=0.8)
dev.off()
par(mfrow=c(2,5), mar=c(1,1,1,1))
for (factorV in happinessFactors){
counts <- table(workingData[[factorV]],workingData$Age)
barplot(counts, main= paste("Distribution by" ,factorV ,"and Age"),
        xlab=factorV, col=c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff")
        )
legend("topright",legend = rownames(counts),fill = c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff") ,ncol = 1,cex=0.4)
}

dev.off()
hist(workingData$BMI,col = "blue",breaks = 100,xlim = c(12,60))
# Since BMI is the numerical value of a scale different from other variables we redcode the data to the same scale.
# 1,2,3,4,5 being underweight,fit,healthy,overweight and obese respectively.
workingData$BMI[workingData$BMI <= 18.5] = 1
workingData$BMI[workingData$BMI > 18.5 & workingData$BMI <= 20] = 2
workingData$BMI[workingData$BMI > 20 & workingData$BMI <= 25] = 3
workingData$BMI[workingData$BMI > 25 & workingData$BMI <= 30] = 4
workingData$BMI[workingData$BMI > 30] = 5
dev.off()
par(mfrow=c(2,5),mar=c(1.2,1.2,1.2,1.2))
for (factorV in happinessFactors){
  counts <- table(workingData[[factorV]],workingData$BMI)
  barplot(counts, main= paste("Dist by" ,factorV ,"and Health Factor"),
          xlab=factorV, col=c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff"))
  legend("topright",legend = rownames(counts),fill = c("#000019","#0000ff","#7f7fff","#b2b2ff","#e5e5ff") ,ncol = 1,cex=0.4)
}

reqVariables = names(IndianData) %in% c("ID","Name","Sex","Age","Height","Weight")
newData = unique(IndianData[!nreqVariables])

# Analysis of various personality traits wrt Happy Label.
workingData <- df[,c(12:67)]
nreqVariables = names(df) %in% happinessFactors
personalityTraits <- workingData[!nreqVariables]
workingData$Happy = df$Happy
workingData$Happy[workingData$Happy == TRUE] = 1
workingData$Happy[workingData$Happy == FALSE] = 0

corrVals <- list()
i <- 1
for(variable in colnames(personalityTraits)){
  print(variable)
  corrVals[i] <- GoodmanKruskalGamma(personalityTraits[[variable]],workingData$Happy)
  i <- i + 1
}
corrVals <- as.numeric(corrVals)
setNames(corrVals,colnames(personalityTraits))

corrVals <- data.frame(corrVals)
corrVals$Traits = as.vector(colnames(personalityTraits))
corrValsModified <- corrVals
corrValsModified$Traits <- factor(corrValsModified$Traits, levels = corrValsModified$Traits[order(-corrValsModified$corrVals)])

dev.off()
ggplot(corrValsModified, aes(x=Traits,y=corrVals,fill = Traits)) + geom_bar(stat="identity") + scale_fill_hue() + coord_flip()

relevantTraits <- corrValsModified[(corrValsModified$corrVals >= 0.25 | corrValsModified$corrVals <= -0.25),]
ggplot(relevantTraits, aes(x=Traits,y=corrVals,fill = Traits)) + geom_bar(stat="identity") + scale_fill_hue() + coord_flip()
 
print(relevantTraits$Traits)
# We observe that amongst the personality traits in consideration only a few show plausible correlation with the Happiness quotient.
# 