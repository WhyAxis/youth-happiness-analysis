library(corrplot)
library(dplyr)
library(ggplot2)
library(DescTools)
setwd("~/Downloads/DA/young-people-survey/youth-happiness-analysis/")
df <- read.csv("imputedResponses.csv",na.strings=c(""," ","NA"))
colnames <- read.csv("columns.csv")
# Following are the columns we chose as parameters to judge how happy or sad a person is.
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
dev.off()
par(mfrow=c(2,5),mar=c(2,2,2,2))
for (factorV in happinessFactors){
  hist(happinessSadness[[factorV]],breaks = c(0,1,2,3,4,5),freq = FALSE,col="#3399FF",main="",mgp=c(1,0,0),xlab=factorV)
}

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
    plot(density(femaleData[[factorV]]),col="blue")
    lines(density(maleData[[factorV]]),col="red")
        
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

# Analysis of various personality traits wrt Happy Label.
workingData <- df[,c(12:67)]
nreqVariables = names(workingData) %in% happinessFactors
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
# Analysis of music preferences wrt Happy Label.
musicData <- df[,121:139]
musicColNames <- colnames(musicData)
musicData$Happy = df$Happy
musicData$Happy[musicData$Happy == TRUE] = 1
musicData$Happy[musicData$Happy == FALSE] = 0

corrValsMusic <- list()
i <- 1
for(variable in musicColNames){
  corrValsMusic[i] <- GoodmanKruskalGamma(musicData[[variable]],musicData$Happy)
  i <- i + 1
}
corrValsMusic <- as.numeric(corrValsMusic)
corrValsMusic <- data.frame(corrValsMusic)
corrValsMusic$Music = as.vector(musicColNames)

corrValsModifiedMusic <- corrValsMusic
corrValsModifiedMusic$Music <- factor(corrValsModifiedMusic$Music, levels = corrValsModifiedMusic$Music[order(-corrValsModifiedMusic$corrValsMusic)])
dev.off()
ggplot(corrValsModifiedMusic, aes(x=Music,y=corrValsMusic,fill = Music)) + geom_bar(stat="identity") + scale_fill_hue() + coord_flip()
relevantMusic <- corrValsModifiedMusic[(corrValsModifiedMusic$corrVals >= 0.1 | corrValsModifiedMusic$corrVals <= -0.1),]

#From the plot we observe that none of the music preferences show high correlation with the happiness label.

# K-means clustering on music preferences which have correlation greater than 0.1
clusterVar <- musicData[relevantMusic$Music]
clusterVar$Happy = musicData$Happy
clusterVar$Happy <- as.numeric(clusterVar$Happy)
no_rows_train_data<-0.9*nrow(clusterVar)
trainMusic<-clusterVar[1:no_rows_train_data,]
testMusic<-clusterVar[no_rows_train_data:nrow(clusterVar),]

fviz_nbclust(trainMusic, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
set.seed(123)
km.res <- kmeans(clusterVar, 4, nstart = 25)
km.res$centers
fviz_cluster(km.res, data = clusterVar)
predictedCluster <- as.vector(cl_predict(km.res,testMusic))
testMusic$Predicted = predictedCluster
testMusic$Predicted[testMusic$Predicted <=2] = 0
testMusic$Predicted[testMusic$Predicted >2] = 1
confusionMatrix(as.factor(testMusic$Happy),as.factor(testMusic$Predicted))
#We have achieved an accuracy of 0.6275 in predicting the happiness quotient according to music preferences.

# Analysis of movie preferences wrt Happy Label.
movieData <- df[,140:151]
movieColNames <- colnames(movieData)
movieData$Happy = df$Happy
movieData$Happy[movieData$Happy == TRUE] = 1
movieData$Happy[movieData$Happy == FALSE] = 0

corrValsMovie <- list()
i <- 1
for(variable in movieColNames){
  corrValsMovie[i] <- GoodmanKruskalGamma(movieData[[variable]],movieData$Happy)
  i <- i + 1
}
corrValsMovie <- as.numeric(corrValsMovie)
corrValsMovie <- data.frame(corrValsMovie)
corrValsMovie$Movie = as.vector(movieColNames)

corrValsModifiedMovie <- corrValsMovie
corrValsModifiedMovie$Movie <- factor(corrValsModifiedMovie$Movie, levels = corrValsModifiedMovie$Movie[order(-corrValsModifiedMovie$corrValsMovie)])
dev.off()
ggplot(corrValsModifiedMovie, aes(x=Movie,y=corrValsMovie,fill = Movie)) + geom_bar(stat="identity") + scale_fill_hue() + coord_flip()
relevantMovie <- corrValsModifiedMovie[(corrValsModifiedMovie$corrVals >= 0.1 | corrValsModifiedMovie$corrVals <= -0.1),]

# K-means clustering on movie preferences which have correlation greater than 0.1
clusterVar <- movieData[relevantMovie$Movie]
clusterVar$Happy = movieData$Happy
clusterVar$Happy <- as.numeric(clusterVar$Happy)
no_rows_train_data <- 0.9*nrow(clusterVar)
trainMovie <- clusterVar[1:no_rows_train_data,]
testMovie <- clusterVar[no_rows_train_data:nrow(clusterVar),]

fviz_nbclust(trainMusic, kmeans, method = "wss") 
set.seed(123)
km.res <- kmeans(clusterVar, 4, nstart = 25)
km.res$centers
fviz_cluster(km.res, data = clusterVar)
predictedCluster <- as.vector(cl_predict(km.res,testMovie))
testMovie$Predicted = predictedCluster
testMovie$Predicted[testMovie$Predicted <3 ] = 0
testMovie$Predicted[testMovie$Predicted ==3] = 1
confusionMatrix(as.factor(testMovie$Happy),as.factor(testMovie$Predicted))
#We get an accuracy of 0.58 when predicting happiness label using K-means clustering on movie preferences.



#Analysing phobias wrt happy label
phobias<-df[,69:78]
phobiasColNames<-colnames(phobias)
phobias$Happy<-df$Happy
phobias$Happy[phobias$Happy == TRUE] = 1
phobias$Happy[phobias$Happy == FALSE] = 0
corrPhobias <- list()
i <- 1
for(variable in phobiasColNames){
  corrPhobias[i] <- GoodmanKruskalGamma(phobias[[variable]],phobias$Happy)
  i <- i + 1
}
corrPhobias <- as.numeric(corrPhobias)
corrPhobias <- data.frame(corrPhobias)
corrPhobias$Phobias = as.vector(phobiasColNames)
corrValsModifiedPhobias<-corrPhobias
dev.off()
ggplot(corrValsModifiedPhobias, aes(x=Phobias,y=corrPhobias,fill =Phobias)) + geom_bar(stat="identity") + scale_fill_hue() + coord_flip()
relevantPhobias <- corrValsModifiedPhobias[(corrValsModifiedPhobias$corrVals >= 0.1 | corrValsModifiedPhobias$corrVals <= -0.1),]

