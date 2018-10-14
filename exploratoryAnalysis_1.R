library(corrplot)
library(dplyr)
library(ggplot2)

setwd("~/Downloads/DA/young-people-survey/youth-happiness-analysis/")

df <- read.csv("imputedResponses.csv",na.strings=c(""," ","NA"))

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
  return(0.45455*weight/(.0254*height)^2)
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

ggplot(workingData, aes(x=workingData[["Loneliness"]], fill=workingData$Gender)) +
  ggtitle(factorV) +
  geom_density(alpha=0.4)+
  theme(plot.title = element_text(color="red", size=14, face="bold.italic")
        ,legend.position ="bottom",legend.text=element_text(size=8))

plots <- list()
i = 1
for (factorV in happinessFactors){
    p <- plot.new()
    jpeg("rplot"+i+".jpg", width = 350, height = "350")
    ggplot(workingData, aes(x=workingData[[factorV]], fill=workingData$Gender)) +
    ggtitle(factorV) +
    geom_density(alpha=0.4)+
    theme(plot.title = element_text(color="red", size=14, face="bold.italic")
          ,legend.position ="bottom",legend.text=element_text(size=8))
    dev.off()
  # print(p)
  # plots[[i]] <- p
  # i = i + 1
}
# grid.arrange(grobs = plots,ncol=5)
# grid.newpage()
# grid.draw(plots[[1]])
# grid.newpage()
# grid.draw(lg[[2]])
# plots[[2]]
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# multiplot(plotlist = plots, cols = 5)
