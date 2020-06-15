# Read the data set
su<-read.csv(file.choose())
summary(su)
attach(su)

# Plot multiple variables across each other (scatterplot)
pairs(su)


# Plot multiple variables across each other (scatterplot + cor)

library(GGally)
ggpairs(su)

# Building the basic model
msu<-lm(Profit~.,data=su)
summary(msu)

#So, it seems that only R.D. Spend is having significant co-efficient, contributing towards the prediction of model.
#Also an error  of '1 not defined because of singularities' exist due to co linearity b/w  predictors. In that case the dummy variables will be marked as n-1 for further implementations.

#Now finding the model with Administration only to see its contribution
msuadmin<-lm(Profit~Administration)
summary(msuadmin)

#So, again it's not significant.
#Now finding the model with Marketing Spend only to see its contribution

msumrktspnd<-lm(Profit~Marketing.Spend)
summary(msumrktspnd)

#Now, the Marketing. Spend is significant. So, it is supportive that Administration is not contributing much towards the model prediction. 

#Also to double confirm
avPlots(msu)

#So, it supports that none of the predictors (Administration, CA, and NY) are significant or contributing towards the prediction of model except R.D. Spend and Marketing. Spend.
#Building a new model with R.D. Spend and Marketing. Spend
msuf<-lm(Profit~R.D.Spend+Marketing.Spend)
summary(msuf)

#Finding the most influence indicator
influenceIndexPlot(msuf,id.n=3)
#So, as it looks like 50 is the most common influence indicator/outlier in the data set

#Building a model (excluding the influence indicator)
msufei<-lm(Profit~R.D.Spend+Marketing.Spend, data = su[-50,])
summary(msufei)

#So, finally we have got a basic model (without any transformations) where we have the significant co-efficients contributing towards the prediction of the model.

#Finding mean of the residuals
mean(msufei$residuals)
#9.030418e-14

#It should be closer to zero.

#Finding the RMSE
sqrt(mean((msufei$residuals)^2))
#7452.7

#to double confirm your final model, will give you the fittest model
# the least StepAIC, the better is the model
install.packages("MASS")
library(MASS)
stepAIC(msu)

