library(corrplot)
library(ggplot2)
library(ggpubr)
library(forecast)
require(forecast)
require(corrplot)
require(ggplot2)
require(ggpubr)

train <- read.csv("/Users/seshuram1996/Desktop/train.csv")
head(train)
dim(train)


names(train)
summary(train)

for(i in 1:ncol(train)) {
  colName <- colnames(train[i])
  pctNull <- sum(is.na(train[,i]))/length(train[,i])
  if (pctNull > 0.90) {
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
  }
}

train$Alley <- NULL
train$PoolQC <- NULL
train$MiscFeature <- NULL

#Remove id column
train$Id <- NULL

glimpse(train)

#Convert categorical variable to numeric
categoricalToNumeric <- function(data){
    must.convert <- sapply(data ,is.factor)            
    data.aux<-sapply(data[,must.convert],unclass)     
    data.out<-cbind(data[,!must.convert],data.aux)   
    return(data.out)    
}
#Categorical to numeric
train <- categoricalToNumeric(train)

head(train)

glimpse(train)


sum(is.na(train))
train[is.na(train)] <- 0
head(train)

# Draw a higtogram to figure out the distribution of SalePrice
options(scipen=10000)
ggplot(train, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))
  
#From the histogram above, the distribution of our target variable-- SalePrice is skewed to right. 
#Thus, a log term of SalePrice should be generated for linear regression.
#Here, we name it log_SalePrice.
  
#log term of SalePrice
train$log_SalePrice <- log(train$SalePrice)

# Draw a higtogram to figure out the distribution of log SalePrice

ggplot(train, aes(x = log_SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 0.05) +
  ggtitle("Figure 2 Histogram of log SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))

lm.fit=lm(log_SalePrice~MSZoning,data=train)
summary(lm.fit)

lm.fitall=lm(log_SalePrice~.-SalePrice, data = train)
summary(lm.fitall)

#observed imp features in prediction
#OverallCOnd, MasVnrArea, BsmtFinSF1,BsmtFinSF2, BsmtUnfSF, X2ndFlrSF, BsmtFullBath,
#BedroomAbvGr, GarageYrBlt, PoolArea, RoofMatl,ExterQual, BsmtQual, BsmtExposure, CentralAir,YearBuilt
#KitchenQua, PoolQC, 

#select variables that be used for model buidling and heat map
model_var <- c('SalePrice', 
               'OverallQual','OverallCond','YearBuilt','ExterCond2',
               'TotalBsmtSF','HeatingQC2', 
               'CentralAir2','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces',
               'GarageArea','OpenPorchSF','PoolArea',
               'YrSold')
heat <- train[,model_var]

#plot correlation heatmap for SalePrice
options(repr.plot.width=8, repr.plot.height=6)
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(heat, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Figure 7 Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))

head(train)
#prediction of lm
#build model dataset for linear regression 
train_var <-c('log_SalePrice', 'OverallCond', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'X2ndFlrSF',
              'BsmtFullBath', 'BedroomAbvGr', 'GarageYrBlt', 'PoolArea', 'RoofMatl', 'ExterQual',
              'BsmtQual', 'BsmtExposure', 'CentralAir', 'KitchenQual','YearBuilt')
train_var <- train[, train_var]
set.seed(42)
train_var.split <- sample(2, nrow(train_var)
                        , replace = TRUE
                        , prob = c(0.8, 0.2))
train_var = train_var[train_var.split == 1,]
train_val   = train_var[train_var.split == 2,]
paste('Length Training Set: ',nrow(train_var),'| Length Validation Set: ',nrow(train_val))
head(train_var)

model = lm(log_SalePrice~., data = train_var)
summary(model)


#use predict() to make prediction on a new set
pred1 <- predict(model,train_val,type = "response")
residuals <- train_val$log_SalePrice - pred1
pred <- data.frame("Predicted" = pred1, "Actual" = train_val$log_SalePrice, "Residual" = residuals)
accuracy(pred1, train_val$log_SalePrice)

plot(pred1, train_val$log_SalePrice, main = "Figure 9 Predicted vs. Actual log SalePrice") 
abline(0,1)
