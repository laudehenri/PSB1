# exemple de création de modèle et prédiction sur iris

library(caret)
set.seed(666)
trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]

str(iris)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(666)
Fit1 <- train(Species ~ ., data = irisTrain, 
              method = "gbm", 
              trControl = fitControl,
              ## This last option is actually one
              ## for gbm() that passes through
              verbose = FALSE)

Fit1


prediction_irisTest    <- predict(Fit1, irisTest)
df_pred                <- data.frame(irisTest$Species,prediction_irisTest )

ecart <- df_pred[,1] == df_pred[,2]
nombre_reussites <- sum(ecart)
nombre           <- nrow(df_pred)
accuracy         <- nombre_reussites/nombre * 100

