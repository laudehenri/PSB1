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
# essais rf,C5.0,ranger,kernelpls,gbm,glmnet,svmLinear3,avNNet,nnet,rbfDDA,regLogistic
t_before <- Sys.time()
Fit1 <- train(Species ~ ., data = irisTrain, 
              method = "rf", 
              trControl = fitControl,
              verbose = FALSE)
t_after <- Sys.time()
duree <- t_after - t_before 
print(duree)


Fit1


prediction_irisTest    <- predict(Fit1, irisTest)
df_pred                <- data.frame(irisTest$Species,prediction_irisTest )

ecart <- df_pred[,1] == df_pred[,2]
nombre_reussites <- sum(ecart)
nombre           <- nrow(df_pred)
accuracy         <- nombre_reussites/nombre * 100

qplot(df_pred[,1],df_pred[,2])

ggplot(df_pred) +
  aes(x = irisTest.Species, fill = prediction_irisTest) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "Réalité", y = "Prévision", title = "Réalité versus prevision par classe", fill = "Répartition") +
  theme_gray()

ligne_nouvelle_data <- data.frame(irisTest[3,])
ligne_nouvelle_data$Species <- NULL

nouvelle_fleur    <- predict(Fit1, ligne_nouvelle_data)
print(nouvelle_fleur)
