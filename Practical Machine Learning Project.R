train <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"), stringsAsFactors = FALSE)
train$classe <- as.factor(train$classe) 
train2 <- train[ , colSums(is.na(train))< 1962]
train3 <- train2[ , 8:60]
set.seed(4242)

validation <- createDataPartition(y=train3$classe, p=0.3, list = FALSE)
ForTest <- train3[validation,]
ForTrain <- train3[-validation, ]
fitMod <- randomForest(classe~., data=ForTrain)
pred <- predict(fitMod, ForTest)
ForTest$Correct <- pred==ForTest$classe
table(pred,ForTest$classe)
# correct for A: 1674/1674, B: 1129/1140, C: 1022/1027, D: 951/965, E: 1083/1083
# overall misclassification: 30/5889 = .0051

varImpPlot(fitMod, sort=TRUE, n.var=8, main = "variable importance")

## par(mfrow=c(1,3))
qplot(roll_belt, yaw_belt, colour=Correct, data=ForTest, main = "cross-validation predictions")
qplot(roll_forearm, magnet_dumbbell_x, colour=Correct, data=ForTest, main = "cross-validation predictions")
qplot(total_accel_belt, magnet_forearm_y, colour=Correct, data=ForTest, main = "cross-validation predictions")

test <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"), stringsAsFactors = FALSE)
model <- randomForest(classe~., data=train3)
# varImpPlot(model, sort=TRUE, main = "variable importance")
varImpPlot(model, sort=TRUE, n.var=8, main = "variable importance")

prediction <- predict(model, test)

pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}
test <- read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"), stringsAsFactors = FALSE)
#test$classe <- as.factor(test$classe) 
