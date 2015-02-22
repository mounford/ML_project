



training <- read.csv("pml-training.csv", header = TRUE, na.strings = "NA", colClasses = "character", stringsAsFactors = FALSE)

testing <- read.csv("pml-testing.csv", header = TRUE, na.strings = "NA", colClasses = "character", stringsAsFactors = FALSE)



training <- training[,-c(1:7)]


k <- c(rep(TRUE,152),FALSE)
j <- c(rep(FALSE,7),rep(TRUE,153))


training[k] <- lapply(training[k], as.numeric)
training[j] <- lapply(training[j], as.numeric)
training[,153] <- as.factor(training[,153])

trainingwna <- training[, which(as.numeric(colSums(is.na(training))) < 1)] 

colnames(trainingwna)


library(caret)

it <- createDataPartition(y = trainingwna$classe, p = 0.60, list = FALSE)

trainingwna <- trainingwna[it,]

trainingwna <- trainingwna[-it,]

set.seed(1234)


library(doParallel)

registerDoParallel(cores = 4)

mod <- train(classe ~., data = trainingwna, method = "C5.0", prox = TRUE, trControl = trainControl("cv", number = 4, verboseIter = TRUE, allowParallel = TRUE))


predictions <- predict(mod, newdata = trainingwna)


confusionMatrix(predictions, trainingwna$classe)

library(plyr)
library(dplyr)
library(psych)

library(e1071)
library(C50)

answ <- as.character(predict(mod, newdata=testing))


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(answ)


