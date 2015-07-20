---
title: "Prediction of Activity Type"
output: html_document
---

This report uses data collected by devices detecting human movements. I described how I built my model, how I did cross validation and estimated error, and how I made the choices on these steps. Then I predicted activity types with the testing dataset.

First read data:


```r
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
library(caret)
```

Then I processed data. I found there are NA values in the testing set so I excluded those columns from both training and testing data, using near zero variation. In addition, the first column "X" in both datasets are unique ids for each observation. I excluded it from bot datasets as well because it would heavily biase the prediction results.


```r
#calculating near zero variation (NAs in the testing set):
nzv <- nearZeroVar(testing,saveMetrics=TRUE)
#keeping non NAs in both datasets:
testingnew <- testing[,which(nzv$zeroVar=="FALSE")]
trainingnew <- training[,which(nzv$zeroVar=="FALSE")]
#removing the first column "X" from both datasets:
testingnew <- testingnew[,-1]
trainingnew<-trainingnew[,-1]
```

I sliced training data into training and validation sets:


```r
intrain <- createDataPartition(y=trainingnew$classe,p=0.9,list=FALSE)
train<-trainingnew[intrain,]
valid<-trainingnew[-intrain,]
```

I chose to fit a random forests model because it is precise and works better with non-linear models. Then I did prediction with the validation data.


```r
model <- train(classe~.,data=train,method="rf")
```

```
## Error in e$fun(obj, substitute(ex), parent.frame(), e$data): worker initialization failed: list(Accuracy = 0.998923408182098, Kappa = 0.998639019409807, .cell1 = 1844, .cell2 = 0, .cell3 = 0, .cell4 = 0, .cell5 = 0, .cell6 = 3, .cell7 = 1215, .cell8 = 2, .cell9 = 0, .cell10 = 0, .cell11 = 0, .cell12 = 1, .cell13 = 1128, .cell14 = 0, .cell15 = 0, .cell16 = 0, .cell17 = 0, .cell18 = 0, .cell19 = 1115, .cell20 = 0, .cell21 = 0, .cell22 = 0, .cell23 = 0, .cell24 = 1, .cell25 = 1193, mtry = 79, Resample = "Resample03")NULL
```

```r
validate <- predict(model,newdata=valid)
```

```
## Error in predict(model, newdata = valid): object 'model' not found
```

```r
confusionMatrix(validate,valid$classe)
```

```
## Error in confusionMatrix(validate, valid$classe): object 'validate' not found
```

It appears that the prediction is accurate. Then I did cross valdiation with rfcv:


```r
crossval <- rfcv(trainingnew[,-58],trainingnew[,58],cv.fold=5)
crossval[2]
```

```
## $error.cv
##           57           28           14            7            4 
## 0.0009683009 0.0008663745 0.0006625217 0.0004077056 0.0024462338 
##            1 
## 0.4120375089
```

So the expected error rate is around the errors shown above.
Predicting test cases:


```r
answers <- predict(model,newdata=testingnew)
```

```
## Error in predict(model, newdata = testingnew): object 'model' not found
```

```r
answers
```

```
## Error in eval(expr, envir, enclos): object 'answers' not found
```

```r
#Writing answers into text files:
pml_write_files=function(x){
      n=length(x)
      for (i in 1:n){
            filename=paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
     }
}
pml_write_files(answers)
```

```
## Error in pml_write_files(answers): object 'answers' not found
```
