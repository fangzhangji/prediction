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
validate <- predict(model,newdata=valid)
confusionMatrix(validate,valid$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   A   B   C   D   E
##          A 558   1   0   0   0
##          B   0 378   0   0   0
##          C   0   0 342   0   0
##          D   0   0   0 321   0
##          E   0   0   0   0 360
## 
## Overall Statistics
##                                      
##                Accuracy : 0.9995     
##                  95% CI : (0.9972, 1)
##     No Information Rate : 0.2847     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 0.9994     
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9974   1.0000   1.0000   1.0000
## Specificity            0.9993   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         0.9982   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   0.9994   1.0000   1.0000   1.0000
## Prevalence             0.2847   0.1934   0.1745   0.1638   0.1837
## Detection Rate         0.2847   0.1929   0.1745   0.1638   0.1837
## Detection Prevalence   0.2852   0.1929   0.1745   0.1638   0.1837
## Balanced Accuracy      0.9996   0.9987   1.0000   1.0000   1.0000
```

It appears that the prediction is accurate. Then I did cross valdiation with rfcv:


```r
crossval <- rfcv(trainingnew[,-58],trainingnew[,58],cv.fold=5)
crossval[2]
```

```
## $error.cv
##           57           28           14            7            4 
## 0.0006625217 0.0005096320 0.0003057792 0.0001528896 0.0024462338 
##            1 
## 0.4124961778
```

So the expected error rate is around the errors shown above.
Predicting test cases:


```r
answers <- predict(model,newdata=testingnew)
answers
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
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
