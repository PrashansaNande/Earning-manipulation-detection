#Read Data
  library(readxl)
  Sample_Data <- read_excel("~/UIC/Courses/DataMining/Assignments/Case Study 1/Dataset/Sample_Data.xlsx")

#View and primary analysis of the imported dataset
  View(Sample_Data)
  dim(Sample_Data) #220  11
  str(Sample_Data)
  summary(Sample_Data)

#Converting target variable to factor type
  sample_final <- Sample_Data
  sample_final$`C-MANIPULATOR` <- as.factor(sample_final$`C-MANIPULATOR`)
  class(sample_final$`C-MANIPULATOR`)

#Removing unwanted variables
  sample_final$`Company ID` <- NULL
  sample_final$Manipulator <- NULL

# Changing the target variable name to a proper format
  colnames(sample_final)[9] <- "C_MANIPULATOR"

#Checking the count of classes of target variable
  tab <- table(sample_final$C_MANIPULATOR)
  tab
  #  0   1 
  #  181  39 

#As we can see from the above result(count) that 
#the number of observation for the event class is very less as compared to 
#the other class of the target variable. This indicates that the dataset is 
#unbalanced.

#***************** Classification before Data - balancing *****************
# Sampling the sample dataset - Partition the sample data into training & Test data
  set.seed(1234)
  index <- sample(2, nrow(sample_final), replace = TRUE, prob = c(0.65,0.35))
  sample_train <- sample_final[index == 1,]
  sample_test <- sample_final[index == 2,]


  View(sample_train)
  summary(sample_train)
  tab <- table(sample_train$C_MANIPULATOR)
  tab



################### Model 1 : Logistic Regression

# Variable Selection
  null = glm(C_MANIPULATOR~1, data = sample_train, family = binomial) #Includes only the intercept
  full = glm(C_MANIPULATOR~., data = sample_train, family = binomial)
#Forward Selection
  step(null, scope=list(lower=null, upper=full), direction="forward")
#Backward Selection
  step(full, scope=list(lower=null, upper=full), direction="backward")

#After  running either or both (forward & backward) variable selection method, 
#we can see from the output that the important variables are
# DSRI + SGI + ACCR + AQI + GMI
#Hence, we will run our model using only these important input variables

#Runnig Logistic Regression model 
  lg_model<- 
    glm(C_MANIPULATOR~., data= sample_train, family = "binomial")
  summary(lg_model)
  lg_model_impVar <- glm(C_MANIPULATOR~DSRI + SGI + ACCR + AQI + GMI, data= sample_train, family = "binomial")
  summary(lg_model_impVar)
  
  

#Checking the performance of our model on train and test data
  # predict_train_lr1 <- predict(lg_model_impVar, sample_train, type = "response") 
  # predict_train_lr1
  # predict_test_lr1 <- predict(lg_model_impVar, sample_test, type = "response")  #Check Predicted Class for TrainData
  # predict_test_lr1
  predict_test_lr1 <- predict.glm(lg_model_impVar, sample_test, type = "response")
  predict_test_lr1 <- round(predict_test_lr1)
  predict_test_lr1
  class(predict_test_lr1)
  predict_test_lr1$

# # Condusion matrix - for checking prediction accuracy of the model
# # Installing necessary packages
install.packages("caret")
install.packages("e1071")
# 
library(caret)
library(e1071)
# 
# pred_train_lr <- predict(lg_model_impVar, sample_train, type="class")
# confusionMatrix(pred_train_lr, sample_train$C_MANIPULATOR, positive = '1')
# confusionMatrix(test_tab_lr1)


#*************************************ROC curve************************************
# Install package
install.packages("ROCR")
# Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
# Calculating the values for ROC curve
pred_roc = prediction(predict_test_lr1, sample_test$C_MANIPULATOR)
pred_roc
perf_roc = performance(pred_roc,"tpr","fpr")
perf_roc
# Plotting the ROC curve
plot(perf_roc, col = "black", lty = 3, lwd = 3)
# Calculating AUC
auc = performance(pred_roc, "auc")
auc
# Now converting S4 class to a vector
auc = unlist(slot(auc, "y.values"))
# Adding min and max ROC AUC to the center of the plot
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep = "")
maxauct = paste(c("max(AUC) = "), maxauc, sep = "")
legend(0.8, 0.3, c(minauct, maxauct, "\n"), 
       border = "pink", cex = 0.5, box.col = "white")
abline(a= 0, b=1)
#Getting an optimal cut point
opt.cut = function(perf_roc, pred_roc){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, perf_roc@x.values, perf_roc@y.values, pred_roc@cutoffs)}
print(opt.cut(perf_roc, pred_roc))
#sensitivity 0.9000000
#specificity 0.8070175
#cutoff      0.1197836

predict_test_lr1 <- ifelse(predict_test_lr1>0.1197836,1,0)
predict_test_lr1
#sum(predict_test_lr1$C_MANIPULATOR[predict_test_lr1$C_MANIPULATOR=="1"])


#levels(sample_test$C_MANIPULATOR)
ptab<-table(predict_test_lr1, sample_test$C_MANIPULATOR, dnn = c("Predicted","Actual"))
ptab

confusionMatrix(ptab,positive = "1")
# Sensitivity : 0.8000          
# Specificity : 0.8070 
# Accuracy    : 0.8059701


#***************** Classification after Data - balancing *****************
  # Balancing the data
  
  install.packages("ROSE")
  library(ROSE)
  over_sample <- ovun.sample(C_MANIPULATOR~., data = sample_train, method = "over", N= 248)$data
  over_sample
  #*************** Logistic Regression ********************
   # Variable Selection
  null = glm(C_MANIPULATOR~1, data= over_sample, family = "binomial") # Includes only the intercept
  full = glm(C_MANIPULATOR~., data= over_sample, family = "binomial")
  #Forward Selection
  step(null, scope=list(lower=null, upper=full), direction="forward")
 
  lr_model_bal<- glm(C_MANIPULATOR ~ ACCR + DSRI + SGI + AQI + GMI + DEPI + LEVI, data= over_sample, family = "binomial")
  summary(lr_model_bal)
  
  lr_model_bal$null.deviance-lr_model_bal$deviance
  
  
  pred_test_bal = predict.glm(lr_model_bal, newdata = sample_test, type="response")
  pred_test_bal
  
  # Calculating the values for ROC curve
  pred_ROC_bal = prediction(pred_test_bal,sample_test$C_MANIPULATOR)
  pred_ROC_bal
  
  perf_bal = performance(pred_ROC_bal,"tpr","fpr")
  # Plotting the ROC curve
  plot(perf_bal, col = "black", lty = 3, lwd = 3)
  
  # Calculating AUC
  auc = performance(pred_ROC_bal,"auc")
  # Now converting S4 class to a vector
  auc = unlist(slot(auc, "y.values"))
  # Adding min and max ROC AUC to the center of the plot
  minauc = min(round(auc, digits = 2))
  maxauc = max(round(auc, digits = 2))
  minauct = paste(c("min(AUC) = "), minauc, sep = "")
  maxauct = paste(c("max(AUC) = "), maxauc, sep = "")
  legend(0.7, 0.5, c(minauct, maxauct, "\n"), border = "white", cex = 0.7, box.col = "white")
  abline(a= 0, b=1)
  
  opt.cut = function(perf_bal, pred_ROC_bal){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
        cutoff = p[[ind]])
    }, perf_bal@x.values, perf_bal@y.values, pred_ROC_bal@cutoffs)}
  
  print(opt.cut(perf_bal, pred_ROC_bal))
  
  #sensitivity 0.9000000
  #specificity 0.7719298
  #cutoff      0.2940217
  
  pred_test_bal = ifelse(pred>0.2940217,1,0)
  pred_test_bal
  
  
  levels(sample_test$C_MANIPULATOR)
  ptab<-table(pred_test_bal, sample_test$C_MANIPULATOR, dnn = c("Predicted","Actual"))
  ptab
  
  library(robustbase)
  library(caret)
  
  confusionMatrix(ptab,positive = "1")
  # Accuracy : 0.7761 
  # Sensitivity : 0.8000          
  # Specificity : 0.7719  
  
  # Conclusion
  
  #Before balancing
  # Sensitivity : 0.8000          
  # Specificity : 0.8070 
  # Accuracy    : 0.8059701
  
  #After balancing
  # Accuracy : 0.7761 
  # Sensitivity : 0.8000          
  # Specificity : 0.7719
  
  #*************** Decision Tress ********************
  install.packages("partykit")
  install.packages("rpart")
  install.packages("rpart.plot")
  
  library(rpart)
  library(rpart.plot)
  library(partykit)
  
  nrow(over_sample)
  table(over_sample$C_MANIPULATOR)
  
  dt_bal_full = rpart(C_MANIPULATOR~., data = over_sample,
                           control= rpart.control(cp= -1, minsplit = 0,
                                                  minbucket = 0 ), 
                           parms = list(split="gini"))
  
  
  
  printcp(dt_bal_full)
  opt <-which.min(dt_bal_full$cptable[ ,"xerror"])
  opt
  
  cp<-dt_bal_full$cptable[opt, "CP"]
  cp
  
  
  dt_bal_pruned <- prune(dt_bal_full, cp = 0.004)
  
  summary(dt_bal_pruned)
  dt_bal_pruned$variable.importance
  dt_bal_pruned$splits
  
  
  tab_train_dt<-table(predict(dt_bal_pruned, type="class"), over_sample$C_MANIPULATOR, dnn = c("Predicted","Actual"))
  
  confusionMatrix(tab_train_dt, positive = "1")
  
  tab_test_dt<-table(predict(dt_bal_pruned, type="class", newdata = sample_test), sample_test$C_MANIPULATOR, dnn = c("Predicted","Actual"))
  confusionMatrix(tab_test_dt, positive = "1")

  # Accuracy : 0.8209
  # Sensitivity : 0.30000          
  # Specificity : 0.91228 
  
  
  
  
# Question 8 
  
  ###############################complete data#############################
  library(readxl)
  Complete_Data <- read_excel("~/UIC/Courses/DataMining/Assignments/Case Study 1/Dataset/Complete_Data.xlsx")
  View(Complete_Data) 
  
  Complete_Data$`Company ID`<- NULL
  Complete_Data$Manipulater<- NULL
  
  colnames(Complete_Data)[9]<-"C_MANIPULATOR"
  
  summary(Complete_Data)
  str(Complete_Data)
  View(Complete_Data)
  
  Complete_final <- Complete_Data
  Complete_final$C_MANIPULATOR <- as.factor(Complete_final$C_MANIPULATOR)
  class(Complete_final$C_MANIPULATOR)
  
  table(Complete_final$C_MANIPULATOR)
  # 0      1 
  # 1200   39
  
  set.seed(1234)
  index = sample(2, nrow(Complete_final), replace = TRUE, prob = c(0.65,0.35))
  TrainData = Complete_final[index == 1, ]
  nrow(TrainData)
  table(TrainData$C_MANIPULATOR)
  TestData = Complete_final[index == 2,]
  nrow(TestData)
  table(TestData$C_MANIPULATOR)
  
  ##############oversampling########
  Complete_Data_bal <- ovun.sample(C_MANIPULATOR~.,
                                   data = TrainData,method = "over", 
                                   N=1566)$data
  table(Complete_Data_bal$C_MANIPULATOR)
  
  # Variable Selection
  null = glm(C_MANIPULATOR~1, data= Complete_Data_bal, family = "binomial") 
  full = glm(C_MANIPULATOR~., data= Complete_Data_bal, family = "binomial")
  #Forward Selection
  step(null, scope=list(lower=null, upper=full), direction="forward")
  
  lr_complete_bal<- glm(C_MANIPULATOR ~ DSRI + ACCR + SGI + AQI + LEVI+ 
                        GMI + DEPI, data= Complete_Data_bal, 
                        family = "binomial")
  summary(lr_complete_bal)
  #Null deviance: 2170.9  on 1565  degrees of freedom
  #Residual deviance: 1097.3  on 1558  degrees of freedom
  #AIC: 1113.3
  
  lr_complete_bal$null.deviance-lr_complete_bal$deviance
  #1073.635

  pred = predict.glm(lr_complete_bal, newdata = TestData, type="response")
  
  # Calculating the values for ROC curve
  pred_ROC = prediction(pred,TestData$C_MANIPULATOR)
  pred_ROC
  
  perf = performance(pred_ROC,"tpr","fpr")
  # Plotting the ROC curve
  plot(perf, col = "black", lty = 3, lwd = 3)
  
  # Calculating AUC
  auc = performance(pred_ROC,"auc")
  # Now converting S4 class to a vector
  auc = unlist(slot(auc, "y.values"))
  # Adding min and max ROC AUC to the center of the plot
  minauc = min(round(auc, digits = 2))
  maxauc = max(round(auc, digits = 2))
  minauct = paste(c("min(AUC) = "), minauc, sep = "")
  maxauct = paste(c("max(AUC) = "), maxauc, sep = "")
  legend(0.7, 0.5, c(minauct, maxauct, "\n"), border = "white", cex = 0.7, box.col = "white")
  abline(a= 0, b=1)

  opt.cut = function(perf, pred_ROC){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred_ROC@cutoffs)}
  
  opt.cut
  
  print(opt.cut(perf, pred_ROC))
  
  #sensitivity 1.0000000
  #specificity 0.7601918
  #cutoff      0.2711244
  
  pred$C_MANIPULATOR = ifelse(pred>0.2711244,1,0)
  pred
  
  
  #levels(TestData$C_MANIPULATOR)
  tab<-table(pred$C_MANIPULATOR, TestData$C_MANIPULATOR, dnn = c("Predicted","Actual"))
  tab
  
  library(robustbase)
  library(caret)
  
  confusionMatrix(tab,positive = "1")
  #Accuracy : 0.7658
  #Sensitivity : 1.00000         
  #Specificity : 0.76019 
  
  
  
  
  # Question 9
 
  #*************** Random Forest ********************
  install.packages("randomForest")
  library(randomForest)
  
  rf_sample_bal = randomForest(C_MANIPULATOR~., 
                           data = over_sample, ntree = 100, 
                           proximity = TRUE, replace= TRUE, 
                           importance = TRUE, 
                           mtry = sqrt(ncol(over_sample)))
  #DSRI + SGI + ACCR
  print(rf_sample_bal)
  # Confusion matrix:
  #   0     1    class.error
  # 0 119   5    0.04032258
  # 1   0   124  0.00000000
  plot(rf_sample_bal)
  legend("topright", legend = colnames(rf_sample_bal$err.rate), 
         cex = 0.5, lty = c(1,2,3,4), col = c(1,2,3,4), horiz = T)
  attributes(rf_sample_bal)
  importance(rf_sample_bal)
  varImpPlot(rf_sample_bal)
  rf_sample_bal$predicted
  rf_sample_bal$votes
  getTree(rf_sample_bal, k = 50, labelVar = TRUE)
  
  #Get accuracy of prediction on Test Data
  rfPred_test=predict(rf_sample_bal,newdata = sample_test)
  rfPred_test
  rf_pred_tab <- table(rfPred_test,sample_test$C_MANIPULATOR)
  confusionMatrix(rf_pred_tab, positive = "1")
  # Accuracy : 0.8657
  # Sensitivity : 0.30000          
  # Specificity : 0.96491
  
  #*************** Ada Boosting ********************
  install.packages("adabag")
  library("adabag")
  
  adaboost_sample <- boosting(C_MANIPULATOR ~ ACCR+ DSRI + LEVI + SGI, 
                            data = over_sample, 
                            mfinal = 10, control = rpart.control(maxdepth = 1))
  adaboost_sample
  
  # trees show the weaklearners used at each iteration
  adaboost_sample$trees
  adaboost_sample$trees[[1]]
  
  # weights returns the voting power
  adaboost_sample$weights
  
  # prob returns the confidence of predictions
  adaboost_sample$prob
  
  # class returns the predicted class
  adaboost_sample$class
  
  # votes indicates the weighted predicted class
  adaboost_sample$votes
  
  #importance returns important variables
  adaboost_sample$importance
  
  length(adaboost_sample$class)
  length(sample_train$C_MANIPULATOR)
  
  table(adaboost_sample$class, sample_train$C_MANIPULATOR, 
        dnn = c("Predicted Class", "Observed Class"))
  errorrate <- 1 - sum(adaboost_sample$class == sample_train$C_MANIPULATOR) /length(over_sample$C_MANIPULATOR)
  errorrate
  
  # To get predicted class on test data we can use predict function
  pred <- predict(adaboost_sample, newdata = TestData)
  # However if you use predict.boosting, you can change mfinal
  complete.predboosting <- predict.boosting(adaboost_sample, newdata = sample_test)
  
  # errorevol calculates errors at each iteration of adaboost
  err.train <- errorevol(adaboost_sample,iris[train, ])
  err.test <- errorevol(adaboost_sample,iris[-train, ])
  
  plot(err.test$error, type = "l", ylim = c(0,1), col = "red", lwd = 2)
  lines(err.train$error, cex = 0.5, col = "blue", lty = 2, lwd = 2)
  #####################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Cross - Validation
  
  #Randomly shuffle the data 
  
  sample_final<-sample_final[sample(nrow(sample_final)),] 
  View(sample_final)
  
  k=10;
  
  
  #Create k equally size folds 
  folds <- cut(seq(1,nrow(sample_final)),breaks=k,labels=FALSE) 
  folds
  
  #Perform k fold cross validation 
  for(i in 1:k)
  { 
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i, arr.ind=TRUE) 
    testData <- sample_final[testIndexes, ] 
    trainData <- sample_final[-testIndexes, ] 
    # Use the test and train data for whatever you want
     
    #*************** Logistic Regression ********************
    
     # lr_cv_model <- glm(C_MANIPULATOR~., data = trainData, family = "binomial")
     # lr_cv_model
     # # summary(lr_cv_model)
     # #print(lr_cv_model)
  }
  