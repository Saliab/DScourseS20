set.seed(100)
library(mlr)
library(tidyverse)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
income <- read.table("http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#Question5
#Define the task
trainTask <- makeClassifTask(id= "taskname",data = income.train,target = "high.earner")
testTask <- makeClassifTask(id="taskname", data = income.test, target = "high.earner")
# Set resampling strategy (3-fold CV)
resampleStart <- makeResampleDesc(method="CV",iters = 3)

# The tuning strategy
tuneMethod <- makeTuneControlRandom(maxit = 10L)

#LEARNERS
#Tree 
tree <- makeLearner("classif.rpart", predict.type = "response")

#logistic
logistic <- makeLearner("classif.glmnet",predict.type = "response")

#Neural Network 
NN <- makeLearner("classif.nnet", predict.type = "response")

#Naive Bayes
NB <- makeLearner("classif.naiveBayes", predict.type = "response")

#KNN
KNN <- makeLearner("classif.kknn", predict.type = "response")

#SVM
SVM <- makeLearner("classif.svm", predict.type = "response")

#Question 6
#HYPERPARAMETERS 

#Tree model
hp_tree <- makeParamSet(
  makeIntegerParam("minsplit",lower=10,upper=50),
  makeIntegerParam("minbucket",lower=5,upper=50),
  makeNumericParam("cp", lower=0.001,upper=0.2)
)

#Logit model
hp_logit <- makeParamSet(
  makeNumericParam("lambda",lower=0,upper=3),
  makeNumericParam("alpha",lower=0,upper=1)
)

#Neural Network model
hp_nn <- makeParamSet(
  makeIntegerParam("size" ,lower=1,upper=10),
  makeNumericParam("decay",lower=0.1,upper=0.5),
  makeIntegerParam("maxit",lower=1000,upper=1000)
)

#Naive Bayes model


#KNN
hp_knn <- makeParamSet(
  makeIntegerParam("k",lower=1,upper=30)
)

#SVM
hp_svm <- makeParamSet(
  makeDiscreteParam("kernel", values = "radial"),
  makeDiscreteParam("cost", values = 2^c(-2,-1,0, 1,2,10)), #cost parameters
  makeDiscreteParam("gamma", values = 2^c(-2,-1,0, 1,2,10)) #RBF Kernel Parameter
)


#Question 7
#Tree
Tune_Tree <- tuneParams(learner = tree, 
                        task = trainTask, 
                        resampling =resampleStart, 
                        measures=list(f1, gmean),
                        par.set = hp_tree, 
                        control = tuneMethod, 
                        show.info = TRUE)
#Logit
Tune_logit <- tuneParams(learner = logistic,
                         task = trainTask,
                         resampling = resampleStart,
                         measures=list(f1, gmean),      
                         par.set = hp_logit,
                         control = tuneMethod,
                         show.info = TRUE)



#Neural Network
Tune_NN <- tuneParams(learner = NN,
                      task = trainTask,
                      resampling = resampleStart ,
                      measures=list(f1, gmean),      
                      par.set = hp_nn,
                      control = tuneMethod,
                      show.info = TRUE)


#KNN
Tune_KNN <- tuneParams(learner = KNN,
                       task = trainTask,
                       resampling = resampleStart ,
                       measures=list(f1, gmean),      
                       par.set = hp_knn,
                       control = tuneMethod,
                       show.info = TRUE)



#SVM
Tune_SVM <- tuneParams(learner = SVM,
                       task = trainTask,
                       resampling =resampleStart,
                       measures=list(f1, gmean),      
                       par.set = hp_svm,
                       control = tuneMethod,
                       show.info = TRUE)


#Question 8
# Apply the optimal algorithm parameters to the model
pred_tree <- setHyperPars(learner=tree, par.vals = Tune_Tree$x)
pred_logit <- setHyperPars(learner=logistic, par.vals = Tune_logit$x)
pred_nn <- setHyperPars(learner=NN, par.vals = Tune_NN$x)
pred_knn <- setHyperPars(learner=KNN, par.vals = Tune_KNN$x)
pred_svm <- setHyperPars(learner=SVM, par.vals = Tune_SVM$x)



# Verify performance on cross validated sample sets
resample_tree <- resample(learner=pred_tree,task=trainTask,resampling=resampleSart,measures=list(f1, gmean))
resample_logit <- resample(learner=pred_logit,task=trainTask,resampling=resampleStart,measures=list(f1, gmean))
resample_nn <- resample(learner=pred_nn,task=trainTask,resampling=resampleStart,measures=list(f1, gmean))
resample_knn <- resample(learner=pred_knn,task=trainTask,resampling=resampleStart,measures=list(f1, gmean))
resample_svm <- resample(learner=pred_svm,task=trainTask,resampling=resampleStart,measures=list(f1, gmean))


# Train the final model
finalModel_tree <- train(learner = pred_tree, task = trainTask)
finalModel_logit <- train(learner = pred_logit, task = trainTask)
finalModel_nn <- train(learner = pred_nn, task = trainTask)
finalModel_knn <- train(learner = pred_knn, task = trainTask)
finalModel_svm <- train(learner = pred_svm, task = trainTask)



# Predict in test set!
prediction_tree <- predict(finalModel_tree, newdata = income.test)
prediction_logit <- predict(finalModel_logit, newdata = income.test)
prediction_nn <- predict(finalModel_nn, newdata = income.test)
prediction_knn <- predict(finalModel_knn, newdata = income.test)
prediction_svm <- predict(finalModel_svm, newdata = income.test) 




print(Tune_Tree)
print(Tune_logit)
print(Tune_NN)
print(Tune_KNN)
print(Tune_SVM)


print(performance(prediction_tree, measures = list(f1, gmean)))
print(performance(prediction_logit, measures = list(f1, gmean)))
print(performance(prediction_nn, measures = list(f1, gmean)))
print(performance(prediction_knn, measures = list(f1, gmean)))
print(performance(prediction_svm, measures = list(f1, gmean)))













