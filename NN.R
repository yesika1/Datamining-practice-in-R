# Neural Networks NN
# =================== 

# Needs to normalize the data
# allow the pc to learn in a similar manner to humans - reinforcement.
# used for: pattern recognitiion, Time series predictions, signal processing, anomaly detection, control.
# NN are used for problems that are easier for humans recognize or describe imagens
# its like a blackbox and not tell the importance of every variable
# for categorical var: You don't normalize, you just convert it to either dummy variables or keep them as factors

# Perceptron: Easiest NN
# consists of one or more inputs, a processor and a single output
# follows the feed-forward model: inputs (0,1) are sent into neuron (processor) > processed > result an output
# steps:
# 1.Receive inputs: input0: x1=12, input1: x2=4
# 2. Weight inputs: multiply by a value [-1,1], weight0:0.5, weight1:-1
#     input0*weight0: x1=12*0.5=6, input1*weight1: x2=4*-1=-4
# 3. Sum inputs 
# 4. Generate output: passing sum in a activation function (wether to fire or not)
#     Activation function: if sum is positive number, output:1 & 
#                           if sum is negative, output: -1
# Bias: add it to avoid cases where sum=0. bias=1


# Train the perceptron:
# 1. provide perceptron wiht inputs with known values
# 2. Aks perceptron to guess an answer
# 3. Compute error. 
# 4. Adjust all weights 
# 5. Return to step 1 and repeat

# create a Neural Network:
# link many perceptrons in layers
# input layer, output layer, hidden layers(we dont see values there)

# Deep learning:
# Neural Network with many hidden layers (~150)

library(MASS)
head(Boston) #1976 data of properties
str(Boston) #506 obs. of  14 variables:
sum(is.na(Boston)) #0
data <- Boston

# Normalize data
# using scale
standardized_Boston <- scale(data) 
var(standardized_Boston[,1]) # 1

# Customize normalization: scale data with max and min value 
#---------
# substracting from the center value and then dividing by the scale value

# max value of every feature
maxs <- apply(data, MARGIN =2, max) #margin 2: apply fucntion over the columns
# min value of every feature
mins <- apply(data, MARGIN =2, min) #margin 2: apply fucntion over the columns
# Normalize data
normalized_Boston <- scale(data, center=mins, scale = maxs-mins) #matrix
normalized_Boston <- as.data.frame(normalized_Boston)

#Model
library(caTools)
split <- sample.split(normalized_Boston$medv, SplitRatio = 0.7)
train <- subset(normalized_Boston, split=T)
test <- subset(normalized_Boston, split=F)

library(neuralnet)
# to dont write feauture by feature: bc cannot use '.'
n <- names(train)
f <- as.formula(paste('medv ~', paste(n[!n %in% 'medv'], collapse = '+'))) #paste: concatenate strings

# regression model
boston_nn <- neuralnet(f, data=train, hidden= c(5,3), linear.output = T) 
# linear.output = T bc regression problem, if classification problem, then linear.output = F

#plotting
plot(boston_nn) # blue lines are the bias(intersept in the linear model)

#predictions
predicted_nn <-compute(boston_nn, test[1:13], )
str(predicted_nn) # list of neurons and results

# get the predicted values inverting the normalization
true_predictions <- predicted_nn$net.result * (max(data$medv)-min(data$medv))+min(data$medv)

# converting test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

#check errors
MSE.nn <- sum((test.r - true_predictions)^2)/nrow(test)

#plotiting erros: true predictions by test values
library(ggplot2)
error.df <- data.frame(test.r,true_predictions)

ggplot(error.df,aes(x=test.r,y=true_predictions)) + 
  geom_point() + 
  stat_smooth()

# not to bad, but its not a perfect line.



# Bank Authentication
df <- read.csv("~/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bank_note_data.csv")
View(df)
head(df)
str(df)
summary(df)

#EDA
# We'll skip this step because the data isn't easily interpretable since its just statistical info on images.
# Check the structure of the train data and note that Class is still an int data type. 
# We won't convert it to a factor for now because the neural net requires all numeric information.

# model
# We dont scale the data bc the actual columns were at the same order of magnitud
# Entropy was a little smaller around [-5,5]
# We normalize when there are large ranges of min and max values.
library(caTools)
set.seexd(101)
split <- sample.split(df$Class, SplitRatio = 0.70)
train <- subset(df, split == TRUE)
test  <- subset(df, split == FALSE)

library(neuralnet)
n <- names(train)
f <- as.formula(paste('Class ~', paste(n[!n %in% 'Class'], collapse = '+'))) #paste: concatenate strings

#classification model
model_nn <- neuralnet(f, data=train,  hidden= c(5,10), linear.output = F)

#plotting
plot(model_nn) # blue lines are the bias(intersept in the linear model)

#predictions
predictions_nn <- compute(model_nn, test[1:4])
str(predictions_nn) # list of neurons and results

# get the predicted values
true_predictions <- predictions_nn$net.result
head(true_predictions) # gives probabilities

#Applying round function to get 0s and 1s as your predicted classes.
predictions <- sapply(predictions_nn$net.result,round) # classify 0 & 1

#confusion matrix: check errors
library(caret)
table(predictions,test$Class)
confusionMatrix(data=predictions, test$Class) 


# Comparing with Random Forest

library(randomForest)
df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

#model
model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.pred <- predict(model,test)

#confusion matrix: check errors
table(rf.pred,test$Class)
confusionMatrix(data=rf.pred, test$Class) 


