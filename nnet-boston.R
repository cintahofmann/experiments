library(MASS)
library(neuralnet)
set.seed(50)
data <- Boston

apply(data,2,function(x) sum(is.na(x))) #check for missing values

#split data into train and test sets
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#fit linear regression model
lm.fit <- glm(medv~., data=train)
summary(lm.fit)

#test model on test set
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

#scale and split data
maxs <- apply(data,2,max)
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center=mins, scale=maxs
                              - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#13:5:3:1 configuration
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"],
                                      collapse = " + ")))
nn <- neuralnet(f, data=train_, hidden=c(5,3),linear.output=T)

#y~ is not allowed, needs to be passed as an argument in the fitting function

#plot
plot(nn)
nn$net.result

#predicting medv using the neural network
pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.rsult*(max(data$medv)-min(data$medv)+
                           min(data$medv))
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#copare two MSEs
print(paste(MSE.lm,MSE.nn))
