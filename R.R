library(data.table)


###### СОЗДАНИЕ ВЫБОРКИ
######
setwd("C:/Users/user/Desktop/саша/kagle")
getwd()
test<- fread("test.csv")
train<- fread("train.csv")
test$y <- NA
data <- rbind(train,test)
data$X0 <- as.factor(data$X0)
data$X1 <- as.factor(data$X1)
data$X2 <- as.factor(data$X2)
data$X3 <- as.factor(data$X3)
data$X4 <- as.factor(data$X4)
data$X5 <- as.factor(data$X5)
data$X6<- as.factor(data$X6)
data$X8 <- as.factor(data$X8)
# str(data)



# определяет какие столбцы факторы!
# factor<- sapply(data,function(x){is.factor(x)})
# names(which(factor))
# a<- as.numeric(which(factor))

nom<- ncol(data)
for(i in levels(data$X0)){
  data<- cbind(data,data$X0==i)
  names(data)[ncol(data)] <- paste0("X0",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X1)){
  data<- cbind(data,data$X1==i)
  names(data)[ncol(data)] <- paste0("X1",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X2)){
  data<- cbind(data,data$X2==i)
  names(data)[ncol(data)] <- paste0("X2",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X3)){
  data<- cbind(data,data$X3==i)
  names(data)[ncol(data)] <- paste0("X3",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X4)){
  data<- cbind(data,data$X4==i)
  names(data)[ncol(data)] <- paste0("X4",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X5)){
  data<- cbind(data,data$X5==i)
  names(data)[ncol(data)] <- paste0("X5",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X6)){
  data<- cbind(data,data$X6==i)
  names(data)[ncol(data)] <- paste0("X6",i)
}
(ncol(data)-nom)
nom<- ncol(data)
# X7 число
for(i in levels(data$X8)){
  data<- cbind(data,data$X8==i)
  names(data)[ncol(data)] <- paste0("X8",i)
}
(ncol(data)-nom)

data$X0 <- NULL
data$X1 <- NULL
data$X2 <- NULL
data$X3 <- NULL
data$X4 <- NULL
data$X5 <- NULL
data$X6 <- NULL
data$X8 <- NULL


data<- apply(data,2,function(x){ as.numeric(x)})
data <- data.table(data)
# fwrite(data,"data.csv")

data$chetnoe <- data$ID%%2




data$ID <- NULL
a<- apply(data[1:4209,],2,function(x){
  sum(x,na.rm = T) == (4209 | 0)
  
})

data <- as.data.frame(data)
data<- data[,-which(a)]
data <- data.table(data)
y<- data$y
data$y <- NULL
is_multicol <- function(data){
  
    d<- data
    d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  print(length(index))
  if (length(index) == 0){      
    return(data)
    print('There is no collinearity in the data')
    
  } else {      
   
    data<- subset(data,select = -index[1,2])
    is_multicol(data)
    
  }      

  # data$y
  
  }



data<- is_multicol(data)
data$y <- y

# PCA

get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

data<- cbind(data$y,get_pca2(subset(data,select = -y)))

names(data)[1] <- "y"

# K  - means 
# 14 Кластеров
# Запсук расчет для определения

# for(i in 1:100){
#   irisCluster <- kmeans(data[,-1], i, nstart = 100)
#   kluster[i] <- sum(irisCluster$withinss)
# }
# 
# plot(kluster)
# plot(kluster/c(kluster[2:100],kluster[100]))
# save(irisCluster,file="irisCluster.rda")



# Построе кластера

k_cluster <- kmeans(data[,-1], 14, nstart = 100)
data$cluster <-  k_cluster$cluster
plot(y=data$y,x=data$cluster)
data <- data.table(data)
# nrow(data[y<=(IQR(data$y,na.rm = T)*1.5+quantile(x = data$y,probs = 0.75,na.rm = T))])

# Добавление фич PCA в полиноме
transform_x = function(data)
{
  do_transform = function(x, lambd) {
    if (lambd > 0) x ^ lambd else if (lambd < 0) -(x ^ lambd) else log(x) }
  
  x_data = data[,2]
  y_data = data[,1]
  lambdas = seq(-5, 5, 0.1)
  corrs = sapply(lambdas, 
                 function(lambd) cor(do_transform(x_data, lambd), y_data))
  lambda = lambdas[which.max(abs(corrs))]
  return(lambda)
}


nc<- ncol(data)
for(i in min(grep("PC1",names(data))):(nc-1)) {print(i)
  data <- cbind(data,
                subset(data,select = i)^transform_x(subset(data[1:4209,],select = c(1,i)))
  )
  names(data)[ncol(data)] <- paste0(names(data)[i],"_",transform_x(subset(data[1:4209,],select = c(1,i))))
}


# fwrite(data,"data.csv")

######

#Разделение выборки
#####

# train/test
# test<- data[which(is.na(data$y)),]
# train<- data[!which(is.na(data$y)),]
train <- data[1:4209]
test <- data[4210:8418]

# cross validation

library(xgboost)
train_2 <- train
y<- train_2$y
train_2$y <- NULL
y<- unlist(y)
dtrain <- xgb.DMatrix(as.matrix(train_2),label=y)
cv <- xgb.cv(data = dtrain, nrounds = 10, nthread = 2, nfold = 5, metrics = list("rmse"),
             max_depth = 3, eta = 1)
print(cv)
cv$folds

# BOOSTRAP
set.seed(777)
a<- sample(4209,4209,replace = T)

boot<- train[a,]

summary(boot$y)
boot_test <- train[-unique(a),]
summary(boot_test$y)

#####

######  Построение модели
######


smart_model <- function(data)
{
  a<-   sapply(names(data[-1]), function(name) {
    m = lm(as.formula(paste(name, "~ .")), data[-1])
    r2 = summary(m)$r.squared
    1 / (1 - r2)
    
  })
  print(length(a))
  if (a[which.max(a)]>10) (data <- smart_model(data[,-(which.max(a)+1)])) else ( lm(as.formula(paste(names(data[1]), "~ .")), data) )
}

model_smart<- smart_model(train_m)
save(model_smart, file = "model_smart.rda")
load("model_smart.rda")



trainLabel <-  boot$y
testlabel <- boot_test$y
boot$y <-  NULL
boot_test$y <-  NULL
# rm(data)


dtrain = xgb.DMatrix(data=data.matrix(boot),label=trainLabel,missing = NaN)
dtest = xgb.DMatrix(data=data.matrix(boot_test),label=testlabel,missing = NaN)

watchlist<-list(test=dtest,train=dtrain)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <-sum((preds-mean(labels))^2)/sum((labels-mean(labels))^2)
  return(list(metric = "R2", value = err))
}


param <- list(  objective           = "reg:linear",
                booster = "gbtree",
                eval_metric = evalerror,
                eta                 = 0.2, # 0.06, #0.01,
                max_depth           = 6, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.77 # 0.7
                # num_parallel_tree   = 2
                # alpha = 0.0001,
                # lambda = 1
                # lambda_bias = 1
)
set.seed(777)
clf2 <- xgb.train(   params              = param,
                     data                = dtrain,
                     nrounds             = 500,
                     verbose             = 1,
                     # early.stop.round    = 50,
                     #feval = normalizedGini,
                     watchlist           = watchlist,
                     maximize            = T
                     
)



which(clf2$evaluation_log$test_R2==max(subset(clf2$evaluation_log,test_R2<1,select = test_R2)$test_R2))
which(clf2$evaluation_log$test_R2==max(subset(clf2$evaluation_log,test_R2<0.79,select = test_R2)$test_R2))



train$pred_y<- predict(clf2,data.matrix(train[,-1]),ntreelimit = which(clf2$evaluation_log$test_R2==max(subset(clf2$evaluation_log,test_R2<0.79,select = test_R2)$test_R2)))

############
  
# предсказаине
########### 
test_zaliv<- fread("test.csv")

# pred_xgboots
test_zaliv$y<- predict(clf2,data.matrix(test[,-1]),ntreelimit = which(clf2$evaluation_log$test_R2==max(subset(clf2$evaluation_log,test_R2<0.79,select = test_R2)$test_R2)))
test_zaliv<- test_zaliv[,.(ID,y)]

test$y<- predict(lm_boot,test_m[,-1])


fwrite(test_zaliv,"kaggle_9.csv")



