library(plyr)
# library(randomForest)
library(caret)
# library(e1071)  # for SVM
library(gbm)  # for GBM
# library(kernlab)  # for SVM
library(miscTools)
library(mice)  # for imputing NAs
library(RANN)
library(lubridate)  # for working with dates
library(readr) # for reading data

start_modelling <- Sys.time()


##################  Reading and preprocessing data  ##########################

train <-read_csv("Kaggle/Walmart/train.csv")
train[,1]<-as.Date(train[,1])
train[,2]<-as.factor(train[,2])
train[,3]<-as.factor(train[,3])

# getting and preprocessing weather data
weather <- read.csv("Kaggle/Walmart/weather.csv", na.strings=c("M","-"))

weather$station_nbr <- as.factor(weather$station_nbr)
weather$date <- as.Date(weather$date)

# handling with T = TRACE value of snowfall
levels(weather$snowfall) <- c(levels(weather$snowfall), "0")
weather[grep(".*T.*",weather$snowfall),]$snowfall <- "0"
weather$snowfall <- droplevels(weather$snowfall)

weather$snowfall <- as.numeric(as.character(weather$snowfall))

# adding zero level to factor variable preciptotal
levels(weather$preciptotal) <- c(levels(weather$preciptotal), "0")
weather[grep(".*T.*",weather$preciptotal),]$preciptotal <- "0"
weather$preciptotal <- droplevels(weather$preciptotal)

weather$preciptotal <- as.numeric(as.character(weather$preciptotal))


# some event's codes are pasted together - need to split them by space
weather$codesum <- gsub("([A-Z+]{2})([A-Z+]{2})","\\1 \\2", weather$codesum)

# getting reference list of events
events <- read.table("Kaggle/Walmart/events.txt",sep="%")

# to get code, we need to cut everything after first space
events$code <- sub("\\s.*$","", events$V1)

# loading test dataset
test <-read_csv("Kaggle/Walmart/test.csv")
test[,1]<-as.Date(test[,1])
test[,2]<-as.factor(test[,2])
test[,3]<-as.factor(test[,3])

# checking time line continuouty
max(test$date)-min(test$date)-length(unique(test$date))

# loading key to link weather stations and stores
key <- read_csv("Kaggle/Walmart/key.csv")

# add stations to test set
test <- join(test, key, by="store_nbr")
test$station_nbr <- as.factor(test$station_nbr)

# add stations to train set
train <- join(train, key, by="store_nbr")
train$station_nbr <- as.factor(train$station_nbr)

# checking that each item has equal number of dates in test data
test_dates <- sapply(unique(test$item_nbr), function (x) {length(unique(test[test$item_nbr==x,]$date))})

# same for -- equal number of stores
test_stores <- sapply(unique(test$item_nbr), function (x) {length(unique(test[test$item_nbr==x,]$store_nbr))})


# cut edge spaces in codesum column
weather$codesum <- gsub("^\\s+|\\s+$", "", weather$codesum)

# add column for each event type to weather dataset
# weather[,events$code] <- NA

for (i in 1:nrow(weather))
{
     ev_list <- unlist(strsplit(weather$codesum[i], split=" ", fixed=TRUE))
     for (ev in ev_list)
          weather[i,ev] = 1  
}
rm(list=c("i", "ev", "ev_list"))

evcols <- events$code[which(events$code %in% names(weather))]

# put 0 instead of NA
weather[,evcols] <- ifelse(is.na(weather[,evcols]),0,1)

# drop the common events column codesum
weather <- subset(weather, select=-codesum)

# rename FG+ column to FGP
names(weather)[names(weather)=="FG+"]<-"FGP"
evcols[evcols=="FG+"] <- "FGP"

# set wind direction (1-36) to factor
weather$resultdir <- as.factor(weather$resultdir)

# preprocess sunrise and sunset columns
weather$sunrise <- strptime(paste0(weather$date," ",
        weather$sunrise %/% 100,":",
        weather$sunrise %% 100), "%Y-%m-%d %H:%M", tz="UTC")

weather$sunset <- strptime(paste0(weather$date," ",
        weather$sunset %/% 100,":",
        weather$sunset %% 100), "%Y-%m-%d %H:%M", tz="UTC")


# we don't need absolute time. Rather number of minutes from the midnight
weather$sunrise <- (weather$sunrise %/% 100)*60 + weather$sunrise %% 100
weather$sunset <- (weather$sunset %/% 100)*60 + weather$sunset %% 100


# check the days when more than 2 inches of snow or 1 inch of rain
ev_days <- subset(weather[,evcols],
                 (weather$snowfall>=2 | weather$preciptotal>=1),na.rm=TRUE)

# check events on these days
sapply(weather[,evcols], sum)
sapply(ev_days[,evcols], sum)


# we will treat dates as count of days starting from some initial date

start <- as.Date("2009-04-06")

weather$numday <- as.integer(weather$date - start)

test$numday <- as.integer(test$date - start)

train$numday <- as.integer(train$date - start)


# impute NA's in weather dataset with mice package
tmp <- Sys.time()

weather[,c(-1,-3)] <- complete(mice(weather[,c(-1,-3)]))

print(Sys.time()-tmp)

# to look at preprocessed data in excel-like editor
write.csv(weather, "Kaggle/Walmart/weather_ready.csv", row.names=FALSE)

# # to skip preprocessing of weather data
# weather <-read_csv("Kaggle/Walmart/weather_ready.csv")
# 
# weather$station_nbr <- as.factor(weather$station_nbr)
# weather$date <- as.Date(weather$date)



#####################  Diagnostics  ############################

# aggregating train data by number of units sold
train1 <- aggregate(units ~ store_nbr + item_nbr, train, sum)
head(train1[order(train1$units, decreasing=T),], 10)

# another way to get the same result, slightly faster
# train1 <- ddply(.data=train, .(store_nbr, item_nbr), summarise, units=sum(units))

# we can get a "square" view of the table
xtabs(units ~ ., data=train1)

# or again the same with plyr and reshape2 package
# dcast(train1, store_nbr ~ item_nbr, value.var = "units")


# how many pairs "item-store" have zero sold units
sum(aggregate(train1$x, list(item=train1$item, store=train1$store), function (x) {x==0})$x)

# pairs "item-store" with zero sales
train1_zero <- train1[train1$x==0,]

# how many stores sell each product
train11 <- aggregate(train1$x, list(item=train1$item), function (x) {sum(x>0)})
table(train11$x)

# how many units have been sold by each item
aggregate(train1$x, list(item=train1$item), sum)


# number of entries (dates) for each pair "item-store" in the TEST set
test1 <- aggregate(date ~ store_nbr + item_nbr, test,
                   function (x) {
                        length(unique(x))
                   })

# rename aggregated columns
names(test1)[3]<-"test_entries"
names(train1)[3]<-"train_sold"

# !NB
# when constructing dataset for modelling (on each iteration of the loop)
# we take next item
# and take all stores with non-zero sales from train dataset
# AND plus all stores from test set
# then subset on all these pairs (item-store) the train set


# join aggregated test and train data by pairs "item-store"
test_train_izzero <- join(test1,train1, by=c("store_nbr","item_nbr"))
summary(test_train_izzero)

# otherwise we can exchange train and test while joining:
train_test_izzero <- join(train1,test1, by=c("store_nbr","item_nbr"))
summary(train_test_izzero)

# note that some pairs absent in test data
# so these are NA in train_test_izzero dataset

# now we can see cases (pairs) in test data which have zero sales in train data
# one possible approach is to put zeros to all such entries in test data


######################  Predicting  #############################

# get array of pairs "store_item" (which exist in test set) with zero sales in train set
pairs_zero_test <- test_train_izzero[test_train_izzero$train_sold==0,]
pairs_zero_test <- paste0(pairs_zero_test$store, "_", pairs_zero_test$item)

# create units column in test dataset
test$units <- NA

# move units column to fourth position
# using my tiny lib for reordering columns in dataset
source("_library/reorder_columns.R")
test <- reorder_columns(test,"units",4)

# !NB give zero prediction for pairs "store_item" (which exist in test set)
# with zero sales in train set
test[paste0(test$store_nbr,"_",test$item_nbr) %in% pairs_zero_test,]$units <- 0


# a little more diagnostics
# find pair with minimum non-zero sales
train_nonzero <- train1[train1$train_sold!=0,]
train_nonzero[train_nonzero$train_sold==min(train_nonzero$train_sold),]


# !!! now we start processing of non-zero pairs
pairs_non_zero_test <- test_train_izzero[test_train_izzero$train_sold!=0,]

pairs_non_zero_test$store_nbr <- as.numeric(as.character(pairs_non_zero_test$store_nbr))
pairs_non_zero_test$item_nbr <- as.numeric(as.character(pairs_non_zero_test$item_nbr))


# compute number of stores with non-zero sales for each item
items_vs_stores_test <- aggregate(store_nbr ~ item_nbr, pairs_non_zero_test,
                            function (x) { length(unique(x)) })
nrow(items_vs_stores_test[items_vs_stores_test$store_nbr>1,])



# !!! analog but starting with train
pairs_non_zero_train <- train_test_izzero[train_test_izzero$train_sold!=0,]

pairs_non_zero_train$store_nbr <- as.numeric(as.character(pairs_non_zero_train$store_nbr))
pairs_non_zero_train$item_nbr <- as.numeric(as.character(pairs_non_zero_train$item_nbr))

# compute number of stores with non-zero sales for each item
items_vs_stores_train <- aggregate(store_nbr ~ item_nbr, pairs_non_zero_train,
                                  function (x) { length(unique(x)) })
nrow(items_vs_stores_train[items_vs_stores_train$store_nbr>1,])


# making a tepmorary container for results
test_temp<-data.frame(numday=integer(0), units=numeric(0),
                       store_nbr=numeric(0), item_nbr=numeric(0))

# assign distributions for modelling
# due to earlier analisys of CV error
distr <- character(length(unique(pairs_non_zero_train$item_nbr)))
distr <- rep("laplace", length(distr))
distr[c(2,15,46,64,77,78,93)] <- "poisson"


#####################  Modelling  ############################

# Starting main loop of predicting over all items except total zeros (3 items)

for(i in unique(pairs_non_zero_test$item_nbr))  
{

print(paste("Starting with item:", i))
     
# getting set of stores actual for current item and subsetting train data
store_set_train <- unique(pairs_non_zero_train[pairs_non_zero_train$item_nbr==i,]$store_nbr)

train2 <- train[train$store_nbr %in% store_set_train & train$item_nbr==i,]

# same for test data
store_set_test <- unique(pairs_non_zero_test[pairs_non_zero_test$item_nbr==i,]$store_nbr)

test2 <- test[test$store_nbr %in% store_set_test & test$item_nbr==i,]

# adding weather data
train22 <- join(train2,weather, by=c("date","station_nbr","numday"))
train22 <- subset(train22, select=-c(date,item_nbr,station_nbr))

test22 <- join(test2,weather, by=c("date","station_nbr","numday"))
test22 <- subset(test22, select=-c(date,item_nbr,station_nbr))


# drop almost constant variables except units column
nzv <- nearZeroVar(train22[,c(-1,-2)])
train22 <- train22[,-2-nzv]
test22 <- test22[,-2-nzv]

levels(test22$store_nbr) <- c(levels(test22$store_nbr),35)


set.seed(777)


modelFit1_gbm <- NA


myControl <- trainControl(method='cv', number=5,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE)

my.grid <- expand.grid(.interaction.depth = c(3,5,7),
                       .n.trees = c(4001),
                       .shrinkage = c(0.01, 0.003, 0.001),
                       .n.minobsinnode = c(7, 10, 12))

modelFit1_gbm <- train(units ~ .,
                       data=train22,
                       method='gbm',
                       distribution=distr[i],
                       preProcess=c("center", "scale"),
                       trControl=myControl,
                       tuneGrid = my.grid) 

print(paste("Best tune: ", modelFit1_gbm$bestTune))

print(modelFit1_gbm$results)

# saving model
save(modelFit1_gbm, file=paste0("Kaggle/Walmart/models/model_gbm_item_",i,".rda"))

print("Model is saved to file")

# to skip actual modelling
# load(paste0("Kaggle/Walmart/models/model_gbm_item_",i,".rda"))


# calculate prediction on train and test subset

res <- predict(modelFit1_gbm, train22)

res_test <- predict(modelFit1_gbm, test22)

res_test[res_test<0] <- 0

test22$units <- res_test



#######################  Checking and plotting  ###########################

common_results <- train22[,c("units","numday")]
common_results$from <- "train"

test22$from <- "test"
common_results <- rbind(common_results, test22[,c("units","numday","from")])
common_results$from <- as.factor(common_results$from)

common_results <- common_results[order(common_results$numday),]


rmse <- min(modelFit1_gbm$results$RMSE)

# Relative influence of variables, for GBM
# as.data.frame(relative.influence(modelFit1_gbm,best.iter))
# or
summary(modelFit1_gbm)


plot(common_results$numday, common_results$units, col=common_results$from, pch=20, xlab="")

title(main=paste("Item:",i, ", train observs:", nrow(train22),
                 ", test observs:", nrow(test22)),
      sub=paste("Best tune:", "RMSE: ", rmse ,"\n",
                "n.trees:", modelFit1_gbm$bestTune$n.trees,
                ", shrinkage: ", modelFit1_gbm$bestTune$shrinkage,
                ", interaction.depth: ", modelFit1_gbm$bestTune$interaction.depth,
                ", n.minobsinnode: ", modelFit1_gbm$bestTune$n.minobsinnode))


test_temp<-rbind(test_temp,cbind(test22[,c("store_nbr","numday","units")], item_nbr=i))


# saving preliminary results to avoid data loss
if(i %% 10 == 0)
{
   write.csv(test_temp, paste0("Kaggle/Walmart/test_temp_",i,".csv"), row.names=FALSE)
   write.csv(params, paste0("Kaggle/Walmart/params.csv"), row.names=FALSE)
}



# end of main loop
}



###################  Finish  #########################

# put predictions into test dataset

test <- merge(test,test_temp, by=c("numday","store_nbr","item_nbr"), all.x=TRUE)
test[is.na(test$units.x),]$units.x <- test[is.na(test$units.x),]$units.y
test <- test[,-7]
names(test)[5] <- "units"

# check time spent
finish <- Sys.time()-start_modelling
print(finish)

# making version variable via lubridate package
version <- paste0(today(),"_",hour(now()),".",minute(now()))

# write results into file
write.csv(test, paste0("Kaggle/Walmart/test_results_",version,".csv"), row.names=FALSE)


# prepare submission file

submission <- test[,c("id","units")]

write.csv(submission, "Kaggle/Walmart/submission.csv", row.names=FALSE)



