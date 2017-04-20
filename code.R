setwd('/home/yasar/kaggle/two_sigma_renthop/')
train <- read.csv('train.csv')
train <- train[complete.cases(train[, 'price']),]
train$street_address <- as.character(train$street_address)
train$street_address <- tolower(train$street_address)
train$street_address <- gsub('[^a-z0-9 ]','',train$street_address)
train$street_address <- gsub('[0-9]st','',train$street_address)
train$street_address <- gsub('[0-9]th','',train$street_address)
train$street_address <- gsub('3rd','3',train$street_address)
train$street_address <- gsub('2nd','2',train$street_address)
train$street_address <- gsub('street','st',train$street_address)
train$street_address <- gsub(' str',' st',train$street_address)
train$street_address <- gsub(' avenue',' ave',train$street_address)
train$street_address <- gsub(' w ',' west ',train$street_address)
train$street_address <- gsub(' e ',' east ',train$street_address)
train$street_address <- gsub(' n ',' north ',train$street_address)
train$street_address <- gsub(' s ',' south ',train$street_address)
train$street_address <- gsub('^[0-9]* ','',train$street_address)
train$street_address <- gsub('^[ ]*','',train$street_address)
train$direction <- 'unknown'
train$direction[grep('east ',train$street_address)]='east'
train$direction[grep('west ',train$street_address)]='west'
train$direction[grep('north ',train$street_address)]='north'
train$direction[grep('south ',train$street_address)]='south'
train$street_address <- gsub('east ','',train$street_address)
train$street_address <- gsub('west ','',train$street_address)
train$street_address <- gsub('north ','',train$street_address)
train$street_address <- gsub('south ','',train$street_address)

train$X <- NULL
train$created <- NULL
train$display_address <- NULL
train$photos <- NULL
train$description <- NULL

train$bathrooms <- as.character(train$bathrooms)
train$bedrooms <- as.character(train$bedrooms)
train$bathrooms <- as.numeric(train$bathrooms)
train$bedrooms <- as.numeric(train$bedrooms)
train$building_id <- NULL
train$interest_level <- as.factor(as.character(train$interest_level))

train$latitude <- as.numeric(as.character(train$latitude))
train$listing_id <- NULL
train$manager_id <- NULL
train$street_address <- as.factor(train$street_address)

train$latitude_group <- as.factor(as.integer(train$latitude*10))
train$longitude_group <- as.factor(as.integer(train$longitude*10))
train$features <- as.character(train$features)
train$features<- gsub('[^a-zA-Z,]','',train$features)
train$features<- gsub('^u','',train$features)
train$features<- gsub(',u',',',train$features)
train$features <- tolower(train$features)
train$features1 <- sapply(train$features,FUN = function(x) as.character(unlist(strsplit(x,','))))

all_features <- unique(unlist(train$features1))

t <- table(all_features)
for (feature in all_features){
  t[feature] <- length(grep(feature,train$features))
}

feature_counts <- as.data.frame(t)
print (t[t>200])
train$ac <- 0
train$ac[grep('ac', train$features)] <- 1

train$cat <- 0
train$cat[grep('cat', train$features)] <- 1

train$dog <- 0
train$dog[grep('dog', train$features)] <- 1

train$balcony <- 0
train$balcony[grep('balcony', train$features)] <- 1

train$common <- 0
train$common[grep('common', train$features)] <- 1

train$deck <- 0
train$deck[grep('deck', train$features)] <- 1

train$dining <- 0
train$dining[grep('dining', train$features)] <- 1
#################################
train$laundry <- 0
train$laundry[grep('laundry', train$features)] <- 1

train$doorman <- 0
train$doorman[grep('doorman', train$features)] <- 1

train$garden <- 0
train$garden[grep('garden', train$features)] <- 1

train$garage <- 0
train$garage[grep('garage', train$features)] <- 1

train$furnished <- 0
train$furnished[grep('furnished', train$features)] <- 1

train$fitness <- 0
train$fitness[grep('fitness', train$features)] <- 1

train$dryer <- 0
train$dryer[grep('dryer', train$features)] <- 1

train$hardwood <- 0
train$hardwood[grep('hardwood', train$features)] <- 1

train$highceil <- 0
train$highceil[grep('highceil', train$features)] <- 1

train$internet <- 0
train$internet[grep('internet', train$features)] <- 1

train$new <- 0
train$new[grep('new', train$features)] <- 1

train$washer <- 0
train$washer[grep('washer', train$features)] <- 1

train$wheelchair <- 0
train$wheelchair[grep('wheelchair', train$features)] <- 1

train$nofee <- 0
train$nofee[grep('nofee', train$features)] <- 1

train$duplex <- 0
train$duplex[grep('duplex', train$features)] <- 1
train$duplex[grep('multilevel', train$features)] <- 1

train$outdoor <- 0
train$outdoor[grep('outdoor', train$features)] <- 1

train$swimming <- 0
train$swimming[grep('swimming', train$features)] <- 1

train$terrace <- 0
train$terrace[grep('terrace', train$features)] <- 1

train$green <- 0
train$green[grep('green', train$features)] <- 1

train$steel <- 0
train$steel[grep('steel', train$features)] <- 1

train$deck <- 0
train$deck[grep('deck', train$features)] <- 1
train$deck[grep('loft', train$features)] <- 1

train_final <- train
train$features <- NULL
train$features1 <- NULL

train$street_address <- gsub(' ','',train$street_address)

strt<-table(train$street_address)
strt <- as.data.frame(strt)
strt <- strt[strt$Freq>100,]
train$street_address <- sapply(train$street_address,FUN = function(x) ifelse(x %in% strt$Var1,x,''))
train$street_address <- as.factor(train$street_address)
train$direction <- as.factor(train$direction)

library(dummies)

train_d <- dummy.data.frame(train)

target <- train$interest_level
train_d$interest_levelhigh <- NULL
train_d$interest_levellow <- NULL
train_d$interest_levelmedium <- NULL
###0-low, 1-medium, 2-high
target <- as.character(target)
target[target=='low']=0
target[target=='medium']=1
target[target=='high']=2
target<-as.numeric(target)

xgb_model1 <- xgb.cv(data = as.matrix(train_d), label = target, nrounds = 100, max_depth=10,objective = 'multi:softprob',num_class = 3, nfold = 5)

#xgb_model <- xgboost(data = as.matrix(train_d), label = target, nrounds = 100, max_depth=10,objective = 'multi:softprob',num_class = 3)

#############################################################################################
####################################### TO RUN ############################################

test <- read.csv('test.csv')
test <- test[complete.cases(test[, 'price']),]
identifier <- test$listing_id
test$street_address <- as.character(test$street_address)
test$street_address <- tolower(test$street_address)
test$street_address <- gsub('[^a-z0-9 ]','',test$street_address)
test$street_address <- gsub('[0-9]st','',test$street_address)
test$street_address <- gsub('[0-9]th','',test$street_address)
test$street_address <- gsub('3rd','3',test$street_address)
test$street_address <- gsub('2nd','2',test$street_address)
test$street_address <- gsub('street','st',test$street_address)
test$street_address <- gsub(' str',' st',test$street_address)
test$street_address <- gsub(' avenue',' ave',test$street_address)
test$street_address <- gsub(' w ',' west ',test$street_address)
test$street_address <- gsub(' e ',' east ',test$street_address)
test$street_address <- gsub(' n ',' north ',test$street_address)
test$street_address <- gsub(' s ',' south ',test$street_address)
test$street_address <- gsub('^[0-9]* ','',test$street_address)
test$street_address <- gsub('^[ ]*','',test$street_address)
test$direction <- 'unknown'
test$direction[grep('east ',test$street_address)]='east'
test$direction[grep('west ',test$street_address)]='west'
test$direction[grep('north ',test$street_address)]='north'
test$direction[grep('south ',test$street_address)]='south'
test$street_address <- gsub('east ','',test$street_address)
test$street_address <- gsub('west ','',test$street_address)
test$street_address <- gsub('north ','',test$street_address)
test$street_address <- gsub('south ','',test$street_address)

test$X <- NULL
test$created <- NULL
test$display_address <- NULL
test$photos <- NULL
test$description <- NULL

test$bathrooms <- as.character(test$bathrooms)
test$bedrooms <- as.character(test$bedrooms)
test$bathrooms <- as.numeric(test$bathrooms)
test$bedrooms <- as.numeric(test$bedrooms)
test$building_id <- NULL
test$interest_level <- ''

test$latitude <- as.numeric(as.character(test$latitude))
test$listing_id <- NULL
test$manager_id <- NULL
test$street_address <- as.factor(test$street_address)

test$latitude_group <- as.factor(as.integer(test$latitude*10))
test$longitude_group <- as.factor(as.integer(test$longitude*10))
test$features <- as.character(test$features)
test$features<- gsub('[^a-zA-Z,]','',test$features)
test$features<- gsub('^u','',test$features)
test$features<- gsub(',u',',',test$features)
test$features <- tolower(test$features)
test$features1 <- sapply(test$features,FUN = function(x) as.character(unlist(strsplit(x,','))))

#all_features <- unique(unlist(test$features1))

#t <- table(all_features)
#for (feature in all_features){
#  t[feature] <- length(grep(feature,test$features))
#}

#feature_counts <- as.data.frame(t)
#print (t[t>200])
test$ac <- 0
test$ac[grep('ac', test$features)] <- 1

test$cat <- 0
test$cat[grep('cat', test$features)] <- 1

test$dog <- 0
test$dog[grep('dog', test$features)] <- 1

test$balcony <- 0
test$balcony[grep('balcony', test$features)] <- 1

test$common <- 0
test$common[grep('common', test$features)] <- 1

test$deck <- 0
test$deck[grep('deck', test$features)] <- 1

test$dining <- 0
test$dining[grep('dining', test$features)] <- 1
#################################
test$laundry <- 0
test$laundry[grep('laundry', test$features)] <- 1

test$doorman <- 0
test$doorman[grep('doorman', test$features)] <- 1

test$garden <- 0
test$garden[grep('garden', test$features)] <- 1

test$garage <- 0
test$garage[grep('garage', test$features)] <- 1

test$furnished <- 0
test$furnished[grep('furnished', test$features)] <- 1

test$fitness <- 0
test$fitness[grep('fitness', test$features)] <- 1

test$dryer <- 0
test$dryer[grep('dryer', test$features)] <- 1

test$hardwood <- 0
test$hardwood[grep('hardwood', test$features)] <- 1

test$highceil <- 0
test$highceil[grep('highceil', test$features)] <- 1

test$internet <- 0
test$internet[grep('internet', test$features)] <- 1

test$new <- 0
test$new[grep('new', test$features)] <- 1

test$washer <- 0
test$washer[grep('washer', test$features)] <- 1

test$wheelchair <- 0
test$wheelchair[grep('wheelchair', test$features)] <- 1

test$nofee <- 0
test$nofee[grep('nofee', test$features)] <- 1

test$duplex <- 0
test$duplex[grep('duplex', test$features)] <- 1
test$duplex[grep('multilevel', test$features)] <- 1

test$outdoor <- 0
test$outdoor[grep('outdoor', test$features)] <- 1

test$swimming <- 0
test$swimming[grep('swimming', test$features)] <- 1

test$terrace <- 0
test$terrace[grep('terrace', test$features)] <- 1

test$green <- 0
test$green[grep('green', test$features)] <- 1

test$steel <- 0
test$steel[grep('steel', test$features)] <- 1

test$deck <- 0
test$deck[grep('deck', test$features)] <- 1
test$deck[grep('loft', test$features)] <- 1

test_final <- test
test$features <- NULL
test$features1 <- NULL

test$street_address <- gsub(' ','',test$street_address)

test$street_address <- sapply(test$street_address,FUN = function(x) ifelse(x %in% strt$Var1,x,''))
test$street_address <- as.factor(test$street_address)
test$direction <- as.factor(test$direction)

library(dummies)
test <- test[colnames(train)]
test_d <- dummy.data.frame(test)

predicted <- predict(xgb_model,as.matrix(test_d))

output <- data.frame('listing_id'=identifier,'high'=0.3,'medium'=0.3,'low'=0.4)
output$low <- predicted[seq(1,221307,3)]
output$medium <- predicted[seq(2,221307,3)]
output$high <- predicted[seq(3,221307,3)]

sample$listing_id <- as.character(sample$listing_id)
output$listing_id <- as.character(output$listing_id)

result <- merge(sample['listing_id'],output,by='listing_id',all.x=TRUE)
write.csv(result,'solution.csv',row.names = F)
summary(result)
result[is.na(result)] = 0.3333
