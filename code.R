suppressMessages(library("jsonlite"))
suppressMessages(library("plotly"))
suppressMessages(library("purrr"))
suppressMessages(library("RecordLinkage"))
setwd('/home/yasar/kaggle/two_sigma_renthop/')

#trainData <- fromJSON("train.json")
#variables <- setdiff(names(trainData), c("photos"))
#train <-map_at(trainData, variables, unlist) %>% tibble::as_tibble(.)
#testData <- fromJSON("test.json")
#variables <- setdiff(names(testData), c("photos"))
#test <-map_at(testData, variables, unlist) %>% tibble::as_tibble(.)

#READING INPUT
train <- read.csv('train.csv')
test <- read.csv('test.csv')

#ADDING ADDRESS SIMILARITY USING LENENSHTEIN DISTANCE
train$addressSimilarity <- levenshteinSim(tolower(train$street_address),tolower(train$display_address))
test$addressSimilarity <- levenshteinSim(tolower(test$street_address),tolower(test$display_address))

#PRE_PROCESSING TRAIN DATA
#REMOVING NAS
train <- train[complete.cases(train[, 'price']),]


#GETTING PROBABILITIES OF TARGET LEVELS
train$interest_level <- as.factor(as.character(train$interest_level))
table(train$interest_level)/nrow(table)

#CLEANING THE STREET ADDRESS TO STANDARDISE ADDRESS
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

#CREATING NEW VARIABLE - DIRECTION
train$direction <- 'unknown'
train$direction[grep('east ',train$street_address)]='east'
train$direction[grep('west ',train$street_address)]='west'
train$direction[grep('north ',train$street_address)]='north'
train$direction[grep('south ',train$street_address)]='south'


train$street_address <- gsub('east ','',train$street_address)
train$street_address <- gsub('west ','',train$street_address)
train$street_address <- gsub('north ','',train$street_address)
train$street_address <- gsub('south ','',train$street_address)

#REMOVING UNWANTED FEATURES
train$X <- NULL
train$created <- NULL
train$display_address <- NULL
train$description <- NULL

#CLEANING FEATURES
train$bathrooms <- as.character(train$bathrooms)
train$bedrooms <- as.character(train$bedrooms)
train$bathrooms <- as.numeric(train$bathrooms)
train$bedrooms <- as.numeric(train$bedrooms)
train$building_id <- NULL
train$interest_level <- as.factor(as.character(train$interest_level))

train$latitude <- as.numeric(as.character(train$latitude))
train$listing_id <- NULL
train$street_address <- as.factor(train$street_address)

#CREATING LATITUDE AND LONGITUDE GROUP FACTORS
train$latitude_group <- as.factor(as.integer(train$latitude*10))
train$longitude_group <- as.factor(as.integer(train$longitude*10))

#EXTRACTING LIST OF ALL TRAIN$FEATURES - THE VALUES IN TRAIN$FEATURES ARE PYTHONLISTS
train$features <- as.character(train$features)
train$features<- gsub('[^a-zA-Z,]','',train$features)
train$features<- gsub('^u','',train$features)
train$features<- gsub(',u',',',train$features)
train$features <- tolower(train$features)
train$features1 <- sapply(train$features,FUN = function(x) as.character(unlist(strsplit(x,','))))


#COMBINING ALL FEATURES
all_features <- unique(unlist(train$features1))

t <- table(all_features)
for (feature in all_features){
  t[feature] <- length(grep(feature,train$features))
}

#GETTING FEATURES THAT ARE AVAILABLE IN ATLEAST 200 DATA POINTS IN ORDER TO RETAIN THEM
print (t[t>200])

#CREATING NEW VARIABLES BASED ON TOP FEATURES IN TRAIN$FEATURES
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

train$features <- NULL
train$features1 <- NULL

#RETAINING STREET ADDRESS WITH ATLEAST OBSERVATIONS SO THAT THERE IS ENOUGH EVIDENCE TO FIGURE OUT THE INTEREST LEVEL
train$street_address <- gsub(' ','',train$street_address)
strt<-table(train$street_address)
strt <- as.data.frame(strt)
strt <- strt[strt$Freq>5,]
train$street_address <- sapply(train$street_address,FUN = function(x) ifelse(x %in% strt$Var1,x,''))
train$street_address <- as.factor(train$street_address)
train$direction <- as.factor(train$direction)

#GETTING THE NUMBER OF PICTURES POSTED
library(stringr)
train$photos <- as.character(train$photos)
train$photos <- str_count(train$photos,"u'")
train$photos <- as.numeric(train$photos)

#GETTING INTEREST LEVEL PROBABILITIES OF MANAGERS
library(dplyr)
train$manager_id <- as.character(train$manager_id)
manager_table <- table(train$manager_id)
manager_table <- as.data.frame(manager_table)
manager_probs <- train %>% group_by(manager_id,interest_level) %>% summarise('manager_interest_count' = length(interest_level)) 
manager_probs <- merge(manager_probs,manager_table,by.x='manager_id',by.y='Var1')
manager_probs$manager_probability <- manager_probs$manager_interest_count/manager_probs$Freq
library(reshape2)
manager_probs$manager_interest_count <- NULL
manager_probs$Freq<-NULL
manager_probs <- dcast(manager_probs,manager_id~interest_level)
#FIXING NAS TO FIXED PROBABILITY VALUES
manager_probs$low[is.na(manager_probs$low)] = 0.69
manager_probs$medium[is.na(manager_probs$medium)] = 0.225
manager_probs$high[is.na(manager_probs$high)] = 0.085
train <- merge(train,manager_probs,by = c('manager_id'))
train$manager_id <- NULL

#GETTING INTEREST LEVEL PROBABILITIES FOR DIFFERENT LATITUDE LONGITUDE GROUPS
train_latitude_groups <- as.character(train$latitude_group)
train_longitude_groups <- as.character(train$longitude_group)
lat_long_counts <- train %>% group_by(latitude_group,longitude_group) %>% count()
lat_long_probs <- train %>% group_by(latitude_group,longitude_group,interest_level) %>% summarise('lat_long_interest_count'=length(interest_level))
lat_long_probs <- merge(lat_long_probs,lat_long_counts,by=c('latitude_group','longitude_group'))
lat_long_probs$lat_long_prob <- lat_long_probs$lat_long_interest_count/lat_long_probs$n
lat_long_probs$lat_long_interest_count <- NULL
lat_long_probs$n <- NULL
library(reshape2)
lat_long_probs = dcast(lat_long_probs,latitude_group+longitude_group~interest_level)
#FIXING NAS TO FIXED PROBABILITY VALUES
lat_long_probs$high[is.na(lat_long_probs$high)] <- 0.085
lat_long_probs$medium[is.na(lat_long_probs$medium)] <- 0.225
lat_long_probs$low[is.na(lat_long_probs$low)] <- 0.69
train <- merge(train,lat_long_probs,by = c('latitude_group','longitude_group'))

#CREATING DUMMIES
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

#############################################################################################
####################################### SAME DATA CLEANING ON TEST ############################################

test <- test[complete.cases(test[, 'price']),]
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
test$description <- NULL
test$bathrooms <- as.character(test$bathrooms)
test$bedrooms <- as.character(test$bedrooms)
test$bathrooms <- as.numeric(test$bathrooms)
test$bedrooms <- as.numeric(test$bedrooms)
test$building_id <- NULL
test$interest_level <- ''
test$latitude <- as.numeric(as.character(test$latitude))
test$street_address <- as.factor(test$street_address)
test$latitude_group <- as.factor(as.integer(test$latitude*10))
test$longitude_group <- as.factor(as.integer(test$longitude*10))
test$features <- as.character(test$features)
test$features<- gsub('[^a-zA-Z,]','',test$features)
test$features<- gsub('^u','',test$features)
test$features<- gsub(',u',',',test$features)
test$features <- tolower(test$features)
test$features1 <- sapply(test$features,FUN = function(x) as.character(unlist(strsplit(x,','))))
#CREATING VARIABLES BASED ON FEATURES
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

test$features <- NULL
test$features1 <- NULL

test$street_address <- gsub(' ','',test$street_address)
test$street_address <- sapply(test$street_address,FUN = function(x) ifelse(x %in% strt$Var1,x,''))
test$direction <- as.factor(test$direction)
test$photos <- as.character(test$photos)
test$photos <- str_count(test$photos,"u'")
test$photos <- as.numeric(test$photos)
test$manager_id <- as.character(test$manager_id)
manager_probs$manager_id <- as.character(manager_probs$manager_id)
lat_long_probs$latitude_group <- as.character(lat_long_probs$latitude_group)
lat_long_probs$longitude_group <- as.character(lat_long_probs$longitude_group)
test$latitude_group <- as.character(test$latitude_group)
test$longitude_group <- as.character(test$longitude_group)
test <- merge(test,manager_probs,by='manager_id')
test$manager_id <- NULL
test <- merge(test,lat_long_probs,by=c('latitude_group','longitude_group'))
test$latitude_group <- as.factor(test$latitude_group)
test$longitude_group <- as.factor(test$longitude_group)
identifier <- test$listing_id
test$listing_id <- NULL
#CREATING DUMMIES
library(dummies)
test <- test[colnames(train)]
test_d <- dummy.data.frame(test)
#RETAINING TRAIN FEATURES ONLY IF IT IS PRESENT IN TEST SET
colnames(train_d)%in%colnames(test_d)
train_d <- train_d[colnames(test_d)]

xgb_model <- xgboost(data = as.matrix(train_d), label = target, nrounds = 150, max_depth=30,objective = 'multi:softprob',num_class = 3)

predicted <- predict(xgb_model,as.matrix(test_d))
output <- data.frame('listing_id'=identifier,'high'=0.085,'medium'=0.225,'low'=0.69)
output$low <- predicted[seq(1,216921,3)]
output$medium <- predicted[seq(2,216921,3)]
output$high <- predicted[seq(3,216921,3)]
sample <- read.csv('sample_submission.csv')
sample$low <- NULL
sample$medium <- NULL
sample$high <- NULL
sample$listing_id <- as.character(sample$listing_id)
output$listing_id <- as.character(output$listing_id)
result <- merge(sample['listing_id'],output,by='listing_id',all.x=TRUE)
summary(result)
result$low[is.na(result$low)]= 0.69
result$medium[is.na(result$medium)]= 0.225
result$high[is.na(result$high)]= 0.085
write.csv(result,'solution1.csv',row.names = F)

