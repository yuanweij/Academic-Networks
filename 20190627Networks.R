##############################
##Retrieving relational data##
##############################

#####Data cleaning########

library("twitteR")
library("rtweet")
library("wordcloud")
library("tm")


consumer_key <- 'VPXR0O6pTFZvRWL95fKp736s4'
consumer_secret <- '5Bb1TDnsCcC1U0BfHfALucPAdugFmK6G9wgvyElPue9LDJo5Us'
access_token <- '1143210205854892032-L9WgZxHZzLGfqFr6n9J4gqP0bQBTrf'
access_secret <- '8DMaWUl34MDvPRXzLwnpMdwaUhaFe9HMBynUy637ZvWlr'

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

token <- create_token(
  app = "Academic_network",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret)


name_list <-read.csv("Desktop/SICSS/academics twitter project/0626 AP name list.csv", header=T)
name_list$uncertain[is.na(name_list$uncertain)] <- 0
name_list$private.account[is.na(name_list$private.account)] <- 0

name_list$has_twitter <- NA
name_list$has_twitter[!is.na(name_list$twitter_username)] <- 1
name_list$has_twitter[is.na(name_list$twitter_username)] <- 0
table(name_list$has_twitter)
table(name_list$has_twitter, name_list$gender)
chisq.test(name_list$has_twitter, name_list$gender)

# (I added the following—Weijun)
#create variable: generation 
name_list$generation <- NA
name_list$generation[name_list$job_yr<=2000] <- 1
name_list$generation[2000<name_list$job_yr & name_list$job_yr<=2010] <- 2
name_list$generation[2010<name_list$job_yr] <- 3

#generation x has_twitter 
table(name_list$has_twitter, name_list$generation)
chisq.test(name_list$has_twitter, name_list$generation)
#X-squared = 23.295, df = 2, p-value = 8.739e-06

#create twitter_list
twitter_list <- name_list[!is.na(name_list$twitter_username) & name_list$uncertain==0 & name_list$private.account==0,]


# generate the data for the basic information for the users themselves

twitter_list$twitter_username <- as.character(twitter_list$twitter_username)
twitter_list$twitter_username <- gsub(" ", "", twitter_list$twitter_username) 

lookup_vector <- vector()
for (i in 1: length(twitter_list$twitter_username)) {
  
  lookup_vector[i] <- lookupUsers(twitter_list$twitter_username[i], includeNA=F)
  
}

followersCount <- vector()
friendsCount <- vector()
created <- vector()
description <- vector()
id <- vector()
lastStatus_created <- vector()
name <- vector()
location <- vector()
screenName <- vector()
for (i in 1: length(lookup_vector)) {
  
  followersCount[i] <- lookup_vector[[i]]$followersCount
  friendsCount[i] <- lookup_vector[[i]]$friendsCount
  created[i] <- as.character(as.Date(lookup_vector[[i]]$created))
  description[i] <- lookup_vector[[i]]$description
  id[i] <- lookup_vector[[i]]$id
  name[i] <- lookup_vector[[i]]$name
  location[i] <- lookup_vector[[i]]$location
  screenName[i] <- lookup_vector[[i]]$screenName
  
  # There are people who haven't tweeted anything, lastStatus_created will be NAs for them
  lastStatus_created[i] <- ifelse(!is.null(lookup_vector[[i]]$lastStatus$created), as.character(as.Date(lookup_vector[[i]]$lastStatus$created)), NA)
  
}

self_data <- as.data.frame(cbind(id, screenName, name, followersCount, friendsCount, created, description, lastStatus_created, location))
self_data$created <- as.Date(self_data$created)
self_data$lastStatus_created <- as.Date(self_data$lastStatus_created)


self_data_merged <- merge(self_data, twitter_list, by.x = "screenName", by.y = "twitter_username", all = T)
self_data_merged$followersCount <- as.numeric(as.character(self_data_merged$followersCount))
self_data_merged$friendsCount <- as.numeric(as.character(self_data_merged$friendsCount))

sum(self_data_merged$friendsCount, na.rm = t)



# gender inequality in # of followers
table(self_data_merged$gender)
t.test(self_data_merged$followersCount[self_data_merged$gender=="F"], self_data_merged$followersCount[self_data_merged$gender=="M"])


# asymmetric recognition ratio
self_data_merged$ratio <- self_data_merged$followersCount/self_data_merged$friendsCount
self_data_merged$ratio[self_data_merged$ratio==Inf] <- NA
t.test(self_data_merged$ratio[self_data_merged$gender=="F"], self_data_merged$ratio[self_data_merged$gender=="M"])
     #t = -1.2257, df = 63.906, p-value = 0.2248


# generational inequality in x followers  G1 vs G3
t.test(self_data_merged$followersCount[name_list$generation==1], self_data_merged$followersCount[name_list$generation==3])
     #t = -1.0405, df = 94.888, p-value = 0.3007

#select the users
## select one or more twitter users to lookup
users <- as.character(twitter_list$twitter_username)
View(users)
##replace"  " by " "
usernames <- gsub(" ", "", users) 

#lookUpUser loop
lookup_vector <- vector()
for (i in 1: length(usernames)) {
  
  lookup_vector[i] <- lookupUsers(usernames[i], includeNA=F)
  
}

followersCount <- vector()
friendsCount <- vector()
created <- vector()
description <- vector()
id <- vector()
lastStatus_created <- vector()
name <- vector()
location <- vector()
screenName <- vector()

for (i in 1: length(lookup_vector)) {
  
  followersCount[i] <- lookup_vector[[i]]$followersCount
  friendsCount[i] <- lookup_vector[[i]]$friendsCount
  created[i] <- as.character(as.Date(lookup_vector[[i]]$created))
  description[i] <- lookup_vector[[i]]$description
  id[i] <- lookup_vector[[i]]$id
  name[i] <- lookup_vector[[i]]$name
  location[i] <- lookup_vector[[i]]$location
  screenName[i] <- lookup_vector[[i]]$screenName
  
  # There are people who haven't tweeted anything, lastStatus_created will be NAs for them
  lastStatus_created[i] <- ifelse(!is.null(lookup_vector[[i]]$lastStatus$created), as.character(as.Date(lookup_vector[[i]]$lastStatus$created)), NA)
  
}

self_data <- as.data.frame(cbind(id, screenName, name, followersCount, friendsCount, created, description, lastStatus_created, location))
self_data$created <- as.Date(self_data$created)
self_data$lastStatus_created <- as.Date(self_data$lastStatus_created)

self_data_merged <- merge(self_data, ap_names_twitter, by.x = "screenName", by.y = "twitter_username")
self_data_merged$followersCount <- as.numeric(self_data_merged$followersCount)
self_data_merged$friendsCount <- as.numeric(self_data_merged$friendsCount)

# gender inequality in # of followers
table(self_data_merged$gender)
t.test(self_data_merged$followersCount[self_data_merged$gender=="F"], self_data_merged$followersCount[self_data_merged$gender=="M"])
     #t = -1.1462, df = 138.31, p-value = 0.2537
     #mean of x: 61.78873; mean of y: 69.15714 

# generational inequality in x followers  G1 vs G3
t.test(self_data_merged$followersCount[name_list$generation==1], self_data_merged$followersCount[name_list$generation==3])
      #t = -1.0405, df = 94.888, p-value = 0.3007

# generational inequality in x followers  G2 vs G3
t.test(self_data_merged$followersCount[name_list$generation==2], self_data_merged$followersCount[name_list$generation==3])
      #t = -0.59294, df = 91.483, p-value = 0.5547

# generational inequality in x followers  G1 vs G2
t.test(self_data_merged$followersCount[name_list$generation==1], self_data_merged$followersCount[name_list$generation==2])
      #t = -0.4433, df = 88.852, p-value = 0.6586

View(self_data_merged)


#for all 141 faculty in the dataset get friends
#friends <- list()
#for (a in 1:length(id)){
  #friends[[a]] <- get_friends(id[a], retryonratelimit = T)
#}

# Combine data tables in list
#friends <- bind_rows(friends) %>% 
#  rename(friend = user_id)

#install.packages('here')
#install.packages("readr")
#library(here)
#library(readr)
#library(dplyr)
#write_csv(friends, here("twitter_friends.csv"))


#for all 130 faculty in the dataset get followers
followers <- list()
for (a in 1:length(id)){
  followers[[a]] <- get_followers(id[a])
  followers[[a]][2] <-  id[a]
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

options(scipen=999)
View(followers)

# Combine data tables in list
followers <- bind_rows(followers) %>% 
  rename(follower = user_id) %>%
  rename(user = V2)

write_csv(followers, here("academic_followers.csv"))


#import academic_followers and see the unique names
academic_followers <-read.csv("/Users/Ginger/academic_followers.csv", header=T) 
library(dplyr)
unique <- tapply(academic_followers$follower, academic_followers$user, FUN = function(x) length(unique(x)))
