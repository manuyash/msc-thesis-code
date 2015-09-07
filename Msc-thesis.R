library(relaimpo)
library(inTrees)
library("tree")
library("relaimpo")
library("AUC")
library("maptree")
library("rfPermute")
library("kernlab")
library("subselect")
library("ElemStatLearn")
library("nnet")
library("NeuralNetTools")
library('randomForest')
library('party')
library("rpart")
library("arules")
library("NbClust")
library("Kernlab")
library("inTrees")
library("neuralnet")
library("knitr")
library("aod")
library("party")
library("C50")
library("Deducer")
library("rpart.plot")
library("rattle")
library(rattle)
library(rpart.plot)
library(Deducer)
library(kernlab)
library(party)
library(aod)
library(knitr)
library(neuralnet)
install.packages("clusterGeneration")
library(clusterGeneration)
library(RMySQL)
library(ggplot2) 
library(doBy)
library(scales)
library("devtools")
library(plotly)
library("RColorBrewer")
library(neuralnet)
source("http://freakonometrics.free.fr/probit.R")
source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')

analytics_sync<-read.csv("analytics_sync.csv", header= TRUE, sep= ",")
df_temp <-read.csv("df_temp.csv", header= TRUE, sep= ",")
push_notifications<-read.csv("push_notifications.csv", header= TRUE, sep= ",")
quit_attempts_sync<-read.csv("quit_attempts_sync.csv", header= TRUE, sep= ",")
user_behaviour_sync<-read.csv("user_behaviour_sync.csv", header= TRUE, sep= ",")
user_filtered<-read.csv("user_filtered.csv", header= TRUE, sep= ",")
user_filtered_unique<-read.csv("user_filtered_unique.csv", header= TRUE, sep= ",")
users<-read.csv("users.csv", header= TRUE, sep= ",")
users_all<-read.csv("users_all.csv", header= TRUE, sep= ",")

analytics_sync<-analytics_sync[,-1]
df_temp<-df_temp[,-1]
push_notifications<-push_notifications[,-1]
quit_attempts_sync<-quit_attempts_sync[,-1]
user_behaviour_sync<-user_behaviour_sync[,-1]
user_filtered<-user_filtered[,-1]
user_filtered_unique<-user_filtered_unique[,-1]
users<- users[,-1]
users_all<- users_all[,-1]

analytics_sync$quitID<- as.character(analytics_sync$quitID)
analytics_sync$action<- as.character(analytics_sync$action)
analytics_sync$value<- as.character(analytics_sync$value)
analytics_sync$timeStamp<- as.character(analytics_sync$timeStamp)

push_notifications$status_message<-as.character(push_notifications$status_message)
push_notifications$messageID<-as.character(push_notifications$messageID)
push_notifications$timeStamp<-as.character(push_notifications$timeStamp)

quit_attempts_sync$quitID<- as.character(quit_attempts_sync$quitID)
quit_attempts_sync$quit_date<- as.character(quit_attempts_sync$quit_date)
quit_attempts_sync$timeStamp<- as.character(quit_attempts_sync$timeStamp)

user_behaviour_sync$action<-as.character(user_behaviour_sync$action)
user_behaviour_sync$value<-as.character(user_behaviour_sync$value)
user_behaviour_sync$quitID<-as.character(user_behaviour_sync$quitID)
user_behaviour_sync$timeStamp<-as.character(user_behaviour_sync$timeStamp)

user_filtered$country<-as.character(user_filtered$country)
user_filtered$weekly_spend_currency<-as.character(user_filtered$weekly_spend_currency)
user_filtered$motivation_other<-as.character(user_filtered$motivation_other)
user_filtered$register_date<-as.character(user_filtered$register_date)
user_filtered$intervention<-as.character(user_filtered$intervention)
user_filtered$register_date_no_time <- as.Date(users$register_date_no_time)

user_filtered_unique_unique$country<-as.character(user_filtered_unique$country)
user_filtered_unique$weekly_spend_currency<-as.character(user_filtered_unique$weekly_spend_currency)
user_filtered_unique$motivation_other<-as.character(user_filtered_unique$motivation_other)
user_filtered_unique$register_date<-as.character(user_filtered_unique$register_date)
user_filtered_unique$intervention<-as.character(user_filtered_unique$intervention)
user_filtered_unique$register_date_no_time <- as.Date(user_filtered_unique$register_date_no_time)

users$register_date<- as.character(users$register_date)
users$intervention<- as.character(users$intervention)
users$campaign<- as.character(users$campaign)
users$register_date_no_time<- as.Date(users$register_date_no_time)

users_all$country<- as.character(users_all$country)
users_all$motivation_other<- as.character(users_all$motivation_other)
users_all$register_date<- as.character(users_all$register_date)
users_all$intervention<- as.character(users_all$intervention)
users_all$register_date_no_time<- as.Date(users_all$register_date_no_time)
## ToDO - one hot encode users
# users_ohe - hot encode demographic data

quit_attempts_sync<-quit_attempts_sync[with(quit_attempts_sync, order(userID, attempt_no)), ]
cols <- c( 'userID' , 'attempt_no')
quit_attempts_sync$userID_attempt <- sort(apply( quit_attempts_sync[ , cols ] , 1 , paste , collapse = "-" ))
quit_attempts_sync$timeStamp <- strptime(as.character(quit_attempts_sync$timeStamp),  "%Y-%m-%d %H:%M:%S")
quit_attempts_sync<-quit_attempts_sync[with(quit_attempts_sync, order(userID_attempt, rev(timeStamp))), ]
quit_attempts_sync <- quit_attempts_sync[!duplicated(quit_attempts_sync$userID_attempt),]



date_list <- strsplit(user_behaviour_sync$timeStamp, " ")
df_temp <- data.frame(matrix(unlist(date_list), nrow=length(date_list), byrow=T))
user_behaviour_sync$date <- df_temp$X1
user_behaviour_sync$time <- substr(df_temp$X2,1,2)
user_behaviour_sync$date <- as.Date(user_behaviour_sync$date)
user_behaviour_sync$weekday <- weekdays(as.Date(user_behaviour_sync$date))

## DemoGraphic Data Setup
###convertind users into data_set
dd<- user_filtered[, c(1,3,5)]
dd1<-with(dd, data.frame(model.matrix(~employment-1,dd), userID , age))
ddm <- user_filtered[, c(1,2,7)]
dd2<-with(ddm, data.frame(model.matrix(~gender-1,ddm), userID ))              
dd3<-with(ddm, data.frame(model.matrix(~quit_attempt_last_year-1,ddm), userID ))  
dd4<- cbind( dd2, dd3)
dd5<- cbind( dd4, dd1) ### new one hot coded dataset for eligible users up to 7 columns
ddj<- user_filtered[ , c(1,8,9,10)]
dd6<-with(ddj, data.frame(model.matrix(~past_urges-1,ddj), userID ))
dd7<- with(ddj, data.frame(model.matrix(~urge_strength-1,ddj), userID ))
dd8<- with(ddj, data.frame(model.matrix(~stopped_more_than_week-1,ddj), userID ))
dd10<- cbind( dd5, dd6,dd7,dd8)
dd11<- subset(dd10, select= -c(6,12,20,27,30)) ## one hot coded dataset upto urge_strength
ddk<- user_filtered[, c(1,11)]
dd12<-with(ddk, data.frame(model.matrix(~onwake_smoke-1,ddj), userID ))
dd13<- cbind(dd11, dd12[, c(1,2,3,4)]) ## one hot encoding done. merging other columns
dd14<- user_filtered[ , c(12,14,15)]
data_set<- cbind( dd13,dd14)


#### new stuff
user_quit <- merge(x=quit_attempts_sync, y=data_set, by="userID")

# put users_ohe instead of users
####


user_behaviour_sync <- subset(user_behaviour_sync, user_behaviour_sync$date!=as.Date("2015-05-20"))


behav_by_quitID <- split( user_behaviour_sync , f = user_behaviour_sync$quitID )

# day parameter
max_days_considered <- 5

real_quitIDs <- c()
first_n_days_behav_split <- list()

i<-1

for (i in 1:length(behav_by_quitID)){
  
  current_set <-behav_by_quitID[i][[1]]
  
  current_set$day_no <- factor(as.numeric(current_set$day_no))
  all_days <- as.numeric(levels(current_set$day_no))
  pick_days <- subset(all_days, all_days>0)
  
  pick_days <- na.omit(pick_days[1:max_days_considered])
  
  if (length(pick_days)>0){
    pick_days <- as.numeric(pick_days)
    
    needed_data <- subset(current_set, as.numeric(as.character(current_set$day_no)) >= min(pick_days) & 
                            as.numeric(as.character(current_set$day_no)) <= max(pick_days))
    first_n_days_behav_split[[names(behav_by_quitID[i])]] <- needed_data
    real_quitIDs <- cbind(real_quitIDs, names(behav_by_quitID[i])) 
    
  }
}


behavioral_data <- data.frame()
one_line <- c()
i<-1
for (i in 1:length(real_quitIDs)) {
  print (i)
  one_line <- c()
  one_line <- cbind(one_line, real_quitIDs[i])
  this_quitID_behav <- first_n_days_behav_split[[real_quitIDs[i]]]
  
  this_quitID_behav$day_no <- factor(this_quitID_behav$day_no)
  
  number_of_days <- length(levels(this_quitID_behav$day_no))
  one_line <- cbind(one_line, number_of_days)
  
  logins <- subset(this_quitID_behav, this_quitID_behav$action=="login")
  
  
  if (length(logins[,1]) > 0){
    mean_logins_per_day <- length(logins[,1])/number_of_days
    one_line <- cbind(one_line, mean_logins_per_day)
    
    logins$day_no <- as.numeric(logins$day_no)
    logins_by_day <- split( logins , f = logins$day_no )
    first_time <- mean(as.numeric(sapply(logins_by_day , function(x) min(x$time))))
    one_line <- cbind(one_line, first_time)
    
    last_time <- mean(as.numeric(sapply(logins_by_day , function(x) max(x$time))))
    one_line <- cbind(one_line, last_time)
  } else {
    mean_logins_per_day <- 0
    one_line <- cbind(one_line, mean_logins_per_day)
    
    first_time <- 0
    one_line <- cbind(one_line, first_time)
    
    last_time <- 0
    one_line <- cbind(one_line, last_time)
  }
  
  
  advice <- subset(this_quitID_behav, this_quitID_behav$action=="clicked" & this_quitID_behav$value=="menu_lifestyles")
  mean_advice_per_day <- length(advice[,1])/number_of_days
  one_line <- cbind(one_line, mean_advice_per_day)
  
  cravings <- subset(this_quitID_behav, this_quitID_behav$action=="craving")
  
  if (length(cravings[,1]) > 0){
    mean_number_cravings_per_day <- length(cravings[,1])/number_of_days
    one_line <- cbind(one_line, mean_number_cravings_per_day)
    
    cravings$day_no <- as.numeric(cravings$day_no)
    cravings_by_day <- split( cravings , f = cravings$day_no )
    
    mean_cravings <- mean(as.numeric(cravings$value))
    one_line <- cbind(one_line, mean_cravings)
    
    min_cravings <- mean(as.numeric(sapply(cravings_by_day , function(x) min(x$value))))
    one_line <- cbind(one_line, min_cravings)
    
    max_cravings <- mean(as.numeric(sapply(cravings_by_day , function(x) max(x$value))))
    one_line <- cbind(one_line, max_cravings)
    
  } else {
    
    mean_number_cravings_per_day <- 0
    one_line <- cbind(one_line, mean_number_cravings_per_day)
    
    mean_cravings <- 0
    one_line <- cbind(one_line, mean_cravings)
    
    min_cravings <- 0
    one_line <- cbind(one_line, min_cravings)
    
    max_cravings <- 0
    one_line <- cbind(one_line, max_cravings)
    
  }
  
  smokes <- subset(this_quitID_behav, this_quitID_behav$action=="smoked")
  
  did_smoke <- as.numeric(sum(as.numeric(smokes$value)) > 0)
  one_line <- cbind(one_line, did_smoke)
  
  behavioral_data <- rbind(behavioral_data,one_line)
}

behavioral_data[is.na(behavioral_data)] <- 0

behavioral_data[behavioral_data=="NaN"] <- 0


behavioral_data$mean_logins_per_day <- as.numeric(as.character(behavioral_data$mean_logins_per_day))
behavioral_data <- subset(behavioral_data, behavioral_data$mean_logins_per_day>0)

behavioral_data$V1 <- as.character(behavioral_data$V1)

behavioral_data$number_of_days <- as.numeric(as.character(behavioral_data$number_of_days))
behavioral_data$first_time <- as.numeric(as.character(behavioral_data$first_time))
behavioral_data$last_time <- as.numeric(as.character(behavioral_data$last_time))
behavioral_data$mean_advice_per_day <- as.numeric(as.character(behavioral_data$mean_advice_per_day))
behavioral_data$mean_number_cravings_per_day <- as.numeric(as.character(behavioral_data$mean_number_cravings_per_day))
behavioral_data$mean_cravings <- as.numeric(as.character(behavioral_data$mean_cravings))
behavioral_data$min_cravings <- as.numeric(as.character(behavioral_data$min_cravings))
behavioral_data$max_cravings <- as.numeric(as.character(behavioral_data$max_cravings))
behavioral_data$did_smoke <- as.numeric(as.character(behavioral_data$did_smoke))

#### new stuff
names(behavioral_data)[names(behavioral_data) == 'V1'] <- 'quitID'
user_quit_beh <- merge(x=behavioral_data, y=user_quit, by="quitID")

#summaryBy(did_smoke~intervention, data=user_quit_beh, fun=sum)

# put user_quit_hot_enc instead of user_quit - 197 observation ~
####

#write.csv(user_quit_beh,file="beh.csv")

## new features
session_data <- data.frame()
one_line <- c()

i<-1
for (i in 1:length(real_quitIDs)) {
  
  print (i)
  one_line <- c()
  
  this_quitID_sess <- first_n_days_behav_split[[real_quitIDs[i]]]
  login_ids <- which(this_quitID_sess$action %in% "login")
  
  if (length(login_ids) >0) {
    one_line <- cbind(one_line, real_quitIDs[i])
    each_session_end_id <- login_ids-1
    each_session_end_id <- each_session_end_id[-c(1)]
    each_session_end_id = c(each_session_end_id,length(this_quitID_sess[,1]))
    
    time_login <- this_quitID_sess$timeStamp[login_ids]
    time_end <- this_quitID_sess$timeStamp[each_session_end_id]
    
    time_login <- strptime(as.character(time_login),  "%Y-%m-%d %H:%M:%S")
    time_end <- strptime(as.character(time_end),  "%Y-%m-%d %H:%M:%S")
    session_length <- time_end - time_login 
    
    # add to behavioural data
    
    
    one_line <- cbind(one_line, as.numeric(mean(session_length)/60.0))
    
    k<-1
    session_richness_record <- c()
    for (k in 1:length(login_ids)){
      current_session <- this_quitID_sess[c(login_ids[k]:each_session_end_id[k]),]
      number_of_actions <- length(current_session[!duplicated(current_session$action),][,1])-1
      click_subset <- subset(current_session, current_session$action=="clicked")
      various_clicks <- length(click_subset[!duplicated(click_subset$value),][,1])
      session_richness <- number_of_actions + various_clicks - 1
      
      session_richness_record <- c(session_richness_record,session_richness)
      
    }
    # add to behavioural data
    one_line <- cbind(one_line, mean(session_richness_record))
    
  }
  
  session_data <- rbind(session_data,one_line)
}
colnames(session_data)<- c("quitID", "Mean_session_length","Session_richness")
user_beh_final<- merge(x= behavioral_data, y = session_data, by = "quitID")
user_beh_final$Mean_session_length<- as.numeric(as.character(user_beh_final$Mean_session_length))
user_beh_final$Session_richness<- as.numeric(as.character(user_beh_final$Session_richness))
user_demo_final<- user_quit_beh[,-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17)]
user_demo_final$confidence_to_quit[user_demo_final$confidence_to_quit== 999 ] <- 4

# Data Set Plots:
#############
thm <- theme(panel.background = element_rect(colour = "white", fill = "white"),
             legend.key = element_rect(colour = "white", fill = "white"),
             axis.line = element_line(colour="black"),
             #       legend.position = c(.9,.9),
             panel.grid.major = element_line(colour="#D3D3D3"),
             panel.grid.minor = element_line(colour="#D3D3D3"),
             axis.title.x = element_text(vjust = -0.3),
             axis.text.x = element_text(angle=90, colour = "black", vjust=1),
             
             axis.text.y = element_text(colour = "black"),
             text = element_text(size=10)
             #   text = element_text(size=16, family="Arial")
             
)

thm0 <- theme(panel.background = element_rect(colour = "white", fill = "white"),
              legend.key = element_rect(colour = "white", fill = "white"),
              axis.line = element_line(colour="black"),
              #       legend.position = c(.9,.9),
              panel.grid.major = element_line(colour="#D3D3D3"),
              panel.grid.minor = element_line(colour="#D3D3D3"),
              axis.title.x = element_text(vjust = -0.3),
              axis.text.x = element_text(angle=0, colour = "black", vjust=1),
              
              axis.text.y = element_text(colour = "black"),
              text = element_text(size=10)
              #  text = element_text(size=16, family="Arial")
              
)

thm_grid <- theme(
  legend.key = element_rect(colour = "white", fill = "white"),
  axis.line = element_line(colour="black"),
  #       legend.position = c(.9,.9),
  panel.grid.major = element_line(colour="white"),
  panel.grid.minor = element_line(colour="white"),
  axis.title.x = element_text(vjust = -0.3),
  axis.text.x = element_text(angle=90, colour = "black", vjust=1),
  
  axis.text.y = element_text(colour = "black"),
  text = element_text(size=14)
  # text = element_text(size=14, family="Arial")
  
)

save_plot <- function(a_plot, w=4, h=4, name=""){
  #name = ""
  if (name==""){
    file_name <- paste("./plots/",deparse(substitute(a_plot)), ".png", sep="")
  } else {
    file_name = paste("./plots/",name, ".png", sep="")
  }
  ggsave(plot=a_plot, 
         filename = file_name,
         width = w,
         height = h, units = "in"
         #dpi = 300, limitsize = TRUE, compression="lzw", type="cairo"
  )
}



#py <- plotly()

#urls <- c()



user_gender <- ggplot(users, aes(x = gender)) + 
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue2", aes(y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ggtitle("User Gender Distribution") + 
  xlab("Gender") +ylab("% of users") + thm0 + theme(legend.position="none")

save_plot(user_gender, name="02")
##response <- py$ggplotly(user_gender, kwargs=list(filename="user_gender", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

##urls <- rbind(urls, c("user_gender", response$response$url))

users$intervention <- factor(users$intervention)

user_interventions <- ggplot(users, aes(x=intervention)) + 
  geom_bar(colour = "deepskyblue3", fill = "deepskyblue2", aes(y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ggtitle("User Intervention Groups") + 
  xlab("Intervention") +ylab("% of users") + thm0 + theme(legend.position="none")

save_plot(user_interventions, name="03")
#response <- py$ggplotly(user_interventions, kwargs=list(filename="user_interventions", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("user_interventions", response$response$url))



user_age <- ggplot(users, aes(x = age)) + 
  geom_bar(binwidth = 5, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+
  coord_cartesian(xlim=c(10, 65)) + scale_x_continuous(breaks=seq(15, 65, 5))+
  ggtitle("User Age Distribution") + 
  xlab("Years") +ylab("% of users") + thm0 + theme(legend.position="none")

save_plot(user_age, name="04")
#response <- py$ggplotly(user_age, kwargs=list(filename="user_age", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("user_age", response$response$url))

user_employment <- ggplot(users, aes(x = employment)) + 
  geom_bar(aes(color=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ggtitle("User Employment") + coord_flip()+
  xlab("") +ylab("% of users") + thm + theme(legend.position="none")

save_plot(user_employment,w=5,name="05")
#response <- py$ggplotly(user_employment, kwargs=list(filename="user_employment", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("user_employment", response$response$url))


money_spent <- ggplot(users, aes(x = weekly_spend_value)) + 
  geom_bar(binwidth = 5, aes( color=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  ggtitle("Weekly Spent Value") + xlim(0,100) +
  xlab("") +ylab("% of users") + thm + theme(legend.position="none")

save_plot(money_spent,w=5,name="money_spent")
#response <- py$ggplotly(user_employment, kwargs=list(filename="user_employment", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

no_cigs_plot <- ggplot(users, aes(x = no_cigs)) + 
  geom_bar(binwidth = 5, aes( color=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  ggtitle("Number of Cigarettes Smoked per Week") + xlim(0,50) +
  xlab("") +ylab("% of users") + thm + theme(legend.position="none")

save_plot(no_cigs_plot,w=5,name="no_cigs")



smoking_money <- ggplot(users, aes(no_cigs,weekly_spend_value)) + geom_point() + geom_smooth()+
  ggtitle("Number of Cigarettes Smoked per Week \nvs. Weekly Spent Value") + 
  xlab("Number of Cigarettes") +ylab("Weekly Spent Value") + 
  scale_y_continuous(limits=c(0, 110), breaks=seq(0, 110, 10))+
  scale_x_continuous(limits=c(0, 40), breaks=seq(1, 40, 10))+
  thm0 + theme(legend.position="none") 

save_plot(smoking_money, name="06")


quit_attempts <- ggplot(users, aes(x = quit_attempt_last_year)) + 
  geom_bar( colour = "deepskyblue3", fill = "deepskyblue2", aes(y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ggtitle("Quit attempt last year") + 
  xlab("") +ylab("% of users") + thm0

save_plot(quit_attempts, name="07")
#response <- py$ggplotly(quit_attempts, kwargs=list(filename="quit_attempts", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("quit_attempts", response$response$url))


stopped_week <- ggplot(users, aes(x = stopped_more_than_week)) + 
  geom_bar( colour = "deepskyblue3", fill = "deepskyblue2", aes(y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ggtitle("Stopped smoking for \nmore than one week") + 
  xlab("") +ylab("% of users") + thm0

save_plot(stopped_week, name="08")
#response <- py$ggplotly(stopped_week, kwargs=list(filename="stopped_week", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("stopped_week", response$response$url))


# urge_strength  

urge_strength <- ggplot(users, aes(x = urge_strength)) + 
  geom_bar(  aes(colour = ..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ coord_flip()+
  ggtitle("Urge strength")+xlab("") +ylab("% of users") + thm + theme(legend.position="none")

save_plot(urge_strength,w=5, name="09")
#response <- py$ggplotly(urge_strength, kwargs=list(filename="urge_strength", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("urge_strength", response$response$url))



# onwake smoke


onwake_smoke <- ggplot(users, aes(x = onwake_smoke)) + 
  geom_bar( aes(colour = ..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+
  ggtitle("First cigarette after waking up")+xlab("Time after waking up") +
  ylab("% of users") + thm0 + theme(legend.position="none")

save_plot(onwake_smoke, name="10")
#response <- py$ggplotly(onwake_smoke, kwargs=list(filename="onwake_smoke", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("onwake_smoke", response$response$url))


# confidence to quit  

confidence_to_quit <- ggplot(users, aes(x = confidence_to_quit)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(1,8),breaks=seq(1, 7, 1))+ theme(legend.position="none") +
  ggtitle("Confidence to quit")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(confidence_to_quit, name="11")
#response <- py$ggplotly(confidence_to_quit, kwargs=list(filename="confidence_to_quit", world_readable=TRUE, fileopt="overwrite", auto_open=FALSE))

#urls <- rbind(urls, c("confidence_to_quit", response$response$url))

# todo: make more behaviour plots similar to the mean logins per day:
beh_mean_days <- ggplot(user_quit_beh, aes(x = mean_logins_per_day)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Mean logins per day")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_mean_days, name="551")

beh_number_of_days <- ggplot(user_quit_beh, aes(x = number_of_days)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Number of days ")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_number of days, name="551")

beh_adv <- ggplot(user_quit_beh, aes(x = mean_advice_per_day)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Mean advice per day")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_adv, name="552")

beh_mean_number_cravings <- ggplot(user_quit_beh, aes(x = mean_number_cravings_per_day)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Mean number of cravings per day")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_mean_number_cravings, name="553")

beh_min_cravings <- ggplot(user_quit_beh, aes(x = min_cravings)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Minimum Cravings")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_min _cravings, name="554")

beh_max_cravings <- ggplot(user_quit_beh, aes(x = max_cravings)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("MAximum Cravings ")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_max_carvings, name="555")

beh_first_time <- ggplot(user_quit_beh, aes(x = first_time)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("First time")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_first_time, name="556")

beh_last_time <- ggplot(user_quit_beh, aes(x = last_time)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Last time")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_last_time, name="557")

beh_mean_cravings <- ggplot(user_quit_beh, aes(x = mean_cravings)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Mean cravings")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_mean_cravings, name="558")

beh_mean_session_length <- ggplot(user_quit_beh, aes(x = Mean_sessio_length)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Mean session length")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_mean_session_length, name="559")

beh_session_richness <- ggplot(user_quit_beh, aes(x = Session_richness)) + 
  geom_bar( binwidth=1, aes(colour=..count.., fill = ..count.., y = 100*((..count..)/sum(..count..)))) + 
  scale_y_continuous(breaks=seq(0, 100, 5))+ scale_x_continuous(limits=c(0,7),breaks=seq(0, 7, 1))+ 
  theme(legend.position="none") +
  ggtitle("Session Richness")+xlab("") +ylab("% of users") + thm0 + theme(axis.text.x = element_text(hjust=-3))

save_plot(beh_session_richness, name="560")

##### Naive Bayes

sub = sample(nrow(user_beh_final), floor(nrow(user_beh_final) * 0.6))
train = user_beh_final[sub,]
test = user_beh_final[-sub,]


xTrain = train[,-c(1,11)]
yTrain = train$did_smoke

xTest = test[,-c(1,11)]
yTest = test$did_smoke

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=3))

gg<-predict(model$finalModel,xTest)$class
h<- data.frame(gg)
j<- cbind(h, test[,11])
#j<- round(j[,1],0)
u<- data.frame(j)
#u<- cbind(u, yTest)
colnames(u)<- c("pred","target")
cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1

correlationMatrix <- cor(user_beh_final[,-c(1,11)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
## correlated features are found and feature selection is done based on it.

## Naive Bayes: Demographic Data
## applying naive bayes:
user_demo_final$did_smoke<- as.factor(user_demo_final$did_smoke)
sub = sample(nrow(user_demo_final), floor(nrow(user_demo_final) * 0.6))
train = user_demo_final[sub,]
test = user_demo_final[-sub,]


xTrain = train[,-c(1,2)]
yTrain = train$did_smoke

xTest = test[,-c(1,2)]
yTest = test$did_smoke

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=3))
model
gg<-predict(model$finalModel,xTest)$class
h<- data.frame(gg)
j<- cbind(h, test[,2])
#j<- round(j[,1],0)
u<- data.frame(j)
#u<- cbind(u, yTest)
colnames(u)<- c("pred","target")
cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1


correlationMatrix <- cor(user_demo_final[,-c(1,2)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#Logisitc Regression: Behaviour Data
n <- names(user_beh_final[,-1])
n
f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))

user_beh_final$did_smoke<- as.factor(user_beh_final$did_smoke)
sub = sample(nrow(user_beh_final), floor(nrow(user_beh_final) * 0.6))
train = user_beh_final[sub,]
test = user_beh_final[-sub,]


xTrain = train[,-1]
yTrain = train$did_smoke

xTest = test[,-c(1,11)]
yTest = test$did_smoke

mylogit <- glm(f, xTrain, family = "binomial")
nothing<- glm( did_smoke ~ 1 ,data = user_beh_final[,-1], family = "binomial" )
backwards<- step(mylogit)
forwards <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(mylogit)), direction="forward")
bothways <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(mylogit)), direction="both")
summary(mylogit)
summary(backwards)
summary(forwards)
summary(bothways)

gg<-predict(bothways, xTest, type="response")
h<- data.frame(gg)
j<- round(h,0)
m<- cbind(j, test[,11])
u<- data.frame(m)
#u<- cbind(u, user_beh_final[,11])
colnames(u)<- c("pred","target")
#U<-ifelse(u$target==u$pred,1,0)
#sort(U)
cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1

plot(mylogit,which=1)
plot(predict(mylogit),residuals(mylogit),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")
lines(lowess(predict(mylogit),residuals(mylogit)),col="black",lwd=2)

##Logistic regression: Demographic data
n <- names(user_demo_final[,-1])
n
f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))

user_demo_final$did_smoke<- as.factor(user_demo_final$did_smoke)
sub = sample(nrow(user_demo_final), floor(nrow(user_demo_final) * 0.6))
train = user_demo_final[sub,]
test = user_demo_final[-sub,]


xTrain = train[,-1]
yTrain = train$did_smoke

xTest = test[,-c(1,11)]
yTest = test$did_smoke

mylogit <- glm(f, xTrain, family = "binomial")
nothing<- glm( did_smoke ~ 1 ,data = user_demo_final[,-1], family = "binomial" )
backwards<- step(mylogit)
forwards <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(mylogit)), direction="forward")
bothways <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(mylogit)), direction="both")
summary(mylogit)
summary(backwards)
summary(forwards)
summary(bothways)

gg<-predict(bothways, xTest, type="response")
h<- data.frame(gg)
j<- round(h,0)
m<- cbind(j, test[,2])
u<- data.frame(m)
#u<- cbind(u, user_beh_final[,2])
colnames(u)<- c("pred","target")
#U<-ifelse(u$target==u$pred,1,0)
#sort(U)
cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1

plot(mylogit,which=1)
plot(predict(mylogit),residuals(mylogit),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")
lines(lowess(predict(mylogit),residuals(mylogit)),col="black",lwd=2)

## Decision Trees

user_beh_final$did_smoke<- as.factor(user_beh_final$did_smoke)
sub = sample(nrow(user_beh_final), floor(nrow(user_beh_final) * 0.6))
train = user_beh_final[sub,]
test = user_beh_final[-sub,]


xTrain = train[,-1]
yTrain = train$did_smoke

xTest = test[,-c(1,11)]
yTest = test$did_smoke

fit<-rpart(did_smoke~., data=xTrain, method="class", minsplit= 2, minbucket = 1)
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"] )
fancyRpartPlot(pfit)
print(fit)
print(pfit)
summary(fit)
summary(pfit)
pred <- predict(fit, newdata = xTest, type= "class")
pred1<- predict(pfit, newdata = xTest, type= "class")
cm1<-confusionMatrix(pred, test$did_smoke)
cm2<-confusionMatrix(pred1, test$did_smoke)

#demograpic data
user_demo_final$did_smoke<- as.factor(user_demo_final$did_smoke)
sub = sample(nrow(user_demo_final), floor(nrow(user_demo_final) * 0.6))
train = user_demo_final[sub,]
test = user_demo_final[-sub,]


xTrain = train[,-1]
yTrain = train$did_smoke

xTest = test[,-c(1,2)]
yTest = test$did_smoke

fit<-rpart(did_smoke~., data=xTrain, method="class", minsplit= 2, minbucket = 1)
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"] )
summary(pfit)
fancyRpartPlot(pfit)
pred <- predict(fit, newdata = xTest, type= "class")
pred1<- predict(pfit, newdata = xTest, type= "class")
confusionMatrix(pred, test$did_smoke)
summary(fit)


# Random Forests
user_beh_final$did_smoke<- as.factor(user_beh_final$did_smoke)
sub = sample(nrow(user_beh_final), floor(nrow(user_beh_final) * 0.6))
train = user_beh_final[sub,]
test = user_beh_final[-sub,]


xTrain = train[,-c(1,8)]
yTrain = train$did_smoke

xTest = test[,-c(1,11,8)]
yTest = test$did_smoke

user_beh_final$did_smoke<-as.factor(user_beh_final$did_smoke)
n <- names(user_beh_final[,-c(1,8)])
n


f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))
fit1<- randomForest(f, data= xTrain, importance= TRUE,proximity= TRUE, ntree=100, forest= TRUE, mtry= 3)
fit1
pred<-predict(fit1, xTest)
#pred


#confusionMatrix(fit1, test$did_smoke)
bb<- data.frame(pred)
mm<- cbind(bb,test$did_smoke)
mm<- data.frame(mm)
colnames(mm)<- c("pred","target")
U<-ifelse(mm$target==mm$pred,1,0)
#D<-ifelse(mm$fit1.predicted==mm$target,1,0)
cm1 <- confusionMatrix(mm$pred, mm$target, positive="0")
cm1
varImpPlot(fit1)
MDSplot(fit1, user_beh_final$did_smoke)

X<- user_beh_final[,-c(1,11)]
target<- user_beh_final[,11]
treeList <- RF2List(fit1)
exec <- extractRules(treeList,X)
exec[1:200,]
ruleMetric <- getRuleMetric(exec,X,target)
ruleMetric[1:2,]
ruleMetric <- pruneRule(ruleMetric,X,target)
ruleMetric[1:5,]
ruleMetric <- selectRuleRRF(ruleMetric,X,target)
ruleMetric
learner <- buildLearner(ruleMetric,X,target)
learner
readableRules <- presentRules(ruleMetric,colnames(X))  
readableRules[1:5,]

#random forests: demogpraphic data
user_demo_final$did_smoke<- as.factor(user_demo_final$did_smoke)
sub = sample(nrow(user_demo_final), floor(nrow(user_demo_final) * 0.6))
train = user_demo_final[sub,]
test = user_demo_final[-sub,]


xTrain = train[,-1]
yTrain = train$did_smoke

xTest = test[,-c(1,2)]
yTest = test$did_smoke

user_beh_final$did_smoke<-as.factor(user_beh_final$did_smoke)
n <- names(user_demo_final[,-1])
n


f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))
fit1<- randomForest(f, data= xTrain, importance= TRUE,proximity= TRUE, ntree=80, forest= TRUE, mtry= 6)
fit1
pred<-predict(fit1, xTest)
#pred
#printcp(pfit)

#confusionMatrix(fit1, test$did_smoke)
bb<- data.frame(pred)
mm<- cbind(bb,test$did_smoke)
mm<- data.frame(mm)
colnames(mm)<- c("pred","target")
U<-ifelse(mm$target==mm$pred,1,0)
#D<-ifelse(mm$fit1.predicted==mm$target,1,0)
cm1 <- confusionMatrix(mm$pred, mm$target, positive="0")
cm1
X<- user_demo_final[,-c(1,2)]
target<- user_demo_final[,2]
treeList <- RF2List(fit1)
exec <- extractRules(treeList,X)
exec[1:200,]
ruleMetric <- getRuleMetric(exec,X,target)
ruleMetric[1:2,]
ruleMetric <- pruneRule(ruleMetric,X,target)
ruleMetric[1:5,]
ruleMetric <- selectRuleRRF(ruleMetric,X,target)
ruleMetric
learner <- buildLearner(ruleMetric,X,target)
learner
readableRules <- presentRules(ruleMetric,colnames(X))  
readableRules[1:5,]

#ANN
user_beh_final$did_smoke<-as.numeric(as.character(user_beh_final$did_smoke))
set.seed(123456)
sub = sample(nrow(user_beh_final), floor(nrow(user_beh_final) * 0.6))
train = user_beh_final[sub,]
test = user_beh_final[-sub,]

test1<- test[,-1]


n <- names(user_beh_final[,-1])
f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))
f


nest1<- neuralnet( f ,  train[,-1], hidden=5, rep= 10, algorithm= "rprop+", learningrate.limit = NULL,
                   learningrate= 0.01, learningrate.factor = list(minus = 0.5, plus = 1.2),
                   threshold = 0.01, act.fct= "logistic" )
#plot.nn(nest1, rep= "best")
#gg<-data.frame(prediction(nest1, xTest))
#plotnet(nest1)
gg<- data.frame(compute(nest1,test1[,-10]))
print(nest1)
h<- gg[,19]
a<- data.frame(h)
j<- round(a,0)
m<- cbind(j, test[,11])
u<- data.frame(m)
#u<- cbind(u, user_beh_final[,11])
colnames(u)<- c("pred","target")
#U<-ifelse(u$target==u$pred,1,0)
#sort(U)
u$pred[u$pred== -1 ] <- 0
u$pred[u$pred== -2 ] <- 0
u$pred[u$pred>= 2 ] <- 1
cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1
neuralweights(nest1)
names(user_beh_final[,-1])<-'y'
y<-names(user_beh_final[,-1])
par(mar=c(3,4,1,1),family='serif', las =1)
gg<- gar.fun(y,nest1)
plot(gg)
f<-gg$data

## Deographic data
user_demo_final$did_smoke<-as.numeric(as.character(user_demo_final$did_smoke))
set.seed(123456)
sub = sample(nrow(user_demo_final), floor(nrow(user_demo_final) * 0.6))
train = user_demo_final[sub,]
test = user_demo_final[-sub,]

test1<- test[,-1]


n <- names(user_demo_final[,-1])
f <- as.formula(paste("did_smoke ~", paste(n[!n %in% "did_smoke"], collapse = " + ")))
f


nest1<- neuralnet( f ,  train[,-1], hidden=7, rep= 20, algorithm= "rprop+", learningrate.limit = NULL,
                   learningrate= 0.01,  learningrate.factor = list(minus = 0.5, plus = 1.2),
                   threshold = 0.01, act.fct= "logistic", )
#plot.nn(nest1, rep= "best")
#gg<-data.frame(prediction(nest1, xTest))
gg<- data.frame(compute(nest1,test1[,-2]))
print(nest1)
h<- gg[,41]
a<- data.frame(h)
j<- round(a,0)
m<- cbind(j, test[,2])
u<- data.frame(m)
#u<- cbind(u, user_beh_final[,11])
colnames(u)<- c("pred","target")
#U<-ifelse(u$target==u$pred,1,0)
#sort(U)
u$pred[u$pred== -1 ] <- 0
u$pred[u$pred== -2 ] <- 0
u$pred[u$pred>= 2 ] <- 1

cm1 <- confusionMatrix(u$pred, u$target, positive="0")
cm1
names(user_demo_final[,-1])<-'y'
y<-names(user_demo_final[,-1])
par(mar=c(3,4,1,1),family='serif', las =1)
gg<- gar.fun(y,nest1)
plot(gg)
f<-gg$data
