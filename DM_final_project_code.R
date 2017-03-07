#Data Mining Final Project

#Name: Yifeng Song
#computing ID: ys8mz



#####################################################################
library(stringr)
library(tm)
library(topicmodels)
library(SnowballC)
library(slam)
library(lattice)
library(reshape2)



email.input<-read.csv("Emails.csv",header=T)
email.input$RawText <- as.character(email.input$RawText)
email.input$MetadataTo <- as.character(email.input$MetadataTo)
email.input$MetadataFrom <- as.character(email.input$MetadataFrom)





######################################################################
###Sort the email list
######################################################################
#find all emails containing "state.gov" or "stategov"
is_official_address <- rep(F,nrow(email.input))
for (i in 1:nrow(email.input)) {
  if (grepl("state.gov",email.input$RawText[i])==T) {
    is_official_address[i] <- T
  }
  if (grepl("stategov",email.input$RawText[i])==T) {
    is_official_address[i] <- T
  }
}

#Find all emails that contain the name "Sid"
is_Sid <- rep(F,nrow(email.input))
for (i in 1:nrow(email.input)) {
  if (grepl("Sid[^aeiou]",email.input$RawText[i])==T) {
    is_Sid[i] <- T
  }
}

#For the emails that do not contain "state.gov" or "stategov",
#they can be divided into two groups, one is with "Sid" in them
#and the other is without "Sid" in them
interesting_email_indx <- which(is_official_address == F & is_Sid == T)
non_official_address_non_Sid <- which(is_official_address == F & is_Sid == F)
official_address <- which(is_official_address==T)

#reorganize the data frame such that the first portion of the data frame are all emails
#containing "state.gov" or "stategov", the second portion are all emails without state.gov
#and without "Sid", the third portion are all emails without state.gov but with "Sid"
email.input.new <- email.input[c(official_address,non_official_address_non_Sid,interesting_email_indx),]
length(official_address) #6549
length(non_official_address_non_Sid) #1037
length(interesting_email_indx) #359
#so 1-6549 are emails having state.gov addresses, 6550-7586 are emails without
#state.gov and without "Sid" in it, 7587-7945 are emails without state.gov but
#with "Sid" in it. 

#save(email.input.new, file="Emails_sorted.RData")





######################################################################
###Initial Text Cleaning
######################################################################
cleaned.email <- rep("",nrow(email.input.new))

for (i in 1:nrow(email.input.new)) {
  text <- email.input.new$RawText[i]
  indx <- str_locate(text,"Subject:")[2] #find the first occurrence of "Subject:" string, any content before it can be removed
  if (is.na(indx) == F) {
    text <- str_sub(text,indx+1,-1)
  }
  text <- gsub("\n"," ",text) #replace all newline character with " "
  #the following strings or regular expressions show up in the email raw texts very often,
  #but they are not the actual contents of the email, so they are replaced by " "
  text <- gsub("UNCLASSIFIED"," ",text) #this is not the content of the email, so replaced by " "
  text <- gsub("Original Message"," ",text)
  text <- gsub("U.S. Department of State"," ",text)
  text <- gsub("Case No. [A-Z0-9-]{10,14}"," ",text)
  text <- gsub("Doc No. [A-Z0-9-]{7,11}"," ",text)
  text <- gsub("Date: [0-9/]{10}"," ",text)
  text <- gsub("Sent:|From:|To:|FW:|Re:|state.gov|stategov|Attachments:|Cc:"," ",text,ignore.case=T)
  text <- gsub("hrod17@clintonemail.com"," ",text,ignore.case = T)
  #check the sender's and recipient's email address in the data frame,
  #if they do exist, the strings for those email addresses can be removed from the raw texts of the emails
  if (str_length(email.input.new$MetadataTo[i])>5) {
    text <- gsub(email.input.new$MetadataTo[i]," ",text,ignore.case = T)
  }
  if (str_length(email.input.new$MetadataFrom[i])>5) {
    text <- gsub(email.input.new$MetadataFrom[i]," ",text,ignore.case = T)
  }
  #any long string which contains all upper case letters or special characters are not likely
  #to be the contents of the email, so they are all searched and removed
  text <- gsub("[0-9A-Z. @!?><%*$#:;/\\\'\"&_-]{15,} "," ",text)
  #store the cleaned email texts in a vector
  cleaned.email[i] <- text
}

email.input.new$CleanedText <- cleaned.email #add the cleaned email texts to the data frame

#save(email.input.new, file="Emails_sorted.RData")





######################################################
#####NLP/Set up document term matrix construction#####
######################################################

#extract/clean subjects into separate corpus
email.docs<-data.frame(email.input.new$CleanedText)
email.docs<-VCorpus(DataframeSource(email.docs))

###NLP################################################

#replace all special characters and numbers by " " (space):
Replace1 <- function(x,pattern) {
  gsub(pattern, " ",x)
}
email.clean <- tm_map(email.docs,content_transformer(Replace1),"[^a-zA-Z]")
email.clean<-tm_map(email.clean,stripWhitespace) #remove extra whitespace

email.clean<-tm_map(email.clean,removeWords,stopwords("english")) #remove stopwords using the built-in stop words list
#using an additional stop words list:
stop.words.2 <- read.table("stopwords.txt",header=F)
stop.words.2 <- as.vector(stop.words.2$V1)
email.clean<-tm_map(email.clean,removeWords,stop.words.2)
email.clean<-tm_map(email.clean,stripWhitespace) #need to remove extra whitespace again

#remove all 1-character "words":
email.clean<-tm_map(email.clean,removeWords,"[a-zA-Z]{1}")
#remove all 2-character "words" that do not start with a capital letter:
email.clean<-tm_map(email.clean,removeWords,"[a-z]{2}")
email.clean<-tm_map(email.clean,stripWhitespace) #need to remove extra whitespace again

#stem all words
email.clean<-tm_map(email.clean,stemDocument)

#remove possibly empty documents (emails)
removeEmptyStrings <- function(x) {
  x[!x == ""]
}
email.clean<- tm_map(email.clean, content_transformer(removeEmptyStrings))



#Read in the word dictionary (109,582 words in total)
word.dict <- read.table("wordsEn.txt",header=F)
word.dict <- as.vector(word.dict$V1)

word.dict <- wordStem(word.dict) # stem all words in the dictionary first
word.dict <- word.dict[which(!duplicated(word.dict))] # remove the duplicates



#compute term_frequency (tf) matrix, do not transform the upper case letters into lower case letters
#in this step
email.clean.tf<- DocumentTermMatrix(email.clean, control = list(weighting = weightTf,
                                                                tolower=F))

#remove all "words" that do not start with a capital letter and which do not exist
#in the word dictionary
names <- colnames(email.clean.tf)
is_word_or_not <- rep(T,length(names))
for (i in 1:length(names)) {
  if (grepl("[a-z]",substr(names[i],1,1))==T) {
    is_word_or_not[i] <- is.element(names[i],word.dict)
  }
}
email.clean.tf <- email.clean.tf[,is_word_or_not]

#remove all "words" which contain 3 same letters in a row:
names_2 <- colnames(email.clean.tf)
is_word_or_not <- rep(T,length(names_2))
for (i in 1:length(names_2)) {
  if (grepl("aaa|bbb|ccc|ddd|eee|fff|ggg|hhh|iii|jjj|kkk|lll|mmm|nnn|ooo|ppp|qqq|rrr|sss|ttt|uuu|vvv|www|xxx|yyy|zzz",names_2[i],ignore.case=T)==T) {
    is_word_or_not[i] <- F
  }
}
email.clean.tf <- email.clean.tf[,is_word_or_not]


#convert all words which start with an upper case letter into lower case,
#check for duplicates and find the indices (column number) of the duplicated words in the
#tf matrix
colnames(email.clean.tf) <- tolower(colnames(email.clean.tf))
names_3 <- colnames(email.clean.tf)
duplicated_name_indx <- which(duplicated(colnames(email.clean.tf)))



#convert the Document Term Frequency matrix into normal matrices (data frames)
#this operation is done in 3 steps, because if directly converting the Document-Term matrix
#into a single normal matrix, the program and exceed the memory limit of the computer
n <- email.clean.tf$nrow

tf_df_1_3000 <- data.frame()
for (i in 1:3000) {
  tf_df_1_3000 <- rbind(tf_df_1_3000,as.vector(email.clean.tf[i,]))
}
#save(tf_df_1_3000,file="tf_df_1_3000.RData")
#rm(tf_df_1_3000)

tf_df_3001_5500 <- data.frame()
for (i in 3001:5500) {
  tf_df_3001_5500 <- rbind(tf_df_3001_5500,as.vector(email.clean.tf[i,]))
}
#save(tf_df_3001_5500,file="tf_df_3001_5500.RData")
#rm(tf_df_3001_5500)

tf_df_5501_7945 <- data.frame()
for (i in 5501:n) {
  tf_df_5501_7945 <- rbind(tf_df_5501_7945,as.vector(email.clean.tf[i,]))
}
#save(tf_df_5501_7945,file="tf_df_5501_7945.RData")
#rm(tf_df_5501_7945)

#combine the duplicate words that arose after words starting with an upper case letter
#are converted into the lower case letter
#load("tf_df_1_3000.RData")
for (i in duplicated_name_indx) {
  name <- names_3[i]
  indx <- which(names_3==name)[1]
  tf_df_1_3000[,indx] <- tf_df_1_3000[,indx]+tf_df_1_3000[,i]
}
tf_df_1_3000 <- tf_df_1_3000[,-duplicated_name_indx]
#save(tf_df_1_3000,file="tf_df_1_3000.RData")
#rm(tf_df_1_3000)

#load("tf_df_3001_5500.RData")
for (i in duplicated_name_indx) {
  name <- names_3[i]
  indx <- which(names_3==name)[1]
  tf_df_3001_5500[,indx] <- tf_df_3001_5500[,indx]+tf_df_3001_5500[,i]
}
tf_df_3001_5500 <- tf_df_3001_5500[,-duplicated_name_indx]
#save(tf_df_3001_5500,file="tf_df_3001_5500.RData")
#rm(tf_df_3001_5500)

#load("tf_df_5501_7945.RData")
for (i in duplicated_name_indx) {
  name <- names_3[i]
  indx <- which(names_3==name)[1]
  tf_df_5501_7945[,indx] <- tf_df_5501_7945[,indx]+tf_df_5501_7945[,i]
}
tf_df_5501_7945 <- tf_df_5501_7945[,-duplicated_name_indx]
save(tf_df_5501_7945,file="tf_df_5501_7945.RData")
rm(tf_df_5501_7945)

#convert the 3 data frames into the simple triplet matrix format, then combine (stack) them
#load("tf_df_1_3000.RData")
#load("tf_df_3001_5500.RData")
#load("tf_df_5501_7945.RData")
names_4 <- names_3[-duplicated_name_indx]
colnames(tf_df_1_3000) <- names_4
colnames(tf_df_3001_5500) <- names_4
colnames(tf_df_5501_7945) <- names_4
dtm1 <- as.simple_triplet_matrix(tf_df_1_3000)
dtm2 <- as.simple_triplet_matrix(tf_df_3001_5500)
dtm3 <- as.simple_triplet_matrix(tf_df_5501_7945)
dtm_1 <- rbind(dtm1,dtm2,dtm3)
#save(dtm_1,file="dtm_1.RData")
#rm(tf_df_1_3000,tf_df_3001_5500,tf_df_5501_7945,dtm1,dtm2,dtm3)





##############################################################
###Remove some columns and rows from the DocumentTermMatrix###
##############################################################
#remove stop words again (some words starting with the capital letter can still be
#stop words):
#stop words from the built-in list (stem them first, then compare them against the colnames)
stopwords_stemmed <- wordStem(stopwords("english"))
stop_word_or_not <- rep(F,length(names_4))
for (i in 1:length(names_4)) {
  if (is.element(names_4[i],stopwords_stemmed)==T) {
    stop_word_or_not[i] <- T
  }
}
dtm_1 <- dtm_1[,!stop_word_or_not]

#remove stop words from the additional list, stem the stop words list first as well
names_5 <- colnames(dtm_1)
stopwords_stemmed_2 <- wordStem(stop.words.2)
stop_word_or_not_2 <- rep(F,length(names_5))
for (i in 1:length(names_5)) {
  if (is.element(names_5[i],stopwords_stemmed_2)==T) {
    stop_word_or_not_2[i] <- T
  }
}
dtm_1 <- dtm_1[,!stop_word_or_not_2]

#save(dtm_1,file="dtm_1.RData")

#check if there are still words that belong to the original stop words list, and remove them
#(if there are words in the corpus that are not properly stemmed)
names_6 <- colnames(dtm_1)
stop_word_or_not <- rep(F,length(names_6))
for (i in 1:length(names_6)) {
  if (is.element(names_6[i],stopwords("english"))==T) {
    stop_word_or_not[i] <- T
  }
}
dtm_1 <- dtm_1[,!stop_word_or_not]

names_7 <- colnames(dtm_1)
stop_word_or_not_2 <- rep(F,length(names_7))
for (i in 1:length(names_7)) {
  if (is.element(names_7[i],stop.words.2)==T) {
    stop_word_or_not_2[i] <- T
  }
}
dtm_1 <- dtm_1[,!stop_word_or_not_2]



#remove words that are months or days of week (not meaningful in topic modeling)
names_8 <- colnames(dtm_1)
is_month_or_day <- rep(F,length(names_8))
for (i in 1:length(names_8)) {
  if (is.element(names_8[i],c("mon","tue","wed","thu","fri","sat","sun","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))==T) {
    is_month_or_day[i] <- T
  }
}
dtm_1 <- dtm_1[,!is_month_or_day]

#save(dtm_1,file="dtm_1.RData")



###caculate column and row sums of term frequency matrix######################################
x <- dtm_1$nrow
y <- dtm_1$ncol
row.counts<-rep.int(0,x)
column.counts<-rep.int(0,y)



# column sums
for (j in 1:y){
  column.counts[j]<-sum(dtm_1[,j])
}

#check the top 2000 words (in terms of total frequency in the entire corpus)
colnames(dtm_1)[order(column.counts,decreasing=T)[1:2000]]
#select the words which are not actually words or not meaningful words from the top 2000 words
#and drop them from the matrix
column.names.drop <- colnames(dtm_1)[order(column.counts,decreasing=T)[c(1, 10, 21, 36, 107, 121, 126, 139, 150, 152, 157, 180, 186, 190, 194, 202, 213, 228, 236, 248, 269, 274, 282, 294, 314, 428, 492, 679,740,776,884,932,986,1048,1064,1159,1641,1805,1938)]]
column.names.drop
# [1] "subject"    "com"        "clinton"    "hdr"        "abedinh"    "hillari"    "friday"    
# [8] "monday"     "septemb"    "fyi"        "thursday"   "tuesday"    "wednesday"  "sunday"    
# [15] "millscd"    "august"     "june"       "januari"    "novemb"     "october"    "decemb"    
# [22] "april"      "hrc"        "saturday"   "juli"       "abl"        "februari"   "sullivanjj"
# [29] "sbu"        "mcchrystal" "mrs"        "wjc"        "ntc"        "ops"        "cdm"       
# [36] "tnc"        "koh"        "fwd"        "lmb"   
dtm_1 <- dtm_1[,!sapply(colnames(dtm_1),is.element,column.names.drop)]



# row sums
for (i in 1:x){
  row.counts[i]<-sum(dtm_1[i,])
}
email.indx <- (1:(dtm_1$nrow))[row.counts!=0] #remove all empty emails

#empty email indices
which(row.counts==0)
# [1]  850 1005 1145 1280 1283 1390 1455 1752 2188 2526 2569 2570 2816 3135 3202 3279 3489 3687
# [19] 3856 3862 4004 4025 4223 4303 4951 5306 5340 5782 5873 6134 6308 6327 6431 6556 6660 6689
# [37] 6780 6827 6843 6855 6881 6884 6888 6896 6967 7145 7177 7272 7344 7352 7372 7381 7406 7407
# [55] 7415 7416 7472

which(email.indx==6549)
#so emails that contain "state.gov" are 1-6516 in the new matrix
which(email.indx==7586)
#so emails that do not contain "state.gov" and do not contain "Sid" are 6517-7529;
#emails that do not contain "state.gov" but contain "Sid" are 7530-7888; in the new matrix

#final Document-Term matrix
dtm <- dtm_1[which(row.counts!=0),] #remove the empty documents



#save(dtm,file="dtm.RData")
#save(email.indx,file="email_indx.RData") #email.indx maps the row index of each email in dtm to the row index of the email in the email.input.new





###########################################################################
###Topic modeling using LDA
###########################################################################
set.seed(1)
topic.model.100 <- LDA(dtm,100)
#save(topic.model.100,file="topic.model.100.RData")





############################################################################
###Clustering Analysis
############################################################################
# cluster documents in topic space (50 clusters)
document.topic.probabilities = topic.model.100@gamma  # topic distribution for each document
topic.space.kmeans.clusters = kmeans(document.topic.probabilities, 50)
#email.list contains all non-empty emails after text cleaning
email.list <- email.input.new$RawText[email.indx]
topic.space.clustered.emails = split(1:length(email.list), topic.space.kmeans.clusters$cluster)
#get a list containing the email indices within each cluster (1-50)



#convert the list "topic.space.clustered.emails" into a data frame for easier manipulation
email.cluster<-data.frame()
n<-length(topic.space.clustered.emails)
#loop through each cluster
for (i in 1:n){
  x<-data.frame(topic.space.clustered.emails[[i]])
  names(x)<-c('email_index')
  x$cluster<-i
  email.cluster<-rbind.fill(email.cluster,x)
}

#emails that contain "state.gov" are 1-6516 in the new matrix (dtm),
#emails that do not contain "state.gov" and do not contain "Sid" are 6517-7529;
#emails that do not contain "state.gov" but contain "Sid" are 7530-7888
#add the column "email_type" to the email.cluster dataframe to indicate the category
#that each email belongs to
email.cluster$email_type<-"contain_state.gov"
email.cluster$email_index<-as.numeric(email.cluster$email_index)
email.cluster$email_type[6516<email.cluster$email_index & email.cluster$email_index<7530]<-"dont_contain_state.gov_or_Sid"
email.cluster$email_type[7529<email.cluster$email_index]<-"dont_contain_state.gov_but_Sid"

#get frequency table for the 3 email categories, convert to data frame:
flag.summary<-table(email.cluster$cluster,email.cluster$email_type)
a<-flag.summary[,1]
b<-flag.summary[,2]
c<-flag.summary[,3]
flag.summary<-data.frame(cbind(a,b,c))
names(flag.summary)<-c("contain_state.gov","dont_contain_state.gov_but_Sid","dont_contain_state.gov_or_Sid")
#add another column that contains the total number of non_gov emails in each cluster
flag.summary$total_non_gov<-flag.summary$dont_contain_state.gov_or_Sid+flag.summary$dont_contain_state.gov_but_Sid
flag.summary$ratio<-(flag.summary[,2]+flag.summary[,3])/flag.summary[,1]
#0.1 is the threshold for the non_gov to gov emails ratio,
#the clusters with the ratio below 0.1 are considered as "bad" clusters,
#and the non_gov emails in those clusters will be read and inspected by us
bad_clusters <- as.numeric(rownames(flag.summary[flag.summary$ratio<=0.1,]))
bad_clusters
# the bad clusters:
# [1] 1  2 11 12 13 20 29 36 37 39 45 49 50

#extract the raw texts of all "bad" emails from the email.list
bad_emails_indx <- email.cluster[email.cluster$cluster %in% bad_clusters & email.cluster$email_type!="contain_state.gov",]$email_index
bad_emails <- email.list[bad_emails_indx] 

#save all "bad" emails in a data frame and output as .csv file
bad_emails_df <- as.data.frame(cbind(bad_emails_indx,bad_emails))
colnames(bad_emails_df) <- c("index","RawText")
bad_emails_df <- bad_emails_df[order(bad_emails_df$index),]
#write to .csv
write.csv(bad_emails_df,"bad_emails_df.csv",row.names = F)





###########################################################################################
#####Create bar plot for email clusters, each cluster is divided into 3 email categories###
###########################################################################################
flags<-flag.summary[,1:3]
flags$id<-rownames(flags)
#write a function to add an additional "0" in front of eachs single digit number
add.zero <- function(x) {
  if (str_length(x)<2) {
    return(paste0("0",x))
  }
  else {
    return(x)
  }
}
flags$id <- sapply(flags$id,add.zero)
flags<-melt(flags)

barchart(value~id,data=flags,groups=variable,
         ylab="Count", xlab="Cluster", main ="Email Types by Cluster",
         auto.key=list(space='right'), scales=list(x=list(rot=45)),
         par.settings=list(superpose.polygon=list(col=c('darkgreen','yellow','red'))))





#########################################################################
###Get the data ready for building the visualization in Shiny
#########################################################################
top.two.topics <- topics(topic.model.100,2)
top.two.topics <- t(top.two.topics)
colnames(top.two.topics) <- c("first","second")
top.two.topics <- as.data.frame(top.two.topics)
email.cluster <- email.cluster[,c(1,2)]
email.cluster$first <- top.two.topics$first[email.cluster$email_index]
email.cluster$second <- top.two.topics$second[email.cluster$email_index]
#The "first" column of email.cluster contains the most likely topic for each email,
#the "second" column of email.cluster contains the second likely topic for each email
#save(email.cluster,file="email_cluster.RData")

#save(document.topic.probabilities,file="document_topic_probabilities.RData")

#compute the topic_word_matrix as a result of LDA topic modeling
topic_word_matrix <- posterior(topic.model.100)$terms
#save(topic_word_matrix,file="topic_word_matrix.RData")

#Get the list of emails that the cluster each email corresponds to
email.cluster.1 <- email.cluster[,c(1,2)]
#save(email.cluster.1,file="email_cluster_1.RData")