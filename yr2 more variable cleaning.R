# year 2 variable recoding - incentives, class start time, field, advanced stats degree, post test location
data1516 <- read.csv("Final 15.16.csv")

### recoding incentives 
# high stakes - assignment points, quiz points
# low stakes - exam, extra credit
data1516$incentive.pre.bin <- NULL
data1516$incentive.post.bin <- NULL
for (i in 1:nrow(data1516)) {
  if (data1516$incentive.pre.ind[i] %in% c("assignment points", "quiz points")) {data1516$incentive.pre.bin[i] <- "Low stakes"}
  else if (data1516$incentive.pre.ind[i] %in% c("extra credit")) {data1516$incentive.pre.bin[i] <- "High stakes"}
  else {data1516$incentive.pre.bin[i] <- "None"} # includes required 
  
  if (data1516$incentive.post.ind[i] %in% c("HW points", "quiz points")) {data1516$incentive.post.bin[i] <- "Low stakes"}
  else if (data1516$incentive.post.ind[i] %in% c("extra credit", "exam")) {data1516$incentive.post.bin[i] <- "High stakes"}
  else {data1516$incentive.post.bin[i] <- "None"}
}
table(data1516$incentive.pre.bin)
table(data1516$incentive.post.bin)

### recoding class start time 
# 1 (7-9am)
# 2 (10-11am)
# 3 (12-1pm)
# 4 (2pm and later)
data1516$time.meet.new <- NULL
data1516$time.meet <- as.character(data1516$time.meet)
for (i in 1:nrow(data1516)) {
  if (data1516$time.meet[i]=="") {data1516$time.meet.new[i] <- NA}
  
  else if (data1516$time.meet[i]=="7:10 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="7:20 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="7:50 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8-9:20 a.m.") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8:00 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8:00am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8:05 am - 9:50 am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8:10 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8:30 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="8am-9:20am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:00") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:00 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:00 am - 10:20 am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:05 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:10 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:15 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:20 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:25 - 10:40 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:25 am - 10:40 am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:25-10:40") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:30 - 10:50 am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:30 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9:30am-10:50am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="09:00 AM for M,W 10:00 AM for Th") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="9am Tuesdays and Thursdays, 10am Fridays") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="Lecture 8am, lab 10am") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="Sections 1-4   9:05 AM") {data1516$time.meet.new[i]<-1}
  else if (data1516$time.meet[i]=="Lecture and lab 8am") {data1516$time.meet.new[i]<-1}
  
  else if (data1516$time.meet[i]=="10:00 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:05 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:10") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:10 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:30 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:50 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10:55 am - 12:15 pm") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="10am") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00 - 12:15 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00 a.m - 11:50 a.m") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00 a.m.") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:00 am - 12 :20 pm") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:10 - 12:25") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:10am - 12:20 am") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:30 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:30 PM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:30-12:30") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="11:50 AM") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="from 11:00 am - 12:20 pm") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="Lecture 11am, lab 12:10pm") {data1516$time.meet.new[i]<-2}
  else if (data1516$time.meet[i]=="Mon and Wed 11:00-12:20, Fri 11:00-11:50") {data1516$time.meet.new[i]<-2}
  
  else if (data1516$time.meet[i]=="01:10PM - 03:35PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:00") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:00 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:00pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:10 - 2:30 pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:10 AM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:10 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:10 PM Tuesday and Thursday, 12:10 Friday") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:15 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:25pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:30 AM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="1:40 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:00") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:00 AM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:00 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:00 pm - 1:20 pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:00pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:10 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:15") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:15 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:15 pm - 1:45 pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:20 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:30 PM") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:30pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:45 - 2:00") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="12:45 pm - 2:00 pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="from 12:30 pm - 1:50 pm") {data1516$time.meet.new[i]<-3}
  else if (data1516$time.meet[i]=="M, 12:10 pm; W, 10:50 am, Th, 12:10 pm, Fri, 12:55 pm") {data1516$time.meet.new[i]<-3}
  
  else if (data1516$time.meet[i]=="2:00 - 3:20 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:00 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:00-3:15") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:10 - 2:00 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:10 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:10PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="2:40 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Lecture 2:10pm, lab 12:10pm") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Lecture and lab 2:10pm") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Lecture: 2:10PM-3:00PM; Lab: 2:10PM-4:00PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Mon and Wed 2:00-3:20, Thur 3:00-3:50") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Mon and Wed 2:30pm; Thu 1pm") {data1516$time.meet.new[i]<-4}
  
  else if (data1516$time.meet[i]=="3:00") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:00 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:05 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:05 to 4:20") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:10 - 4:00pm") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:10 - 5:00 pm") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:10 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="03:45PM - 06:10PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="3:55 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="4:00 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="4:10 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="Mon and Wed 3:30-4:50, Thur 4:00-4:50") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="6:10 PM") {data1516$time.meet.new[i]<-4}
  else if (data1516$time.meet[i]=="6:10pm") {data1516$time.meet.new[i]<-4}
  
  else if (data1516$time.meet[i]=="M, 8:30 am; W, 9:40am; Th, 1:45 pm; Fri, 10:40 am") {data1516$time.meet.new[i]<-NA}
  else if (data1516$time.meet[i]=="Online") {data1516$time.meet.new[i]<-NA}
  else if (data1516$time.meet[i]=="times are not the same through the week") {data1516$time.meet.new[i]<-NA}
  else if (data1516$time.meet[i]=="Times varied from 8:15-3:10 on a rotation") {data1516$time.meet.new[i]<-NA}
  else if (data1516$time.meet[i]=="varied depending on day, some days 11 AM other days 2 PM") {data1516$time.meet.new[i]<-NA}
  
  else {data1516$time.meet.new[i]<-NA}
}
table(data1516$time.meet.new)

### recoding field (major)
# 1 - social sciences 
# 2 - natural and applied sciences
# 3 - liberal arts
# 4 - undeclared
# NA includes not in college, = 0, = 6 (not sure what these represent)

## if field_pre is missing, then recode field_pre as pre_post (if available)
sum(is.na(data1516$field_pre))
sum(is.na(data1516$field_post))
sum(is.na(data1516$field_pre & is.na(data1516$field_post)))
for (i in 1:nrow(data1516)) {
  if (is.na(data1516$field_pre[i])) {data1516$field_pre[i] <- data1516$field_post[i]}
}

# recoding 1:4
data1516$field_pre_new <- NULL
data1516$field_post_new <- NULL
for (i in 1:nrow(data1516)) {
  # what is field_pre = 0?
  if (is.na(data1516$field_pre[i])==1) {data1516$field_pre_new[i] <- NA}
  else if (data1516$field_pre[i] == 0) {data1516$field_pre_new[i] <- NA} 
  else if (data1516$field_pre[i] == 1) {data1516$field_pre_new[i] <- 1}
  else if (data1516$field_pre[i] == 2) {data1516$field_pre_new[i] <- 2}
  else if (data1516$field_pre[i] == 3) {data1516$field_pre_new[i] <- 3}
  else if (data1516$field_pre[i] == 4) {data1516$field_pre_new[i] <- 4}
  else if (data1516$field_pre[i] == 5) {data1516$field_pre_new[i] <- NA}
  
  # what is field_post = 0? 6?
  if (is.na(data1516$field_post[i])==1) {data1516$field_post_new[i] <- NA}
  else if (data1516$field_post[i] == 0) {data1516$field_post_new[i] <- NA}
  else if (data1516$field_post[i] == 1) {data1516$field_post_new[i] <- 1}
  else if (data1516$field_post[i] == 2) {data1516$field_post_new[i] <- 2}
  else if (data1516$field_post[i] == 3) {data1516$field_post_new[i] <- 3}
  else if (data1516$field_post[i] == 4) {data1516$field_post_new[i] <- 4}
  else if (data1516$field_post[i] == 5) {data1516$field_post_new[i] <- NA}
}
table(data1516$field_pre_new)
table(data1516$field_post_new)

## check if field_pre matches field_post    
sum(data1516$field_pre != data1516$field_post, na.rm=T) #699
# data1516[!is.na(data1516$field_pre) & !is.na(data1516$field_post) & (data1516$field_pre != data1516$field_post), c("field_pre","field_post")]

## check if field_pre_new matches field_post_new 
sum(data1516$field_pre_new != data1516$field_post_new, na.rm=T) #468
# data1516[!is.na(data1516$field_pre_new) & !is.na(data1516$field_post_new) & (data1516$field_pre_new != data1516$field_post_new), c("field_pre_new","field_post_new")]

## if field_pre_new = 4 (undeclared), then replace with field_post_new (if not 4)
sum(data1516$field_pre_new == 4 & data1516$field_post_new != 4, na.rm=T)
data1516$field_final <- NULL
for (i in 1:nrow(data1516)) {
  if (is.na(data1516$field_pre[i])==1) {data1516$field_final[i] <- NA}
  else if (data1516$field_pre[i] == 4) {data1516$field_final[i] <- data1516$field_post_new[i]}
  else {data1516$field_final[i] <- data1516$field_pre_new[i]}
}
table(data1516$field_final)

### recoding advanced stats degree
# 1 - Yes (including other subject, masters, phd)
# 0 - No
table(data1516$advanced.stats.degree)
data1516$advanced.stats.degree <- as.character(data1516$advanced.stats.degree)
data1516$advanced.stats.degree.bin <- NULL
for (i in 1:nrow(data1516)) {
  if (is.na(data1516$advanced.stats.degree[i]) == 1) {data1516$advanced.stats.degree.bin[i] <- NA}
  else if (data1516$advanced.stats.degree[i] %in% c("Yes","Masters","PhD","PhD math")) {data1516$advanced.stats.degree.bin[i] <- 1}
  else if (data1516$advanced.stats.degree[i] == "No") {data1516$advanced.stats.degree.bin[i] <- 0}
  else {data1516$advanced.stats.degree.bin[i] <- NA}
}
table(data1516$advanced.stats.degree.bin)

### recoding post test location 
# 1 - In class (including Attitudes out of class)
# 0 - Out of class
table(data1516$admin.post.location)
data1516$admin.post.location.bin <- NULL
for (i in 1:nrow(data1516)) {
  if (is.na(data1516$admin.post.location[i]) == 1) {data1516$admin.post.location.bin[i] <- NA}
  else if (data1516$admin.post.location[i] == "In class") {data1516$admin.post.location.bin[i] <- "1"}
  else if (data1516$admin.post.location[i] == "Out of class") {data1516$admin.post.location.bin[i] <- "0"}
  else {data1516$admin.post.location.bin[i] <- NA}  
}
table(data1516$admin.post.location.bin)

## "BurowSection1_spring" did not give
data1516[data1516$admin.post.location=="Did not give","InstructorName_Section_Semester"]
# remove them
data1516 <- data1516[-which(data1516$InstructorName_Section_Semester %in% c("BurowSection1_spring")),]
write.csv(data1516, file = "FinalV2 1516.csv")
