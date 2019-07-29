# year 3 variable recoding - incentives, class start time, field, advanced stats degree, post test location
data1617 <- read.csv("Final 16.17.csv")

### recoding incentives 
# high stakes - assignment points, quiz points
# low stakes - exam, extra credit
data1617$incentive.pre.bin <- NULL
data1617$incentive.post.bin <- NULL
for (i in 1:nrow(data1617)) {
  if (data1617$incentive.pre.ind[i] %in% c("assignment points", "quiz points")) {data1617$incentive.pre.bin[i] <- "Low stakes"}
  else if (data1617$incentive.pre.ind[i] %in% c("extra credit")) {data1617$incentive.pre.bin[i] <- "High stakes"}
  else {data1617$incentive.pre.bin[i] <- "None"}
  
  if (data1617$incentive.post.ind[i] %in% c("assignment points", "quiz points")) {data1617$incentive.post.bin[i] <- "Low stakes"}
  else if (data1617$incentive.post.ind[i] %in% c("extra credit", "exam")) {data1617$incentive.post.bin[i] <- "High stakes"}
  else {data1617$incentive.post.bin[i] <- "None"}
}
table(data1617$incentive.pre.bin)
table(data1617$incentive.post.bin)

### recoding class start time 
# 1 (8-9am)
# 2 (10-11am)
# 3 (12-2pm)
# 4 (after 2pm)
data1617$time.meet <- as.character(data1617$time.meet)
data1617$time.meet.new <- NULL
for (i in 1:nrow(data1617)) {
  if (is.na(data1617$time.meet[i])==1) {data1617$time.meet.new[i] <- NA}
  else if (data1617$time.meet[i]=="") {data1617$time.meet.new[i]<-NA}
  
  else if (data1617$time.meet[i]=="8:00") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="8:00 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="8:00am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="8:10-9:10am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="8:35") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:00 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:00am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:15-10:20 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:25") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:25 - 10:40") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:25 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:30 - 10:50") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:30 - 10:50 am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:30 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:30-11 a.m.") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:40 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:40am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9:45 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="9am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="Lecture 9am; Lab 10am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="MW 8 - 9.20am, F 8.30-9.20am") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="section 8 met at 8:00 AM") {data1617$time.meet.new[i]<-1}
  else if (data1617$time.meet[i]=="Lecture 9:05 AM Mon and Wed/Hourly Lab 8:00 AM - noon by section)") {data1617$time.meet.new[i]<-1}
  
  else if (data1617$time.meet[i]=="10:50AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:00 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:05 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:20 - 11:10 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:20-11:20") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:30 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:50 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10:50am") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="10AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11:00") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11:00 a.m") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11:00 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11:30 AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11:40am") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="11AM") {data1617$time.meet.new[i]<-2}
  else if (data1617$time.meet[i]=="Lecture 11am; Lab 12:10pm") {data1617$time.meet.new[i]<-2}
  
  else if (data1617$time.meet[i]=="1-2:30pm") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:00 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:10") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:10 - 2:30 pm") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:10 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:15 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:25 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="1:50") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12 noon") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:00 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:00pm") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:15 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:30 PM") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:30pm") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="12:50") {data1617$time.meet.new[i]<-3}
  else if (data1617$time.meet[i]=="Noon-1:20") {data1617$time.meet.new[i]<-3}
  
  else if (data1617$time.meet[i]=="2:00 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="2:10") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="2:10 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="2:30") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="2:30 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="2PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="3:05 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="3:10pm") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="3:30 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="3:45 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="My class met M (2-2:50), W (2-2:50), F(2-3:15). ") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="4:00 PM") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="5:00 p.m.") {data1617$time.meet.new[i]<-4}
  else if (data1617$time.meet[i]=="Section 21 - 6:10") {data1617$time.meet.new[i]<-4}
  
  else if (data1617$time.meet[i]=="MWF") {data1617$time.meet.new[i]<-NA}
  else if (data1617$time.meet[i]=="online") {data1617$time.meet.new[i]<-NA}
}
table(data1617$time.meet.new)

### recoding field (major)
# 1 - social sciences 
# 2 - natural and applied sciences
# 3 - liberal arts
# 4 - undeclared
# NA includes not in college, = 0, = 6 (not sure what these represent)

## if field_pre is missing, then recode field_pre as pre_post (if available)
sum(is.na(data1617$field_pre))
sum(is.na(data1617$field_post))
sum(is.na(data1617$field_pre & is.na(data1617$field_post)))
for (i in 1:nrow(data1617)) {
  if (is.na(data1617$field_pre[i])) {data1617$field_pre[i] <- data1617$field_post[i]}
}

# recoding 1:4
data1617$field_pre_new <- NULL
data1617$field_post_new <- NULL
for (i in 1:nrow(data1617)) {
  # what is field_pre = 0? 6?
  if (is.na(data1617$field_pre[i])==1) {data1617$field_pre_new[i] <- NA}
  else if (data1617$field_pre[i] == 0) {data1617$field_pre_new[i] <- NA} 
  else if (data1617$field_pre[i] == 1) {data1617$field_pre_new[i] <- 1}
  else if (data1617$field_pre[i] == 2) {data1617$field_pre_new[i] <- 2}
  else if (data1617$field_pre[i] == 3) {data1617$field_pre_new[i] <- 3}
  else if (data1617$field_pre[i] == 4) {data1617$field_pre_new[i] <- 4}
  else if (data1617$field_pre[i] == 5) {data1617$field_pre_new[i] <- NA}
  else if (data1617$field_pre[i] == 6) {data1617$field_pre_new[i] <- NA}
  
  # what is field_post = 0? 6?
  if (is.na(data1617$field_post[i])==1) {data1617$field_post_new[i] <- NA}
  else if (data1617$field_post[i] == 0) {data1617$field_post_new[i] <- NA}
  else if (data1617$field_post[i] == 1) {data1617$field_post_new[i] <- 1}
  else if (data1617$field_post[i] == 2) {data1617$field_post_new[i] <- 2}
  else if (data1617$field_post[i] == 3) {data1617$field_post_new[i] <- 3}
  else if (data1617$field_post[i] == 4) {data1617$field_post_new[i] <- 4}
  else if (data1617$field_post[i] == 5) {data1617$field_post_new[i] <- NA}
  else if (data1617$field_post[i] == 6) {data1617$field_pre_new[i] <- NA} 
}
table(data1617$field_pre_new)
table(data1617$field_post_new)

## check if field_pre matches field_post    
sum(data1617$field_pre != data1617$field_post, na.rm=T) #911
# data1617[!is.na(data1617$field_pre) & !is.na(data1617$field_post) & (data1617$field_pre != data1617$field_post), c("field_pre","field_post")]

## check if field_pre_new matches field_post_new 
sum(data1617$field_pre_new != data1617$field_post_new, na.rm=T) #579
# data1617[!is.na(data1617$field_pre_new) & !is.na(data1617$field_post_new) & (data1617$field_pre_new != data1617$field_post_new), c("field_pre_new","field_post_new")]

## if field_pre_new = 4 (undeclared), then replace with field_post_new (if not 4)
sum(data1617$field_pre_new == 4 & data1617$field_post_new != 4, na.rm=T)
data1617$field_final <- NULL
for (i in 1:nrow(data1617)) {
  if (is.na(data1617$field_pre[i])==1) {data1617$field_final[i] <- NA}
  else if (data1617$field_pre[i] == 4) {data1617$field_final[i] <- data1617$field_post_new[i]}
  else {data1617$field_final[i] <- data1617$field_pre_new[i]}
}
table(data1617$field_final)

### recoding advanced stats degree
# 1 - Yes (including other subject, masters, phd)
# 0 - No
data1617$advanced.stats.degree <- as.character(data1617$advanced.stats.degree)
data1617$advanced.stats.degree.bin <- NULL
for (i in 1:nrow(data1617)) {
  if (is.na(data1617$advanced.stats.degree[i]) == 1) {data1617$advanced.stats.degree.bin[i] <- NA}
  else if (stringr::word(data1617$advanced.stats.degree[i], 1) == "Yes") {data1617$advanced.stats.degree.bin[i] <- 1}
  else if (data1617$advanced.stats.degree[i] == "No") {data1617$advanced.stats.degree.bin[i] <- 0}
  else {data1617$advanced.stats.degree.bin[i] <- NA}
}
table(data1617$advanced.stats.degree.bin)

### recoding post test location 
# 1 - In class (including Attitudes out of class)
# 0 - Out of class
table(data1617$admin.post.location)
data1617$admin.post.location.bin <- NULL
for (i in 1:nrow(data1617)) {
  if (is.na(data1617$admin.post.location[i]) == 1) {data1617$admin.post.location.bin[i] <- NA}
  else if (stringr::word(data1617$admin.post.location[i],1) == "In") {data1617$admin.post.location.bin[i] <- "1"}
  else if (data1617$admin.post.location[i] == "Out of class") {data1617$admin.post.location.bin[i] <- "0"}
  else {data1617$admin.post.location.bin[i] <- NA}  
}
table(data1617$admin.post.location.bin)

## "Quartey_Section1_Fall" and "Gorguis_Section1_Fall" did not give
data1617[data1617$admin.post.location=="Did not give","InstructorName_Section_Semester"]
# remove them
data1617 <- data1617[-which(data1617$InstructorName_Section_Semester %in% c("Quartey_Section1_Fall","Gorguis_Section1_Fall")),]
write.csv(data1617, file = "FinalV2 1617.csv")

