work_dir <- "C:/Users/Jason/Desktop/Chance Files"
setwd(work_dir)

# year 1 variable recoding - incentives, class start time, field, advanced stats degree, post test location
data1415 <- read.csv("Final 14.15.csv")

### recoding incentives 
# high stakes - assignment points, quiz points
# low stakes - exam, extra credit
table(data1415$incentive.pre.ind)
table(data1415$incentive.post.ind)
data1415$incentive.pre.bin <- NULL
data1415$incentive.post.bin <- NULL
for (i in 1:nrow(data1415)) {
  if (data1415$incentive.pre.ind[i] %in% c("assignment points", "quiz points")) {data1415$incentive.pre.bin[i] <- "Low stakes"}
  else if (data1415$incentive.pre.ind[i] %in% c("extra credit")) {data1415$incentive.pre.bin[i] <- "High stakes"}
  else {data1415$incentive.pre.bin[i] <- "None"} # includes requirement
  
  if (data1415$incentive.post.ind[i] %in% c("assignment points", "quiz points")) {data1415$incentive.post.bin[i] <- "Low stakes"}
  else if (data1415$incentive.post.ind[i] %in% c("extra credit", "exam")) {data1415$incentive.post.bin[i] <- "High stakes"}
  else {data1415$incentive.post.bin[i] <- "None"} # includes requirement
}
table(data1415$incentive.pre.bin)
table(data1415$incentive.post.bin)

### recoding class start time 
# 1 (8-9am)
# 2 (10-11am)
# 3 (12-2pm)
# 4 (after 2pm)
table(data1415$time.meet.ind)
data1415$time.meet.new <- NULL
data1415$time.meet <- as.character(data1415$time.meet)
for (i in 1:length(data1415$Q17_pre_c)) {
  if (is.na(data1415$time.meet[i])==1) {data1415$time.meet.new[i] <- NA}
  else if (data1415$time.meet[i]=="") {data1415$time.meet.new[i]<-NA}
  
  else if (data1415$time.meet[i]=="8:10am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="8:30am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="8am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:05am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:20am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:25am  ") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:30") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:30am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9:35am") {data1415$time.meet.new[i]<-1}
  else if (data1415$time.meet[i]=="9am") {data1415$time.meet.new[i]<-1}
  
  else if (data1415$time.meet[i]=="10:30am") {data1415$time.meet.new[i]<-2}
  else if (data1415$time.meet[i]=="10:50 AM OR 12:15 PM TEAL") {data1415$time.meet.new[i]<-2}
  else if (data1415$time.meet[i]=="10:50am") {data1415$time.meet.new[i]<-2}
  else if (data1415$time.meet[i]=="10am") {data1415$time.meet.new[i]<-2}
  else if (data1415$time.meet[i]=="11:10") {data1415$time.meet.new[i]<-2}
  else if (data1415$time.meet[i]=="11am") {data1415$time.meet.new[i]<-2}
  
  else if (data1415$time.meet[i]=="1:15pm") {data1415$time.meet.new[i]<-3}
  else if (data1415$time.meet[i]=="1:30pm") {data1415$time.meet.new[i]<-3}
  else if (data1415$time.meet[i]=="12:15pm") {data1415$time.meet.new[i]<-3}
  else if (data1415$time.meet[i]=="12:30pm") {data1415$time.meet.new[i]<-3}
  else if (data1415$time.meet[i]=="12pm") {data1415$time.meet.new[i]<-3}
  else if (data1415$time.meet[i]=="1pm") {data1415$time.meet.new[i]<-3}
  
  else if (data1415$time.meet[i]=="2:30pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="2pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="3:30pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="3pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="4pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="5:30pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="5pm") {data1415$time.meet.new[i]<-4}
  else if (data1415$time.meet[i]=="6pm") {data1415$time.meet.new[i]<-4}
  
  else if (data1415$time.meet[i]=="2 sections ") {data1415$time.meet.new[i]<-NA}
  else if (data1415$time.meet[i]=="online") {data1415$time.meet.new[i]<-NA}
  else if (data1415$time.meet[i]=="pm") {data1415$time.meet.new[i]<-NA}
  else if (data1415$time.meet[i]=="Rotated") {data1415$time.meet.new[i]<-NA}
  
  else {data1415$class.size.new[i]<-NA}
}
table(data1415$time.meet.new)

### recoding field (major)
# 1 - social sciences 
# 2 - natural and applied sciences
# 3 - liberal arts
# 4 - undeclared
# NA includes not in college, = 0, = 6 (not sure what these represent)

## if field_pre is missing, then recode field_pre as pre_post (if available)
sum(is.na(data1415$field_pre))
sum(is.na(data1415$field_post))
sum(is.na(data1415$field_pre & is.na(data1415$field_post)))
for (i in 1:nrow(data1415)) {
  if (is.na(data1415$field_pre[i])) {data1415$field_pre[i] <- data1415$field_post[i]}
}

# recoding 1:4
data1415$field_pre_new <- NULL
data1415$field_post_new <- NULL
for (i in 1:nrow(data1415)) {
  # what is field_pre = 0?
  if (is.na(data1415$field_pre[i])==1) {data1415$field_pre_new[i] <- NA}
  else if (data1415$field_pre[i] == 0) {data1415$field_pre_new[i] <- NA} 
  else if (data1415$field_pre[i] == 1) {data1415$field_pre_new[i] <- 1}
  else if (data1415$field_pre[i] == 2) {data1415$field_pre_new[i] <- 2}
  else if (data1415$field_pre[i] == 3) {data1415$field_pre_new[i] <- 3}
  else if (data1415$field_pre[i] == 4) {data1415$field_pre_new[i] <- 4}
  
  # what is field_post = 0?
  if (is.na(data1415$field_post[i])==1) {data1415$field_post_new[i] <- NA}
  else if (data1415$field_post[i] == 0) {data1415$field_post_new[i] <- NA}
  else if (data1415$field_post[i] == 1) {data1415$field_post_new[i] <- 1}
  else if (data1415$field_post[i] == 2) {data1415$field_post_new[i] <- 2}
  else if (data1415$field_post[i] == 3) {data1415$field_post_new[i] <- 3}
  else if (data1415$field_post[i] == 4) {data1415$field_post_new[i] <- 4}
}
table(data1415$field_pre_new)
table(data1415$field_post_new)

## check if field_pre matches field_post    
sum(data1415$field_pre != data1415$field_post, na.rm=T) #516
# data1415[!is.na(data1415$field_pre) & !is.na(data1415$field_post) & (data1415$field_pre != data1415$field_post), c("field_pre","field_post")]

## check if field_pre_new matches field_post_new 
sum(data1415$field_pre_new != data1415$field_post_new, na.rm=T) #332
# data1415[!is.na(data1415$field_pre_new) & !is.na(data1415$field_post_new) & (data1415$field_pre_new != data1415$field_post_new), c("field_pre_new","field_post_new")]

## if field_pre_new = 4 (undeclared), then replace with field_post_new (if not 4)
sum(data1415$field_pre_new == 4 & data1415$field_post_new != 4, na.rm=T)
data1415$field_final <- NULL
for (i in 1:nrow(data1415)) {
  if (is.na(data1415$field_pre[i])==1) {data1415$field_final[i] <- NA}
  else if (data1415$field_pre[i] == 4) {data1415$field_final[i] <- data1415$field_post_new[i]}
  else {data1415$field_final[i] <- data1415$field_pre_new[i]}
}
table(data1415$field_final)

### recoding advanced stats degree
# 1 - Yes (including other subject, masters, phd)
# 0 - No
table(data1415$Graduate_Applied_Stats)
table(data1415$Graduate_Theoretical_Stats)
# data1415$advanced.stats.degree <- as.character(data1415$advanced.stats.degree)
# data1415$advanced.stats.degree.bin <- NULL
# for (i in 1:nrow(data1415)) {
#   if (is.na(data1415$advanced.stats.degree[i]) == 1) {data1415$advanced.stats.degree.bin[i] <- NA}
#   else if (stringr::word(data1415$advanced.stats.degree[i], 1) == "Yes") {data1415$advanced.stats.degree.bin[i] <- 1}
#   else if (data1415$advanced.stats.degree[i] == "No") {data1415$advanced.stats.degree.bin[i] <- 0}
#   else {data1415$advanced.stats.degree.bin[i] <- NA}
# }
# table(data1415$advanced.stats.degree.bin)

### recoding post test location 
# 1 - In class (including Attitudes out of class)
# 0 - Out of class
table(data1415$admin.post.location)
data1415$admin.post.location.bin <- NULL
for (i in 1:nrow(data1415)) {
  if (is.na(data1415$admin.post.location[i]) == 1) {data1415$admin.post.location.bin[i] <- NA}
  else if (data1415$admin.post.location[i] == "In class") {data1415$admin.post.location.bin[i] <- "1"}
  else if (data1415$admin.post.location[i] == "Out of class") {data1415$admin.post.location.bin[i] <- "0"}
  else {data1415$admin.post.location.bin[i] <- NA}  
}
table(data1415$admin.post.location.bin)

## "HollingsworthSection1_fall", "SimpsonSection1_fall", "WillSection1_fall" did not give
data1415[data1415$admin.post.location=="Did not give","InstructorName_Section_Semester"]
# remove them
data1415 <- data1415[-which(data1415$InstructorName_Section_Semester %in% c("HollingsworthSection1_fall", "HollingsworthSection2_fall", "SimpsonSection1_fall", "WillSection1_fall")),]
write.csv(data1415, file = "FinalV2 1415.csv")