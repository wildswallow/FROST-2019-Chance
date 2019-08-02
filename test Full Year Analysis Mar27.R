### SET WORKING DIRECTORY ###
#setwd("/Users/sheareynolds/Dropbox/IS Assessment Data Summer 2017/final files/Data Files")
#setwd("/Users/bchance/Dropbox/ISI Assessment Data Summer 2017/final files/Data Files")
work_dir <- "C:/Users/Jason/Desktop/Chance Files"
setwd(work_dir)

### LOAD IN PACKAGES ###
load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))

#######################################################################################
############################## Options For Data Set ###################################
#######################################################################################

### CHOOSE DATA SET ###
# 16/17 #
data1617 <- read.csv("FinalV2 1617.csv")
data = data1617

# 15/16 #
data1516 <- read.csv("FinalV2 1516.csv")
data = data1516

# 14/15 #
data1415 <- read.csv("FinalV2 1415.csv")
data = data1415

# All 3 years #
data1617 <- cbind(data1617, year="16-17")
data1516 <- cbind(data1516, year="15-16")
data1415 <- cbind(data1415, year="14-15")
library(plyr)
allyears <- rbind.fill(data1617, data1516, data1415)
data = allyears

## High school only ?
dataHS = data[data$CarnegieClassification=="High School",]

#data$both = !is.na(data$Opt.out_post) & !is.na(data$Opt.out_pre) & (data$Opt.out_pre==1) & (data$Opt.out_post==1)
#iscamsummary(data$both, data$textbook)

### RUN LINE TO MOVE OPT OUTERS ###
data = data[data$Opt.out_pre==1 & !is.na(data$Opt.out_pre),]
data = data[data$Opt.out_post==1 & !is.na(data$Opt.out_post),]
nrow(data)

### RUN LINE TO TAKE OUT HS STUDENTS ###
HSdata1617 = data[data$CarnegieClassification=="High School",]
data <- data[data$CarnegieClassification!="High School",]
nrow(data)

### REMOVE ISCAM PEOPLE ###
data <- data[data$textbook.classification!="ISCAM",]

### RUN LINE TO REMOVE CONDENSED STUDENTS ###
data <- data[data$semester!="Condensed",]
nrow(data)

### RUN LINE TO REMOVE EXTREME ACHEIVABALE GAIN ###
data <- data[data$new_ach_gain > -1.1,]
nrow(data)

#table(data$new_ach_gain < mean(data$new_ach_gain, na.rm=T) - 4*sd(data$new_ach_gain, na.rm=T))
#table(data$new_ach_gain > mean(data$new_ach_gain, na.rm=T) + 3.5*sd(data$new_ach_gain, na.rm=T))
#table(data$new_ach_gain < -1)


### RUN LINES TO REMOVE STUDENTS WHOSE RESPONSE RATE (across 36) IS LESS THAN 80% ###
hist(data$PRE_c_RR)
table(data$PRE_c_RR < .8)
table(data$POST_c_RR < .8)
data <- data[data$PRE_c_RR >=.8,]
data <- data[data$POST_c_RR>=.8,]
nrow(data)


### CHOOSE TEXTBOOK CATEGORY ###
# Run line to use ISCAM/ISI/OtherSBI/NotSBI/NotSBI2 categories #
data$textbook <- data$textbook.classification
# Run line to use ISI/OtherSBI/NotSBI/NotSBI2 categories #
data$textbook <- data$textbook.classification_noISCAM
# Run line to use ISI/OtherSBI/NotSBI categories #probably want this one
new_textbook = factor(data$textbook, levels = c("ISI", "ISI1st", "OtherSBI", "NotSBI", "NotSBI2", "own"))
new_textbook = factor(data$textbook, levels = c("ISI", "ISI1st", "OtherSBI", "NotSBI", "NotSBI2"))


data$textbook <- data$textbook.classification2
# Run line to use SBI/NotSBI categories #
#data$textbook <- data$textbook.classification3



### Do not currently have access to section response rates
#hist(data$section_c_rr)

### RUN LINE TO REMOVE SECTIONS WITH RESPONSE RATE LESS THAN 60% ###
#data <- data[data$section_c_rr>=.6,]

### RUN LINE TO REMOVE SECTIONS WITH LESS THAN 10 STUDENTS ###
#data <- data[data$class.size.end>10,]



#######################################################################################
###################################### Analysis #######################################
#######################################################################################

### Table 1. Student Characteristics ###

# Number of Institutions #
length(unique(data$institution))
# Number of Instructors #
length(unique(paste(data$instructor.first.name,data$instructor.last.name)))
# Number of Sections #
length(unique(data$InstructorName_Section_Semester))
# Number of Students #
length(data$X)


### Table 2. Instructor Characteristics ###
# Instructors per year: 94, 90, 69

instructor.gender <- as.vector(tapply(data$instructor.gender2,as.character(paste(data$instructor.first.name,data$instructor.last.name)), mean,na.rm = T))
instructor.gaise <- as.vector(tapply(data$gaise.familiar2,as.character(paste(data$instructor.first.name,data$instructor.last.name)), mean,na.rm = T))
instructor.dept <- as.vector(tapply(data$dep_ind3,as.character(paste(data$instructor.first.name,data$instructor.last.name)), mean,na.rm = T))
instructor.yrs <- as.vector(tapply(data$years.teaching.intro.stats,as.character(paste(data$instructor.first.name,data$instructor.last.name)), mean,na.rm = T))
instructor.exp <- as.vector(tapply(data$analyzing.data.experience2,as.character(paste(data$instructor.first.name,data$instructor.last.name)), mean,na.rm = T))

table2 <- apply(cbind(instructor.gender, instructor.gaise, instructor.dept, instructor.yrs, instructor.exp), 2, mean, na.rm=T)
table2
median(instructor.yrs, na.rm=T)


### Table 3. Institution Characteristics ###
# Institutions per year: 46, 49, 36

## Prereqs
isNone = (data$math.prereq.coded == "None")
prereq.none <- as.vector(tapply(isNone,as.character(data$institution), mean,na.rm = T))

isHS = (data$math.prereq.coded == "High School Algebra")
prereq.hs <- as.vector(tapply(isHS,as.character(data$institution), mean,na.rm = T))

isColl = (data$math.prereq.coded == "College Algebra")
prereq.coll <- as.vector(tapply(isColl,as.character(data$institution), mean,na.rm = T))

isPre = (data$math.prereq.coded == "Precalculus")
prereq.pre <- as.vector(tapply(isPre,as.character(data$institution), mean,na.rm = T))

isCalc = (data$math.prereq.coded == "Calculus")
prereq.calc <- as.vector(tapply(isCalc,as.character(data$institution), mean,na.rm = T))

isOther = (data$math.prereq.coded == "Other")
prereq.oth <- as.vector(tapply(isOther,as.character(data$institution), mean,na.rm = T))

table3.prereq <- apply(cbind(prereq.none, prereq.hs, prereq.coll, prereq.pre, prereq.calc, prereq.oth), 2, mean, na.rm=T)
table3.prereq


## Carnegie Classification
isComm = (data$CarnegieClassification == "Community College")
cc.com <- as.vector(tapply(isComm,as.character(data$institution), mean,na.rm = T))

isBac = (data$CarnegieClassification == "Baccalaureate Colleges")
cc.bac <- as.vector(tapply(isBac,as.character(data$institution), mean,na.rm = T))

isMS = (data$CarnegieClassification == "Master's")
cc.ms <- as.vector(tapply(isMS,as.character(data$institution), mean,na.rm = T))

isDoc = (data$CarnegieClassification == "Doctoral Universities")
cc.doc <- as.vector(tapply(isDoc,as.character(data$institution), mean,na.rm = T))

table3.cc <- apply(cbind(cc.com, cc.bac, cc.ms, cc.doc), 2, mean, na.rm=T)
table3.cc


## Student Type
isLowGE = (data$student.type == "LowerGE")
type.lowge <- as.vector(tapply(isLowGE,as.character(data$institution), mean,na.rm = T))

isLowReq = (data$student.type == "LowerReq")
type.lowreq <- as.vector(tapply(isLowReq,as.character(data$institution), mean,na.rm = T))

isUpGE = (data$student.type == "UpperGE")
type.upge <- as.vector(tapply(isUpGE,as.character(data$institution), mean,na.rm = T))

isUpReq = (data$student.type == "UpperReq")
type.upreq <- as.vector(tapply(isUpReq,as.character(data$institution), mean,na.rm = T))

table3.type <- apply(cbind(type.lowge, type.lowreq, type.upge, type.upreq), 2, mean, na.rm=T)
table3.type


#################################### BASIC STATISTICS #################################


### MEANS BY TEXTBOOK ###

#data$textbook <- data$textbook.classification_noISCAM

# Pre Percent #
iscamsummary(data$new_pre_perc, new_textbook)
boxplot(data$new_pre_perc~new_textbook, xlab="Textbook", ylab="Pre-concept")
# SATACT zscore by textbook #
iscamsummary(data$SATACTzscore,new_textbook)
boxplot(data$SATACTzscore~new_textbook,  xlab="Textbook", ylab="SAT/ACT z-score")

# Post Percent #
iscamsummary(data$new_post_perc,data$textbook)
# Ach_Gain #
# iscamsummary(data$new_ach_gain)
# data$new_gain = data$new_post_perc - data$new_pre_perc
# iscamsummary(data$new_gain, data$textbook)
iscamsummary(data$new_ach_gain,new_textbook)

mean(data$new_ach_gain)
means <- tapply(data$new_ach_gain,data$textbook,mean)
means

# Pre Concept Subscales #
iscamsummary(data$new_Pre_DC,data$textbook)
iscamsummary(data$new_Pre_DS,data$textbook)
iscamsummary(data$new_Pre_CI,data$textbook)
iscamsummary(data$new_Pre_ST,data$textbook)
iscamsummary(data$new_Pre_Sim,data$textbook)
# Post Concept Subscales #
iscamsummary(data$new_Post_DC,data$textbook)
iscamsummary(data$new_Post_DS,data$textbook)
iscamsummary(data$new_Post_CI,data$textbook)
iscamsummary(data$new_Post_ST,data$textbook)
iscamsummary(data$new_Post_Sim,data$textbook)


### CHECKING MISSING QUESTIONS ###

miss_pre <-colSums(is.na(data[,colnames(data) %in% 
                     c("Q17_pre_c","Q18_pre_c","Q19_pre_c","Q20_pre_c",
                       "Q21_pre_c","Q22_pre_c","Q23_pre_c","Q24_pre_c",
                       "Q25_pre_c","Q26_pre_c","Q27_pre_c","Q28_pre_c",
                       "Q29_pre_c","Q30_pre_c","Q31_pre_c","Q32_pre_c",
                       "Q33_pre_c","Q34_pre_c","Q35_pre_c","Q36_pre_c",
                       "Q37_pre_c","Q38_pre_c","Q39_pre_c","Q40_pre_c",
                       "Q41_pre_c","Q42_pre_c","Q43_pre_c","Q44_pre_c",
                       "Q45_pre_c","Q46_pre_c","Q46.a_pre_c","Q46.b_pre_c",
                       "Q47_pre_c")]))
miss_post <- colSums(is.na(data[,colnames(data) %in% 
                     c("Q17_post_c","Q18_post_c","Q19_post_c","Q20_post_c",
                       "Q21_post_c","Q22_post_c","Q23_post_c","Q24_post_c",
                       "Q25_post_c","Q26_post_c","Q27_post_c","Q28_post_c",
                       "Q29_post_c","Q30_post_c","Q31_post_c","Q32_post_c",
                       "Q33_post_c","Q34_post_c","Q35_post_c","Q36_post_c",
                       "Q37_post_c","Q38_post_c","Q39_post_c","Q40_post_c",
                       "Q41_post_c","Q42_post_c","Q43_post_c","Q44_post_c",
                       "Q45_post_c","Q46_post_c","Q46.a_post_c","Q46.b_post_c",
                       "Q47_post_c")]))
rbind(miss_pre,miss_post)


############################## INDIVIDUAL QUESTIONS TABLES ##########################
## **** redid below to make comments consistent *****

### (Only works for ISI/OtherSBI/NotSBI or SBI/NotSBI textbook classifications) ###
### DIRECTIONS: RUN ENTIRE CHUNK OF CODE ###
# 
# data$textbook <- data$textbook.classification2 #ISI/OtherSBI/NotSBI
# data$textbook <- data$textbook.classification3 #SBI/NotSBI
# 
# first_pre = which(colnames(data)=="Q17_pre_c_RW")
# last_pre = which(colnames(data)=="Q47_pre_c_RW")
# first_post = which(colnames(data)=="Q17_post_c_RW")
# last_post = which(colnames(data)=="Q47_post_c_RW")
# 
# 
# if (length(levels(data$textbook))==3) {
#   ISI_frame_pre <- data[which(data$textbook=="ISI"),c(first_pre:last_pre)]
#   ISI_frame_post <- data[which(data$textbook=="ISI"),c(first_post:last_post)]
#   otherSBI_frame_pre <- data[which(data$textbook=="OtherSBI"),c(first_pre:last_pre)]
#   otherSBI_frame_post <- data[which(data$textbook=="OtherSBI"),c(first_post:last_post)]
#   nonSBI_frame_pre <- data[which(data$textbook=="NotSBI"),c(first_pre:last_pre)]
#   nonSBI_frame_post <- data[which(data$textbook=="NotSBI"),c(first_post:last_post)]
#   
#   Percent.Correct = c("Pre","Post", "Comments") 
#   tables_short <- function(questions_short){
#     for (i in questions_short){
#       isi_c = NULL
#       other_c = NULL
#       non_c = NULL 
#       #Creating comments for ISI column#
#       if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {isi_c[i] = "still low at the end"}
#       else if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) > (round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2) + .15)) {isi_c[i] = "improved a lot"}
#       else if (abs((round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) - (round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)))) < 0.05 ) {isi_c[i] = "didn't change much"}
#       else if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < (-0.05 + round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2))) {isi_c[i] = "decreased"}
#       else {isi_c[i]=""}
#       #Creating comments for OtherSBI column#
#       if (round(mean(otherSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {other_c[i] = "still low at the end"}
#       else if (round(mean(otherSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) > (round(mean(otherSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2) + .15)) {other_c[i] = "improved a lot"}
#       else if (abs((round(mean(otherSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) - (round(mean(otherSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)))) < 0.05 ) {other_c[i] = "didn't change much"}
#       else if (round(mean(otherSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < (-0.05 + round(mean(otherSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2))) {other_c[i] = "decreased"}
#       else {other_c[i]=""}
#       #Creating comments for NonSBI column#
#       if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {non_c[i] = "still low at the end"}
#       else if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) > (round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2) + .15)) {non_c[i] = "improved a lot"}
#       else if (abs((round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) - (round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)))) < 0.05 ) {non_c[i] = "didn't change much"}
#       else if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < (-0.05 + round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2))) {non_c[i] = "decreased"}
#       else{non_c[i]=""}
#       ISI = c(round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2),isi_c[i])
#       OtherSBI = c(round(mean(otherSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),other_c[i])
#       NonSBI = c(round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),non_c[i])
#       print(paste("Question",i,sep=" "))
#       print(data.frame(Percent.Correct,ISI,OtherSBI,NonSBI,row.names=NULL)) 
#     }}
# }
# 
# if (length(levels(data$textbook))==2) {
#   SBI_frame_pre <- data[ which(data$textbook=="SBI"),c(first_pre:last_pre)]
#   SBI_frame_post <- data[ which(data$textbook=="SBI"),c(first_post:last_post)]
#   nonSBI_frame_pre <- data[ which(data$textbook=="NotSBI"),c(first_pre:last_pre)]
#   nonSBI_frame_post <- data[ which(data$textbook=="NotSBI"),c(first_post:last_post)]
#   
#   Percent.Correct = c("Pre","Post","Comments") 
# 
#   tables_short <- function(questions_short){
#     for (i in questions_short){
#       isi_c = NULL
#       non_c = NULL 
#       #Creating comments for ISI column#
#       if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {isi_c[i] = "still low at the end"}
#       else if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) > (round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2) + .15)) {isi_c[i] = "improved a lot"}
#       else if (abs((round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) - (round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)))) < 0.05 ) {isi_c[i] = "didn't change much"}
#       else if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < (-0.05 + round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2))) {isi_c[i] = "decreased"}
#       else {isi_c[i]=""}
#       #Creating comments for NonSBI column#
#       if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {non_c[i] = "still low at the end"}
#       else if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) > (round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2) + .15)) {non_c[i] = "improved a lot"}
#       else if (abs((round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) - (round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)))) < 0.05 ) {non_c[i] = "didn't change much"}
#       else if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < (-0.05 + round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2))) {non_c[i] = "decreased"}
#       else{non_c[i]=""}
#       SBI = c(round(mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),isi_c[i])
#       NonSBI = c(round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),non_c[i])
#       print(paste("Question",i,sep=" "))
#       print(data.frame(Percent.Correct,SBI,NonSBI,row.names=NULL)) 
#     }}
#   }
# 
# questions_short <- c(17:47)
# tables_short(questions_short)


## **redo here**
##################********** QUESTIONS TABLES AND VISUAL **********#######################

### (Only works for ISI/OtherSBI/NotSBI or SBI/NotSBI textbook classifications) ###
data$textbook <- data$textbook.classification2 #ISI/OtherSBI/NotSBI
#data$textbook <- data$textbook.classification3 #SBI/NotSBI

### DIRECTIONS: RUN WHOLE CHUNK OF CODE TO CREATE TWO FUNCTIONS:
#   table_short will display average pre, post, diff (w/ comment) by textbook for each question
#   tables_visual will make visuals of average pre-post by textbook for each question

library(reshape2)
library(ggplot2)
library(dplyr)

first_pre = which(colnames(data)=="Q17_pre_c_RW")
last_pre = which(colnames(data)=="Q47_pre_c_RW")
first_post = which(colnames(data)=="Q17_post_c_RW")
last_post = which(colnames(data)=="Q47_post_c_RW")

Percent.Correct = c("Pre","Post", "Diff", "Comments") 

subnames <- c("DS","CI","CI","CI","DS","DC","ST","ST","DC","CI","ST","ST","ST","ST","ST","DS","DS","Sim","Sim","DS","Sim","Sim","Sim","DS","DS","DC","ST","ST","ST","ST","ST")

if (length(levels(data$textbook))==3) {
  ISI_frame_pre <- data[which(data$textbook=="ISI"),c(first_pre:last_pre)]
  ISI_frame_post <- data[which(data$textbook=="ISI"),c(first_post:last_post)]
  SBI_frame_pre <- data[ which(data$textbook=="OtherSBI"),c(first_pre:last_pre)]
  SBI_frame_post <- data[ which(data$textbook=="OtherSBI"),c(first_post:last_post)]
  nonSBI_frame_pre <- data[ which(data$textbook=="NotSBI"),c(first_pre:last_pre)]
  nonSBI_frame_post <- data[ which(data$textbook=="NotSBI"),c(first_post:last_post)]
  
  ## Function to make table average pre, post, diff (w/ comment) by textbook for each question
  
  tables_short <- function(questions_short){
    for (i in questions_short){
      diff1 = round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE) - mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
      diff2 = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE) - mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
      diff3 = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE) - mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
      isi_c = NULL
      other_c = NULL
      non_c = NULL 
      #Creating comments for ISI column#
      if (round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {isi_c[i] = "still low at the end"}
      if (diff1>=.15) {isi_c[i] = "improved a lot *"}
      else if (diff1>=.10) {isi_c[i] = "improved"}
      else if (diff1>=.05) {isi_c[i] = "improved a little"}
      else if (abs(diff1) < 0.05 ) {isi_c[i] = "no change"}
      else if (diff1< -.05) {isi_c[i] = "decreased"}
      else {isi_c[i]=""}
      #Creating comments for OtherSBI column#
      if (round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {other_c[i] = "still low at the end"}
      if (diff2>=.15) {other_c[i] = "improved a lot *"}
      else if (diff2>=.10) {other_c[i] = "improved"}
      else if (diff2>=.05) {other_c[i] = "improved a little"}
      else if (abs(diff2) < 0.05 ) {other_c[i] = "no change"}
      else if (diff2< -.05) {other_c[i] = "decreased"}
      else {other_c[i]=""}
      #Creating comments for NonSBI column#
      if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {non_c[i] = "still low at the end"}
      if (diff3>=.15) {non_c[i] = "improved a lot *"}
      else if (diff3>=.10) {non_c[i] = "improved"}
      else if (diff3>=.05) {non_c[i] = "improved a little"}
      else if (abs(diff3) < 0.05 ) {non_c[i] = "no change"}
      else if (diff3< -.05) {non_c[i] = "decreased"}
      else {non_c[i]=""}
      ISI = c(round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2),diff1,isi_c[i])
      OtherSBI = c(round(mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),diff2,other_c[i])
      NonSBI = c(round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),diff3,non_c[i])
      print(paste("Question",i,sep=" "))
      print(data.frame(Percent.Correct,ISI,OtherSBI,NonSBI,row.names=NULL)) 
    }}
  
  diff0=NULL
  diff1=NULL
  diff2=NULL
  pre0=NULL
  pre1=NULL
  pre2=NULL
  post0=NULL
  post1=NULL
  post2=NULL
  
  for (i in c(17:47)){
    diff0[i-16] = round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE) - mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    pre0[i-16] = round(mean(ISI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    post0[i-16] = round(mean(ISI_frame_post[,(i-16)],na.rm=TRUE),digits=2)
    diff1[i-16] = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE) - mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    diff2[i-16] = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE) - mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    pre1[i-16] = round(mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    pre2[i-16] = round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    post1[i-16] = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2)
    post2[i-16] = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2)
  }
  
  diffdf <- data.frame(ISI=diff0, SBI=diff1, nonSBI=diff2, question=c(17:47))
  ddf <- melt(diffdf, id="question", variable.name = "Textbook", value.name = "Post-Pre")
  isidf <- data.frame(Pre=pre0, Post=post0, question=seq(16.8,46.8))
  idf <- melt(isidf, id="question", variable.name = "Pre.Post", value.name = "Avg")
  idf$dsign <- sign(isidf$Post-isidf$Pre)
  sbidf <- data.frame(Pre=pre1, Post=post1, question=c(17:47))
  sdf <- melt(sbidf, id="question", variable.name = "Pre.Post", value.name = "Avg")
  sbijitter <- data.frame(Pre=pre1, Post=post1, question=c(17:47))
  sjitdf <- melt(sbijitter, id="question", variable.name = "Pre.Post", value.name = "Avg")
  sjitdf$dsign <- sign(sbijitter$Post-sbijitter$Pre)
  nondf <- data.frame(Pre=pre2, Post=post2, question=c(17:47))
  ndf <- melt(nondf, id="question", variable.name = "Pre.Post", value.name = "Avg")
  nonjitter <- data.frame(Pre=pre2, Post=post2, question=seq(17.2,47.2))
  njitdf <- melt(nonjitter, id="question", variable.name = "Pre.Post", value.name = "Avg")
  njitdf$dsign <- sign(nonjitter$Post-nonjitter$Pre)
  
  mergedf <- cbind(rbind(data.frame(idf, Textbook="ISI"), data.frame(sjitdf, Textbook="SBI"), data.frame(njitdf, Textbook="NonSBI")), Category = subnames)
  mergedf$dsign <- recode_factor(mergedf$dsign, `-1` = "Decrease", `0` = "Increase", `1` = "Increase")
  
  ## Function to make visuals of average pre-post by textbook for each question
  
  tables_visual <- function(statistic="Pre.Post", textbook="All", by="") {
    
    if (statistic == "Diff") {
      if (textbook == "All") {
        ggplot(data=ddf,aes(x=question,y=`Post-Pre`,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point() + geom_line() + theme_bw() + ggtitle("Differences")
      } else if (textbook == "ISI") {
        qplot(c(17:47),diff0,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_line() + theme_bw() + ggtitle("ISI Differences")
      } else if (textbook == "SBI") {
        qplot(c(17:47),diff1,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_line() + theme_bw() + ggtitle("SBI Differences")
      } else if (textbook == "nonSBI") {
        qplot(c(17:47),diff2,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_line() + theme_bw() + ggtitle("Non-SBI Differences")
      }
      
    } else if (statistic == "Pre.Post") {
      if (textbook == "ISI") {
        ggplot(data=idf,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI")
      } else if (textbook == "SBI") {
        ggplot(data=sdf,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("SBI")
      } else if (textbook == "nonSBI") {
        ggplot(data=ndf,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("Non-SBI")
      } else if (textbook == "All") {
        if (by == "Category") {
          ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + facet_grid(.~Category) +
            theme_bw() + ggtitle("ISI vs SBI vs Non-SBI by Concept Category")
        } else if (by == "Sign") {
          # g <- ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
          #   geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI vs SBI vs Non-SBI by sign")
          # g + facet_grid(rows = vars(dsign)) 
          ggplot(data=mergedf,aes(x=question,y=Avg,colour=dsign)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question, linetype = Textbook)) + theme_bw() + ggtitle("ISI vs SBI vs Non-SBI")
        } else {
          ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI vs SBI vs Non-SBI")
        }
      }
    }
  }
}

if (length(levels(data$textbook))==2) {
  
  SBI_frame_pre <- data[ which(data$textbook=="SBI"),c(first_pre:last_pre)]
  SBI_frame_post <- data[ which(data$textbook=="SBI"),c(first_post:last_post)]
  nonSBI_frame_pre <- data[ which(data$textbook=="NotSBI"),c(first_pre:last_pre)]
  nonSBI_frame_post <- data[ which(data$textbook=="NotSBI"),c(first_post:last_post)]
  
  ## Function to make table average pre, post, diff (w/ comment) by textbook for each question
  
  tables_short <- function(questions_short){
    for (i in questions_short){
      diff1 = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE) - mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
      diff2 = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE) - mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
      sbi_c = NULL
      non_c = NULL 
      #Creating comments for SBI column#
      if (round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {sbi_c[i] = "still low at the end"}
      if (diff1>=.15) {sbi_c[i] = "improved a lot *"}
      else if (diff1>=.10) {sbi_c[i] = "improved"}
      else if (diff1>=.05) {sbi_c[i] = "improved a little"}
      else if (abs(diff1) < 0.05 ) {sbi_c[i] = "no change"}
      else if (diff1< -.05) {sbi_c[i] = "decreased"}
      else {sbi_c[i]=""}
      #Creating comments for NonSBI column#
      if (round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2) < .2) {non_c[i] = "still low at the end"}
      if (diff2>=.15) {non_c[i] = "improved a lot *"}
      else if (diff2>=.10) {non_c[i] = "improved"}
      else if (diff2>=.05) {non_c[i] = "improved a little"}
      else if (abs(diff2) < 0.05 ) {non_c[i] = "no change"}
      else if (diff2< -.05) {non_c[i] = "decreased"}
      else {non_c[i]=""}
      SBI = c(round(mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),diff1,sbi_c[i])
      NonSBI = c(round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2),diff2,non_c[i])
      print(paste("Question",i,sep=" "))
      print(data.frame(Percent.Correct,SBI,NonSBI,row.names=NULL)) 
    }}
  
  diff1=NULL
  diff2=NULL
  pre1=NULL
  pre2=NULL
  post1=NULL
  post2=NULL
  
  for (i in c(17:47)){
    diff1[i-16] = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE) - mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    diff2[i-16] = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE) - mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    pre1[i-16] = round(mean(SBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    pre2[i-16] = round(mean(nonSBI_frame_pre[,(i-16)],na.rm=TRUE),digits=2)
    post1[i-16] = round(mean(SBI_frame_post[,(i-16)],na.rm=TRUE),digits=2)
    post2[i-16] = round(mean(nonSBI_frame_post[,(i-16)],na.rm=TRUE),digits=2)
  }
  
  diffdf <- data.frame(SBI=diff1, nonSBI=diff2, question=c(17:47))
  ddf <- melt(diffdf, id="question", variable.name = "Textbook", value.name = "Post-Pre")
  sbidf <- data.frame(Pre=pre1, Post=post1, question=c(17:47))
  sdf <- melt(sbidf, id="question", variable.name = "Pre.Post", value.name = "Avg")
  nondf <- data.frame(Pre=pre2, Post=post2, question=c(17:47))
  ndf <- melt(nondf, id="question", variable.name = "Pre.Post", value.name = "Avg")
  sbijitter <- data.frame(Pre=pre1, Post=post1, question=seq(16.9,46.9))
  sjitdf <- melt(sbijitter, id="question", variable.name = "Pre/Post", value.name = "Avg")
  sjitdf$dsign <- sign(sbijitter$Post-sbijitter$Pre)
  nonjitter <- data.frame(Pre=pre2, Post=post2, question=seq(17.1,47.1))
  njitdf <- melt(nonjitter, id="question", variable.name = "Pre/Post", value.name = "Avg")
  njitdf$dsign <- sign(nonjitter$Post-nonjitter$Pre)
  
  mergedf <- cbind(rbind(data.frame(sjitdf, Textbook="SBI"), data.frame(njitdf, Textbook="NonSBI")), Category = subnames)
  mergedf$dsign <- recode_factor(mergedf$dsign, `-1` = "Decrease", `0` = "Increase", `1` = "Increase")
  
  ## Function to make visuals of average pre-post by textbook for each question
  
  tables_visual <- function(statistic="Pre.Post", textbook="All", by="") {
    
    if (statistic == "Diff") {
      if (textbook == "All") {
        ggplot(data=ddf,aes(x=question,y=`Post-Pre`,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point() + geom_line() + theme_bw() + ggtitle("Differences")
      } else if (textbook == "SBI") {
        qplot(c(17:47),diff1,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_line() + theme_bw() + ggtitle("SBI Differences")
      } else if (textbook == "nonSBI") {
        qplot(c(17:47),diff2,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_line() + theme_bw() + ggtitle("Non-SBI Differences")
      }
    } else if (statistic == "Pre.Post") {
      if (textbook == "SBI") {
        ggplot(data=sdf,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("SBI")
      } else if (textbook == "nonSBI") {
        ggplot(data=ndf,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(17,47,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("Non-SBI")
      } else if (textbook == "All") {
        if (by == "Category") {
          ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + facet_grid(.~Category) +
            theme_bw() + ggtitle("SBI vs Non-SBI by Concept Category")
        } else if (by == "Sign") {
          g <- ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("SBI vs Non-SBI by sign")
          g + facet_grid(rows = vars(dsign)) 
        } else {
          ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(17,47,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("SBI vs Non-SBI")
        }
      }
    }
  }
}
##### END OF CHUNK #####


### OUTPUT OPTIONS ###

## Print questions table *****
# >=.15 improved a lot, >=.10 improved, >=.05 improved a little, -.05 to .05 no change, < -.05 decreased
questions_short <- c(17:47)
tables_short(questions_short)

# Display graphs
  # default: statistic="Pre.Post", textbook="All", by=""
  # options: 
  # statistic = "Pre.Post", "Diff"
  # textbook = "All", "ISI", "SBI", nonSBI"
  # by = "Category", "sign" (only works for Pre.Post, All)
tables_visual()
tables_visual("Diff")
tables_visual("Diff", "SBI")
tables_visual("Pre.Post", "nonSBI")
tables_visual(by="Category")
tables_visual(by="Sign")

########## ******************* FINAL PLOT ******************* want this one ##########
# for textbook.classification2 only
# sorts by subcategory and reorders questions  

mergedfsort <- mergedf[order(mergedf$Category),]  
# mergedfsort$question2 <- c(1:4,1:4,1:4-.2,1:4-.2,1:4+.2,1:4+.2,
#                           5:7,5:7,5:7-.2,5:7-.2,5:7+.2,5:7+.2,
#                           8:14,8:14,8:14-.2,8:14-.2,8:14+.2,8:14+.2,
#                           15:19,15:19,15:19-.2,15:19-.2,15:19+.2,15:19+.2,
#                           20:31,20:31,20:31-.2,20:31-.2,20:31+.2,20:31+.2)
mergedfsort$question2 <- c(1:4-.2,1:4-.2,1:4,1:4,1:4+.2,1:4+.2,
                           5:7-.2,5:7-.2,5:7,5:7,5:7+.2,5:7+.2,
                           8:14-.2,8:14-.2,8:14,8:14,8:14+.2,8:14+.2,
                           15:19-.2,15:19-.2,15:19,15:19,15:19+.2,15:19+.2,
                           20:31-.2,20:31-.2,20:31,20:31,20:31+.2,20:31+.2)

mergedfsort$Category  <- factor(mergedfsort$Category , levels = c("DC","DS", "Sim", "ST", "CI"))

# choose increase/decrease colors here
group.colors <- c(Increase = "springgreen4", Decrease = "dodgerblue3")

Category_status <- c(
  DC = "Data Collection",
  DS = "Descriptive Statistics",
  Sim = "Simulation",
  ST = "Tests of Significance",
  CI = "Confidence Intervals"
)
idx <- match(mergedfsort$Category, names(Category_status))
mergedfsort$Category2 <- Category_status[idx]
mergedfsort$Category2  <- factor(mergedfsort$Category2 , levels = c("Data Collection","Descriptive Statistics", "Simulation", "Tests of Significance", "Confidence Intervals"))

library(ggthemes)
ggplot(data=mergedfsort,aes(x=factor(question),y=Avg,colour=dsign)) + scale_x_discrete(breaks = 1:47) +
  geom_point(aes(shape=Pre.Post), size=2) + geom_line(aes(group = question2, linetype=Textbook)) + 
  facet_grid(.~Category2, scales="free_x", space="fixed", labeller = labeller(Category2 = label_wrap_gen(10))) + 
  theme_calc() + theme(strip.text.x = element_text(size=20),
                       axis.text.y = element_text(size=16), legend.text = element_text(size=12))+ scale_colour_manual(values = group.colors) +
  labs(y=element_blank(),x=element_blank(),colour=element_blank(),shape=element_blank(),linetype=element_blank()) +
  ggtitle("ISI vs SBI vs Non-SBI by Concept Category")
# See document titled Concept Visual Explained #


###################### Examine subscales #############################################

data_pre = data[data$Opt.out_pre==1 & !is.na(data$Opt.out_pre) &!is.na(data$pre_perc),]
data_post = data[data$Opt.out_post==1 & !is.na(data$Opt.out_post) &!is.na(data$post_perc),]

Percent.Correct = c("Pre","Post", "Diff")  

ISI_frame_pre <- data_pre[ which(data_pre$textbook.classification=="ISI"),]
ISI_frame_post <- data_post[ which(data_post$textbook.classification=="ISI"),]
ISI1st_frame_pre <- data_pre[ which(data_pre$textbook.classification=="ISI1st"),]
ISI1st_frame_post <- data_post[ which(data_post$textbook.classification=="ISI1st"),]
otherSBI_frame_pre <- data_pre[ which(data_pre$textbook.classification=="OtherSBI"),]
otherSBI_frame_post <- data_post[ which(data_post$textbook.classification=="OtherSBI"),]
nonSBI_frame_pre <- data_pre[ which(data_pre$textbook.classification=="NotSBI"),]
nonSBI_frame_post <- data_post[ which(data_post$textbook.classification=="NotSBI"),]
nonSBI2_frame_pre <- data_pre[ which(data_pre$textbook.classification=="NotSBI2"),]
nonSBI2_frame_post <- data_post[ which(data_post$textbook.classification=="NotSBI2"),]

#Data Collection#
ISI = c(round(mean(ISI_frame_pre$Pre_DC,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_DC,na.rm=TRUE),digits=2), round(mean(ISI_frame_post$Post_DC - ISI_frame_post$Pre_DC, na.rm=TRUE), digits=2))
ISI1st = c(round(mean(ISI1st_frame_pre$Pre_DC,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_DC,na.rm=TRUE),digits=2), round(mean(ISI1st_frame_post$Post_DC - ISI1st_frame_post$Pre_DC, na.rm=TRUE), digits=2))
OtherSBI = c(round(mean(otherSBI_frame_pre$Pre_DC,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_DC,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_DC - otherSBI_frame_post$Pre_DC, na.rm=TRUE), digits=2))
NonSBI = c(round(mean(nonSBI_frame_pre$Pre_DC,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_DC,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_DC - nonSBI_frame_post$Pre_DC, na.rm=TRUE), digits=2))
NonSBI2 = c(round(mean(nonSBI2_frame_pre$Pre_DC,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_DC,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_DC - nonSBI2_frame_post$Pre_DC, na.rm=TRUE), digits=2))
print(paste("Data Collection"));print(data.frame(Percent.Correct,ISI,ISI1st,OtherSBI,NonSBI,NonSBI2,row.names=NULL)) 

#Descriptive Statistics#
ISIb = c(round(mean(ISI_frame_pre$Pre_DS,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_DS,na.rm=TRUE), digits=2), round(mean(ISI_frame_post$Post_DS - ISI_frame_post$Pre_DS, na.rm=TRUE), digits=2))
ISI1stb = c(round(mean(ISI1st_frame_pre$Pre_DS,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_DS,na.rm=TRUE), digits=2), round(mean(ISI1st_frame_post$Post_DS - ISI1st_frame_post$Pre_DS, na.rm=TRUE), digits=2))
OtherSBIb = c(round(mean(otherSBI_frame_pre$Pre_DS,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_DS,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_DS - otherSBI_frame_post$Pre_DS, na.rm=TRUE), digits=2))
NonSBIb = c(round(mean(nonSBI_frame_pre$Pre_DS,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_DS,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_DS - nonSBI_frame_post$Pre_DS, na.rm=TRUE), digits=2))
NonSBI2b = c(round(mean(nonSBI2_frame_pre$Pre_DS,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_DS,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_DS - nonSBI2_frame_post$Pre_DS, na.rm=TRUE), digits=2))
print(paste("Descriptive Statistics"));print(data.frame(Percent.Correct,ISIb,ISI1stb,OtherSBIb,NonSBIb,NonSBI2b,row.names=NULL)) 

#Confidence Intervals#
ISIc = c(round(mean(ISI_frame_pre$Pre_CI,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_CI,na.rm=TRUE),digits=2), round(mean(ISI_frame_post$Post_CI - ISI_frame_post$Pre_CI, na.rm=TRUE), digits=2))
ISI1stc = c(round(mean(ISI1st_frame_pre$Pre_CI,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_CI,na.rm=TRUE),digits=2), round(mean(ISI1st_frame_post$Post_CI - ISI1st_frame_post$Pre_CI, na.rm=TRUE), digits=2))
OtherSBIc = c(round(mean(otherSBI_frame_pre$Pre_CI,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_CI,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_CI - otherSBI_frame_post$Pre_CI, na.rm=TRUE), digits=2))
NonSBIc = c(round(mean(nonSBI_frame_pre$Pre_CI,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_CI,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_CI - nonSBI_frame_post$Pre_CI, na.rm=TRUE), digits=2))
NonSBI2c = c(round(mean(nonSBI2_frame_pre$Pre_CI,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_CI,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_CI - nonSBI2_frame_post$Pre_CI, na.rm=TRUE), digits=2))
print(paste("Confidence Intervals"));print(data.frame(Percent.Correct,ISIc,ISI1stc,OtherSBIc,NonSBIc,NonSBI2c,row.names=NULL)) 

#ST#
ISId = c(round(mean(ISI_frame_pre$Pre_ST,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_ST,na.rm=TRUE),digits=2), round(mean(ISI_frame_post$Post_ST - ISI_frame_post$Pre_ST, na.rm=TRUE), digits=2))
ISI1std = c(round(mean(ISI1st_frame_pre$Pre_ST,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_ST,na.rm=TRUE),digits=2), round(mean(ISI1st_frame_post$Post_ST - ISI1st_frame_post$Pre_ST, na.rm=TRUE), digits=2))
OtherSBId = c(round(mean(otherSBI_frame_pre$Pre_ST,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_ST,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_ST - otherSBI_frame_post$Pre_ST, na.rm=TRUE), digits=2))
NonSBId = c(round(mean(nonSBI_frame_pre$Pre_ST,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_ST,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_ST - nonSBI_frame_post$Pre_ST, na.rm=TRUE), digits=2))
NonSBI2d = c(round(mean(nonSBI2_frame_pre$Pre_ST,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_ST,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_ST - nonSBI2_frame_post$Pre_ST, na.rm=TRUE), digits=2))
print(paste("ST"));print(data.frame(Percent.Correct,ISId,ISI1std,OtherSBId,NonSBId,NonSBI2d,row.names=NULL)) 

#Simulation#
ISIe = c(round(mean(ISI_frame_pre$Pre_Sim,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_Sim,na.rm=TRUE),digits=2), round(mean(ISI_frame_post$Post_Sim - ISI_frame_post$Pre_Sim, na.rm=TRUE), digits=2))
ISI1ste = c(round(mean(ISI1st_frame_pre$Pre_Sim,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_Sim,na.rm=TRUE),digits=2), round(mean(ISI1st_frame_post$Post_Sim - ISI1st_frame_post$Pre_Sim, na.rm=TRUE), digits=2))
OtherSBIe = c(round(mean(otherSBI_frame_pre$Pre_Sim,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_Sim,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_Sim - otherSBI_frame_post$Pre_Sim, na.rm=TRUE), digits=2))
NonSBIe = c(round(mean(nonSBI_frame_pre$Pre_Sim,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_Sim,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_Sim - nonSBI_frame_post$Pre_Sim, na.rm=TRUE), digits=2))
NonSBI2e = c(round(mean(nonSBI2_frame_pre$Pre_Sim,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_Sim,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_Sim - nonSBI2_frame_post$Pre_Sim, na.rm=TRUE), digits=2))
print(paste("Simulation"));print(data.frame(Percent.Correct,ISIe,ISI1ste,OtherSBIe,NonSBIe,NonSBI2e,row.names=NULL)) 

#Scope of Conclusions#
ISIf = c(round(mean(ISI_frame_pre$Pre_SC,na.rm=TRUE),digits=2),round(mean(ISI_frame_post$Post_SC,na.rm=TRUE),digits=2), round(mean(ISI_frame_post$Post_SC - ISI_frame_post$Pre_SC, na.rm=TRUE), digits=2))
ISI1stf = c(round(mean(ISI1st_frame_pre$Pre_SC,na.rm=TRUE),digits=2),round(mean(ISI1st_frame_post$Post_SC,na.rm=TRUE),digits=2), round(mean(ISI1st_frame_post$Post_SC - ISI1st_frame_post$Pre_SC, na.rm=TRUE), digits=2))
OtherSBIf = c(round(mean(otherSBI_frame_pre$Pre_SC,na.rm=TRUE),digits=2),round(mean(otherSBI_frame_post$Post_SC,na.rm=TRUE),digits=2), round(mean(otherSBI_frame_post$Post_SC - otherSBI_frame_post$Pre_SC, na.rm=TRUE), digits=2))
NonSBIf = c(round(mean(nonSBI_frame_pre$Pre_SC,na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post$Post_SC,na.rm=TRUE),digits=2), round(mean(nonSBI_frame_post$Post_SC - nonSBI_frame_post$Pre_SC, na.rm=TRUE), digits=2))
NonSBI2f = c(round(mean(nonSBI2_frame_pre$Pre_SC,na.rm=TRUE),digits=2),round(mean(nonSBI2_frame_post$Post_SC,na.rm=TRUE),digits=2), round(mean(nonSBI2_frame_post$Post_SC - nonSBI2_frame_post$Pre_SC, na.rm=TRUE), digits=2))
print(paste("Scope of Conclusions"));print(data.frame(Percent.Correct,ISIf,ISI1stf,OtherSBIf,NonSBIf,NonSBI2f,row.names=NULL)) 


print(paste("Data Collection"));print(data.frame(Percent.Correct,ISI,ISI1st,OtherSBI,NonSBI,NonSBI2,row.names=NULL)) ;print(paste("Descriptive Statistics"));print(data.frame(Percent.Correct,ISIb,ISI1stb,OtherSBIb,NonSBIb,NonSBI2b,row.names=NULL)) ;print(paste("Confidence Intervals"));print(data.frame(Percent.Correct,ISIc,ISI1stc,OtherSBIc,NonSBIc,NonSBI2c,row.names=NULL)) ;print(paste("ST"));print(data.frame(Percent.Correct,ISId,ISI1std,OtherSBId,NonSBId,NonSBI2d,row.names=NULL)) ;print(paste("Simulation"));print(data.frame(Percent.Correct,ISIe,ISI1ste,OtherSBIe,NonSBIe,NonSBI2e,row.names=NULL)) ;print(paste("Scope of Conclusions"));print(data.frame(Percent.Correct,ISIf,ISI1stf,OtherSBIf,NonSBIf,NonSBI2f,row.names=NULL)) 


######################################## BOXPLOTS ##################################
library(ggplot2)

data = data1617
data = data1516
data = data1415
#data = allyears

### RUN LINE TO MOVE OPT OUTERS ###
data = data[data$Opt.out_pre==1 & !is.na(data$Opt.out_pre),]
data = data[data$Opt.out_post==1 & !is.na(data$Opt.out_post),]
### RUN LINE TO TAKE OUT HS STUDENTS ###
HSdata1617 = data[data$CarnegieClassification=="High School",]
data <- data[data$CarnegieClassification!="High School",]
### REMOVE ISCAM PEOPLE ###
data <- data[data$textbook.classification!="ISCAM",]
### RUN LINE TO REMOVE CONDENSED STUDENTS ###
data <- data[data$semester!="Condensed",]
### RUN LINE TO REMOVE EXTREME ACHEIVABALE GAIN ###
data <- data[data$new_ach_gain > -1.1,]
### RUN LINES TO REMOVE STUDENTS WHOSE RESPONSE RATE (across 36) IS LESS THAN 80% ###
hist(data$PRE_c_RR)
table(data$PRE_c_RR < .8)
table(data$POST_c_RR < .8)
data <- data[data$PRE_c_RR >=.8,]
data <- data[data$POST_c_RR>=.8,]

## CHOOSE TEXTBOOK CATEGORIES ##
data$textbook <- data$textbook.classification_noISCAM #ISI/OtherSBI/NotSBI/NotSBI2
data$textbook <- data$textbook.classification2 #ISI/OtherSBI/NotSBI
data$textbook <- data$textbook.classification3 #SBI/NotSBI

## CHOOSE RESPONSE VARIABLE ##
my_rv <- "new_ach_gain"     # Achievable gains
my_rv <- data$new_post_perc - data$new_pre_perc
my_rv = data$new_post_perc
my_rv <- "new_DC_gains"     # Data Collection
my_rv <- "new_DS_gains"     # Descriptive Statistics
my_rv <- "new_CI_gains"     # Confidence Intervals
my_rv <- "new_ST_gains"     # ST
my_rv <- "new_Sim_gains"    # Simulation

### DIRECTIONS: RUN ENTIRE CHUNK OF CODE ###

my_rv_lab <- if (my_rv == "new_ach_gain") {"Achievable Gains"
} else if (my_rv == "new_DC_gains") {"Data Collection Gains"
} else if (my_rv == "new_DS_gains") {"Descriptive Statistics Gains"
} else if (my_rv == "new_CI_gains") {"Confidence Interval Gains"
} else if (my_rv == "new_ST_gains") {"ST Gains"
} else if (my_rv == "new_Sim_gains") {"Simulation Gains"}

if (length(levels(data$textbook))==2) {
  data_box <- data[is.na(data$textbook)==0,]
  data_box$textbook <- factor(data_box$textbook, levels=c("SBI","NotSBI"))
  means <- tapply(data_box[,my_rv],data_box$textbook,mean)
  grand_mean <- mean(data_box[,my_rv])
  dev.off()
  ggplot(aes_string(y=my_rv,x="textbook",group="InstructorName_Section_Semester",fill="textbook"),data=data_box) + geom_boxplot() + xlab("Section") + ylab(my_rv_lab) + ggtitle(paste("Boxplots of", my_rv_lab,"by Section and Textbook")) + theme_bw() +
    geom_segment(aes(x=.647,y=means[1],xend=1.355,yend=means[1]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=1.613,y=means[2],xend=2.355,yend=means[2]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=.5,y=grand_mean,xend=2.5,yend=grand_mean),colour="navyblue",show.legend=FALSE) 
}

#x <- c("BondSection1_spring", "BrisbinSection1_fall", "AndersonSection2_fall")
#x
#gsub("Section.*$","",x)


if (length(levels(data$textbook))==3) {
  data_box <- data[is.na(data$textbook)==0,]
  data_box$textbook <- factor(data_box$textbook, levels=c("ISI","OtherSBI","NotSBI"))
  means <- tapply(data_box[,my_rv],data_box$textbook,mean)
  grand_mean <- mean(data_box[,my_rv])
  dev.off()
  ggplot(aes_string(y=my_rv,x="textbook",group="instructor.last.name",fill="textbook"),data=data_box) + geom_boxplot() + xlab("Section") + ylab(my_rv_lab) + ggtitle(paste("Boxplots of", my_rv_lab,"by Instructor and Textbook")) + theme_bw() +
    geom_segment(aes(x=.647,y=means[1],xend=1.355,yend=means[1]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=1.613,y=means[2],xend=2.355,yend=means[2]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=2.611,y=means[3],xend=3.363,yend=means[3]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=.5,y=grand_mean,xend=3.5,yend=grand_mean),colour="navyblue",show.legend=FALSE)}

if (length(levels(data$textbook))==5|length(levels(data$textbook))==6) {
  data_box <- data[data$textbook=="ISI"|data$textbook=="ISI1st"|data$textbook=="OtherSBI"|data$textbook=="NotSBI"|data$textbook=="NotSBI2",]
  data_box <- data_box[is.na(data_box$textbook)==0,]
  data_box$textbook <- factor(data_box$textbook, levels=c("ISI","ISI1st","OtherSBI","NotSBI","NotSBI2"))
  #data_box$InstructorName_Section_Semester <- gsub("_Section.*$","",data_box$InstructorName_Section_Semester) #Add if want to do by instructor (removes Section + Semester for unit labels)
  means <- tapply(data_box[,my_rv],data_box$textbook,mean)
  grand_mean <- mean(data_box[,my_rv])
  #  dev.off()
  ggplot(aes_string(y=my_rv,x="textbook",group="instructor.last.name",fill="textbook"),data=data_box) + geom_boxplot() + xlab("Section") + ylab(my_rv_lab) + ggtitle(paste("Boxplots of", my_rv_lab,"by Section and Textbook")) + theme_bw() +
    geom_segment(aes(x=.647,y=means[1],xend=1.355,yend=means[1]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=1.613,y=means[2],xend=2.355,yend=means[2]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=2.611,y=means[3],xend=3.363,yend=means[3]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=3.6,y=means[4],xend=4.374,yend=means[4]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=4.6,y=means[5],xend=5.383,yend=means[5]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=.5,y=grand_mean,xend=5.383,yend=grand_mean),colour="navyblue",show.legend=FALSE)} 

if (length(levels(data$textbook))==7) {
  data_box <- data[data$textbook=="ISCAM"|data$textbook=="ISI"|data$textbook=="ISI1st"|data$textbook=="OtherSBI"|data$textbook=="NotSBI"|data$textbook=="NotSBI2",]
  data_box <- data_box[is.na(data_box$textbook)==0,]
  data_box$textbook <- factor(data_box$textbook, levels=c("ISCAM","ISI","ISI1st","OtherSBI","NotSBI","NotSBI2"))
  means <- tapply(data_box[,my_rv],data_box$textbook,mean)
  grand_mean <- mean(data_box[,my_rv])
  dev.off()
  ggplot(aes_string(y=my_rv,x="textbook",group="InstructorName_Section_Semester",fill="textbook"),data=data_box) + geom_boxplot() + xlab("Section") + ylab(my_rv_lab) + ggtitle(paste("Boxplots of", my_rv_lab,"by Section and Textbook")) + theme_bw() +
    geom_segment(aes(x=.647,y=means[1],xend=1.355,yend=means[1]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=1.613,y=means[2],xend=2.355,yend=means[2]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=2.611,y=means[3],xend=3.363,yend=means[3]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=3.6,y=means[4],xend=4.374,yend=means[4]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=4.6,y=means[5],xend=5.383,yend=means[5]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=5.6,y=means[6],xend=6.393,yend=means[6]), colour="deeppink4", size=1.3) +
    geom_segment(aes(x=.5,y=grand_mean,xend=6.393,yend=grand_mean),colour="navyblue",show.legend=FALSE)} 


# Section RR #
data_box <- data[is.na(data$textbook)==0,]
data_box$textbook <- factor(data_box$textbook, levels=c("ISI","OtherSBI","NotSBI"))
dev.off()
ggplot(aes(y=section_c_rr1,x=textbook,group=InstructorName_Section_Semester,fill=textbook),data=data_box) + geom_boxplot() + xlab("Section") + ylab("Achievable Gain") + ggtitle("Boxplots of Achievable Gains by Section and Textbook") + theme_bw() 


################################# Retention Tables #################################
setwd("/Users/sheareynolds/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
r4y15 <- read.csv("Merged2015Retention4Month.csv")

###### 4-Month ######
### Prep for tables ###
first_post = which(colnames(r4y15)=="Q17_post_c_RW")
last_post = which(colnames(r4y15)=="Q47_post_c_RW")
first_ret = which(colnames(r4y15)=="Q17_ret_RW")
last_ret = which(colnames(r4y15)=="Q47_ret_RW")

ISI_frame_post_s <- r4y15[ which(r4y15$textbook.classification2=="ISI"),c(first_post:last_post)]
ISI_frame_ret_s <- r4y15[ which(r4y15$textbook.classification2=="ISI"),c(first_ret:last_ret)]
otherSBI_frame_post_s <- r4y15[ which(r4y15$textbook.classification2=="OtherSBI"),c(first_post:last_post)]
otherSBI_frame_ret_s <- r4y15[ which(r4y15$textbook.classification2=="OtherSBI"),c(first_ret:last_ret)]
nonSBI_frame_post_s <- r4y15[ which(r4y15$textbook.classification2=="NotSBI"),c(first_post:last_post)]
nonSBI_frame_ret_s <- r4y15[ which(r4y15$textbook.classification2=="NotSBI"),c(first_ret:last_ret)]

Percent.Correct = c("Post","Ret") 

######## Function to get tables (must run prep first) ########
tables_short <- function(questions_short){
  for (i in questions_short){
    
    ISI = c(round(mean(ISI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    OtherSBI = c(round(mean(otherSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(otherSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    NonSBI = c(round(mean(nonSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    print(paste("Question",i,sep=" "))
    print(data.frame(Percent.Correct,ISI,OtherSBI,NonSBI,row.names=NULL)) 
  }
} 
##### Enter into this vector the questions that you want tables for. 17=Q17, 18=Q18, ... 46=Q46.a, 47=Q46.b, 48=Q47#
questions_short <- c(17:47)
tables_short(questions_short)

#########################################################HLM Models####################

data = data1617
data = data1516
data = data1415

data = data[data$Opt.out_pre==1 & !is.na(data$Opt.out_pre),]
data = data[data$Opt.out_post==1 & !is.na(data$Opt.out_post),]
data <- data[data$CarnegieClassification!="High School",]
data <- data[data$textbook.classification!="ISCAM",]
data <- data[data$semester!="Condensed",]
data <- data[data$new_ach_gain > -1.1,]
data <- data[data$PRE_c_RR >=.8,]
data <- data[data$POST_c_RR>=.8,]

data$textbook <- data$textbook.classification_noISCAM
iscamsummary(data$new_ach_gain, data$textbook)
iscamsummary(data$new_ach_gain)

coplot(my_rv ~ data$overall_attitude_pre | data$textbook, columns=3,
       panel=function(x,y,...) {
         panel.smooth(x,y,span=.8,iter=5,...)
         abline(lm(y ~ x), col="blue") } )
my_rv = data$new_post_perc
data$textbook = factor(data$textbook, levels = c("ISI", "OtherSBI", "NotSBI"))

cdata = cbind(my_rv, data[c("new_pre_perc", "gpa", "SATACTzscore", "textbook")])
#cdata = data[complete.cases(cdata),]
cdata=na.omit(cdata)
gpaz = (data$gpa - mean(data$gpa))/sd(data$gpa)
gain = data$new_post_perc - data$new_pre_perc

  #na.omit(cbind(my_rv, data$new_pre_perc, data$gpa, data$SATSCATzscore, data$textbook))
model1 = lm(gain ~ new_pre_perc , data = data )
my_resids = model1$residuals

summary(lm(my_resids ~ cdata$textbook))
boxplot(my_resids +cdata$my_rv~cdata$textbook)




data_box <- data[data$textbook=="ISI"|data$textbook=="ISI1st"|data$textbook=="OtherSBI"|data$textbook=="NotSBI"|data$textbook=="NotSBI2",]
data_box <- data_box[is.na(data_box$textbook)==0,]
data_box$textbook <- factor(data_box$textbook, levels=c("ISI","ISI1st","OtherSBI","NotSBI","NotSBI2"))
means <- tapply(data_box$new_ach_gain,data_box$textbook,mean)
grand_mean <- mean(data_box$new_ach_gain)


fit = lm(new_ach_gain ~ 1 , data = data_box)
library(lme4)
library(lmerTest)

#check to see if institutions use the same book
check <- data_box[,c("institution", "textbook")]


nullmodel <- lmer(new_ach_gain ~ (1 | InstructorName_Section_Semester), data = data, REML = FALSE)
nullmodel = lmer(new_post_perc ~ (1|InstructorName_Section_Semester), data = data, REML = F)
summary(nullmodel)
#ICC = .005418/(.005418 + .058175) = .085 (year 3)

nullmodel <- lmer(new_ach_gain ~  ( 1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)
summary(nullmodel)

#all data=data_box
#section (176.1), instructor (140.1), institution (122.9)
#institution/instructor (121.2), institution/section (121.6), instructor/section (141.4)
#institution/instructor/section (122.4)
#TA, percent.lecture, class.size.end
nullmodel <- lmer(new_ach_gain ~  ( 1 | institution/instructor.last.name), data=data_box, REML = FALSE)
summary(nullmodel)



summary(lmer(new_post_perc ~ data$new_pre_perc*data$textbook + (1 | InstructorName_Section_Semester), data = data))
data$textbook <- data$textbook.classification2 #ISI/OtherSBI/NotSBI



2*(logLik(nullmodel) - logLik(fit))  #166.5

#using data_box because of ISCAM and the own materials
nullmodel1.0 = lmer(new_ach_gain ~ (1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)
nullmodel2.0 = lmer(new_ach_gain ~ pre_perc +  (1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)
nullmodel3.0 = lmer(new_ach_gain ~ pre_perc + textbook + (1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)
nullmodel4.0 = lmer(new_ach_gain ~ pre_perc + textbook + CarnegieClassification + (1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)
nullmodel5.0 = lmer(new_ach_gain ~ pre_perc + textbook + dep_ind2 + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box, REML = FALSE)


my_rv <- "new_ach_gain"     # Achievable gains
my_rv <- data$new_post_perc - data$new_pre_perc
my_rv = data$new_post_perc
my_rv = (data$new_post_perc - data$new_pre_perc)/(1-data$new_pre_perc)

model7 = lmer(new_ach_gain ~ 1 + gpa_pre + SATACTzscore + pre_perc +textbook+ ( 1 | institution/instructor.last.name/InstructorName_Section_Semester), data=data_box)
summary(model7)

data_box$new_gain = data_box$new_post_perc - data_box$new_pre_perc

model8 = lmer(new_ach_gain ~ 1   
              + pre_perc + Affect_pre
               + Cognitive.Competence_pre + Difficulty_pre 
               + Effort_pre 
              + Interest_pre 
              + Value_pre
              + Affect_post
              + Cognitive.Competence_post + Difficulty_post 
              + Value_post
              #change in attitudes
              #     + Affect_change
              #     + Effort_change
              #     + Cog_change
              #     + Value_change
              #     + Difficulty_change
              #     + Interest_change
              #+ Effort_post 
              #  + Interest_post
              #   + as.factor(gender) 
              + as.factor(gender_pre) 
              #+ age_pre
              + gpa_pre            + I(gpa_pre*gpa_pre)
              + SATACTzscore
              + instructor.gender
                 + as.factor(class.size.ind)
              + textbook
              + years.teaching.intro.stats
               + as.factor(math.prereq.coded)
                 + TA
              + department.classification
                          + as.factor(gaise.familiar)
                  + class.session.length.mins.ind
                 + prev.stat.course.ind #0/1 with 0 = no previous stat course
              #     + as.factor(prev.stat.course)  #Yes AP, Yes not AP, No
                 + as.factor(as.factor(class.meet.weeks))  #quarter, semester, full year
                + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
               + as.factor(incentive.post.ind)
              #  + as.factor(incentive.post.ind)
                 + section_gpa
              + section_SATACTzscore
                + section_pre_perc
                + section_Affect_pre
               + section_Interest_pre
                + section_Cog.Comp_pre
                + section_Effort_pre
                + section_Value_pre
                + section_Difficulty_pre
              + isi.workshop
                + as.factor(position.classification)
                 + as.factor(advanced.stats.degree)
               + as.factor(student.type)
                 +  as.factor(analyzing.data.experience)
              + as.factor(CarnegieClassification)
              #  + section_ClassSize
              + class.size.ind
              + pre_perc*instructor.gender 
              + pre_perc*as.factor(gender_pre) 
                        +  instructor.gender*as.factor(gender_pre) 
                   +gpa_pre*as.factor(gender_pre)
               +SATACTzscore*as.factor(gender_pre)
                    + Interest_pre*instructor.gender
                    + Difficulty_pre*instructor.gender
                      + Value_pre*instructor.gender
                      + semester   
              + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
              , data=data_box, na.action = na.exclude )
summary(model8)
anova(model8)

data = data1415
data = data1516
data = data1617

data_box <- data[data$textbook=="ISI"|data$textbook=="ISI1st"|data$textbook=="OtherSBI"|data$textbook=="NotSBI"|data$textbook=="NotSBI2",]
data_box <- data_box[is.na(data_box$textbook)==0,]
data_box$textbook <- factor(data_box$textbook, levels=c("ISI","ISI1st","OtherSBI","NotSBI","NotSBI2"))

model9 = lmer(new_ach_gain ~ 1   
              + pre_perc 
              + overall_attitude_pre
              + gpa            + I(gpa*gpa)
              + SATACTzscore
              #              + section_SATACTzscore
              # +section_gpa
              #+ section_Overall_Attitude_Pre
              + incentive.post.ind
              #+ morecourses
              #+ position.classification
             # + prev.stat.course.ind #0/1 with 0 = no previous stat course
              + race
              #+ whytake
              #+ firstgen  #only year 3, 1 = yes, 2  = no
              # + I(section_gpa*section_gpa)
              #  + section_Interest_pre
              #+math.prereq.coded
              #  + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
              + as.factor(CarnegieClassification)
              + gaise.familiar
              + gender #1 = female, 2 = male
              + instructor.gender
              + pre_perc*instructor.gender 
              + pre_perc*gender 
              + textbook
              #+ (pre_perc |InstructorName_Section_Semester )
              + ( 1 | institution/instructor.last.name)
              , data=data_box )
summary(model9)
anova(model9)

#0.049296 0.22203 without textbook year 3 
#0.049437 0.22234  with textbook year 3

#0.04956  with textbook year 3
#0.049532 without textbook 

ggplot(data_box) + geom_point(aes(x=pre_perc,y=new_ach_gain), shape=1, size=2, alpha=.2, color="navy") +
  geom_smooth(aes(x=pre_perc,y=new_ach_gain,group=textbook, color=textbook),  method="lm", se=FALSE) +
  geom_smooth(aes(x=pre_perc,y=new_ach_gain), color="gold2", method="lm", se=FALSE) +
  xlab("Pre concept") + ylab("Ach gain") + theme_classic() 
#+   ggtitle("Plot of achievable gain versus pre concepts\nperformance by section with regression lines imposed")

model9 = lmer(new_ach_gain ~ 1   
              + pre_perc 
              + overall_attitude_pre
              + gender #1 = female, 2 = male
              + gpa_pre            + I(gpa_pre*gpa_pre)
              + SATACTzscore
              + textbook
              #         + section_SATACTzscore
              #          + section_Interest_pre
              + as.factor(CarnegieClassification)
              + instructor.gender
              + gaise.familiar
              #              + pre_perc*instructor.gender 
              #              + pre_perc*gender 
              + (pre_perc|InstructorName_Section_Semester)
              + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
              , data=data_box )
summary(model9)
anova(model9)




data$pre

intmodel = lm(new_ach_gain ~ pre_perc + instructor.gender, data=data_box)
summary(intmodel)

lines(data_box$pre_perc, .27-.3*data_box$pre_perc+.015*1*data_box$pre_perc)
lines(data_box$pre_perc, .27-.3*data_box$pre_perc+.015*-1*data_box$pre_perc)

intmodel = lm(new_ach_gain ~ pre_perc + gender_pre, data=data_box)
summary(intmodel)

plot(data_box$new_ach_gain~data_box$pre_perc, col=as.factor(data_box$gender_pre))
lines(data_box$pre_perc, .42-.69*data_box$pre_perc-.103*1 + .263*data_box$pre_perc)
lines(data_box$pre_perc, .42-.69*data_box$pre_perc-.103*2 + 2*.263*data_box$pre_perc, col="red")

newdat = data.frame(gpa_pre=seq(1, 4, length.out = 100))
preds = predict(model1, newdata = newdat, se.fit=FALSE)
lines(x = newdat[,1], y = preds, col="blue")

plot(data_box$new_ach_gain~data_box$pre_perc, col = data_box$gender_pre, ylab="achievable gain", xlab="pre-test")
model1b=lm(new_ach_gain ~ pre_perc + gender_pre + pre_perc*gender_pre, data = data_box[data_box$gender_pre==1,])
model1c=lm(new_ach_gain ~ pre_perc + gender_pre + pre_perc*gender_pre, data = data_box[data_box$gender_pre==2,])
abline(model1b)
abline(model1c, col="red")

model1 = lm(new_ach_gain~pre_perc + instructor.gender + pre_perc*instructor.gender, data =data_box)
plot(data_box$new_ach_gain~data_box$pre_perc, col = data_box$instructor.gender, ylab="achievable gain", xlab="pre-test")
newdat = data.frame(pre_perc=seq(.2, .8, length.out = 100), instructor.gender = "Female")
preds = predict(model1, newdata = newdat, se.fit=FALSE)
lines(x = newdat[,1], y = preds, col="black")
newdat = data.frame(pre_perc=seq(.2, .8, length.out = 100), instructor.gender = "Male")
preds = predict(model1, newdata = newdat, se.fit=FALSE)
lines(x = newdat[,1], y = preds, col="red")



model1b=lm(new_ach_gain ~ pre_perc + instructor.gender + pre_perc*instructor.gender, data = data_box[data_box$instructor.gender=="Female",])
model1c=lm(new_ach_gain ~ pre_perc + instructor.gender + pre_perc*instructor.gender, data = data_box[data_box$instructor.gender=="Male",])
abline(model1b)
abline(model1c, col="red")
