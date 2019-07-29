# SET WORKING DIRECTORY #
#setwd("/Users/Stephanie/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
#setwd("/Users/bchance/Dropbox/ISI Assessment Data Summer 2017/Retention Data/Files I Will Use/")

# PACKAGES #
load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))
library(lme4)
library(ggplot2)

# 2015 DATA SET #
data <- read.csv("MERGED_16Month.csv")
# 2016 DATA SET #
data <- read.csv("MERGED_16Month2016.csv")
# 2015 AND 2016 DATA SETS COMBINED #
data1 <- read.csv("MERGED_16Month.csv")
data1$year <- rep(2015,length(data1$ID))
data2 <- read.csv("MERGED_16Month2016.csv") 
data2$year <- rep(2016,length(data2$ID))
library(plyr)
data <- rbind.fill(data1,data2)


# Info About 2015 16 Month Retention #
length(unique(data$institution))
length(unique(data$instructor.last.name))
length(unique(data$InstructorName_Section_Semester))
length(data$Q17_ret)

# Basic averages by textbook #
iscamsummary(data$new_pre_perc,data$textbook.classification2)
iscamsummary(data$new_post_perc,data$textbook.classification2)
iscamsummary(data$new_ret_perc,data$textbook.classification2)
iscamsummary(data$new_ret2_perc,data$textbook.classification2)

############################## ANOVAS ###############################

### 16-M ANOVAS ###

summary(aov(data$new_Ret2_DC~data$textbook.classification2)) #0.98
summary(aov(data$new_Ret2_DS~data$textbook.classification2)) #0.04
summary(aov(data$new_Ret2_CI~data$textbook.classification2)) #0.91
summary(aov(data$new_Ret2_ST~data$textbook.classification2)) #0.03
summary(aov(data$new_Ret2_Sim~data$textbook.classification2)) #0.03
summary(aov(data$new_ret2_perc~data$textbook.classification2)) #0.08
summary(aov((data$new_ret2_perc-data$new_post_perc)~data$textbook.classification2)) #0.05

tapply(data$new_Ret2_DS,data$textbook.classification2,mean)
tapply(data$new_Ret2_ST,data$textbook.classification2,mean,na.rm=TRUE)
tapply(data$new_Ret2_Sim,data$textbook.classification2,mean)
tapply(data$new_ret2_perc,data$textbook.classification2,mean,na.rm=TRUE)
tapply((data$new_ret2_perc-data$new_post_perc),data$textbook.classification2,mean,na.rm=TRUE)

################################ Small Table in Paper ##############################

iscamsummary(data$new_pre_perc,data$textbook.classification2)
mean(data$new_pre_perc)
summary(aov(data$new_pre_perc~data$textbook.classification2))
iscamsummary(data$new_post_perc,data$textbook.classification2)
mean(data$new_post_perc)
summary(aov(data$new_post_perc~data$textbook.classification2))
iscamsummary(data$new_ret_perc,data$textbook.classification2)
mean(data$new_ret_perc)
summary(aov(data$new_ret_perc~data$textbook.classification2))
iscamsummary(data$new_ret2_perc,data$textbook.classification2)
mean(data$new_ret2_perc)
summary(aov(data$new_ret2_perc~data$textbook.classification2))

changeDC = data$new_Ret2_DC - data$new_Post_DC
changeDS = data$new_Ret2_DS - data$new_Post_DS
changeCI = data$new_Ret2_CI - data$new_Post_CI
changeST = data$new_Ret2_ST - data$new_Post_ST
changeSim = data$new_Ret2_Sim - data$new_Post_Sim

tapply(changeDC,data$textbook.classification2,mean,na.rm=TRUE)
mean(changeDC,na.rm=TRUE)
summary(aov(changeDC~data$textbook.classification2))
tapply(changeDS,data$textbook.classification2,mean,na.rm=TRUE)
mean(changeDS,na.rm=TRUE)
summary(aov(changeDS~data$textbook.classification2))
tapply(changeCI,data$textbook.classification2,mean,na.rm=TRUE)
mean(changeCI,na.rm=TRUE)
summary(aov(changeCI~data$textbook.classification2))
tapply(changeST,data$textbook.classification2,mean,na.rm=TRUE)
mean(changeST,na.rm=TRUE)
summary(aov(changeST~data$textbook.classification2))
tapply(changeSim,data$textbook.classification2,mean,na.rm=TRUE)
mean(changeSim,na.rm=TRUE)
summary(aov(changeSim~data$textbook.classification2))


########################## Large Table in Paper ################################

ISI_frame <- data[data$textbook.classification2=="ISI",]
other_frame <- data[data$textbook.classification2=="OtherSBI",]
not_frame <- data[data$textbook.classification2=="NotSBI",]

#PRE TEST COLUMN#
iscamsummary(data$new_pre_perc,data$textbook.classification2)
iscamsummary(data$new_Pre_DC,data$textbook.classification2)
iscamsummary(data$new_Pre_DS,data$textbook.classification2)
iscamsummary(data$new_Pre_CI,data$textbook.classification2)
iscamsummary(data$new_Pre_ST,data$textbook.classification2)
iscamsummary(data$new_Pre_Sim,data$textbook.classification2)

#POST TEST COLUMN#
iscamsummary(data$new_post_perc,data$textbook.classification2)
iscamsummary(data$new_Post_DC,data$textbook.classification2)
iscamsummary(data$new_Post_DS,data$textbook.classification2)
iscamsummary(data$new_Post_CI,data$textbook.classification2)
iscamsummary(data$new_Post_ST,data$textbook.classification2)
iscamsummary(data$new_Post_Sim,data$textbook.classification2)

#RET TEST COLUMN#
iscamsummary(data$new_ret_perc,data$textbook.classification2)
iscamsummary(data$new_Ret_DC,data$textbook.classification2)
iscamsummary(data$new_Ret_DS,data$textbook.classification2)
iscamsummary(data$new_Ret_CI,data$textbook.classification2)
iscamsummary(data$new_Ret_ST,data$textbook.classification2)
iscamsummary(data$new_Ret_Sim,data$textbook.classification2)

#RET2 TEST COLUMN#
iscamsummary(data$new_ret2_perc,data$textbook.classification2)
iscamsummary(data$new_Ret2_DC,data$textbook.classification2)
iscamsummary(data$new_Ret2_DS,data$textbook.classification2)
iscamsummary(data$new_Ret2_CI,data$textbook.classification2)
iscamsummary(data$new_Ret2_ST,data$textbook.classification2)
iscamsummary(data$new_Ret2_Sim,data$textbook.classification2)

#P-VALUE COLUMN#
summary(aov(data$new_ret2_perc~data$textbook.classification2))
summary(aov(data$new_Ret2_DC~data$textbook.classification2))
summary(aov(data$new_Ret2_DS~data$textbook.classification2))
summary(aov(data$new_Ret2_CI~data$textbook.classification2))
summary(aov(data$new_Ret2_ST~data$textbook.classification2))
summary(aov(data$new_Ret2_Sim~data$textbook.classification2))

#CHANGE COLUMN#
iscamsummary((data$new_ret2_perc - data$new_post_perc),data$textbook.classification2)
iscamsummary((data$new_Ret2_DC - data$new_Ret2_DC),data$textbook.classification2)
iscamsummary((data$new_Ret2_DS - data$new_Ret2_DS),data$textbook.classification2)
iscamsummary((data$new_Ret2_CI - data$new_Ret2_CI),data$textbook.classification2)
iscamsummary((data$new_Ret2_ST - data$new_Ret2_ST),data$textbook.classification2)
iscamsummary((data$new_Ret2_Sim - data$new_Ret2_Sim),data$textbook.classification2)

#P-VALUE#
t.test(ISI_frame$new_ret2_perc-ISI_frame$new_post_perc)
t.test(other_frame$new_ret2_perc-other_frame$new_post_perc)
t.test(not_frame$new_ret2_perc-not_frame$new_post_perc)

t.test(ISI_frame$new_Ret2_DC-ISI_frame$new_Post_DC)
t.test(other_frame$new_Ret2_DC-other_frame$new_Post_DC)
t.test(not_frame$new_Ret2_DC-not_frame$new_Post_DC)

t.test(ISI_frame$new_Ret2_DS-ISI_frame$new_Post_DS)
t.test(other_frame$new_Ret2_DS-other_frame$new_Post_DS)
t.test(not_frame$new_Ret2_DS-not_frame$new_Post_DS)

t.test(ISI_frame$new_Ret2_CI-ISI_frame$new_Post_CI)
t.test(other_frame$new_Ret2_CI-other_frame$new_Post_CI)
t.test(not_frame$new_Ret2_CI-not_frame$new_Post_CI)

t.test(ISI_frame$new_Ret2_ST-ISI_frame$new_Post_ST)
t.test(other_frame$new_Ret2_ST-other_frame$new_Post_ST)
t.test(not_frame$new_Ret2_ST-not_frame$new_Post_ST)

t.test(ISI_frame$new_Ret2_Sim-ISI_frame$new_Post_Sim)
t.test(other_frame$new_Ret2_Sim-other_frame$new_Post_Sim)
t.test(not_frame$new_Ret2_Sim-not_frame$new_Post_Sim)


############################################ BOXPLOTS ######################################

# BOXPLOTS OF PRE / POST / RET / RET2 BY TEXTBOOK #
Test <- as.factor(c(rep("Pre",length(data$new_pre_perc)),rep("Post",length(data$new_post_perc)),rep("Ret(4)",length(data$new_ret_perc)),rep("Ret(16)",length(data$new_ret2_perc))))
Test <- factor(Test, levels = c("Pre", "Post", "Ret(4)","Ret(16)"))
perc <- c(data$new_pre_perc,data$new_post_perc,data$new_ret_perc,data$new_ret2_perc)
Textbook <- c(as.character(data$textbook.classification2),as.character(data$textbook.classification2),as.character(data$textbook.classification2),as.character(data$textbook.classification2))
data_box <- data.frame(Test,perc,Textbook)
data_box <- data_box[is.na(data_box$Textbook)==0,]
data_box$Textbook <- factor(data_box$Textbook, levels=c("ISI","OtherSBI","NotSBI"))
data_box$Textbook <- as.character(data_box$Textbook)
data_box$Textbook[data_box$Textbook=="ISI"] <- "ISI (n=98)"
data_box$Textbook[data_box$Textbook=="OtherSBI"] <- "OtherSBI (n=47)"
data_box$Textbook[data_box$Textbook=="NotSBI"] <- "NotSBI (n=112)"
data_box$Textbook <- factor(data_box$Textbook, levels=c("ISI (n=98)","OtherSBI (n=47)","NotSBI (n=112)"))

ggplot(aes(y=perc,x=Test, fill=Textbook),data=data_box) + 
  geom_boxplot() + xlab("Test") + ylab("Percent of Concept Questions Correct") + ggtitle("Boxplots of Percent Correct by Textbook") + 
  theme_bw() 


# Overall Ret by Subscale #
change1DC = data$new_Ret2_DC - data$new_Post_DC
change1DS = data$new_Ret2_DS - data$new_Post_DS
change1CI = data$new_Ret2_CI - data$new_Post_CI
change1ST = data$new_Ret2_ST - data$new_Post_ST
change1Sim = data$new_Ret2_Sim - data$new_Post_Sim
l = length(data$textbook.classification)
subscale <- rep(c("DC","DS","CI","ST","Sim"),c(l,l,l,l,l))
Ret <- c(data$new_Ret2_DC,data$new_Ret2_DS,data$new_Ret2_CI,data$new_Ret2_ST,data$new_Ret2_Sim)
Change <- c(change1DC,change1DS,change1CI,change1ST,change1Sim)
Textbook <- rep(as.character(data$textbook.classification2),5)
summary(aov(Ret~subscale))
data_box <- data.frame(Ret,Change,subscale,Textbook)
data_box <- data_box[is.na(data_box$Textbook)==0,]
data_box$subscale <- factor(data_box$subscale, levels = c("DC","DS","CI","ST","Sim"))
data_box$Textbook <- factor(data_box$Textbook, levels=c("ISI","OtherSBI","NotSBI"))
data_box$Textbook <- as.character(data_box$Textbook)
data_box$Textbook[data_box$Textbook=="ISI"] <- "ISI (n=197)"
data_box$Textbook[data_box$Textbook=="OtherSBI"] <- "OtherSBI (n=127)"
data_box$Textbook[data_box$Textbook=="NotSBI"] <- "NotSBI (n=284)"
data_box$Textbook <- factor(data_box$Textbook, levels=c("ISI (n=197)","OtherSBI (n=127)","NotSBI (n=284)"))
ggplot(aes(y=Ret,x=subscale, fill=Textbook),data=data_box) + 
  geom_boxplot() + xlab("Subscale") + ylab("16-Month Ret Percent Correct") + ggtitle("Retention Score By Subscale and Textbook") + 
  theme_bw()


# BOXPLOTS OF 16-MONTH RET BY TEXTBOOK FOR EACH SECTION #
data_box <- data[is.na(data$textbook.classification2)==0,]
data_box$textbook.classification2 <- factor(data_box$textbook.classification2, levels=c("ISI","OtherSBI","NotSBI"))
means <- tapply(data_box$new_ach_gain,data_box$textbook.classification2,mean)
grand_mean <- mean(data_box$new_ach_gain)
dev.off()
ggplot(aes(y=new_ach_gain,x=textbook.classification2,group=InstructorName_Section_Semester,fill=textbook.classification2),data=data_box) + geom_boxplot() + xlab("Section") + ylab("Achievable Gain") + ggtitle("Boxplots of Achievable Gains by Section and Textbook") + theme_bw() +
  geom_segment(aes(x=.647,y=means[1],xend=1.355,yend=means[1]), colour="deeppink4", size=1.3) +
  geom_segment(aes(x=1.613,y=means[2],xend=2.355,yend=means[2]), colour="deeppink4", size=1.3) +
  geom_segment(aes(x=2.611,y=means[3],xend=3.363,yend=means[3]), colour="deeppink4", size=1.3) +
  geom_segment(aes(x=.5,y=grand_mean,xend=3.5,yend=grand_mean),colour="navyblue",show.legend=FALSE)

# OVERALL RETENTION BY TEXTBOOK AND SUBSCALE #
change1DC = data$new_Ret_DC - data$new_Post_DC
change1DS = data$new_Ret_DS - data$new_Post_DS
change1CI = data$new_Ret_CI - data$new_Post_CI
change1ST = data$new_Ret_ST - data$new_Post_ST
change1Sim = data$new_Ret_Sim - data$new_Post_Sim
l = length(data$textbook.classification)
subscale <- rep(c("DC","DS","CI","ST","Sim"),c(l,l,l,l,l))
Ret <- c(data$new_Ret_DC,data$new_Ret_DS,data$new_Ret_CI,data$new_Ret_ST,data$new_Ret_Sim)
textbook <- rep(as.character(data$textbook.classification2),5)
summary(aov(Ret~subscale))
data_box <- data.frame(Ret,subscale,textbook)
data_box <- data_box[is.na(data_box$textbook)==0,]
data_box$subscale <- factor(data_box$subscale, levels = c("DC","DS","CI","ST","Sim"))
data_box$textbook <- factor(data_box$textbook, levels = c("ISI","OtherSBI","NotSBI"))
ggplot(aes(y=Ret,x=subscale, fill=textbook),data=data_box) + 
  geom_boxplot() + xlab("Subscale") + ylab("4-Month Ret Percent Correct") + ggtitle("Retention Score By Subscale and Textbook") + 
  theme_bw()

# OVERALL RETENTION BY TEXTBOOK AND SUBSCALE #
l = length(data$textbook.classification)
subscale <- rep(c("Affect","Cog. Comp","Difficulty","Effort","Value","Interest"),c(l,l,l,l,l,l))
Ret <- c(data$Affect_ret2,data$Cognitive.Competence_ret2,data$Difficulty_ret2,data$Effort_ret2,data$Value_ret2,data$Interest_ret2)
textbook <- rep(as.character(data$textbook.classification2),6)
summary(aov(Ret~subscale))
data_box <- data.frame(Ret,subscale,textbook)
data_box <- data_box[is.na(data_box$textbook)==0,]
data_box$subscale <- factor(data_box$subscale, levels = c("Affect","Cog. Comp","Difficulty","Effort","Value","Interest"))
data_box$textbook <- factor(data_box$textbook, levels = c("ISI","OtherSBI","NotSBI"))
ggplot(aes(y=Ret,x=subscale, fill=textbook),data=data_box) + 
  geom_boxplot() + xlab("Subscale") + ylab("16-Month Average Attitude (Likert Scale)") + ggtitle("Attitudes By Subscale and Textbook") + 
  theme_bw()

############################## Question-By-Question Analysis ############################

### Prep for tables ###
# New textbook Classification #
first_post = which(colnames(data)=="Q17_post_c_RW")
last_post = which(colnames(data)=="Q47_post_c_RW")
first_ret = which(colnames(data)=="Q17_ret_c_RW")
last_ret = which(colnames(data)=="Q47_ret_c_RW")
first_ret2 = which(colnames(data)=="Q17_ret2_c_RW")
last_ret2 = which(colnames(data)=="Q47_ret2_c_RW")

ISI_frame_post_s <- data[ which(data$textbook.classification2=="ISI"),c(first_post:last_post)]
ISI_frame_ret_s <- data[ which(data$textbook.classification2=="ISI"),c(first_ret:last_ret)]
ISI_frame_ret2_s <- data[ which(data$textbook.classification2=="ISI"),c(first_ret2:last_ret2)]
otherSBI_frame_post_s <- data[ which(data$textbook.classification2=="OtherSBI"),c(first_post:last_post)]
otherSBI_frame_ret_s <- data[ which(data$textbook.classification2=="OtherSBI"),c(first_ret:last_ret)]
otherSBI_frame_ret2_s <- data[ which(data$textbook.classification2=="OtherSBI"),c(first_ret2:last_ret2)]
nonSBI_frame_post_s <- data[ which(data$textbook.classification2=="NotSBI"),c(first_post:last_post)]
nonSBI_frame_ret_s <- data[ which(data$textbook.classification2=="NotSBI"),c(first_ret:last_ret)]
nonSBI_frame_ret2_s <- data[ which(data$textbook.classification2=="NotSBI"),c(first_ret2:last_ret2)]

Percent.Correct = c("Post","Ret","Ret2") 

######## Function to get tables (must run prep first) ########
tables_short <- function(questions_short){
  for (i in questions_short){
    
    ISI = c(round(mean(ISI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_ret2_s[,(i-16)],na.rm=TRUE),digits=2))
    OtherSBI = c(round(mean(otherSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(otherSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(otherSBI_frame_ret2_s[,(i-16)],na.rm=TRUE),digits=2))
    NonSBI = c(round(mean(nonSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_ret2_s[,(i-16)],na.rm=TRUE),digits=2))
    print(paste("Question",i,sep=" "))
    print(data.frame(Percent.Correct,ISI,OtherSBI,NonSBI,row.names=NULL)) 
  }} 
##### Enter into this vector the questions that you want tables for. 17=Q17, 18=Q18, ... 46=Q46.a, 47=Q46.b, 48=Q47#
questions_short <- c(17:48)
tables_short(questions_short)


######################## Concept Subscales #####################
t.test(data$new_ret2_perc~data$textbook.classification3)
t.test(data_short$new_ret2_perc~data_short$textbook.classification3)
t.test((data$new_ret2_perc-data$new_post_perc)~data$textbook.classification3)
t.test((data$new_Ret2_DC-data$new_Post_DC)~data$textbook.classification3)
t.test((data$new_Ret2_DS-data$new_Post_DS)~data$textbook.classification3)
t.test((data$new_Ret2_CI-data$new_Post_CI)~data$textbook.classification3)
t.test((data$new_Ret2_ST-data$new_Post_ST)~data$textbook.classification3)
t.test((data$new_Ret2_Sim-data$new_Post_Sim)~data$textbook.classification3)

t.test(data$new_Ret2_DC~data$textbook.classification3) #0.9
t.test(data$new_Ret2_DS~data$textbook.classification3) #0.4
t.test(data$new_Ret2_CI~data$textbook.classification3) #0.4
t.test(data$new_Ret2_ST~data$textbook.classification3) #0.069
t.test(data$new_Ret2_Sim~data$textbook.classification3) #0.08


### Boxplots of 16-month ret by concept subscale, different colored boxes for SBI / Not ###
ret.DC <- data$new_Ret2_DC
ret.DS <- data$new_Ret2_DS
ret.CI <- data$new_Ret2_CI
ret.ST <- data$new_Ret2_ST
ret.Sim <- data$new_Ret2_Sim
new_ret_perc <- c(ret.DC,ret.DS,ret.CI,ret.ST,ret.Sim)
concept_sub <- rep(c("DC","DS","CI","ST","Sim"),c(length(ret.DC),length(ret.DS),length(ret.CI),length(ret.ST),length(ret.Sim)))
Curriculum <- rep(data$textbook.classification3,5)
box <- data.frame(new_ret_perc,concept_sub,Curriculum)
box <- box[box$Curriculum=="NotSBI"|box$Curriculum=="SBI",]
box <- box[is.na(box$concept_sub)!=1,]
dev.off()
ggplot(aes(y = new_ret_perc, x = concept_sub, fill = Curriculum), data = box) + geom_boxplot() + xlab("Concept Subscale") + ylab("16-Month Retention Percentage Correct")


### Boxplots of Change in perc correct by concept subscale, different colored boxes for SBI/Not ###
change.DC <- data$new_Ret2_DC - data$new_Post_DC
change.DS <- data$new_Ret2_DS - data$new_Post_DS
change.CI <- data$new_Ret2_CI - data$new_Post_CI
change.ST <- data$new_Ret2_ST - data$new_Post_ST
change.Sim <- data$new_Ret2_Sim - data$new_Post_Sim

new_change_perc <- c(change.DC,change.DS,change.CI,change.ST,change.Sim)
concept_sub <- rep(c("DC","DS","CI","ST","Sim"),c(length(change.DC),length(change.DS),length(change.CI),length(change.ST),length(change.Sim)))
Curriculum <- rep(data$textbook.classification3,5)
box <- data.frame(new_change_perc,concept_sub,Curriculum)
box <- box[box$Curriculum=="NotSBI"|box$Curriculum=="SBI",]
box <- box[is.na(box$concept_sub)!=1,]
dev.off()
ggplot(aes(y = new_change_perc, x = concept_sub, fill = Curriculum), data = box) + geom_boxplot() + xlab("Concept Subscale") + ylab("Change in Percentage Correct (16-Month Ret - Post)")




######################### Future Stat Class ###############################
#First group is where they didn't take another stat class#
#16-month retention#
data$subsequent <- rep(0,length(data$ID))
data$subsequent[data$sub2_ret==2] <- 1

data$subsequent2 <- rep(0,length(data$ID))
data$subsequent2[data$sub2_ret2==2] <- 1

table(data$subsequent1,data$textbook.classification2)



table(data$subsequent_ret)
t.test(data$new_post_perc[data$Stats.Course.No=="1"],data$new_post_perc[is.na(data$Stats.Course.No)==1])
t.test(data$new_ret_perc[data$Stats.Course.No=="1"],data$new_ret_perc[is.na(data$Stats.Course.No)==1])
t.test(data$change1[data$Stats.Course.No=="1"],data$change1[is.na(data$Stats.Course.No)==1])


# Creating a data set where we only have students who didn't take another stat class between assessments #
data_short <- data[data$subsequent==0 & data$subsequent2==0,]


###################### ANOVA of Curriculums ########################
summary(aov(data$new_ret_perc~data$textbook.classification2))
summary(aov(data$new_ret2_perc~data$textbook.classification2))
summary(aov(change_ret_post~data$textbook.classification2))

# DC ANOVA #
boxplot(changeDC ~ data$textbook.classification2)
summary(aov(data$new_Ret2_DC~data$textbook.classification2)) 
# 0.94
summary(aov(changeDC~data$textbook.classification2)) 
# 0.022* (otherSBI lower?)

# DS ANOVA #
boxplot(changeDS ~ data$textbook.classification2)
summary(aov(data$new_Ret2_DS~data$textbook.classification2)) 
# 0.11
summary(aov(changeDS~data$textbook.classification2)) 
# 0.36

# CI ANOVA #
boxplot(changeCI ~ data$textbook.classification2)
summary(aov(data$new_Ret2_CI~data$textbook.classification2)) 
# 0.53
summary(aov(changeCI~data$textbook.classification2))
# 0.53

# ST ANOVA #
boxplot(changeST ~ data$textbook.classification2)
summary(aov(data$new_Ret2_ST~data$textbook.classification2))
# 0.16
summary(aov(changeST~data$textbook.classification2))
# 0.51

# Sim ANOVA #
boxplot(changeSim ~ data$textbook.classification2)
summary(aov(data$new_Ret2_Sim~data$textbook.classification2))
# 0.19
summary(aov(changeSim~data$textbook.classification2))
# 0.66


############################## Attitudes ##############################

cor(data$new_ret2_perc,data$Affect_ret2,use="complete.obs")
# Affect: 0.22
cor(data$new_ret2_perc,data$Interest_ret2,use="complete.obs")
# Interest: 0.29
cor(data$new_ret2_perc,data$Value_ret2,use="complete.obs")
# Value: 0.39
cor(data$new_ret2_perc,data$Difficulty_ret2,use="complete.obs")
# Difficulty: 0.18
cor(data$new_ret2_perc,data$Effort_ret2,use="complete.obs")
# Effort: -0.12
cor(data$new_ret2_perc,data$Cognitive.Competence_ret2,use="complete.obs")
# Cog.Comp: 0.31

plot(1,type='n', xlim=c(1, 7), ylim=c(.15, .7),ylab="16-Month Retention Percent Concept Correct",xlab="Retention Attitude Score (Likert Scale)")
abline(lm(data$new_ret2_perc~data$Affect_ret2),col="red")
abline(lm(data$new_ret2_perc~data$Interest_ret2),col="orange")
abline(lm(data$new_ret2_perc~data$Value_ret2),col="yellow")
abline(lm(data$new_ret2_perc~data$Cognitive.Competence_ret2),col="green")
abline(lm(data$new_ret2_perc~data$Difficulty_ret2),col="blue")
abline(lm(data$new_ret2_perc~data$Effort_ret2),col="purple")
legend("bottomright", legend=c("Affect","Interest","Value","Cog. Comp.","Difficulty","Effort"),
       col=c("red","orange","yellow","green","blue","purple"), lty=1, cex=0.8)


plot(1,type='n', xlim=c(1, 7), ylim=c(.15, .7),ylab="16-Month Retention Percent Concept Correct",xlab="Attitude Score (Likert Scale)")
abline(lm(data_short$new_ret2_perc~data_short$Affect_ret2),col="red")
abline(lm(data_short$new_ret2_perc~data_short$Interest_ret2),col="orange")
abline(lm(data_short$new_ret2_perc~data_short$Value_ret2),col="yellow")
abline(lm(data_short$new_ret2_perc~data_short$Cognitive.Competence_ret2),col="green")
abline(lm(data_short$new_ret2_perc~data_short$Difficulty_ret2),col="blue")
abline(lm(data_short$new_ret2_perc~data_short$Effort_ret2),col="purple")
legend("bottomright", legend=c("Affect","Interest","Value","Cog. Comp.","Difficulty","Effort"),
       col=c("red","orange","yellow","green","blue","purple"), lty=1, cex=0.8)



plot(1,type='n', xlim=c(1, 7), ylim=c(.15, .7),ylab="16-Month Retention Percent Concept Correct",xlab="Attitude Score (Likert Scale)")
abline(lm(data$new_ret2_perc~data$Affect_ret2),col="red")
abline(lm(data$new_ret2_perc~data$Interest_ret2),col="orange")
abline(lm(data$new_ret2_perc~data$Value_ret2),col="yellow")
abline(lm(data$new_ret2_perc~data$Cognitive.Competence_ret2),col="green")
abline(lm(data$new_ret2_perc~data$Difficulty_ret2),col="blue")
abline(lm(data$new_ret2_perc~data$Effort_ret2),col="purple")
legend("bottomright", legend=c("Affect","Interest","Value","Cog. Comp.","Difficulty","Effort"),
       col=c("red","orange","yellow","green","blue","purple"), lty=1, cex=0.8)


plot(1,type='n', xlim=c(1, 7), ylim=c(.15, .7),ylab="16-Month Retention Percent Concept Correct",xlab="Pre Attitude Score (Likert Scale)")
abline(lm(data$new_ret2_perc~data$Affect_pre),col="red")
abline(lm(data$new_ret2_perc~data$Interest_pre),col="orange")
abline(lm(data$new_ret2_perc~data$Value_pre),col="yellow")
abline(lm(data$new_ret2_perc~data$Cognitive.Competence_pre),col="green")
abline(lm(data$new_ret2_perc~data$Difficulty_pre),col="blue")
abline(lm(data$new_ret2_perc~data$Effort_pre),col="purple")
legend("bottomright", legend=c("Affect","Interest","Value","Cog. Comp.","Difficulty","Effort"),
       col=c("red","orange","yellow","green","blue","purple"), lty=1, cex=0.8)


summary(lm(data$new_ret2_perc~data$Affect_ret2))
# Affect: 0.004*
summary(lm(data$new_ret2_perc~data$Interest_ret2))
# Interest: 0.00014*
summary(lm(data$new_ret2_perc~data$Value_ret2))
# Value: <0.0001*
summary(lm(data$new_ret2_perc~data$Difficulty_ret2))
# Difficulty: 0.0188
summary(lm(data$new_ret2_perc~data$Effort_ret2))
# Effort: 0.119
summary(lm(data$new_ret2_perc~data$Cognitive.Competence_ret2))
# Cog.Comp: <0.0001*


### Attitude Retention ###
ret.Affect <- data_short$Affect_ret2
ret.Cog <- data_short$Cognitive.Competence_ret2
ret.Diff <- data_short$Difficulty_ret2
ret.Effort <- data_short$Effort_ret2
ret.Interest <- data_short$Interest_ret2
ret.Value <- data_short$Value_ret2


new_ret_perc <- c(ret.Affect,ret.Cog,ret.Diff,ret.Effort,ret.Interest,ret.Value)
attitude_sub <- rep(c("Affect","Cog.Comp","Difficulty","Effort","Interest","Value"),c(length(ret.Affect),length(ret.Cog),length(ret.Diff),length(ret.Effort),length(ret.Interest),length(ret.Value)))
Curriculum <- rep(data_short$textbook.classification3,6)
box <- data.frame(new_ret_perc,attitude_sub,Curriculum)
box <- box[box$Curriculum=="NotSBI"|box$Curriculum=="SBI",]
box <- box[is.na(box$attitude_sub)!=1,]
dev.off()
ggplot(aes(y = new_ret_perc, x = attitude_sub, fill = Curriculum), data = box) + geom_boxplot() + xlab("Attitude Subscale") + ylab("16-Month Retention Average Attitude Response")

# Attitudes #
t.test(data$Affect_ret2~data$textbook.classification3) #0.9
t.test(data$Cognitive.Competence_ret2~data$textbook.classification3) #0.92
t.test(data$Difficulty_ret2~data$textbook.classification3) #0.7
t.test(data$Effort_ret2~data$textbook.classification3) #0.87
t.test(data$Interest_ret2~data$textbook.classification3) #0.17
t.test(data$Value_ret2~data$textbook.classification3) #0.227

t.test((data$Affect_ret2-data$Affect_post)~data$textbook.classification3) #0.2
t.test((data$Cognitive.Competence_ret2-data$Cognitive.Competence_post)~data$textbook.classification3) #0.5
t.test((data$Difficulty_ret2-data$Difficulty_post)~data$textbook.classification3) #0.9
t.test((data$Effort_ret2-data$Effort_post)~data$textbook.classification3) #0.08
t.test((data$Interest_ret2-data$Interest_post)~data$textbook.classification3) #0.13
t.test((data$Value_ret2-data$Value_post)~data$textbook.classification3) #0.34


summary(aov(data$Affect_ret2~data$textbook.classification3)) #0.289
summary(aov(data$Cognitive.Competence_ret2~data$textbook.classification3)) #0.32
summary(aov(data$Effort_ret2~data$textbook.classification3)) #0.76
summary(aov(data$Interest_ret2~data$textbook.classification3)) #0.2
summary(aov(data$Value_ret2~data$textbook.classification3)) #0.14
summary(aov(data$Difficulty_ret2~data$textbook.classification3)) #0.49


tapply(data$Affect_ret2,data$textbook.classification3,mean,na.rm=TRUE) #
tapply(data$Cognitive.Competence_ret2,data$textbook.classification3,mean,na.rm=TRUE) #
tapply(data$Effort_ret2,data$textbook.classification3,mean,na.rm=TRUE) #
tapply(data$Interest_ret2,data$textbook.classification3,mean,na.rm=TRUE) #
tapply(data$Value_ret2,data$textbook.classification3,mean,na.rm=TRUE) #
tapply(data$Difficulty_ret2,data$textbook.classification3,mean,na.rm=TRUE) #

### Seeing if there is an increase ###

#Affect (Increaesed)#
mean(data$Affect_pre,na.rm=TRUE)
mean(data$Affect_post,na.rm=TRUE)
mean(data$Affect_ret,na.rm=TRUE)
mean(data$Affect_ret2,na.rm=TRUE)

#Interest (decreased)#
mean(data$Interest_pre,na.rm=TRUE)
mean(data$Interest_post,na.rm=TRUE)
mean(data$Interest_ret,na.rm=TRUE)
mean(data$Interest_ret2,na.rm=TRUE)

#Value (stayed the same)#
mean(data$Value_pre,na.rm=TRUE)
mean(data$Value_post,na.rm=TRUE)
mean(data$Value_ret,na.rm=TRUE)
mean(data$Value_ret2,na.rm=TRUE)

#Difficulty (increased)#
mean(data$Difficulty_pre,na.rm=TRUE)
mean(data$Difficulty_post,na.rm=TRUE)
mean(data$Difficulty_ret,na.rm=TRUE)
mean(data$Difficulty_ret2,na.rm=TRUE)

#Cog Comp (stayed the same)#
mean(data$Cognitive.Competence_pre,na.rm=TRUE)
mean(data$Cognitive.Competence_post,na.rm=TRUE)
mean(data$Cognitive.Competence_ret,na.rm=TRUE)
mean(data$Cognitive.Competence_ret2,na.rm=TRUE)

### ANOVAS ###
summary(aov(data$Affect_ret2~data$textbook.classification2)) #0.57
summary(aov(data$Cognitive.Competence_ret2~data$textbook.classification2)) #0.23
summary(aov(data$Effort_ret2~data$textbook.classification2)) #0.91
summary(aov(data$Interest_ret2~data$textbook.classification2)) #0.34
summary(aov(data$Value_ret2~data$textbook.classification2)) #0.13
summary(aov(data$Difficulty_ret2~data$textbook.classification2)) #0.64



######################## Miscellaneous Variable Exploration #######################

# Age
mean(data$age_pre,na.rm=TRUE)

# Gender
table(data$gender_pre)

# Race
table(data$race_origin)

# GPA
cor(data$new_ret2_perc,data$gpa_pre,use="complete.obs")
summary(lm(data$new_ret2_perc~data$gpa_pre))

# SAT/ACT
cor(data$new_ret2_perc,data$SATACTzscore,use="complete.obs") 
# Correlation: 0.29
summary(lm(data$new_ret2_perc~data$SATACTzscore))
#  <0.0001

# Carnegie Classification
iscamsummary(data$new_ret2_perc,data$CarnegieClassification)
Bac <- data[data$CarnegieClassification=="Baccalaureate Colleges",]
Doctoral <- data[data$CarnegieClassification=="Doctoral Universities",]
Masters <- data[data$CarnegieClassification=="Master's",]
carnegie_anova <- c(Bac$new_ret2_perc,Doctoral$new_ret2_perc,Masters$new_ret2_perc)
carnegie_desc <- rep(c("Baccalaureate","Doctoral","Masters"),c(length(Bac$new_ret2_perc),length(Doctoral$new_ret2_perc),length(Masters$new_ret2_perc)))
summary(aov(carnegie_anova~carnegie_desc))
# 0.814

# Class Size
cor(data$new_ret2_perc,data$class.size.end,use="complete.obs")
# Correlation: -0.005
summary(lm(data$new_ret2_perc~data$class.size.end)) 
# 0.95

# Percent Lecture
cor(data$new_ret2_perc,as.numeric(data$percent.lecture),use="complete.obs")
# Correlation: 0.046
summary(lm(data$new_ret2_perc~as.numeric(data$percent.lecture))) 
# 0.55 

# Years Teaching
cor(data$new_ret2_perc,data$years.teaching.intro.stats,use="complete.obs")
# Correlation: -0.047
summary(lm(data$new_ret2_perc~data$years.teaching.intro.stats))
# 0.54

# GAISE
iscamsummary(data$new_ret2_perc,data$gaise.familiar)
completely <- data[data$gaise.familiar=="Completely",]
not.familiar <- data[data$gaise.familiar=="I am not familiar with GAISE",]
mostly <- data[data$gaise.familiar=="Mostly",]
gaise_anova <- c(completely$new_ret2_perc,not.familiar$new_ret2_perc,mostly$new_ret2_perc)
gaise_desc <- rep(c("Completely","Not Familiar","Mostly"),c(length(completely$new_ret2_perc),length(not.familiar$new_ret2_perc),length(mostly$new_ret2_perc)))
summary(aov(gaise_anova~gaise_desc)) 
# p-value: 0.13
t.test(completely$new_ret2_perc,not.familiar$new_ret2_perc)
# p-value: 0.077
t.test(completely$new_ret2_perc,mostly$new_ret2_perc)
# p-value: 0.12
t.test(mostly$new_ret2_perc,not.familiar$new_ret2_perc)
# p-value: 0.72


# How well have you done in previous math courses#
table(data$how.well_ret2)
iscamsummary(data$new_ret2_perc,data$how.well_ret2)
iscamsummary(change_ret_post,data$how.well_ret2)
summary(lm(data$new_ret2_perc~data$how.well_ret2))
# 0.0589
summary(lm(change_ret_post~data$how.well_ret2))
# 0.824

# How good at math are you #
table(data$how.good_ret2)
iscamsummary(data$new_ret2_perc,data$how.good_ret2)
iscamsummary(change_ret_post,data$how.good_ret2)
summary(aov(data$new_ret2_perc~data$how.good_ret2))
# 0.0589
summary(aov(change_ret_post~data$how.good_ret2))
# 0.925

# What is your field of study
table(data$field_ret2)
iscamsummary(data$new_ret2_perc,data$field_ret2)
summary(aov(data$new_ret2_perc~data$field_ret2))
# <0.0001***
iscamsummary(change_ret_post,data$field_ret2)
summary(aov(change_ret_post~data$field_ret2))
# 0.384

# How confident are you that you can master the material
table(data$confident_ret2)
cor(data$new_ret2_perc,data$confident_ret2,use="complete.obs")
# 0.12
summary(lm(data$new_ret2_perc~data$confident_ret2))
# <0.0001***
summary(lm(change_ret_post~data$confident_ret2))
# 0.177

# Why take this course
data$why1_ret2[is.na(data$why1_ret2)==1] <- 0
summary(aov(data$new_ret2_perc~data$why1_ret2))
summary(aov(change_ret_post~data$why1_ret2))
# It sounded interesting: 0.0045 / 0.863***
data$why2_ret2[is.na(data$why2_ret2)==1] <- 0
summary(aov(data$new_ret2_perc~data$why2_ret2))
# GE requirement: 0.75
data$why3_ret2[is.na(data$why3_ret2)==1] <- 0
summary(aov(data$new_ret2_perc~data$why3_ret2))
# For my major: 0.15

# If the choice was yours, would you have taken this course
table(data$choice_ret2)
cor(data$new_ret2_perc,data$choice_ret2,use="complete.obs")
# 0.236
summary(lm(data$new_ret2_perc~data$choice_ret2))
summary(lm(change_ret_post~data$choice_ret2))
# 0.0021* / 0.82***

# Instructor gender
table(data$instructor.gender)
summary(aov(data$new_ret2_perc~data$instructor.gender))
summary(aov(change_ret_post~data$instructor.gender))
# 0.146 / 0.118

# Class length
table(data$class.session.length.mins)

# Math prerequisite
table(data$math.prereq)

# Student classification
table(data$student.type)
summary(aov(data$new_ret2_perc~data$student.type))
summary(aov(change_ret_post~data$student.type))
# 0.693 / 0.0531

# TA
table(data$TA)
summary(aov(data$new_ret2_perc~data$TA))
summary(aov(change_ret_post~data$TA))
# 0.556 / 0.327

# Primary new content obtainment
table(data$primary.new.content.obtainment)

# Skills practice
summary(aov(data$new_ret2_perc~data$skills.practice_examples.in.class)) #0.864
summary(aov(data$new_ret2_perc~data$skills.practice_reading.assignment)) #0.226
summary(aov(data$new_ret2_perc~data$skills.practice_guided.activity)) #0.234
summary(aov(data$new_ret2_perc~data$skills.practice_textbook.homework.assignments)) #0.037

# Department Classification
table(data$dep_ind2)
summary(aov(data$new_ret2_perc~data$dep_ind2)) # 0.499
summary(aov(change_ret_post~data$dep_ind2)) # 0.337

# Years Teaching Experience
table(data$years.teaching.experience)
summary(aov(change_ret_post~data$years.teaching.experience)) # 0.652

# Time Meet #
table(data$time.meet.ind)
summary(aov(data$new_ret2_perc~data$time.meet.ind))
# 0.097
iscamsummary(data$new_ret_perc,data$time.meet.ind)



############################ HLM Models #############################
data$change <- data$new_ret_perc - data$new_post_perc

noNA3 <- data[data$textbook.classification!="",]
noNA3$textbook.classification <- as.character(noNA3$textbook.classification)
noNA3$textbook.classification <- as.factor(noNA3$textbook.classification)
noNA3$textbook.classification <- factor(noNA3$textbook.classification, levels = c("NotSBI", "OtherSBI", "ISI"))

hlm2 = lmer(new_ret2_perc ~ 1   
            + new_pre_perc 
            + new_post_perc
            + new_ret_perc
            + textbook.classification
            + new_ret_perc*textbook.classification
            + (1|institution/instructor.last.name/InstructorName_Section_Semester),data=noNA3)
summary(hlm2)


############################## Cronback Alphas #############################
install.packages("psych")
library(psych)

affect_pre <- data.frame(data$Q6.c_pre,data$Q6.d_pre,data$Q7.e_pre,data$Q7.h_pre,data$Q7.i_pre,data$Q8.h_pre)
affect_pre_results = alpha(affect_pre,na.rm=TRUE)
affect_post <- data.frame(data$Q6.c_post,data$Q6.d_post,data$Q7.e_post,data$Q7.h_post,data$Q7.i_post,data$Q8.h_post)
affect_post_results = alpha(affect_post,na.rm=TRUE)
affect_ret <- data.frame(data$Q6.c_ret,data$Q6.d_ret,data$Q7.e_ret,data$Q7.h_ret,data$Q7.i_ret,data$Q8.h_ret)
affect_ret_results = alpha(affect_ret,na.rm=TRUE)
affect_pre_results$total; affect_post_results$total;affect_ret_results$total
# 0.80 / 0.85 / 0.82

comp_pre <- data.frame(data$Q6.e_pre,data$Q7.a_pre,data$Q8.f_pre,data$Q9.a_pre,data$Q9.b_pre,data$Q9.e_pre)
comp_pre_results = alpha(comp_pre,na.rm=TRUE)
comp_post <- data.frame(data$Q6.e_post,data$Q7.a_post,data$Q8.f_post,data$Q9.a_post,data$Q9.b_post,data$Q9.e_post)
comp_post_results = alpha(comp_post,na.rm=TRUE)
comp_ret <- data.frame(data$Q6.e_ret,data$Q7.a_ret,data$Q8.f_ret,data$Q9.a_ret,data$Q9.b_ret,data$Q9.e_ret)
comp_ret_results = alpha(comp_ret,na.rm=TRUE)
comp_pre_results$total; comp_post_results$total; comp_ret_results$total
# 0.84 / 0.84 / 0.79

difficulty_pre <- data.frame(data$Q6.f_pre,data$Q6.h_pre,data$Q8.b_pre,data$Q8.d_pre,data$Q8.j_pre,data$Q9.d_pre,data$Q9.f_pre)
difficulty_pre_results = alpha(difficulty_pre,na.rm=TRUE)
difficulty_post <- data.frame(data$Q6.f_post,data$Q6.h_post,data$Q8.b_post,data$Q8.d_post,data$Q8.j_post,data$Q9.d_post,data$Q9.f_post)
difficulty_post_results = alpha(difficulty_post,na.rm=TRUE)
difficulty_ret <- data.frame(data$Q6.f_ret,data$Q6.h_ret,data$Q8.b_ret,data$Q8.d_ret,data$Q8.j_ret,data$Q9.d_ret,data$Q9.f_ret)
difficulty_ret_results = alpha(difficulty_ret,na.rm=TRUE)
difficulty_pre_results$total; difficulty_post_results$total; difficulty_ret_results$total
# 0.78 / 0.79 / 0.81

effort_pre <- data.frame(data$Q6.a_pre,data$Q6.b_pre,data$Q7.d_pre,data$Q8.g_pre)
effort_pre_results = alpha(effort_pre,na.rm=TRUE)
effort_post <- data.frame(data$Q6.a_post,data$Q6.b_post,data$Q7.d_post,data$Q8.g_post)
effort_post_results = alpha(effort_post,na.rm=TRUE)
effort_ret <- data.frame(data$Q6.a_ret,data$Q6.b_ret,data$Q7.d_ret,data$Q8.g_ret)
effort_ret_results = alpha(effort_ret,na.rm=TRUE)
effort_pre_results$total; effort_post_results$total; effort_ret_results$total
# 0.85 / 0.73 / 0.75

interest_pre <- data.frame(data$Q7.b_pre,data$Q7.j_pre,data$Q8.c_pre,data$Q8.i_pre)
interest_pre_results = alpha(interest_pre,na.rm=TRUE)
interest_post <- data.frame(data$Q7.b_post,data$Q7.j_post,data$Q8.c_post,data$Q8.i_post)
interest_post_results = alpha(interest_post,na.rm=TRUE)
interest_ret <- data.frame(data$Q7.b_ret,data$Q7.j_ret,data$Q8.c_ret,data$Q8.i_ret)
interest_ret_results = alpha(interest_ret,na.rm=TRUE)
interest_pre_results$total; interest_post_results$total; interest_ret_results$total
# 0.90 / 0.92 / 0.93

value_pre <- data.frame(data$Q6.g_pre,data$Q6.i_pre,data$Q6.j_pre,data$Q7.c_pre,data$Q7.f_pre,data$Q7.g_pre,data$Q8.a_pre,data$Q8.e_pre,data$Q9.c_pre)
value_pre_results = alpha(value_pre,na.rm=TRUE)
value_post <- data.frame(data$Q6.g_post,data$Q6.i_post,data$Q6.j_post,data$Q7.c_post,data$Q7.f_post,data$Q7.g_post,data$Q8.a_post,data$Q8.e_post,data$Q9.c_post)
value_post_results = alpha(value_post,na.rm=TRUE)
value_ret <- data.frame(data$Q6.g_ret,data$Q6.i_ret,data$Q6.j_ret,data$Q7.c_ret,data$Q7.f_ret,data$Q7.g_ret,data$Q8.a_ret,data$Q8.e_ret,data$Q9.c_ret)
value_ret_results = alpha(value_ret,na.rm=TRUE)
value_pre_results$total; value_post_results$total; value_ret_results$total
# 0.86 / 0.89 / 0.92



