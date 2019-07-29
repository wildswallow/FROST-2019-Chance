### ATTITUDES ###

### SET WORKING DIRECTORY ###
#setwd("/Users/sheareynolds/Dropbox/IS Assessment Data Summer 2017/final files/Data Files")
#setwd("/Users/bchance/Dropbox/ISI Assessment Data Summer 2017/final files/Data Files")

############################## Options For Data Set ###################################

### READ IN DATA 
# Individual years
data1617 <- read.csv("Final 16.17.csv")
data1516 <- read.csv("Final 15.16.csv")
data1415 <- read.csv("Final 14.15.csv")

# All years
data1617 <- cbind(data1617, year="16-17")
data1516 <- cbind(data1516, year="15-16")
data1415 <- cbind(data1415, year="14-15")
library(dplyr)
data1415$instructor.gender <- recode_factor(data1415$instructor.gender, "female" = "Female", "male" = "Male")
data1617$semester <- recode_factor(data1617$semester, "Fall" = "fall", "Spring" = "spring")
data1415$pre_a_rr = rowSums(!is.na(data1415[,which(colnames(data1415)=="Q6.a_pre_a"):which(colnames(data1415)=="Q9.f_pre_a")]))/36
data1415$POST_a_RR = rowSums(!is.na(data1415[,which(colnames(data1415)=="Q6.a_post_a"):which(colnames(data1415)=="Q9.f_post_a")]))/36
library(plyr)
allyears <- rbind.fill(data1617, data1516, data1415)

### CHOOSE HERE ###
data = data1617
data = data1516
data = data1415
data = allyears

## ISCAM
dataISCAM <- data[data$textbook.classification=="ISCAM",]
## HS
dataHS <- data[data$CarnegieClassification=="High School",]

# Textbook category - ISI/OtherSBI/NotSBI
data$textbook <- data$textbook.classification2
#levels(data$textbook) <- levels(data$textbook)[c(1,3,2)]

first_pre_a = which(colnames(data)=="Q6.a_pre_a")
last_pre_a = which(colnames(data)=="Q9.f_pre_a")
first_post_a = which(colnames(data)=="Q6.a_post_a")
last_post_a = which(colnames(data)=="Q9.f_post_a")

################################################################################################

### DATA SET OPTIONS ###

### RUN LINE TO MOVE OPT OUTERS ###
data = data[data$Opt.out_pre==1 & !is.na(data$Opt.out_pre),]
data = data[data$Opt.out_post==1 & !is.na(data$Opt.out_post),]
nrow(data)

### RUN LINE TO TAKE OUT HS STUDENTS ###
data <- data[data$CarnegieClassification!="High School",]
nrow(data)

### REMOVE ISCAM PEOPLE ###
data <- data[data$textbook.classification!="ISCAM",]

### RUN LINE TO REMOVE CONDENSED STUDENTS ###
data <- data[data$semester!="Condensed",]
nrow(data)

### RUN LINES TO REMOVE STUDENTS WHOSE RESPONSE RATE IS LESS THAN 80% ###
hist(data$pre_a_rr)
table(data$pre_a_rr < .8)
table(data$POST_a_RR < .8)
data <- data[!(data$pre_a_rr <.8),]
data <- data[!(data$POST_a_RR <.8),]
nrow(data)
# 10712

### CHECKING MISSING QUESTIONS ###
miss_pre <- colSums(is.na(data[,c(first_pre_a:last_pre_a)]))
miss_post <- colSums(is.na(data[,c(first_post_a:last_post_a)]))
rbind(miss_pre,miss_post)

################################################################################################

### ATTITUDES VISUAL ###
library(reshape2)
library(ggplot2)
library(dplyr)

## Create labels for table output ##
Percent.Correct = c("Pre","Post", "Diff", "Comments") 
a_names <- c("Effort","Effort","Affect","Affect","Competence","Difficulty","Value","Difficulty","Value","Value",
            "Competence","Interest","Value","Effort","Affect","Value","Value","Affect","Affect","Interest",
            "Value","Difficulty","Interest","Difficulty","Value","Competence","Effort","Affect","Interest","Difficulty",
            "Competence","Competence","Value","Difficulty","Competence","Difficulty")
a_labels <- c("I plan to complete all of my statistics assignments.",
  "I plan to work hard in my statistics course.",
  "I will like statistics.",
  "I will feel insecure when I have to do statistics problems.",
  "I will have trouble understanding statistics because of how I think.",
  "Statistics formulas are easy to understand.",
  "Statistics is worthless.",
  "Statistics is a complicated subject.",
  "Statistics should be a required part of my professional training.",
  "Statistical skills will make me more employable.",
  
  "I will have no idea what's going on in this statistics course.",
  "I am interested in being able to communicate statistical information to others.",
  "Statistics is not useful to the typical profession.",
  "I plan to study hard for every statistics test.",
  "I will get frustrated going over statistics tests in class.",
  "Statistical thinking is not applicable in my life outside my job.",
  "I use statistics in my everyday life.",
  "I will be under stress during statistics class.",
  "I will enjoy taking statistics courses.",
  "I am interested in using statistics.",
  
  "Statistics conclusions are rarely presented in everyday life.",
  "Statistics is a subject quickly learned by most people.",
  "I am interested in understanding statistical information.",
  "Learning statistics requires a great deal of discipline.",
  "I will have no application for statistics in my profession.",
  "I will make a lot of math errors in statistics.",
  "I plan to attend every statistics class session.",
  "I am scared by statistics.",
  "I am interested in learning statistics.",
  "Statistics involves massive computations.",
  
  "I can learn statistics.",
  "I will understand statistics equations.",
  "Statistics is irrelevant in my life",
  "Statistics is highly technical.",
  "I will find it difficult to understand statistical concepts.",
  "Most people have to learn a new way of thinking to do statistics.")

## Function to create table output and visual ##
if (length(levels(data$textbook))==3) {
  ISI_frame_pre_a <- data[which(data$textbook=="ISI"),c(first_pre_a:last_pre_a)]
  ISI_frame_post_a <- data[which(data$textbook=="ISI"),c(first_post_a:last_post_a)]
  SBI_frame_pre_a <- data[ which(data$textbook=="OtherSBI"),c(first_pre_a:last_pre_a)]
  SBI_frame_post_a <- data[ which(data$textbook=="OtherSBI"),c(first_post_a:last_post_a)]
  nonSBI_frame_pre_a <- data[ which(data$textbook=="NotSBI"),c(first_pre_a:last_pre_a)]
  nonSBI_frame_post_a <- data[ which(data$textbook=="NotSBI"),c(first_post_a:last_post_a)]
  
  ## Function to make table of average pre, post, diff (w/ comment) by textbook for each question
  
  a_table <- function(questions_a){
    for (i in questions_a){
      diff1_a = round(mean(ISI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(ISI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
      diff2_a = round(mean(SBI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(SBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
      diff3_a = round(mean(nonSBI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(nonSBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
      isi_c_a = NULL
      other_c_a = NULL
      non_c_a = NULL 
      #Creating comments for ISI column#
      if (diff1_a>0) {isi_c_a[i] = "improved"}
      else if (abs(diff1_a)==0) {isi_c_a[i] = "no change"}
      else if (diff1_a<0) {isi_c_a[i] = "decreased"}
      else {isi_c_a[i]=""}
      #Creating comments for OtherSBI column#
      if (diff2_a>0) {other_c_a[i] = "improved"}
      else if (abs(diff2_a)==0) {other_c_a[i] = "no change"}
      else if (diff2_a<0) {other_c_a[i] = "decreased"}
      else {other_c_a[i]=""}
      #Creating comments for NonSBI column#
      if (diff3_a>0) {non_c_a[i] = "improved"}
      else if (abs(diff3_a)==0) {non_c_a[i] = "no change"}
      else if (diff3_a<0) {non_c_a[i] = "decreased"}
      else {non_c_a[i]=""}
      ISI_a = c(round(mean(ISI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2),round(mean(ISI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2),diff1_a,isi_c_a[i])
      OtherSBI_a = c(round(mean(SBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2),round(mean(SBI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2),diff2_a,other_c_a[i])
      NonSBI_a = c(round(mean(nonSBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2),diff3_a,non_c_a[i])
      print(a_labels[i-5])
      print(data.frame(Percent.Correct,ISI_a,OtherSBI_a,NonSBI_a,row.names=NULL)) 
    }}
  
  diff0_a=NULL
  diff1_a=NULL
  diff2_a=NULL
  pre0_a=NULL
  pre1_a=NULL
  pre2_a=NULL
  post0_a=NULL
  post1_a=NULL
  post2_a=NULL
  
  for (i in c(6:41)){
    diff0_a[i-5] = round(mean(ISI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(ISI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    pre0_a[i-5] = round(mean(ISI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    post0_a[i-5] = round(mean(ISI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2)
    diff1_a[i-5] = round(mean(SBI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(SBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    diff2_a[i-5] = round(mean(nonSBI_frame_post_a[,(i-5)],na.rm=TRUE) - mean(nonSBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    pre1_a[i-5] = round(mean(SBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    pre2_a[i-5] = round(mean(nonSBI_frame_pre_a[,(i-5)],na.rm=TRUE),digits=2)
    post1_a[i-5] = round(mean(SBI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2)
    post2_a[i-5] = round(mean(nonSBI_frame_post_a[,(i-5)],na.rm=TRUE),digits=2)
  }
  
  diffdf_a <- data.frame(ISI=diff0_a, SBI=diff1_a, nonSBI=diff2_a, question=c(6:41))
  ddf_a <- melt(diffdf_a, id="question", variable.name = "Textbook", value.name = "Post-Pre")
  isidf_a <- data.frame(Pre=pre0_a, Post=post0_a, question=c(6:41))
  idf_a <- melt(isidf_a, id="question", variable.name = "Pre.Post", value.name = "Avg")
  idf_a$dsign <- sign(isidf_a$Post-isidf_a$Pre)
  sbidf_a <- data.frame(Pre=pre1_a, Post=post1_a, question=c(6:41))
  sdf_a <- melt(sbidf_a, id="question", variable.name = "Pre.Post", value.name = "Avg")
  sbijitter_a <- data.frame(Pre=pre1_a, Post=post1_a, question=seq(5.8,40.8))
  sjitdf_a <- melt(sbijitter_a, id="question", variable.name = "Pre.Post", value.name = "Avg")
  sjitdf_a$dsign <- sign(sbijitter_a$Post-sbijitter_a$Pre)
  nondf_a <- data.frame(Pre=pre2_a, Post=post2_a, question=c(6:41))
  ndf_a <- melt(nondf_a, id="question", variable.name = "Pre.Post", value.name = "Avg")
  nonjitter_a <- data.frame(Pre=pre2_a, Post=post2_a, question=seq(6.2,41.2))
  njitdf_a <- melt(nonjitter_a, id="question", variable.name = "Pre.Post", value.name = "Avg")
  njitdf_a$dsign <- sign(nonjitter_a$Post-nonjitter_a$Pre)
  
  mergedf_a <- cbind(rbind(data.frame(idf_a, Textbook="ISI"), data.frame(sjitdf_a, Textbook="OtherSBI"), data.frame(njitdf_a, Textbook="NotSBI")), Category = a_names)
  mergedf_a$dsign <- recode_factor(mergedf_a$dsign, `-1` = "Decrease", `0` = "Increase", `1` = "Increase")
  
  ## Function to make visuals of average pre-post by textbook for each question
  
  a_visual <- function(statistic="Pre.Post", textbook="All", by="") {
    
    if (statistic == "Diff") {
      if (textbook == "All") {
        ggplot(data=ddf_a,aes(x=question,y=`Post-Pre`,colour=Textbook)) + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_point() + geom_line() + theme_bw() + ggtitle("Differences")
      } else if (textbook == "ISI") {
        qplot(c(6:41),diff0_a,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_line() + theme_bw() + ggtitle("ISI Differences")
      } else if (textbook == "OtherSBI") {
        qplot(c(6:41),diff1_a,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_line() + theme_bw() + ggtitle("OtherSBI Differences")
      } else if (textbook == "NotSBI") {
        qplot(c(6:41),diff2_a,xlab="Question",ylab="Post-Pre") + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_line() + theme_bw() + ggtitle("NotSBI Differences")
      }
      
    } else if (statistic == "Pre.Post") {
      if (textbook == "ISI") {
        ggplot(data=idf_a,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI")
      } else if (textbook == "OtherSBI") {
        ggplot(data=sdf_a,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("OtherSBI")
      } else if (textbook == "NotSBI") {
        ggplot(data=ndf_a,aes(x=question,y=Avg)) + scale_x_continuous(breaks = seq(6,41,2)) +
          geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("NotSBI")
      } else if (textbook == "All") {
        if (by == "Category") {
          ggplot(data=mergedf_a,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(6,41,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + facet_grid(.~Category) +
            theme_bw() + ggtitle("ISI vs OtherSBI vs NotSBI by Concept Category")
        } else if (by == "Sign") {
          # g <- ggplot(data=mergedf,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(6,41,2)) +
          #   geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI vs SBI vs Non-SBI by sign")
          # g + facet_grid(rows = vars(dsign)) 
          ggplot(data=mergedf_a,aes(x=question,y=Avg,colour=dsign)) + scale_x_continuous(breaks = seq(6,41,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question, linetype = Textbook)) + theme_bw() + ggtitle("ISI vs OtherSBI vs NotSBI")
        } else {
          ggplot(data=mergedf_a,aes(x=question,y=Avg,colour=Textbook)) + scale_x_continuous(breaks = seq(6,41,2)) +
            geom_point(aes(shape=Pre.Post)) + geom_line(aes(group = question)) + theme_bw() + ggtitle("ISI vs OtherSBI vs NotSBI")
        }
      }
    }
  }
}

################################################################################################

## TO GET OUTPUT

# Print questions table
questions_a <- c(6:41)
a_table(questions_a)  

# Display graphs 
  # default: statistic="Pre.Post", textbook="All", by=""
# options: 
  # statistic = "Pre.Post", "Diff"
  # textbook = "All", "ISI", "OtherSBI", NotSBI"
  # by = "Category", "sign" (only works for Pre.Post, All)
a_visual()
a_visual("Diff")
a_visual("Diff", "OtherSBI")
a_visual("Pre.Post", "NotSBI")
a_visual(by="Category")
a_visual(by="Sign")


## FINAL PLOT ## ************************* WANT THIS ONE (POSTER CHART 1) *************************
# for textbook.classification2

# sort by subcategory and renumber questions
mergedfsort_a <- mergedf_a[order(mergedf_a$Category),]  
mergedfsort_a$question <- c(1:6-.2,1:6-.2,1:6,1:6,1:6+.2,1:6+.2,
                            7:12-.2,7:12-.2,7:12,7:12,7:12+.2,7:12+.2,
                            13:19-.2,13:19-.2,13:19,13:19,13:19+.2,13:19+.2,
                            20:23-.2,20:23-.2,20:23,20:23,20:23+.2,20:23+.2,
                            24:27-.2,24:27-.2,24:27,24:27,24:27+.2,24:27+.2,
                            28:36-.2,28:36-.2,28:36,28:36,28:36+.2,28:36+.2)
group.colors <- c(Increase = "springgreen4", Decrease = "dodgerblue3")

# output
library(ggthemes)
ggplot(data=mergedfsort_a,aes(x=question,y=Avg,colour=dsign)) + 
  geom_point(aes(shape=Pre.Post), size=2) + geom_line(aes(group = question, linetype = Textbook)) + facet_grid(.~Category, scale="free_x", space="free_x") +
  theme_calc() + theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), strip.text.x = element_text(size=20),
                       axis.text.y = element_text(size=16), legend.text = element_text(size=12)) + scale_colour_manual(values = group.colors) +
  labs(y=element_blank(),x=element_blank(),colour=element_blank(),shape=element_blank(),linetype=element_blank()) #+ ggtitle("ISI vs OtherSBI vs NotSBI by Attitude Category")

################################################################################################

### BASIC STATISTICS ###

load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))

# Pre Concept Subscales #
iscamsummary(data$Affect_pre,data$textbook)
iscamsummary(data$Cognitive.Competence_pre,data$textbook)
iscamsummary(data$Difficulty_pre,data$textbook)
iscamsummary(data$Effort_pre,data$textbook)
iscamsummary(data$Interest_pre,data$textbook)
iscamsummary(data$Value_pre,data$textbook)
# Post Concept Subscales #
iscamsummary(data$Affect_post,data$textbook)
iscamsummary(data$Cognitive.Competence_post,data$textbook)
iscamsummary(data$Difficulty_post,data$textbook)
iscamsummary(data$Effort_post,data$textbook)
iscamsummary(data$Interest_post,data$textbook)
iscamsummary(data$Value_post,data$textbook)

### TABLE 2 ON POSTER ###
# Change Concept Subscales # (all years mean ISI, NotSBI, OtherSBI) 
iscamsummary(data$Affect_change,data$textbook) # 0.172, -0.008, 0.092
iscamsummary(data$Cog_change,data$textbook) # 0.130, -0.092, 0.017
iscamsummary(data$Difficulty_change,data$textbook) # 0.307, 0.182, 0.235
iscamsummary(data$Effort_change,data$textbook) # -0.891, -0.938, -0.845
iscamsummary(data$Interest_change,data$textbook) # -0.560, -0.625, -0.568
iscamsummary(data$Value_change,data$textbook) # -0.261, -0.317, -0.253

# Subscales #
iscamsummary(data$Affect_pre); iscamsummary(data$Affect_post); iscamsummary(data$Affect_change)
iscamsummary(data$Cognitive.Competence_pre); iscamsummary(data$Cognitive.Competence_post); iscamsummary(data$Cog_change)
iscamsummary(data$Difficulty_pre); iscamsummary(data$Difficulty_post); iscamsummary(data$Difficulty_change)
iscamsummary(data$Effort_pre); iscamsummary(data$Effort_post); iscamsummary(data$Effort_change)
iscamsummary(data$Interest_pre); iscamsummary(data$Interest_post); iscamsummary(data$Interest_change)
iscamsummary(data$Value_pre); iscamsummary(data$Value_post); iscamsummary(data$Value_change)

# Proportion of positive change by textbook
prop.table(table(data$Affect_change>0, data$textbook),2)
prop.table(table(data$Cog_change>0, data$textbook),2)
prop.table(table(data$Difficulty_change>0, data$textbook),2)
prop.table(table(data$Effort_change>0, data$textbook),2)
prop.table(table(data$Interest_change>0, data$textbook),2)
prop.table(table(data$Value_change>0, data$textbook),2)

################################################################################################

### BOXPLOTS ###

# par(mfcol=c(1,2))
# boxplot(Affect_pre ~ textbook, data = data)
# boxplot(Affect_post ~ textbook, data = data)
# 
# boxplot(Cognitive.Competence_pre ~ textbook, data = data)
# boxplot(Cognitive.Competence_post ~ textbook, data = data)
# 
# boxplot(Value_pre ~ textbook, data = data)
# boxplot(Value_pre ~ textbook, data = data)
# 
# 
# affectbox <- melt(data[,c("Affect_pre","Affect_post","textbook")],id.vars='textbook')
# names(affectbox) <- c("Textbook", "Pre.Post", "value")
# ggplot(data=affectbox, aes(y=value, x=Textbook, fill=Pre.Post, na.rm=T)) + geom_boxplot() + labs(y="")
# a_means <- aggregate(value ~ Textbook + Pre.Post, affectbox, mean)

## Boxplots of Pre/Post scores by textbook for an attitude subscale ##
library(tidyverse)
library(reshape2)

# Affect
data %>% 
  select(Affect_pre, Affect_post, textbook) %>%
  melt(id.vars='textbook') %>%
  drop_na(textbook) %>%
  ggplot(aes(y=value, x=textbook, fill=variable)) + geom_boxplot() + 
    scale_y_continuous(breaks=c(1:7)) + scale_fill_manual(values = c("violetred4","grey69"), labels = c("Pre", "Post")) +
    theme_bw() + ggtitle("Affect") + labs(y=element_blank(), x=element_blank(), fill=element_blank()) 

# Cognitive Competence
data %>% 
  select(Cognitive.Competence_pre, Cognitive.Competence_post, textbook) %>%
  melt(id.vars='textbook') %>%
  drop_na(textbook) %>%
  ggplot(aes(y=value, x=textbook, fill=variable)) + geom_boxplot() + 
  scale_y_continuous(breaks=c(1:7)) + scale_fill_manual(values = c("violetred4","grey69"), labels = c("Pre", "Post")) +
  theme_bw() + ggtitle("Cognitive Competence") + labs(y=element_blank(), x=element_blank(), fill=element_blank())

# Value
data %>% 
  select(Value_pre, Value_post, textbook) %>%
  melt(id.vars='textbook') %>%
  drop_na(textbook) %>%
  ggplot(aes(y=value, x=textbook, fill=variable)) + geom_boxplot() + 
  scale_y_continuous(breaks=c(1:7)) + scale_fill_manual(values = c("violetred4","grey69"), labels = c("Pre", "Post")) +
  theme_bw() + ggtitle("Value") + labs(y=element_blank(), x=element_blank(), fill=element_blank())


### Individual Questions ###
## TABLE 1 ON POSTER ##

# I will feel (felt) insecure when I have (had) to do statistics problems.
iscamsummary(data$"Q6.d_pre_a",data$textbook)
iscamsummary(data$"Q6.d_post_a",data$textbook)
iscamsummary(data$"Q6.d_post_a" - data$"Q6.d_pre_a",data$textbook)
# ISI .267, NotSBI .037, OtherSBI .158

# I will find (found) it difficult to understand statistical concepts.
iscamsummary(data$"Q9.e_pre_a",data$textbook)
iscamsummary(data$"Q9.e_post_a",data$textbook)
iscamsummary(data$"Q9.e_post_a" - data$"Q9.e_pre_a",data$textbook)
# ISI .238, NotSBI .000, OtherSBI .071

# Most people have to learn a new way of thinking to do statistics.
iscamsummary(data$"Q9.f_pre_a",data$textbook)
iscamsummary(data$"Q9.f_post_a",data$textbook)
iscamsummary(data$"Q9.f_post_a" - data$"Q9.f_pre_a",data$textbook)
# ISI .036, NotSBI .047, OtherSBI -.021

################################################################################################

## Cronback alphas (want >.7) ## ON POSTER ##
library(psych)

# Affect
affect_pre <- data.frame(data$Q6.c_pre,data$Q6.d_pre,data$Q7.e_pre,data$Q7.h_pre,data$Q7.i_pre,data$Q8.h_pre)
affect_pre_results = alpha(affect_pre,na.rm=TRUE)
affect_post <- data.frame(data$Q6.c_post,data$Q6.d_post,data$Q7.e_post,data$Q7.h_post,data$Q7.i_post,data$Q8.h_post)
affect_post_results = alpha(affect_post,na.rm=TRUE)
affect_pre_results$total; affect_post_results$total
# 0.815, 0.855

# Cognitive Competence
comp_pre <- data.frame(data$Q6.e_pre,data$Q7.a_pre,data$Q8.f_pre,data$Q9.a_pre,data$Q9.b_pre,data$Q9.e_pre)
comp_pre_results = alpha(comp_pre,na.rm=TRUE)
comp_post <- data.frame(data$Q6.e_post,data$Q7.a_post,data$Q8.f_post,data$Q9.a_post,data$Q9.b_post,data$Q9.e_post)
comp_post_results = alpha(comp_post,na.rm=TRUE)
comp_pre_results$total; comp_post_results$total
# 0.836, 0.858

# Difficulty
difficulty_pre <- data.frame(data$Q6.f_pre,data$Q6.h_pre,data$Q8.b_pre,data$Q8.d_pre,data$Q8.j_pre,data$Q9.d_pre,data$Q9.f_pre)
difficulty_pre_results = alpha(difficulty_pre,na.rm=TRUE)
difficulty_post <- data.frame(data$Q6.f_post,data$Q6.h_post,data$Q8.b_post,data$Q8.d_post,data$Q8.j_post,data$Q9.d_post,data$Q9.f_post)
difficulty_post_results = alpha(difficulty_post,na.rm=TRUE)
difficulty_pre_results$total; difficulty_post_results$total
# 0.724, 0.761

# Effort
effort_pre <- data.frame(data$Q6.a_pre,data$Q6.b_pre,data$Q7.d_pre,data$Q8.g_pre)
effort_pre_results = alpha(effort_pre,na.rm=TRUE)
effort_post <- data.frame(data$Q6.a_post,data$Q6.b_post,data$Q7.d_post,data$Q8.g_post)
effort_post_results = alpha(effort_post,na.rm=TRUE)
effort_pre_results$total; effort_post_results$total
# 0.782, 0.712

# Interest
interest_pre <- data.frame(data$Q7.b_pre,data$Q7.j_pre,data$Q8.c_pre,data$Q8.i_pre)
interest_pre_results = alpha(interest_pre,na.rm=TRUE)
interest_post <- data.frame(data$Q7.b_post,data$Q7.j_post,data$Q8.c_post,data$Q8.i_post)
interest_post_results = alpha(interest_post,na.rm=TRUE)
interest_pre_results$total; interest_post_results$total
# 0.883, 0.920 

# Value
value_pre <- data.frame(data$Q6.g_pre,data$Q6.i_pre,data$Q6.j_pre,data$Q7.c_pre,data$Q7.f_pre,data$Q7.g_pre,data$Q8.a_pre,data$Q8.e_pre,data$Q9.c_pre)
value_pre_results = alpha(value_pre,na.rm=TRUE)
value_post <- data.frame(data$Q6.g_post,data$Q6.i_post,data$Q6.j_post,data$Q7.c_post,data$Q7.f_post,data$Q7.g_post,data$Q8.a_post,data$Q8.e_post,data$Q9.c_post)
value_post_results = alpha(value_post,na.rm=TRUE)
value_pre_results$total; value_post_results$total
# 0.878, 0.907 

################################################################################################

### HLMS ###
library(lme4)

fit <- lm(Value_change ~ 1, data = data)
nullmodel <- lmer(Value_change ~ (1 | InstructorName_Section_Semester), data = data, REML = F)
summary(nullmodel)
# ICC = .04085 / (.04085 + .59268) = 0.06448 (year 3)

2*(logLik(nullmodel) - logLik(fit)) # 132.2245 (year 3)
# all years: 187.1362

## Subset low attitudes (any subscale pre scores less than 4)
lowA <- data[data$"Affect_pre"<4 | data$"Cognitive.Competence_pre"<4 | data$"Difficulty_pre"<4 | data$"Value_pre"<4 | data$"Interest_pre"<4, ]

# predicting change in attitudes (Value here, can change to Affect, Cog, Difficulty, Interest, Effort)
modelA = lmer(Value_change ~ 1   
              + pre_perc 
              + as.factor(gender_pre) 
              + age_pre
              + gpa_pre            
              + SATACTzscore
              + instructor.gender
              + as.factor(class.size.ind)
              + textbook
              + prev.stat.course.ind #0/1 with 0 = no previous stat course
              + as.factor(advanced.stats.degree)
              + as.factor(CarnegieClassification)
              #+ instructor.gender*as.factor(gender_pre) 
              + semester
              + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
              , data=lowA)
summary(modelA)
library(car)
Anova(modelA)
# Significant predictors at .01 level **
  # Value_change: gpa_pre, SATACTzscore
  # Affect_change: pre_perc, gpa_pre, SATACTzscore
  # Cog_change: pre_perc, gpa_pre
  # Difficulty_change: pre_perc, gpa_pre, SATACTzscore
  # Interest_change: pre_perc, SATACTzscore
  # Effort_change: gpa_pre


# look at relationship between significant predictors and response
plot(data$pre_perc, data$Value_change)
plot(data$gpa_pre, data$Value_change)
plot(data$SATACTzscore, data$Value_change)


# ## Subset change greater than 1
# a_change <- data[abs(data$Value_change)>1,]
# a_change$Value_sign <- sign(a_change$Value_change)
# a_change$Value_sign <- recode_factor(a_change$Value_sign, `-1` = 0, `1` = 1)
# 
# # predict increase or decrease in attitudes
# modelA2 <- glmer(Value_sign ~
#                + pre_perc 
#                + as.factor(gender_pre) 
#                + age_pre
#                + gpa_pre            
#                + SATACTzscore
#                + instructor.gender
#                + as.factor(class.size.ind)
#                + textbook
#                + prev.stat.course.ind #0/1 with 0 = no previous stat course
#                + as.factor(advanced.stats.degree)
#                + as.factor(CarnegieClassification)
#                #+ instructor.gender*as.factor(gender_pre) 
#                + semester
#                + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
#                , family="binomial", data=a_change, na.action = na.exclude )
# summary(modelA2)
# library(car)
# Anova(modelA2)

################################################################################################

### RETENTION 4 month
setwd("/Users/sheareynolds/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
r4y15 <- read.csv("Merged2015Retention4MonthMay29_Version3.csv")
r4y16 <- read.csv("Merged2016Retention4Month_Version1.csv")
data <- rbind.fill(r4y15,r4y16)
data$textbook <- data$textbook.classification2

first_pre_a = which(colnames(data)=="Q6.a_post_a")
last_pre_a = which(colnames(data)=="Q9.f_post_a")
first_post_a = which(colnames(data)=="Q6.a_ret")
last_post_a = which(colnames(data)=="Q9.f_ret")

################################################################################################

### RETENTION 16 month ###
setwd("/Users/sheareynolds/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
data1 <- read.csv("MERGED_16Month.csv")
data1$year <- rep(2015,length(data1$ID))
data2 <- read.csv("MERGED_16Month2016.csv") 
data2$year <- rep(2016,length(data2$ID))
data <- rbind.fill(data1,data2)
data$textbook <- data$textbook.classification2

first_pre_a = which(colnames(data)=="Q6.a_ret")
last_pre_a = which(colnames(data)=="Q9.f_ret")
first_post_a = which(colnames(data)=="Q6.a_ret2")
last_post_a = which(colnames(data)=="Q9.f_ret2")

################################################################################################

### RETENTION 16 month ###

## MEANS ## TABLE 3 ON POSTER ##
data <- rbind.fill(data1,data2)
data$textbook <- data$textbook.classification2
levels(data$textbook) <- levels(data$textbook)[c(1,3,2)]

# Affect #
mean(data$Affect_pre,na.rm=TRUE)
mean(data$Affect_post,na.rm=TRUE) #4.457
mean(data$Affect_ret,na.rm=TRUE) #4.506
mean(data$Affect_ret2,na.rm=TRUE) #4.492

# Cog Comp #
mean(data$Cognitive.Competence_pre,na.rm=TRUE) 
mean(data$Cognitive.Competence_post,na.rm=TRUE) #5.013
mean(data$Cognitive.Competence_ret,na.rm=TRUE) #5.033
mean(data$Cognitive.Competence_ret2,na.rm=TRUE) #4.987


# Difficulty #
mean(data$Difficulty_pre,na.rm=TRUE)
mean(data$Difficulty_post,na.rm=TRUE) #4.118
mean(data$Difficulty_ret,na.rm=TRUE) #4.118
mean(data$Difficulty_ret2,na.rm=TRUE) #4.095

# Interest #
mean(data$Interest_pre,na.rm=TRUE)
mean(data$Interest_post,na.rm=TRUE) #4.278
mean(data$Interest_ret,na.rm=TRUE) #4.364
mean(data$Interest_ret2,na.rm=TRUE) #4.386

# Value #
mean(data$Value_pre,na.rm=TRUE) 
mean(data$Value_post,na.rm=TRUE) #4.956
mean(data$Value_ret,na.rm=TRUE) #5.007
mean(data$Value_ret2,na.rm=TRUE) #5.070


# Means by textbook
iscamsummary(data$Affect_ret2,data$textbook)
iscamsummary(data$Cognitive.Competence_ret2,data$textbook)
iscamsummary(data$Value_ret2,data$textbook)
iscamsummary(data$Difficulty_ret2,data$textbook)
iscamsummary(data$Interest_ret2,data$textbook)


## ANOVAS ##

# 4 month 
summary(aov(data$Affect_ret~data$textbook)) #0.796
summary(aov(data$Cognitive.Competence_ret~data$textbook)) #0.618
summary(aov(data$Effort_ret~data$textbook)) #0.601
summary(aov(data$Interest_ret~data$textbook)) #0.688
summary(aov(data$Value_ret~data$textbook)) #0.697
summary(aov(data$Difficulty_ret~data$textbook)) #0.917

# 16 month
summary(aov(data$Affect_ret2~data$textbook)) #0.568
summary(aov(data$Cognitive.Competence_ret2~data$textbook)) #0.235
summary(aov(data$Effort_ret2~data$textbook)) #0.911
summary(aov(data$Interest_ret2~data$textbook)) #0.336
summary(aov(data$Value_ret2~data$textbook)) #0.13
summary(aov(data$Difficulty_ret2~data$textbook)) #0.635

################################################################################################

### MULTICOLLINEARITY ###

# enter VIF and Kappa scripts

### multicollinearity tests
## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 3

vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}

vif.mer(modelA) # high VIF: advanced.stats.degree
kappa.mer(modelA) # 71

################################################################################################

# loop to find significant interactions

vars <- data.frame(c('gender','instructor.gender','prev.stat.course.ind','class.size.ind'))

comb <- combn(1:4,2) #make table of all combinations

# allocate list first: 
# https://stackoverflow.com/questions/5599896/how-do-i-store-arrays-of-statistical-models
glmList <- vector(mode="list", length=ncol(comb))

for (k in 1:ncol(comb)){
  par1 = comb[2,k] %>% vars[.,1] %>% as.character(.)#taking index of combination table from variable list
  par2 = comb[1,k] %>% vars[.,1] %>% as.character(.)#needs to be character for dplyr::select to work
  # To do a model in a loop, you make the formula first as a string and run it through the formula function. 
  # https://stackoverflow.com/questions/8121542/r-specifying-variable-name-in-function-parameter-for-a-function-of-general-uni
  frm<-paste("Value_change~", par1,'*', par2, sep="")
  myglm = glm(formula(frm), 
              family = "gaussian", 
              data = data)
  sumglm = summary(myglm)
  glmList[[k]] <- sumglm
}
capture.output(glmList)

################################################################################################

### MULTILEVEL MODELS ###
library(MuMIn)
library(lme4)

# EDA
hist(data$overall_attitude_pre)
hist(data$overall_attitude_post)
hist(data$Affect_change)
hist(data$Cog_change)
hist(data$Value_change)

nullmodel <- lmer(Affect_change ~ (1 | institution/instructor.last.name/InstructorName_Section_Semester), data = lowA, REML = F)
summary(nullmodel)
# Random effects:
#   Groups                                                             Name        Variance Std.Dev.
# InstructorName_Section_Semester:(instructor.last.name:institution) (Intercept) 0.009309 0.09648 
# instructor.last.name:institution                                   (Intercept) 0.039879 0.19970 
# institution                                                        (Intercept) 0.016575 0.12874 
# Residual                                                                       0.961173 0.98039 

(.009309+.0398789+.016575)/(.009309+.0398789+.016575+.961173)
#0.06403798

# ALL YEARS - LOW ATTITUDES
  # Random effects:
  #   Groups                                                             Name        Variance Std.Dev.
  # InstructorName_Section_Semester:(instructor.last.name:institution) (Intercept) 0.02139  0.1463  
  # instructor.last.name:institution                                   (Intercept) 0.06194  0.2489  
  # institution                                                        (Intercept) 0.01660  0.1289  
  # Residual                                                                       0.98252  0.9912 
# no. obs: 7993

## Model Selection 
# resubset low attitudes to only columns of interest for model selection
lowA <- data[data$"Affect_pre"<4 | data$"Cognitive.Competence_pre"<4 | data$"Difficulty_pre"<4 | data$"Value_pre"<4 | data$"Interest_pre"<4,
             c("Affect_pre", "Cognitive.Competence_pre", "Difficulty_pre", "Value_pre", "Interest_pre", 
               "Value_change", "Affect_change", "Cog_change",
               "pre_perc","gender_pre", "age_pre", "gpa_pre", "SATACTzscore", "instructor.gender", "class.size.ind", "textbook",
              "prev.stat.course.ind", "advanced.stats.degree", "CarnegieClassification", "semester",
              "institution","instructor.last.name","InstructorName_Section_Semester","section_gpa","section_SATACTzscore",
              "isi.workshop","position.classification","student.type","analyzing.data.experience",
              "gaise.familiar","TA","department.classification","years.teaching.intro.stats","incentive.post.ind",
              "math.prereq.coded","class.session.length.mins.ind","class.meet.weeks","time.meet.ind")]
# for function to work, can't have missing values
lowA <- na.omit(lowA)
# n = 2423

# OR positive change
posA <- data[data$"Affect_change">0,
             c("Affect_pre", "Cognitive.Competence_pre", "Difficulty_pre", "Value_pre", "Interest_pre", 
               "Value_change", "Affect_change", "Cog_change",
               "pre_perc","gender_pre", "age_pre", "gpa_pre", "SATACTzscore", "instructor.gender", "class.size.ind", "textbook",
               "prev.stat.course.ind", "advanced.stats.degree", "CarnegieClassification", "semester",
               "institution","instructor.last.name","InstructorName_Section_Semester","section_gpa","section_SATACTzscore",
               "isi.workshop","position.classification","student.type","analyzing.data.experience",
               "gaise.familiar","TA","department.classification","years.teaching.intro.stats","incentive.post.ind",
               "math.prereq.coded","class.session.length.mins.ind","class.meet.weeks","time.meet.ind")]
posA <- na.omit(posA)

# predicting (positive) change in attitude
modelA = lmer(Affect_change ~ 1   
              + pre_perc 
              + as.factor(gender_pre) 
              + age_pre
              + gpa_pre            #+ I(gpa_pre*gpa_pre)
              + SATACTzscore
              + instructor.gender
              + as.factor(class.size.ind)
              + years.teaching.intro.stats
              + as.factor(math.prereq.coded)
              + TA
              + as.factor(gaise.familiar)
              + class.session.length.mins.ind
              + as.factor(as.factor(class.meet.weeks))  #quarter, semester, full year
              + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
              + as.factor(incentive.post.ind)
              + textbook
              + section_gpa
              + section_SATACTzscore
              + prev.stat.course.ind #0/1 with 0 = no previous stat course
              + as.factor(advanced.stats.degree)
              + isi.workshop
              + as.factor(position.classification)
              + as.factor(student.type)
              + as.factor(analyzing.data.experience)
              + as.factor(CarnegieClassification)
              + instructor.gender*as.factor(gender_pre) 
              + semester
              + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
              , data=lowA)
summary(modelA)
# Random effects:
#   Groups                                                             Name        Variance Std.Dev.
# InstructorName_Section_Semester:(instructor.last.name:institution) (Intercept) 0.006205 0.07877 
# instructor.last.name:institution                                   (Intercept) 0.029557 0.17192 
# institution                                                        (Intercept) 0.012588 0.11220 
# Residual                                                                       0.929366 0.96404
# Number of obs: 2423

stats::anova(modelA)
library(car)
Anova(modelA)
# significant predictors at .01 level **
# pre_perc, gpa_pre, class.session.length.mins.ind

# only works sometimes ?
library(lmerTest)
lmerTest::step(modelA)

# look at relationship between significant predictors and response
ggplot(data = data, aes(x = pre_perc, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()
ggplot(data = data, aes(x = gpa_pre, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

# second model for comparison
modelB = lmer(Affect_change ~ 1   
            #  + pre_perc 
            #  + as.factor(gender_pre) 
            #  + age_pre
            #  + gpa_pre            #+ I(gpa_pre*gpa_pre)
             # + SATACTzscore
             # + instructor.gender
             # + as.factor(class.size.ind)
             # + years.teaching.intro.stats
             # + as.factor(math.prereq.coded)
             # + TA
             # + as.factor(gaise.familiar)
             # + class.session.length.mins.ind
             # + as.factor(as.factor(class.meet.weeks))  #quarter, semester, full year
             # + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
             # + as.factor(incentive.post.ind)
              + textbook
             # + section_gpa
             # + section_SATACTzscore
             # + prev.stat.course.ind #0/1 with 0 = no previous stat course
             # + as.factor(advanced.stats.degree)
             # + isi.workshop
             # + as.factor(position.classification)
             # + as.factor(student.type)
             # + as.factor(analyzing.data.experience)
             # + as.factor(CarnegieClassification)
             # + instructor.gender*as.factor(gender_pre) 
             # + semester
              + ( 1 | institution/instructor.last.name/InstructorName_Section_Semester)
              , data=lowA)

summary(modelB)
Anova(modelB)
anova(modelA, modelB, test="Chisq")

################################################################################################
## Looking at overall positive change in attitude

# create overall attitude change
data$overall_attitude_change <- data$overall_attitude_post - data$overall_attitude_pre
# EDA
hist(data$overall_attitude_change)
# subset positive
poschange <- data[data$overall_attitude_change>0,
                  c("Affect_pre", "Cognitive.Competence_pre", "Difficulty_pre", "Value_pre", "Interest_pre", 
                    "Value_change", "Affect_change", "Cog_change", "overall_attitude_change",
                    "pre_perc","gender_pre", "age_pre", "gpa_pre", "SATACTzscore", "instructor.gender", "class.size.ind", "textbook",
                    "prev.stat.course.ind", "advanced.stats.degree", "CarnegieClassification", "semester",
                    "institution","instructor.last.name","InstructorName_Section_Semester","section_gpa","section_SATACTzscore",
                    "isi.workshop","position.classification","student.type","analyzing.data.experience",
                    "gaise.familiar","TA","department.classification","years.teaching.intro.stats","incentive.post.ind",
                    "math.prereq.coded","class.session.length.mins.ind","class.meet.weeks","time.meet.ind")]
poschange <- na.omit(poschange)

# predicting overall (positive) change in attitude
modelP <- lmer(overall_attitude_change ~ 1 +
                  #  + pre_perc 
                  #  + as.factor(gender_pre) 
                  #  + age_pre
                  #  + gpa_pre            #+ I(gpa_pre*gpa_pre)
                  # + SATACTzscore
                  # + instructor.gender
                  # + as.factor(class.size.ind)
                  # + years.teaching.intro.stats
                  # + as.factor(math.prereq.coded)
                  # + TA
                  # + as.factor(gaise.familiar)
                 # + class.session.length.mins.ind
                 # + as.factor(as.factor(class.meet.weeks))  #quarter, semester, full year
                 # + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
                 # + as.factor(incentive.post.ind)
                 + textbook
                 # + section_gpa
                 # + section_SATACTzscore
                 # + prev.stat.course.ind #0/1 with 0 = no previous stat course
                 # + as.factor(advanced.stats.degree)
                 # + isi.workshop
                 # + as.factor(position.classification)
                 # + as.factor(student.type)
                 # + as.factor(analyzing.data.experience)
                 # + as.factor(CarnegieClassification)
                 # + instructor.gender*as.factor(gender_pre) 
                 # + semester
                   + ( 1 | institution/instructor.last.name),
                   data = poschange)
summary(modelP)
anova(modelP)

# second model to compare
modelQ <- lmer(overall_attitude_change ~ 1 +
                 #  + pre_perc 
                 #  + as.factor(gender_pre) 
                 #  + age_pre
                 #  + gpa_pre            #+ I(gpa_pre*gpa_pre)
                  + SATACTzscore
                 # + instructor.gender
                 # + as.factor(class.size.ind)
                 # + years.teaching.intro.stats
                 # + as.factor(math.prereq.coded)
                 # + TA
                 # + as.factor(gaise.familiar)
               # + class.session.length.mins.ind
               # + as.factor(as.factor(class.meet.weeks))  #quarter, semester, full year
               # + as.factor(time.meet.ind) # 1 = morning (7-10), 2 = afternoon (11-2), 3 = evening (after 2)
               # + as.factor(incentive.post.ind)
               + textbook
               # + section_gpa
               # + section_SATACTzscore
               # + prev.stat.course.ind #0/1 with 0 = no previous stat course
               # + as.factor(advanced.stats.degree)
               # + isi.workshop
               # + as.factor(position.classification)
               # + as.factor(student.type)
               # + as.factor(analyzing.data.experience)
               # + as.factor(CarnegieClassification)
               # + instructor.gender*as.factor(gender_pre) 
               # + semester
               + ( 1 | institution/instructor.last.name),
               data = poschange, REML = F)
summary(modelQ)
anova(modelQ)

-2*(logLik(modelP)-logLik(modelQ))
anova(modelP, modelQ)

################################################################################################

### LMER MODEL SELECTION FUNCTION ###

forward.lmer <- function(
  start.model, blocks,
  max.iter=1, sig.level=FALSE,
  zt=FALSE, print.log=TRUE)
{
  
  # forward.lmer: a function for stepwise regression using lmer mixed effects models
  # Author: Rense Nieuwenhuis
  
  # Initialysing internal variables
  log.step <- 0
  log.LL <- log.p <- log.block <- zt.temp <- log.zt <- NA
  model.basis <- start.model
  
  # Maximum number of iterations cannot exceed number of blocks
  if (max.iter > length(blocks)) max.iter <- length(blocks)
  
  # Setting up the outer loop
  for(i in 1:max.iter)
  {
    
    models <- list()
    
    # Iteratively updating the model with addition of one block of variable(s)
    # Also: extracting the loglikelihood of each estimated model
    for(j in 1:length(blocks))
    {
      models[[j]] <- update(model.basis, as.formula(paste(". ~ . + ", blocks[j])))
    }
    
    LL <- unlist(lapply(models, logLik))
    
    # Ordering the models based on their loglikelihood.
    # Additional selection criteria apply
    for (j in order(LL, decreasing=TRUE))
    {
      
      ##############
      ############## Selection based on ANOVA-test
      ##############
      
      if(sig.level != FALSE)
      {
        if(anova(model.basis, models[[j]])[2,7] < sig.level)
        {
          
          model.basis <- models[[j]]
          
          # Writing the logs
          log.step <- log.step + 1
          log.block[log.step] <- blocks[j]
          log.LL[log.step] <- as.numeric(logLik(model.basis))
          log.p[log.step] <- anova(model.basis, models[[j]])[2,7]
          
          blocks <- blocks[-j]
          
          break
        }
      }
      
      ##############
      ############## Selection based significance of added variable-block
      ##############
      
      if(zt != FALSE)
      {
        b.model <- summary(models[[j]])@coefs
        diff.par <- setdiff(rownames(b.model), rownames(summary(model.basis)@coefs))
        if (length(diff.par)==0) break
        sig.par <- FALSE
        
        for (k in 1:length(diff.par))
        {
          if(abs(b.model[which(rownames(b.model)==diff.par[k]),3]) > zt)
          {
            sig.par <- TRUE
            zt.temp <- b.model[which(rownames(b.model)==diff.par[k]),3]
            break
          }
        }
        
        if(sig.par==TRUE)
        {
          model.basis <- models[[j]]
          
          # Writing the logs
          log.step <- log.step + 1
          log.block[log.step] <- blocks[j]
          log.LL[log.step] <- as.numeric(logLik(model.basis))
          log.zt[log.step] <- zt.temp
          blocks <- blocks[-j]
          
          break
        }
      }
    }
  }
  
  ## Create and print log
  log.df <- data.frame(log.step=1:log.step, log.block, log.LL, log.p, log.zt)
  if(print.log == TRUE) print(log.df, digits=4)
  
  ## Return the 'best' fitting model
  return(model.basis)
}

poschange$gender_pre <- as.factor(poschange$gender_pre)
poschange$class.size.ind <- as.factor(poschange$class.size.ind)
poschange$time.meet.ind <- as.factor(poschange$time.meet.ind)
poschange$prev.stat.course.ind <- as.factor(poschange$prev.stat.course.ind)

forward.lmer(start.model = modelP, 
             blocks = c("pre_perc", "gender_pre", "age_pre", "gpa_pre",
             "SATACTzscore",
             "instructor.gender",
             "class.size.ind",
             "years.teaching.intro.stats",
             "math.prereq.coded",
             "TA",
             "gaise.familiar",
             "class.session.length.mins.ind",
             "class.meet.weeks.ind",
             "time.meet.ind",
             "incentive.post.ind",
             "section_gpa",
             "section_SATACTzscore",
             "prev.stat.course.ind",
             "advanced.stats.degree",
             "isi.workshop",
             "position.classification",
             "student.type",
             "analyzing.data.experience",
             "CarnegieClassification",
             "semester"),
             max.iter = 30,
             sig.level = .05,
             zt = F,
             print.log = T
)

################################################################################################

### cAIC MODEL SELECTION ###***

library(cAIC4)
stepcAIC(modelP, direction = "backward", trace = TRUE, data = lowA)
## Using cAIC best model:
# predicting overall attitude change for positive
finalmod <- lmer(overall_attitude_change ~ pre_perc + as.factor(gender_pre) +
  age_pre + gpa_pre + SATACTzscore + instructor.gender + as.factor(class.size.ind) +
  years.teaching.intro.stats + as.factor(math.prereq.coded) +
  TA + as.factor(gaise.familiar) + class.session.length.mins.ind +
  as.factor(as.factor(class.meet.weeks)) + as.factor(time.meet.ind) +
  as.factor(incentive.post.ind) + textbook + section_gpa +
  section_SATACTzscore + prev.stat.course.ind + as.factor(advanced.stats.degree) +
  isi.workshop + as.factor(position.classification) + as.factor(student.type) +
  as.factor(analyzing.data.experience) + as.factor(CarnegieClassification) +
  semester + (1 | institution) + (1 | instructor.last.name:institution) +
  as.factor(gender_pre):instructor.gender, 
  data = poschange)

summary(finalmod)
anova(finalmod)


stepcAIC(modelA, direction = "backward", trace = TRUE, data = lowA)
## Using cAIC best model:
# predicting affect change for low attitudes
finalmod2 <- lmer(Affect_change ~ pre_perc + as.factor(gender_pre) + age_pre +  
                    gpa_pre + SATACTzscore + instructor.gender + as.factor(class.size.ind) +  
                    years.teaching.intro.stats + as.factor(math.prereq.coded) +  
                    TA + as.factor(gaise.familiar) + class.session.length.mins.ind +  
                    as.factor(as.factor(class.meet.weeks)) + as.factor(time.meet.ind) +  
                    as.factor(incentive.post.ind) + textbook + section_gpa +  
                    section_SATACTzscore + prev.stat.course.ind + as.factor(advanced.stats.degree) +  
                    isi.workshop + as.factor(position.classification) + as.factor(student.type) +  
                    as.factor(analyzing.data.experience) + as.factor(CarnegieClassification) +  
                    semester + (1 | instructor.last.name:institution) + 
                    (1 |InstructorName_Section_Semester:(instructor.last.name:institution)) +  
                    as.factor(gender_pre):instructor.gender, data = lowA)

summary(finalmod2)
anova(finalmod2)
(.005697+.037644)/(.005697+.037644+.929302)

library(LMERConvenienceFunctions)
fitLMER.fnc(model = modelA, method = "llrt")

finalmod3 <- lmer(Affect_change ~ pre_perc + age_pre + gpa_pre + SATACTzscore +  
  as.factor(math.prereq.coded) + TA + as.factor(gaise.familiar) +  
  class.session.length.mins.ind + as.factor(as.factor(class.meet.weeks)) +  
  as.factor(incentive.post.ind) + as.factor(position.classification) +  
  as.factor(CarnegieClassification) + textbook +
    (1 | institution/instructor.last.name/InstructorName_Section_Semester),
  data = lowA)

summary(finalmod3)
anova(finalmod3)
(.0008892+.0269295+.01664929)/(.0008892+.0269295+.01664929+.9308935)

## most significant variables
ggplot(data = lowA[lowA$gpa_pre<5 & lowA$gpa_pre>1,], aes(x = gpa_pre, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()
ggplot(data = lowA, aes(x = pre_perc, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

ggplot(data = lowA, aes(x = age_pre, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()
ggplot(data = lowA, aes(x = SATACTzscore, y = Affect_change)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

ggplot(data = lowA, aes(y = Affect_change, fill = TA)) + geom_boxplot() + theme_bw()
ggplot(data = lowA, aes(fill = class.session.length.mins.ind, y = Affect_change)) + geom_boxplot() + theme_bw()

################################################################################################
## this was still running after 36 hours...

#you need to change na options to run dredge
options(na.action = "na.fail")
#running dredge
my_dredge <- MuMIn::dredge(modelA)
#resetting na options back to the default in R
options(na.action = "na.omit")

#This function takes all of the dredge output and filters it to only include your null model and those where the delta AIC is <= 10
top_models <- my_dredge %>% 
  dplyr::filter(delta <= 10 ) 
View(top_models) #Easier to see models using view

#writing the top models to a .csv output for making a table to report values
write.csv(top_models, (paste(my_path, "top_models.csv", sep="")), row.names=FALSE)

################################################################################################

## OTHER ATTITUDE STUFF - Question by question likert plots

attitudes <- data[!is.na(data$textbook),c(which(colnames(data)=="textbook"),first_pre_a:last_pre_a,first_post_a:last_post_a)]

apply(attitudes[,2:37], 2, mean, na.rm=T)

colMeans(data[,c(first_pre_a:last_pre_a,first_post_a:last_post_a)], na.rm=T)

## subset low avg attitude
attitudes_pre <- data.frame(attitudes[,1:37], premean=rowMeans(attitudes[,2:37], na.rm=T))
attitudes_low <- attitudes_pre[attitudes_pre$premean < 4,]

# # Avg affect score (low = negative view)
# attitudes_pre$affectavg <- rowMeans(cbind(8-attitudes_pre[,c("Q6.d_pre_a","Q7.e_pre_a","Q7.h_pre_a","Q8.h_pre_a")],
#                                           attitudes_pre[,c("Q6.c_pre_a","Q7.i_pre_a")]))
# # Avg competence score (low = incompetent)
# attitudes_pre$compavg <- rowMeans(cbind(8-attitudes_pre[,c("Q6.e_pre_a","Q7.a_pre_a","Q8.f_pre_a","Q9.e_pre_a")],
#                                           attitudes_pre[,c("Q9.a_pre_a","Q9.b_pre_a")]))
# # Avg difficulty score (low = difficult)
# attitudes_pre$diffavg <- rowMeans(cbind(8-attitudes_pre[,c("Q6.h_pre_a","Q8.d_pre_a","Q8.j_pre_a","Q9.d_pre_a","Q9.f_pre_a")],
#                                           attitudes_pre[,c("Q6.f_pre_a","Q8.b_pre_a")]))
# # Avg value score (low = no value)
# attitudes_pre$valueavg <- rowMeans(cbind(8-attitudes_pre[,c("Q6.g_pre_a","Q7.c_pre_a","Q7.f_pre_a","Q8.a_pre_a","Q8.e_pre_a","Q9.c_pre_a")],
#                                           attitudes_pre[,c("Q6.i_pre_a","Q7.j_pre_a","Q7.g_pre_a")]))
# # Avg interest score (low = uninterested)
# attitudes_pre$interestavg <- rowMeans(attitudes_pre[,c("Q7.b_pre_a","Q7.j_pre_a","Q8.c_pre_a","Q8.i_pre_a")])


### Calculate differences (post-pre)

for (i in 2:37) {
  attitudes[,(i+72)] <- attitudes[,(i+36)] - attitudes[,i]
  colnames(attitudes)[(i+72)] <- paste(substr(colnames(attitudes[i]),1,4),"diff",sep = "_")
}

### Change Likert scores to factor and specify levels
library(likert)

for (i in 2:73) {
  attitudes[,i] <- factor(attitudes[,i],
                          levels = c("1", "2", "3", "4", "5", "6", "7"),
                          labels = c("Very Strongly Disagree", "Strongly Disagree", "Disagree", "Neutral",
                                     "Agree", "Strongly Agree", "Very Strongly Agree"),
                          ordered = TRUE)
}

# Levels of change in rating
for (i in 74:109) {
  attitudes[,i+36] <- attitudes[,i]
  
  # attitudes[,i] <- factor(attitudes[,i],
  #                         levels = c("-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6"),
  #                         ordered = TRUE)
  
  attitudes[,i+36][attitudes[,i+36] %in% c("-6","-5","-4")] <- "Lot more disagree"    # change labels
  attitudes[,i+36][attitudes[,i+36] %in% c("-3","-2","-1")] <- "Some more disagree"
  attitudes[,i+36][attitudes[,i+36] == 0] <- "No change"
  attitudes[,i+36][attitudes[,i+36] %in% c("1","2","3")] <- "Some more agree"
  attitudes[,i+36][attitudes[,i+36] %in% c("4","5","6")] <- "Lot more agree"

  colnames(attitudes)[(i+36)] <- paste(colnames(attitudes[i]),"c",sep = "_")
    
  attitudes[,i+36] <- factor(attitudes[,i+36],
                            levels = c("Lot more disagree","Some more disagree","No change",
                                     "Some more agree","Lot more agree"),
                            ordered = TRUE)
}

# # Decrease, Same, Increase
# for (i in 74:109) {
#   attitudes[,i+72] <- attitudes[,i]
#   
#   attitudes[,i+72][attitudes[,i+72] < 0 ] <- "Decrease"
#   attitudes[,i+72][attitudes[,i+72] == 0] <- "No change"
#   attitudes[,i+72][attitudes[,i+72] > 0 ] <- "Increase"
#   
#   colnames(attitudes)[(i+72)] <- paste(colnames(attitudes[i]),"sign",sep = "_")
#   
#   attitudes[,i+72] <- factor(attitudes[,i+72],
#                              levels = c("Decrease","No change","Increase"),
#                              ordered = TRUE)
# }


### Add questions as variable labels in order to display on output
library(papeR)

labels(attitudes) <- c("Textbook", 
                       "I plan to complete all of my statistics assignments.",
                       "I plan to work hard in my statistics course.",
                       "I will like statistics.",
                       "I will feel insecure when I have to do statistics problems.",
                       "I will have trouble understanding statistics because of how I think.",
                       "Statistics formulas are easy to understand.",
                       "Statistics is worthless.",
                       "Statistics is a complicated subject.",
                       "Statistics should be a required part of my professional training.",
                       "Statistical skills will make me more employable.",
                       
                       "I will have no idea what's going on in this statistics course.",
                       "I am interested in being able to communicate statistical information to others.",
                       "Statistics is not useful to the typical profession.",
                       "I plan to study hard for every statistics test.",
                       "I will get frustrated going over statistics tests in class.",
                       "Statistical thinking is not applicable in my life outside my job.",
                       "I use statistics in my everyday life.",
                       "I will be under stress during statistics class.",
                       "I will enjoy taking statistics courses.",
                       "I am interested in using statistics.",
                       
                       "Statistics conclusions are rarely presented in everyday life.",
                       "Statistics is a subject quickly learned by most people.",
                       "I am interested in understanding statistical information.",
                       "Learning statistics requires a great deal of discipline.",
                       "I will have no application for statistics in my profession.",
                       "I will make a lot of math errors in statistics.",
                       "I plan to attend every statistics class session.",
                       "I am scared by statistics.",
                       "I am interested in learning statistics.",
                       "Statistics involves massive computations.",
                       
                       "I can learn statistics.",
                       "I will understand statistics equations.",
                       "Statistics is irrelevant in my life",
                       "Statistics is highly technical.",
                       "I will find it difficult to understand statistical concepts.",
                       "Most people have to learn a new way of thinking to do statistics.",
                       
                       "I completed all of my statistics assignments.",
                       "I worked hard in my statistics course.",
                       "I liked statistics.",
                       "I felt insecure when I had to do statistics problems.",
                       "I had trouble understanding statistics because of how I think.",
                       "Statistics formulas were easy to understand.",
                       "Statistics was worthless.",
                       "Statistics was a complicated subject.",
                       "Statistics should be a required part of my professional training.",
                       "Statistical skills will make me more employable.",
                       
                       "I had no idea what was going on in this statistics course.",
                       "I am interested in being able to communicate statistical information to others.",
                       "Statistics is not useful to the typical profession.",
                       "I studied hard for every statistics test.",
                       "I got frustrated going over statistics tests in class.",
                       "Statistical thinking is not applicable in my life outside my job.",
                       "I use statistics in my everyday life.",
                       "I was under stress during statistics class.",
                       "I enjoyed taking statistics courses.",
                       "I am interested in using statistics.",
                       
                       "Statistics conclusions are rarely presented in everyday life.",
                       "Statistics is a subject quickly learned by most people.",
                       "I am interested in understanding statistical information.",
                       "Learning statistics requires a great deal of discipline.",
                       "I have no application for statistics in my profession.",
                       "I made a lot of math errors in statistics.",
                       "I attended every statistics class session.",
                       "I was scared by statistics.",
                       "I am interested in learning statistics.",
                       "Statistics involves massive computations.",
                       
                       "I learned statistics.",
                       "I understood statistics equations.",
                       "Statistics is irrelevant in my life",
                       "Statistics is highly technical.",
                       "I found it difficult to understand statistical concepts.",
                       "Most people have to learn a new way of thinking to do statistics.",
                       
                       "I completed all of my statistics assignments.",
                       "I worked hard in my statistics course.",
                       "I liked statistics.",
                       "I felt insecure when I had to do statistics problems.",
                       "I had trouble understanding statistics because of how I think.",
                       "Statistics formulas were easy to understand.",
                       "Statistics was worthless.",
                       "Statistics was a complicated subject.",
                       "Statistics should be a required part of my professional training.",
                       "Statistical skills will make me more employable.",
                       
                       "I had no idea what was going on in this statistics course.",
                       "I am interested in being able to communicate statistical information to others.",
                       "Statistics is not useful to the typical profession.",
                       "I studied hard for every statistics test.",
                       "I got frustrated going over statistics tests in class.",
                       "Statistical thinking is not applicable in my life outside my job.",
                       "I use statistics in my everyday life.",
                       "I was under stress during statistics class.",
                       "I enjoyed taking statistics courses.",
                       "I am interested in using statistics.",
                       
                       "Statistics conclusions are rarely presented in everyday life.",
                       "Statistics is a subject quickly learned by most people.",
                       "I am interested in understanding statistical information.",
                       "Learning statistics requires a great deal of discipline.",
                       "I have no application for statistics in my profession.",
                       "I made a lot of math errors in statistics.",
                       "I attended every statistics class session.",
                       "I was scared by statistics.",
                       "I am interested in learning statistics.",
                       "Statistics involves massive computations.",
                       
                       "I learned statistics.",
                       "I understood statistics equations.",
                       "Statistics is irrelevant in my life",
                       "Statistics is highly technical.",
                       "I found it difficult to understand statistical concepts.",
                       "Most people have to learn a new way of thinking to do statistics.",

                       "I completed all of my statistics assignments.",
                       "I worked hard in my statistics course.",
                       "I liked statistics.",
                       "I felt insecure when I had to do statistics problems.",
                       "I had trouble understanding statistics because of how I think.",
                       "Statistics formulas were easy to understand.",
                       "Statistics was worthless.",
                       "Statistics was a complicated subject.",
                       "Statistics should be a required part of my professional training.",
                       "Statistical skills will make me more employable.",
                       
                       "I had no idea what was going on in this statistics course.",
                       "I am interested in being able to communicate statistical information to others.",
                       "Statistics is not useful to the typical profession.",
                       "I studied hard for every statistics test.",
                       "I got frustrated going over statistics tests in class.",
                       "Statistical thinking is not applicable in my life outside my job.",
                       "I use statistics in my everyday life.",
                       "I was under stress during statistics class.",
                       "I enjoyed taking statistics courses.",
                       "I am interested in using statistics.",
                       
                       "Statistics conclusions are rarely presented in everyday life.",
                       "Statistics is a subject quickly learned by most people.",
                       "I am interested in understanding statistical information.",
                       "Learning statistics requires a great deal of discipline.",
                       "I have no application for statistics in my profession.",
                       "I made a lot of math errors in statistics.",
                       "I attended every statistics class session.",
                       "I was scared by statistics.",
                       "I am interested in learning statistics.",
                       "Statistics involves massive computations.",
                       
                       "I learned statistics.",
                       "I understood statistics equations.",
                       "Statistics is irrelevant in my life",
                       "Statistics is highly technical.",
                       "I found it difficult to understand statistical concepts.",
                       "Most people have to learn a new way of thinking to do statistics.")

### Separate subcategories

# Pre-scores
effort <- which(colnames(attitudes) %in% c("Q6.a_pre_a","Q6.b_pre_a","Q7.d_pre_a","Q8.g_pre_a"))
affect <- which(colnames(attitudes) %in% c("Q6.c_pre_a","Q6.d_pre_a","Q7.e_pre_a","Q7.h_pre_a","Q7.i_pre_a","Q8.h_pre_a"))
competence <- which(colnames(attitudes) %in% c("Q6.e_pre_a","Q7.a_pre_a","Q8.f_pre_a","Q9.a_pre_a","Q9.b_pre_a","Q9.e_pre_a"))
difficulty <- which(colnames(attitudes) %in% c("Q6.f_pre_a","Q6.h_pre_a","Q8.b_pre_a","Q8.d_pre_a","Q8.j_pre_a","Q9.d_pre_a","Q9.f_pre_a"))
value <- which(colnames(attitudes) %in% c("Q6.g_pre_a","Q6.i_pre_a","Q6.j_pre_a","Q7.c_pre_a","Q7.f_pre_a","Q7.g_pre_a","Q8.a_pre_a","Q8.e_pre_a","Q9.c_pre_a"))
interest <- which(colnames(attitudes) %in% c("Q7.b_pre_a","Q7.j_pre_a","Q8.c_pre_a","Q8.i_pre_a"))

# Post-Pre numerical
effort_d <- which(colnames(attitudes) %in% c("Q6.a_diff","Q6.b_diff","Q7.d_diff","Q8.g_diff"))
affect_d <- which(colnames(attitudes) %in% c("Q6.c_diff","Q6.d_diff","Q7.e_diff","Q7.h_diff","Q7.i_diff","Q8.h_diff"))
competence_d <- which(colnames(attitudes) %in% c("Q6.e_diff","Q7.a_diff","Q8.f_diff","Q9.a_diff","Q9.b_diff","Q9.e_diff"))
difficulty_d <- which(colnames(attitudes) %in% c("Q6.f_diff","Q6.h_diff","Q8.b_diff","Q8.d_diff","Q8.j_diff","Q9.d_diff","Q9.f_diff"))
value_d <- which(colnames(attitudes) %in% c("Q6.g_diff","Q6.i_diff","Q6.j_diff","Q7.c_diff","Q7.f_diff","Q7.g_diff","Q8.a_diff","Q8.e_diff","Q9.c_diff"))
interest_d <- which(colnames(attitudes) %in% c("Q7.b_diff","Q7.j_diff","Q8.c_diff","Q8.i_diff"))

# Post-Pre categorical
effort_dc <- which(colnames(attitudes) %in% c("Q6.a_diff_c","Q6.b_diff_c","Q7.d_diff_c","Q8.g_diff_c"))
affect_dc <- which(colnames(attitudes) %in% c("Q6.c_diff_c","Q6.d_diff_c","Q7.e_diff_c","Q7.h_diff_c","Q7.i_diff_c","Q8.h_diff_c"))
competence_dc <- which(colnames(attitudes) %in% c("Q6.e_diff_c","Q7.a_diff_c","Q8.f_diff_c","Q9.a_diff_c","Q9.b_diff_c","Q9.e_diff_c"))
difficulty_dc <- which(colnames(attitudes) %in% c("Q6.f_diff_c","Q6.h_diff_c","Q8.b_diff_c","Q8.d_diff_c","Q8.j_diff_c","Q9.d_diff_c","Q9.f_diff_c"))
value_dc <- which(colnames(attitudes) %in% c("Q6.g_diff_c","Q6.i_diff_c","Q6.j_diff_c","Q7.c_diff_c","Q7.f_diff_c","Q7.g_diff_c","Q8.a_diff_c","Q8.e_diff_c","Q9.c_diff_c"))
interest_dc <- which(colnames(attitudes) %in% c("Q7.b_diff_c","Q7.j_diff_c","Q8.c_diff_c","Q8.i_diff_c"))


### CHOOSE SUBCATEGORY TO PLOT
subcat <- value_dc
# effort, affect, competence, difficulty, value, interest
# Pre-score (name), Post-Pre categorical (name_dc)

# Fix labels
names(attitudes) <- labels(attitudes)
# Output
plot(likert(items=attitudes[,subcat], grouping=attitudes[,1]))


### INTERPRETATION:
# Each statement is displayed by textbook category
# percentage on left sums all orange categories (negative responses)
# percentage in middle represents grey category (neutral)
# percentage on right sums all teal categories (positive responses)


# pre_a <- data[,c(first_pre_a:last_pre_a),drop=FALSE]
# lik_pre <- likert(pre_a, grouping = data$textbook)
# lik_post <- likert(data[,c(first_post_a:last_post_a)])
# plot(lik_pre, type="bar")
# 
# plot(lik_pre,
#      ordered=FALSE,
#      group.order=names(pre_a)) +
#   ggtitle("Teaching Evaluations")

# PreByText <- likert(items=data[,c(first_pre_a:last_pre_a),drop=FALSE],
#                      grouping=data$textbook)
# 
# pregs <- likert(summary = PreByText$results, grouping = PreByText$results[,1])
# 
# 
# plot(pregs, 
#      ordered=TRUE,
#      group.order=c('ISI','OtherSBI','NotSBI')) + 
#   ggtitle("Textbook Use Patterns")
# 
# likert.bar.plot(pregs)
# 
# mytest <- data[,c("Q6.a_pre_a","textbook")]
# t(dcast(mytest, Q6.a_pre_a~textbook))
# 
# as.data.frame(table(data$"Q6.a_pre_a",data$textbook))

# ggplot(data = data, aes(x=textbook, y=Q6.a_pre_a)) + geom_bar(stat="identity")

# mytest <- data.frame(t(dcast(data[,c("Q6.a_pre_a","textbook")], Q6.a_pre_a~textbook)[-8,-1]))
# plot(mytest, type="bar")

################################################################################################

### RETENTION 4-MONTH ###

## Cronback alphas (want >.7)
library(psych)

## 2015-16 4-month ##
affect_pre <- data.frame(r4y15$Q6.c_pre,r4y15$Q6.d_pre,r4y15$Q7.e_pre,r4y15$Q7.h_pre,r4y15$Q7.i_pre,r4y15$Q8.h_pre)
affect_pre_results = alpha(affect_pre,na.rm=TRUE)
affect_post <- data.frame(r4y15$Q6.c_post,r4y15$Q6.d_post,r4y15$Q7.e_post,r4y15$Q7.h_post,r4y15$Q7.i_post,r4y15$Q8.h_post)
affect_post_results = alpha(affect_post,na.rm=TRUE)
affect_ret <- data.frame(r4y15$Q6.c_ret,r4y15$Q6.d_ret,r4y15$Q7.e_ret,r4y15$Q7.h_ret,r4y15$Q7.i_ret,r4y15$Q8.h_ret)
affect_ret_results = alpha(affect_ret,na.rm=TRUE)
affect_pre_results$total; affect_post_results$total;affect_ret_results$total
# 0.7944299, 0.8676422, 0.838863

comp_pre <- data.frame(r4y15$Q6.e_pre,r4y15$Q7.a_pre,r4y15$Q8.f_pre,r4y15$Q9.a_pre,r4y15$Q9.b_pre,r4y15$Q9.e_pre)
comp_pre_results = alpha(comp_pre,na.rm=TRUE)
comp_post <- data.frame(r4y15$Q6.e_post,r4y15$Q7.a_post,r4y15$Q8.f_post,r4y15$Q9.a_post,r4y15$Q9.b_post,r4y15$Q9.e_post)
comp_post_results = alpha(comp_post,na.rm=TRUE)
comp_ret <- data.frame(r4y15$Q6.e_ret,r4y15$Q7.a_ret,r4y15$Q8.f_ret,r4y15$Q9.a_ret,r4y15$Q9.b_ret,r4y15$Q9.e_ret)
comp_ret_results = alpha(comp_ret,na.rm=TRUE)
comp_pre_results$total; comp_post_results$total; comp_ret_results$total
# 0.8360205, 0.8646863, 0.8111369

difficulty_pre <- data.frame(r4y15$Q6.f_pre,r4y15$Q6.h_pre,r4y15$Q8.b_pre,r4y15$Q8.d_pre,r4y15$Q8.j_pre,r4y15$Q9.d_pre,r4y15$Q9.f_pre)
difficulty_pre_results = alpha(difficulty_pre,na.rm=TRUE)
difficulty_post <- data.frame(r4y15$Q6.f_post,r4y15$Q6.h_post,r4y15$Q8.b_post,r4y15$Q8.d_post,r4y15$Q8.j_post,r4y15$Q9.d_post,r4y15$Q9.f_post)
difficulty_post_results = alpha(difficulty_post,na.rm=TRUE)
difficulty_ret <- data.frame(r4y15$Q6.f_ret,r4y15$Q6.h_ret,r4y15$Q8.b_ret,r4y15$Q8.d_ret,r4y15$Q8.j_ret,r4y15$Q9.d_ret,r4y15$Q9.f_ret)
difficulty_ret_results = alpha(difficulty_ret,na.rm=TRUE)
difficulty_pre_results$total; difficulty_post_results$total; difficulty_ret_results$total
# 0.7527862, 0.757824, 0.7533996

effort_pre <- data.frame(r4y15$Q6.a_pre,r4y15$Q6.b_pre,r4y15$Q7.d_pre,r4y15$Q8.g_pre)
effort_pre_results = alpha(effort_pre,na.rm=TRUE)
effort_post <- data.frame(r4y15$Q6.a_post,r4y15$Q6.b_post,r4y15$Q7.d_post,r4y15$Q8.g_post)
effort_post_results = alpha(effort_post,na.rm=TRUE)
effort_ret <- data.frame(r4y15$Q6.a_ret,r4y15$Q6.b_ret,r4y15$Q7.d_ret,r4y15$Q8.g_ret)
effort_ret_results = alpha(effort_ret,na.rm=TRUE)
effort_pre_results$total; effort_post_results$total; effort_ret_results$total
# 0.8300805, 0.6795368**, 0.770567

interest_pre <- data.frame(r4y15$Q7.b_pre,r4y15$Q7.j_pre,r4y15$Q8.c_pre,r4y15$Q8.i_pre)
interest_pre_results = alpha(interest_pre,na.rm=TRUE)
interest_post <- data.frame(r4y15$Q7.b_post,r4y15$Q7.j_post,r4y15$Q8.c_post,r4y15$Q8.i_post)
interest_post_results = alpha(interest_post,na.rm=TRUE)
interest_ret <- data.frame(r4y15$Q7.b_ret,r4y15$Q7.j_ret,r4y15$Q8.c_ret,r4y15$Q8.i_ret)
interest_ret_results = alpha(interest_ret,na.rm=TRUE)
interest_pre_results$total; interest_post_results$total; interest_ret_results$total
# 0.8995473, 0.9288371, 0.9295695

value_pre <- data.frame(r4y15$Q6.g_pre,r4y15$Q6.i_pre,r4y15$Q6.j_pre,r4y15$Q7.c_pre,r4y15$Q7.f_pre,r4y15$Q7.g_pre,r4y15$Q8.a_pre,r4y15$Q8.e_pre,r4y15$Q9.c_pre)
value_pre_results = alpha(value_pre,na.rm=TRUE)
value_post <- data.frame(r4y15$Q6.g_post,r4y15$Q6.i_post,r4y15$Q6.j_post,r4y15$Q7.c_post,r4y15$Q7.f_post,r4y15$Q7.g_post,r4y15$Q8.a_post,r4y15$Q8.e_post,r4y15$Q9.c_post)
value_post_results = alpha(value_post,na.rm=TRUE)
value_ret <- data.frame(r4y15$Q6.g_ret,r4y15$Q6.i_ret,r4y15$Q6.j_ret,r4y15$Q7.c_ret,r4y15$Q7.f_ret,r4y15$Q7.g_ret,r4y15$Q8.a_ret,r4y15$Q8.e_ret,r4y15$Q9.c_ret)
value_ret_results = alpha(value_ret,na.rm=TRUE)
value_pre_results$total; value_post_results$total; value_ret_results$total
# 0.8703791, 0.8949276, 0.9146723


## 2016-17 4-month ##
affect_pre <- data.frame(r4y16$Q6.c_pre,r4y16$Q6.d_pre,r4y16$Q7.e_pre,r4y16$Q7.h_pre,r4y16$Q7.i_pre,r4y16$Q8.h_pre)
affect_pre_results = alpha(affect_pre,na.rm=TRUE)
affect_post <- data.frame(r4y16$Q6.c_post,r4y16$Q6.d_post,r4y16$Q7.e_post,r4y16$Q7.h_post,r4y16$Q7.i_post,r4y16$Q8.h_post)
affect_post_results = alpha(affect_post,na.rm=TRUE)
affect_ret <- data.frame(r4y16$Q6.c_ret,r4y16$Q6.d_ret,r4y16$Q7.e_ret,r4y16$Q7.h_ret,r4y16$Q7.i_ret,r4y16$Q8.h_ret)
affect_ret_results = alpha(affect_ret,na.rm=TRUE)
affect_pre_results$total; affect_post_results$total;affect_ret_results$total
# 0.8363335, 0.8558611, 0.8669706

comp_pre <- data.frame(r4y16$Q6.e_pre,r4y16$Q7.a_pre,r4y16$Q8.f_pre,r4y16$Q9.a_pre,r4y16$Q9.b_pre,r4y16$Q9.e_pre)
comp_pre_results = alpha(comp_pre,na.rm=TRUE)
comp_post <- data.frame(r4y16$Q6.e_post,r4y16$Q7.a_post,r4y16$Q8.f_post,r4y16$Q9.a_post,r4y16$Q9.b_post,r4y16$Q9.e_post)
comp_post_results = alpha(comp_post,na.rm=TRUE)
comp_ret <- data.frame(r4y16$Q6.e_ret,r4y16$Q7.a_ret,r4y16$Q8.f_ret,r4y16$Q9.a_ret,r4y16$Q9.b_ret,r4y16$Q9.e_ret)
comp_ret_results = alpha(comp_ret,na.rm=TRUE)
comp_pre_results$total; comp_post_results$total; comp_ret_results$total
# 0.8509711, 0.859382, 0.8773475

difficulty_pre <- data.frame(r4y16$Q6.f_pre,r4y16$Q6.h_pre,r4y16$Q8.b_pre,r4y16$Q8.d_pre,r4y16$Q8.j_pre,r4y16$Q9.d_pre,r4y16$Q9.f_pre)
difficulty_pre_results = alpha(difficulty_pre,na.rm=TRUE)
difficulty_post <- data.frame(r4y16$Q6.f_post,r4y16$Q6.h_post,r4y16$Q8.b_post,r4y16$Q8.d_post,r4y16$Q8.j_post,r4y16$Q9.d_post,r4y16$Q9.f_post)
difficulty_post_results = alpha(difficulty_post,na.rm=TRUE)
difficulty_ret <- data.frame(r4y16$Q6.f_ret,r4y16$Q6.h_ret,r4y16$Q8.b_ret,r4y16$Q8.d_ret,r4y16$Q8.j_ret,r4y16$Q9.d_ret,r4y16$Q9.f_ret)
difficulty_ret_results = alpha(difficulty_ret,na.rm=TRUE)
difficulty_pre_results$total; difficulty_post_results$total; difficulty_ret_results$total
# 0.7644064, 0.7874377, 0.7661577

effort_pre <- data.frame(r4y16$Q6.a_pre,r4y16$Q6.b_pre,r4y16$Q7.d_pre,r4y16$Q8.g_pre)
effort_pre_results = alpha(effort_pre,na.rm=TRUE)
effort_post <- data.frame(r4y16$Q6.a_post,r4y16$Q6.b_post,r4y16$Q7.d_post,r4y16$Q8.g_post)
effort_post_results = alpha(effort_post,na.rm=TRUE)
effort_ret <- data.frame(r4y16$Q6.a_ret,r4y16$Q6.b_ret,r4y16$Q7.d_ret,r4y16$Q8.g_ret)
effort_ret_results = alpha(effort_ret,na.rm=TRUE)
effort_pre_results$total; effort_post_results$total; effort_ret_results$total
# 0.737754, 0.7022165, 0.6978749*

interest_pre <- data.frame(r4y16$Q7.b_pre,r4y16$Q7.j_pre,r4y16$Q8.c_pre,r4y16$Q8.i_pre)
interest_pre_results = alpha(interest_pre,na.rm=TRUE)
interest_post <- data.frame(r4y16$Q7.b_post,r4y16$Q7.j_post,r4y16$Q8.c_post,r4y16$Q8.i_post)
interest_post_results = alpha(interest_post,na.rm=TRUE)
interest_ret <- data.frame(r4y16$Q7.b_ret,r4y16$Q7.j_ret,r4y16$Q8.c_ret,r4y16$Q8.i_ret)
interest_ret_results = alpha(interest_ret,na.rm=TRUE)
interest_pre_results$total; interest_post_results$total; interest_ret_results$total
# 0.8767123, 0.9235797, 0.9220162

value_pre <- data.frame(r4y16$Q6.g_pre,r4y16$Q6.i_pre,r4y16$Q6.j_pre,r4y16$Q7.c_pre,r4y16$Q7.f_pre,r4y16$Q7.g_pre,r4y16$Q8.a_pre,r4y16$Q8.e_pre,r4y16$Q9.c_pre)
value_pre_results = alpha(value_pre,na.rm=TRUE)
value_post <- data.frame(r4y16$Q6.g_post,r4y16$Q6.i_post,r4y16$Q6.j_post,r4y16$Q7.c_post,r4y16$Q7.f_post,r4y16$Q7.g_post,r4y16$Q8.a_post,r4y16$Q8.e_post,r4y16$Q9.c_post)
value_post_results = alpha(value_post,na.rm=TRUE)
value_ret <- data.frame(r4y16$Q6.g_ret,r4y16$Q6.i_ret,r4y16$Q6.j_ret,r4y16$Q7.c_ret,r4y16$Q7.f_ret,r4y16$Q7.g_ret,r4y16$Q8.a_ret,r4y16$Q8.e_ret,r4y16$Q9.c_ret)
value_ret_results = alpha(value_ret,na.rm=TRUE)
value_pre_results$total; value_post_results$total; value_ret_results$total
# 0.8782935, 0.8896999, 0.8916104


data = r4y15
data = r4y16
### Prep for tables ###
# New textbook Classification #
first_post = which(colnames(data)=="Q6.a_post_a")
last_post = which(colnames(data)=="Q9.f_post_a")
first_ret = which(colnames(data)=="Q6.a_ret")
last_ret = which(colnames(data)=="Q9.f_ret")


ISI_frame_post_s <- data[ which(data$textbook.classification=="ISI"),c(first_post:last_post)]
ISI_frame_ret_s <- data[ which(data$textbook.classification=="ISI"),c(first_ret:last_ret)]
otherSBI_frame_post_s <- data[ which(data$textbook.classification=="OtherSBI"),c(first_post:last_post)]
otherSBI_frame_ret_s <- data[ which(data$textbook.classification=="OtherSBI"),c(first_ret:last_ret)]
nonSBI_frame_post_s <- data[ which(data$textbook.classification=="NotSBI"),c(first_post:last_post)]
nonSBI_frame_ret_s <- data[ which(data$textbook.classification=="NotSBI"),c(first_ret:last_ret)]


Percent.Correct = c("Post","Ret") 

### Function to get tables (must run prep first) ###
tables_short <- function(questions_short){
  for (i in questions_short){
    
    ISI = c(round(mean(ISI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(ISI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    OtherSBI = c(round(mean(otherSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(otherSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    NonSBI = c(round(mean(nonSBI_frame_post_s[,(i-16)],na.rm=TRUE),digits=2),round(mean(nonSBI_frame_ret_s[,(i-16)],na.rm=TRUE),digits=2))
    print(paste("Question",i,sep=" "))
    print(data.frame(Percent.Correct,ISI,OtherSBI,NonSBI,row.names=NULL)) 
  }
} 
### Enter into this vector the questions that you want tables for. 17=Q17, 18=Q18, ... 46=Q46.a, 47=Q46.b, 48=Q47#
questions_short <- c(17:48)
tables_short(questions_short)

################################################################################################

### LOOKING AHEAD ###
## Project?

table(data$"accountability.assignments_other")

# "project" %in% data[i,"accountability.assignments_other"]

for (i in 1:nrow(data)) { 
  if (grepl("project",data[i,"accountability.assignments_other"])) {data$project[i] <- "Yes"}
  else {data$project[i] <- "No"}
}

table(data$project)
# 178 have project

dataProj <- data[data$project == "Yes",]
data=dataProj


## WHY TAKE
table(data$whytake)
# 15. If the choice had been yours, how likely is it that you would have chosen to take any course in
# statistics?
table(data$choice_likelihood_pre)

################################################################################################

## ISCAM
colMeans(dataISCAM[,c("Affect_change","Cog_change","Difficulty_change","Effort_change","Interest_change",
                      "Value_change")], na.rm=T)

## HS
colMeans(dataHS[,c("Affect_change","Cog_change","Difficulty_change","Effort_change","Interest_change",
                   "Value_change")], na.rm=T)


# Compare ISI, SBI, notSBI
iscamsummary(data$Affect_change, data$textbook.classification2)
iscamsummary(data$Cog_change, data$textbook.classification2)
iscamsummary(data$Difficulty_change, data$textbook.classification2)
iscamsummary(data$Interest_change, data$textbook.classification2)
iscamsummary(data$Effort_change, data$textbook.classification2)
iscamsummary(data$Value_change, data$textbook.classification2)


ggplot(data = data, aes(x = overall_attitude_pre, y = new_ach_gain)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
ggplot(data = data, aes(x = overall_attitude_pre, y = overall_attitude_post)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
