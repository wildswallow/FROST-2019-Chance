#setwd("/Users/Stephanie/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
r16y15 <- read.csv("MasterApril27withIDs.csv")

####### RW Columns ######
## RET ##
r16y15$Q16_not_answered_ret <- is.na(r16y15$Q16.a_ret) & 
  is.na(r16y15$Q16.b_ret) & is.na(r16y15$Q16.c_ret) & is.na(r16y15$Q16.d_ret) & is.na(r16y15$Q17_ret) 

#Q16#
r16y15$Q16.a_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret_c_RW[r16y15$Q16.a_ret == "checked**"] <- 1
r16y15$Q16.a_ret_c_RW[r16y15$Q16_not_answered_ret] <- NA
r16y15$Q16.b_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.b_ret_c_RW[r16y15$Q16.b_ret == "blank**"] <- 1
r16y15$Q16.b_ret_c_RW[r16y15$Q16_not_answered_ret] <- NA

r16y15$Q16.c_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret_c_RW[r16y15$Q16.c_ret == "blank**"] <- 1
r16y15$Q16.c_ret_c_RW[r16y15$Q16_not_answered_ret] <- NA

r16y15$Q16.d_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret_c_RW[r16y15$Q16.d_ret == "checked**"] <- 1
r16y15$Q16.d_ret_c_RW[r16y15$Q16_not_answered_ret] <- NA

#Q17#
r16y15$Q17_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q17_ret_c_RW[r16y15$Q17_ret == "c**"] <- 1
#Q18#
r16y15$Q18_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q18_ret_c_RW[r16y15$Q18_ret == "b**"] <- 1
#Q19#
r16y15$Q19_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q19_ret_c_RW[r16y15$Q19_ret == "a**"] <- 1
#Q20#
r16y15$Q20_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q20_ret_c_RW[r16y15$Q20_ret == "b**"] <- 1
#Q21#
r16y15$Q21_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q21_ret_c_RW[r16y15$Q21_ret == "a**"] <- 1
#Q22#
r16y15$Q22_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q22_ret_c_RW[r16y15$Q22_ret == "a**"] <- 1
#Q23#
r16y15$Q23_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q23_ret_c_RW[r16y15$Q23_ret == "a**"] <- 1
#Q24#
r16y15$Q24_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q24_ret_c_RW[r16y15$Q24_ret == "b**"] <- 1
#Q25#
r16y15$Q25_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q25_ret_c_RW[r16y15$Q25_ret == "b**"] <- 1
#Q26#
r16y15$Q26_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q26_ret_c_RW[r16y15$Q26_ret == "b**"] <- 1
#Q27#
r16y15$Q27_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q27_ret_c_RW[r16y15$Q27_ret == "c**"] <- 1
#Q28#
r16y15$Q28_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q28_ret_c_RW[r16y15$Q28_ret == "b**"] <- 1
#Q29#
r16y15$Q29_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q29_ret_c_RW[r16y15$Q29_ret == "a**"] <- 1
#Q30#
r16y15$Q30_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q30_ret_c_RW[r16y15$Q30_ret == "b**"] <- 1
#Q31#
r16y15$Q31_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q31_ret_c_RW[r16y15$Q31_ret == "b**"] <- 1
#Q32#
r16y15$Q32_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q32_ret_c_RW[r16y15$Q32_ret == "d**"] <- 1
#Q33#
r16y15$Q33_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q33_ret_c_RW[r16y15$Q33_ret == "c**"] <- 1
#Q34#
r16y15$Q34_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q34_ret_c_RW[r16y15$Q34_ret == "c**"] <- 1
#Q35#
r16y15$Q35_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q35_ret_c_RW[r16y15$Q35_ret == "e**"] <- 1
#Q36#
r16y15$Q36_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q36_ret_c_RW[r16y15$Q36_ret == "a**"] <- 1
#Q37#
r16y15$Q37_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q37_ret_c_RW[r16y15$Q37_ret == "b**"] <- 1
#Q38#
r16y15$Q38_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q38_ret_c_RW[r16y15$Q38_ret == "a**"] <- 1
#Q39#
r16y15$Q39_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q39_ret_c_RW[r16y15$Q39_ret == "b**"] <- 1
#Q40#
r16y15$Q40_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q40_ret_c_RW[r16y15$Q40_ret == "a**"] <- 1
#Q41#
r16y15$Q41_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q41_ret_c_RW[r16y15$Q41_ret == "b**"] <- 1
#Q42#
r16y15$Q42_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q42_ret_c_RW[r16y15$Q42_ret == "b**"] <- 1
#Q43#
r16y15$Q43_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q43_ret_c_RW[r16y15$Q43_ret == "b**"] <- 1
#Q44#
r16y15$Q44_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q44_ret_c_RW[r16y15$Q44_ret == "b**"] <- 1
#Q45#
r16y15$Q45_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q45_ret_c_RW[r16y15$Q45_ret == "b**"] <- 1
#Q46#
r16y15$Q46.a_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q46.a_ret_c_RW[r16y15$Q46.a_ret == "a**"] <- 1
r16y15$Q46.b_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q46.b_ret_c_RW[r16y15$Q46.b_ret == "b**"] <- 1
#Q47#
r16y15$Q47_ret_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q47_ret_c_RW[r16y15$Q47_ret == "d**"] <- 1

for (i in 1:length(r16y15$Q6.a_ret)){
  if (sum(is.na(r16y15$Q16.a_ret[i]))==1){r16y15$Q16.a_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.b_ret[i]))==1){r16y15$Q16.b_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.c_ret[i]))==1){r16y15$Q16.c_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.d_ret[i]))==1){r16y15$Q16.d_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q17_ret[i]))==1){r16y15$Q17_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q18_ret[i]))==1){r16y15$Q18_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q19_ret[i]))==1){r16y15$Q19_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q20_ret[i]))==1){r16y15$Q20_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q21_ret[i]))==1){r16y15$Q21_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q22_ret[i]))==1){r16y15$Q22_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q23_ret[i]))==1){r16y15$Q23_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q24_ret[i]))==1){r16y15$Q24_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q25_ret[i]))==1){r16y15$Q25_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q26_ret[i]))==1){r16y15$Q26_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q27_ret[i]))==1){r16y15$Q27_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q28_ret[i]))==1){r16y15$Q28_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q29_ret[i]))==1){r16y15$Q29_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q30_ret[i]))==1){r16y15$Q30_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q31_ret[i]))==1){r16y15$Q31_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q32_ret[i]))==1){r16y15$Q32_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q33_ret[i]))==1){r16y15$Q33_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q34_ret[i]))==1){r16y15$Q34_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q35_ret[i]))==1){r16y15$Q35_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q36_ret[i]))==1){r16y15$Q36_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q37_ret[i]))==1){r16y15$Q37_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q38_ret[i]))==1){r16y15$Q38_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q39_ret[i]))==1){r16y15$Q39_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q40_ret[i]))==1){r16y15$Q40_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q41_ret[i]))==1){r16y15$Q41_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q42_ret[i]))==1){r16y15$Q42_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q43_ret[i]))==1){r16y15$Q43_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q44_ret[i]))==1){r16y15$Q44_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q45_ret[i]))==1){r16y15$Q45_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q46.a_ret[i]))==1){r16y15$Q46.a_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q46.b_ret[i]))==1){r16y15$Q46.b_ret_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q47_ret[i]))==1){r16y15$Q47_ret_c_RW[i]<-NA}
}


## RET2 ##
r16y15$Q16_not_answered_ret2 <- is.na(r16y15$Q16.a_ret2) & 
  is.na(r16y15$Q16.b_ret2) & is.na(r16y15$Q16.c_ret2) & is.na(r16y15$Q16.d_ret2) & is.na(r16y15$Q17_ret2) 

#Q16#
r16y15$Q16.a_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret2_c_RW[r16y15$Q16.a_ret2 == "checked**"] <- 1
r16y15$Q16.a_ret2_c_RW[r16y15$Q16_not_answered_ret2] <- NA
r16y15$Q16.b_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.b_ret2_c_RW[r16y15$Q16.b_ret2 == "blank**"] <- 1
r16y15$Q16.b_ret2_c_RW[r16y15$Q16_not_answered_ret2] <- NA

r16y15$Q16.c_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret2_c_RW[r16y15$Q16.c_ret2 == "blank**"] <- 1
r16y15$Q16.c_ret2_c_RW[r16y15$Q16_not_answered_ret2] <- NA

r16y15$Q16.d_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q16.a_ret2_c_RW[r16y15$Q16.d_ret2 == "checked**"] <- 1
r16y15$Q16.d_ret2_c_RW[r16y15$Q16_not_answered_ret2] <- NA

#Q17#
r16y15$Q17_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q17_ret2_c_RW[r16y15$Q17_ret2 == "c**"] <- 1
#Q18#
r16y15$Q18_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q18_ret2_c_RW[r16y15$Q18_ret2 == "b**"] <- 1
#Q19#
r16y15$Q19_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q19_ret2_c_RW[r16y15$Q19_ret2 == "a**"] <- 1
#Q20#
r16y15$Q20_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q20_ret2_c_RW[r16y15$Q20_ret2 == "b**"] <- 1
#Q21#
r16y15$Q21_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q21_ret2_c_RW[r16y15$Q21_ret2 == "a**"] <- 1
#Q22#
r16y15$Q22_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q22_ret2_c_RW[r16y15$Q22_ret2 == "a**"] <- 1
#Q23#
r16y15$Q23_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q23_ret2_c_RW[r16y15$Q23_ret2 == "a**"] <- 1
#Q24#
r16y15$Q24_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q24_ret2_c_RW[r16y15$Q24_ret2 == "b**"] <- 1
#Q25#
r16y15$Q25_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q25_ret2_c_RW[r16y15$Q25_ret2 == "b**"] <- 1
#Q26#
r16y15$Q26_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q26_ret2_c_RW[r16y15$Q26_ret2 == "b**"] <- 1
#Q27#
r16y15$Q27_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q27_ret2_c_RW[r16y15$Q27_ret2 == "c**"] <- 1
#Q28#
r16y15$Q28_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q28_ret2_c_RW[r16y15$Q28_ret2 == "b**"] <- 1
#Q29#
r16y15$Q29_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q29_ret2_c_RW[r16y15$Q29_ret2 == "a**"] <- 1
#Q30#
r16y15$Q30_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q30_ret2_c_RW[r16y15$Q30_ret2 == "b**"] <- 1
#Q31#
r16y15$Q31_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q31_ret2_c_RW[r16y15$Q31_ret2 == "b**"] <- 1
#Q32#
r16y15$Q32_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q32_ret2_c_RW[r16y15$Q32_ret2 == "d**"] <- 1
#Q33#
r16y15$Q33_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q33_ret2_c_RW[r16y15$Q33_ret2 == "c**"] <- 1
#Q34#
r16y15$Q34_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q34_ret2_c_RW[r16y15$Q34_ret2 == "c**"] <- 1
#Q35#
r16y15$Q35_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q35_ret2_c_RW[r16y15$Q35_ret2 == "e**"] <- 1
#Q36#
r16y15$Q36_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q36_ret2_c_RW[r16y15$Q36_ret2 == "a**"] <- 1
#Q37#
r16y15$Q37_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q37_ret2_c_RW[r16y15$Q37_ret2 == "b**"] <- 1
#Q38#
r16y15$Q38_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q38_ret2_c_RW[r16y15$Q38_ret2 == "a**"] <- 1
#Q39#
r16y15$Q39_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q39_ret2_c_RW[r16y15$Q39_ret2 == "b**"] <- 1
#Q40#
r16y15$Q40_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q40_ret2_c_RW[r16y15$Q40_ret2 == "a**"] <- 1
#Q41#
r16y15$Q41_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q41_ret2_c_RW[r16y15$Q41_ret2 == "b**"] <- 1
#Q42#
r16y15$Q42_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q42_ret2_c_RW[r16y15$Q42_ret2 == "b**"] <- 1
#Q43#
r16y15$Q43_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q43_ret2_c_RW[r16y15$Q43_ret2 == "b**"] <- 1
#Q44#
r16y15$Q44_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q44_ret2_c_RW[r16y15$Q44_ret2 == "b**"] <- 1
#Q45#
r16y15$Q45_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q45_ret2_c_RW[r16y15$Q45_ret2 == "b**"] <- 1
#Q46#
r16y15$Q46.a_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q46.a_ret2_c_RW[r16y15$Q46.a_ret2 == "a**"] <- 1
r16y15$Q46.b_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q46.b_ret2_c_RW[r16y15$Q46.b_ret2 == "b**"] <- 1
#Q47#
r16y15$Q47_ret2_c_RW <- numeric(length(r16y15$Q6.a_pre))
r16y15$Q47_ret2_c_RW[r16y15$Q47_ret2 == "d**"] <- 1

for (i in 1:length(r16y15$Q6.a_ret2)){
  if (sum(is.na(r16y15$Q16.a_ret2[i]))==1){r16y15$Q16.a_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.b_ret2[i]))==1){r16y15$Q16.b_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.c_ret2[i]))==1){r16y15$Q16.c_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q16.d_ret2[i]))==1){r16y15$Q16.d_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q17_ret2[i]))==1){r16y15$Q17_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q18_ret2[i]))==1){r16y15$Q18_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q19_ret2[i]))==1){r16y15$Q19_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q20_ret2[i]))==1){r16y15$Q20_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q21_ret2[i]))==1){r16y15$Q21_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q22_ret2[i]))==1){r16y15$Q22_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q23_ret2[i]))==1){r16y15$Q23_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q24_ret2[i]))==1){r16y15$Q24_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q25_ret2[i]))==1){r16y15$Q25_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q26_ret2[i]))==1){r16y15$Q26_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q27_ret2[i]))==1){r16y15$Q27_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q28_ret2[i]))==1){r16y15$Q28_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q29_ret2[i]))==1){r16y15$Q29_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q30_ret2[i]))==1){r16y15$Q30_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q31_ret2[i]))==1){r16y15$Q31_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q32_ret2[i]))==1){r16y15$Q32_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q33_ret2[i]))==1){r16y15$Q33_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q34_ret2[i]))==1){r16y15$Q34_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q35_ret2[i]))==1){r16y15$Q35_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q36_ret2[i]))==1){r16y15$Q36_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q37_ret2[i]))==1){r16y15$Q37_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q38_ret2[i]))==1){r16y15$Q38_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q39_ret2[i]))==1){r16y15$Q39_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q40_ret2[i]))==1){r16y15$Q40_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q41_ret2[i]))==1){r16y15$Q41_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q42_ret2[i]))==1){r16y15$Q42_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q43_ret2[i]))==1){r16y15$Q43_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q44_ret2[i]))==1){r16y15$Q44_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q45_ret2[i]))==1){r16y15$Q45_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q46.a_ret2[i]))==1){r16y15$Q46.a_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q46.b_ret2[i]))==1){r16y15$Q46.b_ret2_c_RW[i]<-NA}
  if (sum(is.na(r16y15$Q47_ret2[i]))==1){r16y15$Q47_ret2_c_RW[i]<-NA}
}


for (i in 1:length(r16y15$Q17_pre)) {
  r16y15$new_Q16_ret[i] <- sum(r16y15$Q16.a_ret_c_RW[i],r16y15$Q16.b_ret_c_RW[i],r16y15$Q16.c_ret_c_RW[i],r16y15$Q16.d_ret_c_RW[i],na.rm=TRUE) / 4
  r16y15$new_CI_ret[i] <- sum(r16y15$Q18_ret_c_RW[i],r16y15$Q19_ret_c_RW[i],r16y15$Q20_ret_c_RW[i],na.rm=TRUE) / 3
  r16y15$new_pvalue_ret[i] <- sum(r16y15$Q28_ret_c_RW[i],r16y15$Q29_ret_c_RW[i],r16y15$Q30_ret_c_RW[i],r16y15$Q31_ret_c_RW[i],na.rm=TRUE) / 4
  r16y15$new_histogram_ret[i] <- sum(r16y15$Q32_ret_c_RW[i],r16y15$Q33_ret_c_RW[i],na.rm=TRUE) / 2
  r16y15$new_simulation_ret[i] <- sum(r16y15$Q37_ret_c_RW[i],r16y15$Q38_ret_c_RW[i],r16y15$Q39_ret_c_RW[i],na.rm=TRUE) / 3
  r16y15$new_variability_ret[i] <- sum(r16y15$Q40_ret_c_RW[i],r16y15$Q41_ret_c_RW[i],na.rm=TRUE) / 2
  
  r16y15$new_Q16_ret2[i] <- sum(r16y15$Q16.a_ret_c_RW2[i],r16y15$Q16.b_ret_c_RW2[i],r16y15$Q16.c_ret_c_RW2[i],r16y15$Q16.d_ret_c_RW2[i],na.rm=TRUE) / 4
  r16y15$new_CI_ret2[i] <- sum(r16y15$Q18_ret2_c_RW[i],r16y15$Q19_ret2_c_RW[i],r16y15$Q20_ret2_c_RW[i],na.rm=TRUE) / 3
  r16y15$new_pvalue_ret2[i] <- sum(r16y15$Q28_ret2_c_RW[i],r16y15$Q29_ret2_c_RW[i],r16y15$Q30_ret2_c_RW[i],r16y15$Q31_ret2_c_RW[i],na.rm=TRUE) / 4
  r16y15$new_histogram_ret2[i] <- sum(r16y15$Q32_ret2_c_RW[i],r16y15$Q33_ret2_c_RW[i],na.rm=TRUE) / 2
  r16y15$new_simulation_ret2[i] <- sum(r16y15$Q37_ret2_c_RW[i],r16y15$Q38_ret2_c_RW[i],r16y15$Q39_ret2_c_RW[i],na.rm=TRUE) / 3
  r16y15$new_variability_ret2[i] <- sum(r16y15$Q40_ret2_c_RW[i],r16y15$Q41_ret2_c_RW[i],na.rm=TRUE) / 2
}



###################### New Concept categories #######################

# Ret Data Collection #
r16y15$new_Ret_DC <- (r16y15$new_Q16_ret+r16y15$Q42_ret_c_RW+r16y15$Q22_ret_c_RW+r16y15$Q25_ret_c_RW) / 4
# Ret2 Data Collection #
r16y15$new_Ret2_DC <- (r16y15$new_Q16_ret2+r16y15$Q42_ret2_c_RW+r16y15$Q22_ret2_c_RW+r16y15$Q25_ret2_c_RW) / 4

# Ret Descriptive Statistics #
r16y15$new_Ret_DS <- (r16y15$Q17_ret_c_RW+r16y15$Q21_ret_c_RW+r16y15$new_histogram_ret+r16y15$Q36_ret_c_RW+r16y15$new_variability_ret) / 5
# Ret2 Descriptive Statistics #
r16y15$new_Ret2_DS <- (r16y15$Q17_ret2_c_RW+r16y15$Q21_ret2_c_RW+r16y15$new_histogram_ret2+r16y15$Q36_ret2_c_RW+r16y15$new_variability_ret2) / 5

# Ret Confidence Intervals #
r16y15$new_Ret_CI <- (r16y15$new_CI_ret+r16y15$Q26_ret_c_RW+r16y15$Q45_ret_c_RW+r16y15$Q46.a_ret_c_RW+r16y15$Q46.b_ret_c_RW) / 5
# Ret2 Confidence Intervals #
r16y15$new_Ret2_CI <- (r16y15$new_CI_ret2+r16y15$Q26_ret2_c_RW+r16y15$Q45_ret2_c_RW+r16y15$Q46.a_ret2_c_RW+r16y15$Q46.b_ret2_c_RW) / 5

# Ret Tests of Significance #
r16y15$new_Ret_ST <- (r16y15$Q23_ret_c_RW+r16y15$Q24_ret_c_RW+r16y15$Q27_ret_c_RW+r16y15$new_pvalue_ret+r16y15$Q43_ret_c_RW+r16y15$Q44_ret_c_RW+r16y15$Q47_ret_c_RW) / 7
# Ret2 Tests of Significance #
r16y15$new_Ret2_ST <- (r16y15$Q23_ret2_c_RW+r16y15$Q24_ret2_c_RW+r16y15$Q27_ret2_c_RW+r16y15$new_pvalue_ret2+r16y15$Q43_ret2_c_RW+r16y15$Q44_ret2_c_RW+r16y15$Q47_ret2_c_RW) / 7

# Ret Simulation #
r16y15$new_Ret_Sim <- (r16y15$Q34_ret_c_RW+r16y15$Q35_ret_c_RW+r16y15$new_simulation_ret) / 3
# Ret2 Simulation #
r16y15$new_Ret2_Sim <- (r16y15$Q34_ret2_c_RW+r16y15$Q35_ret2_c_RW+r16y15$new_simulation_ret2) / 3


### New Percent of Retention Concept Questions Correct ###
ret_c_num_correct <- numeric(length(r16y15$Q17_ret_c_RW))
r16y15$new_ret_perc <- numeric(length(r16y15$Q17_ret_c_RW))
RWsret= c(which(colnames(r16y15)=="Q17_ret_c_RW"),which(colnames(r16y15)=="Q21_ret_c_RW"):which(colnames(r16y15)=="Q27_ret_c_RW"),
          which(colnames(r16y15)=="Q34_ret_c_RW"):which(colnames(r16y15)=="Q36_ret_c_RW"),
          which(colnames(r16y15)=="Q42_ret_c_RW"):which(colnames(r16y15)=="Q47_ret_c_RW"),
          which(colnames(r16y15)=="new_Q16_ret"):which(colnames(r16y15)=="new_variability_ret"))
for(i in 1:length(r16y15$Q17_ret_c_RW)) 
{
  ret_c_num_correct[i] <- sum(r16y15[i,RWsret],na.rm=TRUE)
  r16y15$new_ret_perc[i] <- round(ret_c_num_correct[i] / (24-sum(is.na(r16y15[i,RWsret]))),3)
}

### New Percent of Retention2 Concept Questions Correct ###
ret2_c_num_correct <- numeric(length(r16y15$Q17_ret2_c_RW))
r16y15$new_ret2_perc <- numeric(length(r16y15$Q17_ret2_c_RW))
RWsret2= c(which(colnames(r16y15)=="Q17_ret2_c_RW"),which(colnames(r16y15)=="Q21_ret2_c_RW"):which(colnames(r16y15)=="Q27_ret2_c_RW"),
           which(colnames(r16y15)=="Q34_ret2_c_RW"):which(colnames(r16y15)=="Q36_ret2_c_RW"),
           which(colnames(r16y15)=="Q42_ret2_c_RW"):which(colnames(r16y15)=="Q47_ret2_c_RW"),
           which(colnames(r16y15)=="new_Q16_ret2"):which(colnames(r16y15)=="new_variability_ret2"))
for(i in 1:length(r16y15$Q17_ret2_c_RW)) 
{
  ret2_c_num_correct[i] <- sum(r16y15[i,RWsret2],na.rm=TRUE)
  r16y15$new_ret2_perc[i] <- round(ret2_c_num_correct[i] / (24-sum(is.na(r16y15[i,RWsret2]))),3)
}


########################### Concept Percent Correct ######################


### Percent of Retention Concept Questions Correct ###
Ret_c_num_correct <- numeric(length(r16y15$Q17_ret_c_RW))
r16y15$ret_perc <- numeric(length(r16y15$Q17_ret_c_RW))
RWsRet= which(colnames(r16y15)=="Q16.a_ret_c_RW"):which(colnames(r16y15)=="Q47_ret_c_RW")
for(i in 1:length(r16y15$Q17_ret_c_RW)) 
{
  Ret_c_num_correct[i] <- sum(r16y15[i,RWsRet],na.rm=TRUE)
  r16y15$ret_perc[i] <- Ret_c_num_correct[i] / (36-sum(is.na(r16y15[i,RWsRet])))
}

### Percent of Retention 2 Concept Questions Correct ###
Ret2_c_num_correct <- numeric(length(r16y15$Q17_ret2_c_RW))
r16y15$ret2_perc <- numeric(length(r16y15$Q17_ret2_c_RW))
RWsRet2= which(colnames(r16y15)=="Q16.a_ret2_c_RW"):which(colnames(r16y15)=="Q47_ret2_c_RW")
for(i in 1:length(r16y15$Q17_ret2_c_RW)) 
{
  Ret2_c_num_correct[i] <- sum(r16y15[i,RWsRet2],na.rm=TRUE)
  r16y15$ret2_perc[i] <- Ret2_c_num_correct[i] / (36-sum(is.na(r16y15[i,RWsRet2])))
}



############################# Merging Teacher Data ##############################
### Loading In Non-Retention Files ###
#setwd("/Users/Stephanie/Dropbox/IS Assessment Data Summer 2017/final files/Data Files")
full15 <- read.csv("Further Cleaning Result 15.16.csv")
names(full15)[names(full15)=="X"] <- "ID"

### Subsetting Columns In Retention Files ###
PrePost = which(colnames(r16y15)=="fullname"):which(colnames(r16y15)=="Value_post")
r16y15 <- r16y15[,-PrePost]

#### The Merge ###
r16y15_merge <- merge(r16y15,full15,by="ID")


############################# Change Columns ###############################
r16y15_merge$change1 <- r16y15_merge$new_ret_perc - r16y15_merge$new_post_perc
r16y15_merge$change2 <- r16y15_merge$new_post_perc - r16y15_merge$new_ret_perc

############################### Creating CSV File #################################
#setwd("/Users/Stephanie/Dropbox/IS Assessment Data Summer 2017/Retention Data/Files I Will Use/")
write.csv(r16y15_merge, "MERGED_16Month.csv")



