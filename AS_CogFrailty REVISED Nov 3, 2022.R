#######################################################################################
# ActionSeniors! Cognitive Frailty sub-analysis (FINAL ANALYSIS)
# Author: Ryan Falck
# Last edited: 07-08-2022
#######################################################################################

#----------------------------------------#
#     Set-up for outcomes                #
#----------------------------------------#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(boot,MASS,readxl,mediation,plyr,sandwich,ggplot2,lme4,lmerTest,rpart,survival,survminer,lsmeans,fmsb, mice, mitools, tableone)


setwd("~/Desktop/Manuscripts/Ongoing Projects/ActionSeniors! Cognitive Frailty/")
data<-read_excel("ActionSeniors_CogsSPPB.xlsx")#Data#

data$CogFrail<-NA
data$CogFrail[data$SPPB_1<=9 & data$MoCA_1<26]<-"Yes"
data$CogFrail[data$SPPB_1>9 | data$MoCA_1>=26]<-"No"

data$Education[data$Education=="Less than grade 9" | data$Education=="Grades 9-13, without certificate or diploma"]<- "Less than high school diploma"
data$Education[data$Education=="Some university without certificate or diploma" | data$Education=="Trades or professional certificate or diploma"]<-"Some university"
data$Education[data$Education=="University certificate or diploma" | data$Education=="University degree"]<- "University degree or higher"


data$exclude<-0
data$exclude[data$ID==330]<-1

data$Group.1<-NA
data$Group.1[data$Group==0]<-"Interview"
data$Group.1[data$Group==1]<-"Otago"

data$Group_r[data$Group==0]<-1
data$Group_r[data$Group==1]<-0

data$Gender<-NA
data$Gender[data$Sex==1]<-0
data$Gender[data$Sex==2]<-1

CogFrail<- subset(data, CogFrail=="Yes")

CogFrail$exclude<-0
CogFrail$exclude[CogFrail$ID==330]<-1

CogFrail$Group.1<-NA
CogFrail$Group.1[CogFrail$Group==0]<-"Interview"
CogFrail$Group.1[CogFrail$Group==1]<-"Otago"

CogFrail$Group_r[CogFrail$Group==0]<-1
CogFrail$Group_r[CogFrail$Group==1]<-0

CogFrail$Gender<-NA
CogFrail$Gender[CogFrail$Sex==1]<-0
CogFrail$Gender[CogFrail$Sex==2]<-1

No_CogFrail<- subset(data, CogFrail=="No")

No_CogFrail$exclude<-0
No_CogFrail$exclude[No_CogFrail$ID==330]<-1

No_CogFrail$Group.1<-NA
No_CogFrail$Group.1[No_CogFrail$Group==0]<-"Interview"
No_CogFrail$Group.1[No_CogFrail$Group==1]<-"Otago"

No_CogFrail$Group_r[No_CogFrail$Group==0]<-1
No_CogFrail$Group_r[No_CogFrail$Group==1]<-0

No_CogFrail$Gender<-NA
No_CogFrail$Gender[No_CogFrail$Sex==1]<-0
No_CogFrail$Gender[No_CogFrail$Sex==2]<-1


#Create long data set
library(plyr)
wide<-rename(data,c("Weight_1"="Weightbaseline", "Height_1"="Heightbaseline","BMI_1"="BMIbaseline",
                    "Falls 12M prior to baseline"="Fallsbeforebaseline", "Total_Exposure"="TotalExposure",
                    "Total_Falls"="TotalFalls","PPA_1"="PPAbaseline","GDS_1"="GDSbaseline","MoCA_1"="MoCAbaseline",
                    "Trails_BA_1"="TrailsBAbaseline","DSST_1"="DSSTbaseline","TUG_1"="TUGbaseline",
                    "SPPB_1"="SPPBbaseline","Gait_Speed_1"="GaitSpeedbaseline","Trails_BA_2"="TrailsBA_2",
                    "Trails_BA_3"="TrailsBA_3","Group_r"="Group.r","Gait_Speed_2"="GaitSpeed_2","Gait_Speed_3"="GaitSpeed_3"))
detach("package:plyr", unload = TRUE)

wide$StroopINTbaseline<-as.numeric(wide$Stroop_3_1) - as.numeric(wide$Stroop_2_1)
wide$StroopINT_2<-as.numeric(wide$Stroop_3_2) - as.numeric(wide$Stroop_2_2)
wide$StroopINT_3<-as.numeric(wide$Stroop_3_3) - as.numeric(wide$Stroop_2_3)

wide$DigitsFBbaseline<- wide$Digits_F_1 - wide$Digits_B_1
wide$DigitsFB_2<- wide$Digits_F_2 - wide$Digits_B_2
wide$DigitsFB_3<- wide$Digits_F_3 - wide$Digits_B_3

wide$time<-1

wide2<-wide[c(1,76,2:12,15,16,17,64:69,13,14,18,21,27,28,29,31,70,73,
              32:34,37,43:45,47,71,74,48:50,53,59:61,63,72,75)]
data2 <- reshape(as.data.frame(wide2),idvar="ID",varying=c(33:52),direction="long",sep="_")

CogFrail$Females<-NA
CogFrail$Females[CogFrail$Gender==1]<-"Female"
CogFrail$Females[CogFrail$Gender==0]<-"Male"

No_CogFrail$Females<-NA
No_CogFrail$Females[No_CogFrail$Gender==1]<-"Female"
No_CogFrail$Females[No_CogFrail$Gender==0]<-"Male"

###################################
# CONSORT Flow Chart              #
###################################
#Determine CogFrail Status of Dropouts
CONSORT<- subset(wide2, ID==128 | ID== 131 | ID==132 | ID==134 | ID==158 | ID==163 | ID==176 | ID==185 | ID==192 | ID==195 | ID==213 | ID==214 | ID==216 | ID==220 | ID==222 | ID==223 |
                   ID==270 | ID==278 | ID==281 | ID==284 | ID==306 | ID==310 | ID==318 | ID==321 | ID==330 | ID==334 | ID==339 | ID==357 | ID==362 | ID==369 | ID==373 | ID==375 | ID==378 |
                   ID==385 | ID==395 | ID==398 | ID==407 | ID==408 | ID==412 | ID==413 | ID==424 | ID==426 | ID==427 | ID==428 | ID==432 | ID==438 | ID==455 | ID==461) #All dropouts

CONSORT<-CONSORT[c(1,3,18)]
MONTHS<- c(13,2,1,0,0,0,13,0,0,13,2,4,0,11,3,5,1,5,2,5,0,0,3,0,0,5,0,4,5,5,5,7,6,1,1,9,1,7,9,2,4,1,7,7,5,11,7,10) #Months in study before drop-out
CONSORT<-cbind(CONSORT,MONTHS) 

CONSORT$Group[CONSORT$Group==1]<-"INT"
CONSORT$Group[CONSORT$Group==0]<-"CON"

#Dropouts before 6 months (i.e., Month 7) or after 6 months in study
CONSORT$Timepoint<-NA
CONSORT$Timepoint[CONSORT$MONTHS<=7]<-"6 Months or Less"
CONSORT$Timepoint[CONSORT$MONTHS>7]<-"More than 6 Months"


#Sort by Group, Cognitive Frailty Status, and Time of Dropout
table(CONSORT$Timepoint,CONSORT$Group, CONSORT$CogFrail)


######################################################
# Table 1 - Participant characteristics by group and
# cognitive frailty status
######################################################


#Descriptive statistics for cognitively frail participants
cogfrail_vars<-dput(names(CogFrail[c(4,70,7,8,9,13,15,16,18,28,29,31)]))
Table1_cogfrail_vars<-CreateTableOne(vars=cogfrail_vars, strata="Group", data=CogFrail)
print(Table1_cogfrail_vars,contDigits=2,missing=TRUE,quote=TRUE)


#Descriptive statistics for non-cognitively frail participants
nocogfrail_vars<-dput(names(No_CogFrail[c(4,70,7,8,9,13,15,16,18,28,29,31)]))
Table1_nocogfrail_vars<-CreateTableOne(vars=nocogfrail_vars, strata="Group", data=No_CogFrail)
print(Table1_nocogfrail_vars,contDigits=2,missing=TRUE,quote=TRUE)


########################################################
# Supplementary Table 1 - Participant characteristics  #
# based on cognitive frailty status                    #
########################################################
Supp<-wide2
Supp$Females<-NA
Supp$Females[Supp$Sex==1]<-"Female"
Supp$Females[Supp$Sex==2]<-"Male"

supp_vars<-dput(names(Supp[c(5,53,8,9,10,14,15,23,25,28,29,30)]))
SuppTable1<-CreateTableOne(vars=supp_vars, strata="CogFrail", data=Supp)
print(SuppTable1, contDigits=2, missing=TRUE, quote=TRUE)

##################################################
#           Set-up for Compliance Data           #
##################################################
compliancedata<-read_excel("jcd AS_compliance_Jul 2021.xlsx", sheet = "Compliance_for analysis") #Data#

compliancedata2<-compliancedata[c(1,7,11,15,19,23,27,31,35,39,43,47,51,55,66)]
compliancedata2<-compliancedata2[-c(173:177),]
library(plyr)
compliancedata2<-rename(compliancedata2, c("id"="ID"))
detach("package:plyr", unload = TRUE)

OEP<-subset(wide, Group == 1)
compliancedata3<-merge(OEP,compliancedata2, by = c("ID"))
compliancedata3<-compliancedata3[c(1,3,65,77:90)]
library(plyr)
compliancedata3<-rename(compliancedata3, c("OEPCOMP1"="OEPCOMP_1","OEPCOMP2"="OEPCOMP_2","OEPCOMP3"="OEPCOMP_3","OEPCOMP4"="OEPCOMP_4","OEPCOMP5"="OEPCOMP_5","OEPCOMP6"="OEPCOMP_6",
                                           "OEPCOMP7"="OEPCOMP_7","OEPCOMP8"="OEPCOMP_8","OEPCOMP9"="OEPCOMP_9","OEPCOMP10"="OEPCOMP_10","OEPCOMP11"="OEPCOMP_11","OEPCOMP12"="OEPCOMP_12",
                                           "OEPCOMP13"="OEPCOMP_13"))
detach("package:plyr", unload = TRUE)

#long form
compliancelong <- reshape(as.data.frame(compliancedata3),idvar="ID",varying=c(4:16),direction="long",sep="_")

#Maximize compliance at 100% (i.e., compliance cannot be greater than 100%, regardless of how many times/month OEP was completed)
compliancelong$OEPCOMP[compliancelong$OEPCOMP>100]<-100

#--------------NOTE-------------------------#
#Note: All analyses using estimated average # 
#compliance for CogFrail participants       #
#(44.0%) and non-CogFrail (51.1%). High     #
# compliance is >= mean compliance for each #
#group.                                     #
#-------------------------------------------#


#Maximize compliance at 100% for each individual month
compliancedata2$OEPCOMP1[compliancedata2$OEPCOMP1>100]<-100
compliancedata2$OEPCOMP2[compliancedata2$OEPCOMP2>100]<-100
compliancedata2$OEPCOMP3[compliancedata2$OEPCOMP3>100]<-100
compliancedata2$OEPCOMP4[compliancedata2$OEPCOMP4>100]<-100
compliancedata2$OEPCOMP5[compliancedata2$OEPCOMP5>100]<-100
compliancedata2$OEPCOMP6[compliancedata2$OEPCOMP6>100]<-100
compliancedata2$OEPCOMP7[compliancedata2$OEPCOMP7>100]<-100
compliancedata2$OEPCOMP8[compliancedata2$OEPCOMP8>100]<-100
compliancedata2$OEPCOMP9[compliancedata2$OEPCOMP9>100]<-100
compliancedata2$OEPCOMP10[compliancedata2$OEPCOMP10>100]<-100
compliancedata2$OEPCOMP11[compliancedata2$OEPCOMP11>100]<-100
compliancedata2$OEPCOMP12[compliancedata2$OEPCOMP12>100]<-100
compliancedata2$OEPCOMP13[compliancedata2$OEPCOMP13>100]<-100

compliancedata2$AverageComply<-(compliancedata2[c(2)] + compliancedata2[c(3)] + compliancedata2[c(4)] + compliancedata2[c(5)] + compliancedata2[c(6)] + compliancedata2[c(7)] + 
                                  compliancedata2[c(8)] + compliancedata2[c(9)] + compliancedata2[c(10)] + compliancedata2[c(11)] + compliancedata2[c(12)] + compliancedata2[c(13)] + 
                                  compliancedata2[c(14)])/13

OEP2<-subset(wide2, Group == 1)


#Determine compliant vs. non compiant for CogFrail and Non-Frail
OEP2$Compliant<-NA
OEP2$Compliant[compliancedata2$AverageComply>=51.1 & OEP$CogFrail=="No" | compliancedata2$AverageComply>=44.0 & OEP$CogFrail=="Yes"] <- "Yes"
OEP2$Compliant[compliancedata2$AverageComply<51.1 & OEP$CogFrail=="No" | compliancedata2$AverageComply<44.0 & OEP$CogFrail=="Yes"] <- "No"

OEP3 <- reshape(as.data.frame(OEP2),idvar="ID",varying=c(33:52),direction="long",sep="_") 

##################################################
#     Graph Compliance Across Intervention       #
#     By CogFrailty Status and Sex               #            #########REVISED SECTION##########
##################################################

#Individual Compliance by Cognitive Frailty Status (Not for publication)
compliancelong$Timepoint<-NA
compliancelong$Timepoint[compliancelong$time==1]<-0
compliancelong$Timepoint[compliancelong$time==2]<-1
compliancelong$Timepoint[compliancelong$time==3]<-2
compliancelong$Timepoint[compliancelong$time==4]<-3
compliancelong$Timepoint[compliancelong$time==5]<-4
compliancelong$Timepoint[compliancelong$time==6]<-5
compliancelong$Timepoint[compliancelong$time==7]<-6
compliancelong$Timepoint[compliancelong$time==8]<-7
compliancelong$Timepoint[compliancelong$time==9]<-8
compliancelong$Timepoint[compliancelong$time==10]<-9
compliancelong$Timepoint[compliancelong$time==11]<-10
compliancelong$Timepoint[compliancelong$time==12]<-11
compliancelong$Timepoint[compliancelong$time==13]<-12 
compliancelong$Timepoint<-as.factor(compliancelong$Timepoint)

compliancelong$Sex[compliancelong$Sex==1]<-"Female"
compliancelong$Sex[compliancelong$Sex==2]<-"Male"


compliance.cogfrail<-subset(compliancelong, CogFrail=="Yes") 
library(forcats)

Compliance.Graph<- compliance.cogfrail %>%
  ggplot() + geom_line(aes(x=Timepoint, y=OEPCOMP, group=ID, color=Sex)) +
  labs(x="Months", y="Mean % Compliance to OEP") + ylim(0,100) + labs(color="Cognitive Frailty") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
Compliance.Graph
ggsave("REVISED Supplementary Figure 1 - Individual Compliance Graph of Cognitively Frail Participants.tiff", plot = Compliance.Graph, width = 16, height = 8, units="cm", dpi = 300)


#Linear Mixed Model for Estimating Mean Compliance at Each Timepoint Stratified by Sex
comply.2<-lmer(OEPCOMP~factor(time)*Sex + 1 + (1|ID), compliance.cogfrail)
emms.2<-as.data.frame(lsmeans(comply.2, ~factor(time)|Sex))

Compliance.Graph.3<- emms.2 %>%
  ggplot() + geom_line(aes(x=time, y=lsmean, color=Sex)) +
  labs(x="Months", y="Mean % Compliance to OEP") + ylim(0,100) + scale_x_continuous(breaks=seq(0,13,by=1)) +
  labs(color="Sex") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
Compliance.Graph.3
ggsave("REVISED Supplementary Figure 2 - Sex differences in monthly compliance.tiff", plot = Compliance.Graph.3, width = 16, height = 8, units="cm", dpi = 300)


#Estimated differences in average compliance for CogFrail and non-CogFrail participants
lsmeans(comply,~CogFrail)
contrast(lsmeans(comply, ~CogFrail), "pairwise", adj="none") #No significant difference overall

#Estimated differences in compliance between CogFrail and non-CogFrail participants by timepoint
lsmeans(comply,~CogFrail|factor(time))         
contrast(lsmeans(comply, ~CogFrail|factor(time)), "pairwise", adj="none")#only significant differences at months 1-3 (Non-CogFrail > CogFrail)

#Estimated differences in average compliance based on Sex
lsmeans(comply.2,~Sex)
contrast(lsmeans(comply.2, ~Sex), "pairwise", adj="none") #No significant difference overall

#Estimated differences in compliance based on Sex by timepoint
lsmeans(comply.2,~Sex|factor(time))
contrast(lsmeans(comply.2, ~Sex|factor(time)), "pairwise", adj="none") #No significant difference overall


###########################################
# Effects of intervention for CogFrail    # 
# vs. non-CogFrail participants           #         ######REVISED SECTION#######
###########################################


#------------------------------------------------#
#     PRIMARY OUTCOME                            #
#------------------------------------------------#
#Primary Outcome from Liu-Ambrose et al., 2019
mdl.nb<-glm.nb(TotalFalls~Group.1+scale(Gender,scale=FALSE)+offset(log(TotalExposure/365)),subset(wide2,exclude==0)) 
print(summary(mdl.nb))
SE<-sqrt(diag(vcovHC(mdl.nb,"HC1")))
p=2*pnorm(-abs(coef(mdl.nb)/SE))
print(cbind(coef(mdl.nb),SE,p))
est<-cbind(Estimate=coef(mdl.nb),LL=coef(mdl.nb)-1.96*SE,UL=coef(mdl.nb)+1.96*SE)
exp_group<-exp(est)
print("Use these estimates and LL and UL for the 95% CI");exp_group
print("Significant effect of intervention on falls risk for participants with CogFrailty")      

##Negative binomial analysis of treatment effects on falls count - use LL and UL for the 95% CI
#ONLY COGFRAIL PARTICIPANTS
mdl.nb<-glm.nb(Total_Falls~Group.1+scale(Gender,scale=FALSE)+offset(log(Total_Exposure/365)),subset(CogFrail,exclude==0)) 
print(summary(mdl.nb))
SE<-sqrt(diag(vcovHC(mdl.nb,"HC1")))
p=2*pnorm(-abs(coef(mdl.nb)/SE))
print(cbind(coef(mdl.nb),SE,p))
est<-cbind(Estimate=coef(mdl.nb),LL=coef(mdl.nb)-1.96*SE,UL=coef(mdl.nb)+1.96*SE)
exp_group<-exp(est)
print("Use these estimates and LL and UL for the 95% CI");exp_group
print("Significant effect of intervention on falls risk for participants with CogFrailty")      

#Differences in exposure by group
exposurelm<-lm(TotalExposure~Group, subset(wide2, CogFrail=="Yes"))
lsmeans(exposurelm, ~Group)
contrast(lsmeans(exposurelm, ~Group), "trt.vs.ctrl", adj="none") #No significant differences

#Differences in the cumulative number of falls by group and cognitive frailty
Monthly_falls <- read_excel("AS_Monthly falls_Mar 20 2019.xlsx")
Monthly_falls<-subset(Monthly_falls, ID %in% wide2$ID)

data1c<-cbind(Monthly_falls,wide2$CogFrail,wide2$Group)
data1c<-subset(data1c,ID!=330)

data1c<-plyr::rename(data1c,c("Month_1_Cumulative"="MonthCumulative_1","Month_2_Cumulative"="MonthCumulative_2","Month_3_Cumulative"="MonthCumulative_3",
                              "Month_4_Cumulative"="MonthCumulative_4","Month_5_Cumulative"="MonthCumulative_5","Month_6_Cumulative"="MonthCumulative_6",
                              "Month_7_Cumulative"="MonthCumulative_7","Month_8_Cumulative"="MonthCumulative_8","Month_9_Cumulative"="MonthCumulative_9",
                              "Month_10_Cumulative"="MonthCumulative_10","Month_11_Cumulative"="MonthCumulative_11","Month_12_Cumulative"="MonthCumulative_12",
                              "Month_13_Cumulative"="MonthCumulative_13","wide2$CogFrail"="CogFrail","wide2$Group"="Group"))
data1c_CogFrail_EX<-subset(data1c,Group==1 & CogFrail=="Yes")
data1c_CogFrail_UC<-subset(data1c,Group==0 & CogFrail=="Yes")
data1c_NoCogFrail_EX<-subset(data1c,Group==1 & CogFrail=="No")
data1c_NoCogFrail_UC<-subset(data1c,Group==0 & CogFrail=="No")

print("CogFrail Exercise");apply(data1c_CogFrail_EX[c(2:14)], 2, function(x) length(which(!is.na(x)))) #count people with falls data
print("CogFrail Usual Care");apply(data1c_CogFrail_UC[c(2:14)], 2, function(x) length(which(!is.na(x)))) #count people with falls data
print("No CogFrail Exercise");apply(data1c_NoCogFrail_EX[c(2:14)], 2, function(x) length(which(!is.na(x)))) #count people with falls data
print("No CogFrail Usual Care");apply(data1c_NoCogFrail_UC[c(2:14)], 2, function(x) length(which(!is.na(x)))) #count people with falls data

data1c.stacked<-reshape(data1c,idvar="ID",varying=c(2:27),direction="long",sep="_")
data1c.stacked$time<-data1c.stacked$time-1

Fallsmeans<-data.frame(aggregate(MonthCumulative~Group*CogFrail+time,data1c.stacked,sum))
subset(Fallsmeans, CogFrail=="Yes")

#------------------------------------------#
# Graph of Cumulative Falls over 12 Months #      #####REVISED SECTION#########
# based on group and CogFrail status       #
#------------------------------------------#
Fallsmeans$GroupCogFrail<-NA
Fallsmeans$GroupCogFrail[Fallsmeans$Group==0 & Fallsmeans$CogFrail=="Yes"]<-"Usual Care Group"
Fallsmeans$GroupCogFrail[Fallsmeans$Group==1 & Fallsmeans$CogFrail=="Yes"]<-"Exercise Group"

Fallsmeans.cogfrail<-subset(Fallsmeans, CogFrail=="Yes")


Cumulativefalls.Graph<- Fallsmeans.cogfrail %>%
  ggplot() + geom_line(aes(x=time, y=MonthCumulative, color=GroupCogFrail)) +
  labs(x="Months", y="Cumulative number of falls") + labs(color="Intervention Group") +
  scale_x_continuous(breaks=seq(0,13,by=1)) + scale_y_continuous(breaks=seq(0,300,by=50))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
Cumulativefalls.Graph
ggsave("REVISED Figure 3 - Cumulative falls of cognitively frail participants by intervention group.tiff", plot = Cumulativefalls.Graph, width = 16, height = 8, units="cm", dpi = 300)

#------------------------------------------------#
# Estimated differences in falls per person-year #    #####REVISED SECTION#####
#------------------------------------------------#
wide2.cogfrail<-subset(wide2, CogFrail=="Yes")

wide2.cogfrail$falls_ppy<-wide2.cogfrail$TotalFalls/(wide2.cogfrail$TotalExposure/365)

library(psych)
describeBy(wide2.cogfrail$falls_ppy, wide2.cogfrail$Group)

fallsppy_lm<-lm(falls_ppy~Group, wide2.cogfrail)
summary(fallsppy_lm)
lsmeans(fallsppy_lm, ~Group)
contrast(lsmeans(fallsppy_lm, ~~Group), "trt.vs.ctrl", adj="none")


#-----------------------------------#
# Differences in exposure time      #               ######REVISED SECTION######
#-----------------------------------#
expopsure_lm<-lm(TotalExposure~Group, wide2.cogfrail)
summary(expopsure_lm)
lsmeans(expopsure_lm, ~Group)
contrast(lsmeans(expopsure_lm, ~~Group), "trt.vs.ctrl", adj="none")

  
#------------------------------------------------#
# Differences in falls rate based on compliance  #    ######REVISED SECTION######
#------------------------------------------------#

#All participants
mdl.nb<-glm.nb(TotalFalls~Compliant+scale(Gender,scale=FALSE)+offset(log(TotalExposure/365)),subset(OEP2,exclude==0)) 
print(summary(mdl.nb))Tabel
SE<-sqrt(diag(vcovHC(mdl.nb,"HC1")))
p=2*pnorm(-abs(coef(mdl.nb)/SE))
print(cbind(coef(mdl.nb),SE,p))
est<-cbind(Estimate=coef(mdl.nb),LL=coef(mdl.nb)-1.96*SE,UL=coef(mdl.nb)+1.96*SE)
exp_group<-exp(est)
print("Use these estimates and LL and UL for the 95% CI");exp_group
print("No significant effect of compliance for all participants")      

#CogFrail participants
mdl.nb<-glm.nb(TotalFalls~Compliant+scale(Gender,scale=FALSE)+offset(log(TotalExposure/365)),subset(OEP2,CogFrail=="Yes" & exclude==0)) 
print(summary(mdl.nb))
SE<-sqrt(diag(vcovHC(mdl.nb,"HC1")))
p=2*pnorm(-abs(coef(mdl.nb)/SE))
print(cbind(coef(mdl.nb),SE,p))
est<-cbind(Estimate=coef(mdl.nb),LL=coef(mdl.nb)-1.96*SE,UL=coef(mdl.nb)+1.96*SE)
exp_group<-exp(est)
print("Use these estimates and LL and UL for the 95% CI");exp_group
print("No significant effect of compliance for CogFrail participants")    

#-----------------------------------------------------------------#
# Secondary Effects of intervention                               #     #######REVISED SECTION#######
#-----------------------------------------------------------------#

#########PHYSICAL FUNCTION############
#PPA#
PPA_lm<-lmer(PPA~Group.1*factor(time) + scale(Gender,scale=FALSE) + PPAbaseline + (1|ID), subset(data2, exclude == 0 & CogFrail=="Yes"))
anova(PPA_lm)
lsmeans(PPA_lm, ~Group.1|factor(time))
contrast(lsmeans(PPA_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(PPA_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant differences for CogFrail participants")

#Gait Speed
GS_lm<-lmer(GaitSpeed~Group.1*factor(time) + scale(Gender,scale=FALSE) + GaitSpeedbaseline + (1|ID), subset(data2, exclude == 0 & CogFrail=="Yes"))
anova(GS_lm)
lsmeans(GS_lm, ~Group.1|factor(time))
contrast(lsmeans(GS_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(GS_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant effects for CogFrail participants")

#SPPB
SPPB_lm<-lmer(SPPB~Group.1*factor(time) + scale(Gender,scale=FALSE) + SPPBbaseline + (1|ID), subset(data2, exclude == 0 & CogFrail=="Yes"))
anova(SPPB_lm)
lsmeans(SPPB_lm, ~Group.1|factor(time))
contrast(lsmeans(SPPB_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(SPPB_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant effects")


########COGNITIVE FUNCTION#########
emm_options(opt.digits = FALSE)

#MoCA
MoCA_lm<-lmer(MoCA~Group.1*factor(time) + scale(Gender,scale=FALSE) + MoCAbaseline + (1|ID), subset(data2, exclude==0 & CogFrail=="Yes"))
anova(MoCA_lm)
lsmeans(MoCA_lm, ~Group.1|factor(time))
contrast(lsmeans(MoCA_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(MoCA_lm, ~Group.1|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant differences at 12 months")

#-------------------------------------------#
# Does Compliance Moderate Effects of       #
# Intervention Based on Cog-Frailty Status? #       #######REVISED SECTION#######
#-------------------------------------------#

########FALLS INCIDENT RATE RATIO##############
CogFrail_OEP<-subset(OEP2, CogFrail=="Yes")
table(CogFrail_OEP$Compliant)

#CogFrail
mdl.nb<-glm.nb(TotalFalls~Compliant+scale(Gender,scale=FALSE)+offset(log(TotalExposure/365)),subset(CogFrail_OEP,exclude==0)) 
print(summary(mdl.nb))
SE<-sqrt(diag(vcovHC(mdl.nb,"HC1")))
p=2*pnorm(-abs(coef(mdl.nb)/SE))
print(cbind(coef(mdl.nb),SE,p))
est<-cbind(Estimate=coef(mdl.nb),LL=coef(mdl.nb)-1.96*SE,UL=coef(mdl.nb)+1.96*SE)
exp_group<-exp(est)
print("Use these estimates and LL and UL for the 95% CI");exp_group
print("No significant effect of compliance for CogFrail participants")

#########FALLS PER PERSON-YEAR##########
CogFrail_OEP$falls_ppy<-CogFrail_OEP$TotalFalls/(CogFrail_OEP$TotalExposure/365)

fallsppy_comply<-lm(falls_ppy~Compliant, CogFrail_OEP)
summary(fallsppy_comply)
lsmeans(fallsppy_comply, ~Compliant)
contrast(lsmeans(fallsppy_comply, ~~Compliant), "trt.vs.ctrl", adj="none")
#No significant differences in falls per person year based on compliance

#########PHYSICAL FUNCTION############
#PPA#
PPA_lm<-lmer(PPA~Compliant*factor(time) + scale(Gender,scale=FALSE) + PPAbaseline + (1|ID), subset(OEP3, exclude == 0 & CogFrail=="Yes"))
anova(PPA_lm)
lsmeans(PPA_lm, ~Compliant|factor(time))
contrast(lsmeans(PPA_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(PPA_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant effects")

#Gait Speed
GS_lm<-lmer(GaitSpeed~Compliant*factor(time) + scale(Gender,scale=FALSE) + GaitSpeedbaseline + 1 + (1|ID), subset(OEP3, exclude == 0 & CogFrail=="Yes"))
anova(GS_lm)
lsmeans(GS_lm, ~Compliant|factor(time))
contrast(lsmeans(GS_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(GS_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant effects")

#SPPB
GS_lm<-lmer(SPPB~Compliant*factor(time) + scale(Gender,scale=FALSE) + SPPBbaseline + 1 + (1|ID), subset(OEP3, exclude == 0 & CogFrail =="Yes"))
anova(GS_lm)
lsmeans(GS_lm, ~Compliant|factor(time))
contrast(lsmeans(GS_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(GS_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none"))
print("Higher compliance among CogFrail participants was associated with better SPPB score at 12 months")


########COGNITIVE FUNCTION#########

#MoCA
MoCA_lm<-lmer(MoCA~Compliant*factor(time) + scale(Gender,scale=FALSE) + MoCAbaseline + 1 + (1|ID), subset(OEP3, exclude==0 & CogFrail =="Yes"))
anova(MoCA_lm)
lsmeans(MoCA_lm, ~Compliant|factor(time))
contrast(lsmeans(MoCA_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none")
confint(contrast(lsmeans(MoCA_lm, ~Compliant|factor(time)), "trt.vs.ctrl", adj="none"))
print("No significant differences")


