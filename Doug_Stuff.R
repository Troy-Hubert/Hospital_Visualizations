### SETUP ###

setwd("~/Desktop/Coding/R working directory/Hospital_Visualizations")
library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)

Mothership<-read_sav("~/Desktop/Coding/data/Mothership_DV.sav")
view(Mothership)
Mothership<-select(Mothership,Intake_1,Days_complete_1,ID1:Sexuality_1,MDD_C:Day40_CUXOS)

############################### Demographics ####################################

### Bar Chart for Discharge Status ###

ggplot(data = subset(Mothership, !is.na(DC_status_1)), mapping = aes(x = DC_status_1))+
  geom_bar(color = "black",fill="darkgreen")+
  ggtitle("Treatment Discharge Status") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_discrete(name = "Discharge Status")+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  scale_y_continuous(name = "Frequency",limits=c(0,5000))

### Bar Chart for Education Status ###

ggplot(data = subset(Mothership, !is.na(Education_1)),mapping = aes(x = Education_1))+
  geom_bar(color = "black",fill="darkred")+
  ggtitle("Education Level") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_discrete(name = "Education Level")+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  scale_y_continuous(name = "Frequency",limits=c(0,2500))

### Pie Chart For Education Status ###

#Create Table and Data Frame
Education<-table(Mothership$Education_1)
Education_df<-as.data.frame(Education)
Education_df

str(Mothership$Education_1)

#Rename Columns
colnames(Education_df)[1]="Category"
colnames(Education_df)[2]="Frequency"


Education_df$Category=recode(Education_df$Category,
                             "0"="<Grade 6", 
                             "1"="Grades 7-12",
                             "2"="HS Diploma", 
                             "3"="GED",
                             "4"="Some College",
                             "5"="Associates Degree",
                             "6"="Bachelors Degree",
                             "7"="Some Grad School",
                             "8"="Grad Degree")

ggplot(data = Education_df, mapping = aes(x="", y=Frequency, fill=Category))+
  geom_bar(stat="Identity",width=1,color="white")+
  coord_polar("y",start=0)+
  theme_void()

################## Setup for Treatment Progress Charts #########################

#Make Variables Factors and Recode
Mothership <- mutate(Mothership, 
                     DC_status_1 = as.factor(DC_status_1),
                     DC_status_1 = recode(DC_status_1,
                                          "1"="Complete",
                                          "2"="Nearly Complete",
                                          "3"="Insurance",
                                          "4"="Nonatteneance",
                                          "5"="Inappropriate",
                                          "6"="Inpatient",
                                          "7"="Withdrew",
                                          "8"="Dissatisfaction",
                                          "9"="Other"),
                     Education_1 = as.factor(Education_1),
                     Education_1 = recode(Education_1,
                                          "1"="<Grade 12",
                                          "2"="HS Diploma",
                                          "3"="GED",
                                          "4"="Some College",
                                          "5"="Associates Degree",
                                          "6"="Bachelors Degree",
                                          "7"="Some Grad School",
                                          "8"="Grad Degree"),
                     Gender_1 = as.factor(Gender_1),
                     Gender_1 = recode(Gender_1,
                                       "0"="Female",
                                       "1"="Male",
                                       "2"="Non-Binary",
                                       "3"="Other",
                                       "4"="Unknown"),
                     Relationship_1 = as.factor(Relationship_1),
                     Relationship_1 = recode(Relationship_1,
                                             "0"="Married",
                                             "1"="Cohabitating",
                                             "2"="Widowed",
                                             "3"="Separated",
                                             "4"="Divorced",
                                             "5"="Never Married"),
                     Sexuality_1 = as.factor(Sexuality_1),
                     Sexuality_1 = recode(Sexuality_1,
                                          "1"="Straight",
                                          "2"="Gay",
                                          "3"="Bisexual",
                                          "4"="Other"),
                     IPT_track_1 = as.factor(IPT_track_1),
                     IPT_track_1 = recode(IPT_track_1,
                                          "0"="General",
                                          "1"="Trauma",
                                          "2"="Young Adult",
                                          "3"="BPD"),
                     Race_1 = as.factor(Race_1),
                     Race_1 = recode(Race_1,
                                     "0"="White",
                                     "1"="Black",
                                     "2"="Hispanic",
                                     "3"="Asian",
                                     "4"="Portugese",
                                     "5"="Other"))

#Extract Year From Intake Date
#Convert to Date
library("lubridate")

Mothership$Intake_1<-mdy(Mothership$Intake_1,locale = "en_US.UTF-8")
Mothership$TxYear<-as.numeric(format(Mothership$Intake_1,"%Y"))

Mothership<-mutate(Mothership,
                   TxYear = as.factor(TxYear),
                   TxYear = recode(TxYear,
                                   "1582"="2014",
                                   "2021"="2020",
                                   "2022"="2020"))

################## Depression Treatment Progress Charts #########################

#Wide to Long Data
#rm holds the values of the new column
#CUDOS is the new column to use 
library(tidyr)
Mothership_Long <- gather(Mothership,rm,CUDOS,Day1_CUDOS:Day40_CUDOS,factor_key = "T")
Mothership_Long <- Mothership_Long[order(Mothership_Long$ID1),]

#Create a Time Variable
Mothership_Long$TxDay <- NA
Mothership_Long$TxDay[Mothership_Long$rm=="Day1_CUDOS"]<-1
Mothership_Long$TxDay[Mothership_Long$rm=="Day2_CUDOS"]<-2
Mothership_Long$TxDay[Mothership_Long$rm=="Day3_CUDOS"]<-3
Mothership_Long$TxDay[Mothership_Long$rm=="Day4_CUDOS"]<-4
Mothership_Long$TxDay[Mothership_Long$rm=="Day5_CUDOS"]<-5
Mothership_Long$TxDay[Mothership_Long$rm=="Day6_CUDOS"]<-6
Mothership_Long$TxDay[Mothership_Long$rm=="Day7_CUDOS"]<-7
Mothership_Long$TxDay[Mothership_Long$rm=="Day8_CUDOS"]<-8
Mothership_Long$TxDay[Mothership_Long$rm=="Day9_CUDOS"]<-9
Mothership_Long$TxDay[Mothership_Long$rm=="Day10_CUDOS"]<-10

#Create a New Data Set with an Average Depression Variable
Mothership_Mean_CUDOS<-Mothership_Long %>%
  group_by(TxDay) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Plot the Depression Mean for Full Sample
ggplot(data=Mothership_Mean_CUDOS,aes(x=TxDay,y=mean_CUDOS))+
  geom_line(size=1,color="#0065A3")+
  geom_point(color="#0065A3")+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  ggtitle("Mean Depression Severity During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,35,by=5))+
  theme(panel.background=NULL)

############ Depression Treatment Progress by Characteristics ##################

#RACE
#Create a New Data Set with an Average Depression Variable and Race
Mothership_Mean_CUDOS_Race<-Mothership_Long %>%
  group_by(TxDay,Race_1) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Create the Depression Line Graph by Race
ggplot(data=subset(Mothership_Mean_CUDOS_Race,!is.na(Race_1)),
       aes(x=TxDay,y=mean_CUDOS,group=Race_1,color=Race_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Race During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(18,38,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#A0AFA1",
                              "#0098DB","#0065A3","darkgreen"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#GENDER IDENTITY
#Create a New Data Set with an Average Depression Variable and Gender
Mothership_Mean_CUDOS_Gender<-Mothership_Long %>%
  group_by(TxDay,Gender_1) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

Mothership_Mean_CUDOS_Gender

#Create the Depression Line Graph by Gender
ggplot(data=subset(Mothership_Mean_CUDOS_Gender,!is.na(Gender_1)),
       aes(x=TxDay,y=mean_CUDOS,group=Gender_1,color=Gender_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Gender During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,35,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#A0AFA1",
                              "#0098DB","#0065A3"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#TREATMENT TRACK
#Create a New Data Set with an Average Depression Variable and Track
Mothership_Mean_CUDOS_Track<-Mothership_Long %>%
  group_by(TxDay,IPT_track_1) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Create the Depression Line Graph by Treatment Track
ggplot(data=subset(Mothership_Mean_CUDOS_Track,!is.na(IPT_track_1)),
       aes(x=TxDay,y=mean_CUDOS,group=IPT_track_1,color=IPT_track_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Treatment Track During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,40,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#0065A3"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#EDUCATION
#Create a New Data Set with an Average Depression Variable and Education
Mothership_Mean_CUDOS_Ed<-Mothership_Long %>%
  group_by(TxDay,Education_1) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Create the Depression Line Graph by Education
ggplot(data=subset(Mothership_Mean_CUDOS_Ed,!is.na(Education_1)),
       aes(x=TxDay,y=mean_CUDOS,group=Education_1,color=Education_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Education Level During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,38,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#A0AFA1",
                              "#0098DB","#0065A3","darkgreen",
                              "#91A9F1","#7F8559"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#TREATMENT YEAR
#Create a New Data Set with an Average Depression Variable and Year
Mothership_Mean_CUDOS_Year<-Mothership_Long %>%
  group_by(TxDay,TxYear) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Create the Depression Line Graph by Year
ggplot(data=subset(Mothership_Mean_CUDOS_Year,!is.na(TxYear)),
       aes(x=TxDay,y=mean_CUDOS,group=TxYear,color=TxYear))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Treatment Year During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,36,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#A0AFA1",
                              "#0098DB","#0065A3","darkgreen",
                              "#91A9F1","#7F8559","#4B5D8E"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#SEXUALITY
#Create a New Data Set with an Average Depression Variable and Sexuality
Mothership_Mean_CUDOS_Sex<-Mothership_Long %>%
  group_by(TxDay,Sexuality_1) %>%
  summarise(mean_CUDOS=mean(CUDOS,na.rm=TRUE))

#Create the Depression Line Graph by Sexuality
ggplot(data=subset(Mothership_Mean_CUDOS_Sex,!is.na(Sexuality_1)),
       aes(x=TxDay,y=mean_CUDOS,group=Sexuality_1,color=Sexuality_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Depression Severity by Sexuality During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Depression Severity",limits=c(20,36,by=5))+
  scale_color_manual(values=c("#209845","#3D4A3E","#0065A3","darkgreen"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#################### Anxiety Treatment Progress Charts #########################

#Wide to Long Data for Anxiety
library(tidyr)
Mothership_Long_Anx <- gather(Mothership,rm,CUXOS,Day1_CUXOS:Day40_CUXOS,factor_key = "T")
Mothership_Long_Anx <- Mothership_Long_Anx[order(Mothership_Long_Anx$ID1),]

#Create a Time Variable
Mothership_Long_Anx$TxDay <- NA
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day1_CUXOS"]<-1
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day2_CUXOS"]<-2
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day3_CUXOS"]<-3
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day4_CUXOS"]<-4
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day5_CUXOS"]<-5
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day6_CUXOS"]<-6
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day7_CUXOS"]<-7
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day8_CUXOS"]<-8
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day9_CUXOS"]<-9
Mothership_Long_Anx$TxDay[Mothership_Long_Anx$rm=="Day10_CUXOS"]<-10

#Create a New Data Set with an Average Anxiety Variable
Mothership_Mean_CUXOS<-Mothership_Long_Anx %>%
  group_by(TxDay) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Plot the Depression Mean for Full Sample
ggplot(data=Mothership_Mean_CUXOS,aes(x=TxDay,y=mean_CUXOS))+
  geom_line(size=1,color="#DA2E2E")+
  geom_point(color="#DA2E2E")+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  ggtitle("Mean Anxiety Severity During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(20,40,by=5))+
  theme(panel.background=NULL)

############ Anxiety Treatment Progress by Characteristics ##################

#RACE
#Create a New Data Set with an Average Anxiety Variable and Race
Mothership_Mean_CUXOS_Race<-Mothership_Long_Anx %>%
  group_by(TxDay,Race_1) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Anxiety Line Graph by Race
ggplot(data=subset(Mothership_Mean_CUXOS_Race,!is.na(Race_1)),
       aes(x=TxDay,y=mean_CUXOS,group=Race_1,color=Race_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Race During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(15,45,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E",
                              "gold3","#3E8300",
                              "#77B81C","darkorange2"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#GENDER IDENTITY
#Create a New Data Set with an Average Anxiety Variable and Gender
Mothership_Mean_CUXOS_Gender<-Mothership_Long_Anx %>%
  group_by(TxDay,Gender_1) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Anxiety Line Graph by Gender
ggplot(data=subset(Mothership_Mean_CUXOS_Gender,!is.na(Gender_1)),
       aes(x=TxDay,y=mean_CUXOS,group=Gender_1,color=Gender_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Gender During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(15,41,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E",
                              "gold3",
                              "#77B81C","darkorange2"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#TREATMENT TRACK
#Create a New Data Set with an Average Anxiety Variable and Track
Mothership_Mean_CUXOS_Track<-Mothership_Long_Anx %>%
  group_by(TxDay,IPT_track_1) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Anxiety Line Graph by Treatment Track
ggplot(data=subset(Mothership_Mean_CUXOS_Track,!is.na(IPT_track_1)),
       aes(x=TxDay,y=mean_CUXOS,group=IPT_track_1,color=IPT_track_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Treatment Track During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(20,45,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E","gold3","#77B81C"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#EDUCATION
#Create a New Data Set with an Average Anxiety Variable and Education
Mothership_Mean_CUXOS_Ed<-Mothership_Long_Anx %>%
  group_by(TxDay,Education_1) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Anxiety Line Graph by Education
ggplot(data=subset(Mothership_Mean_CUXOS_Ed,!is.na(Education_1)),
       aes(x=TxDay,y=mean_CUXOS,group=Education_1,color=Education_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Education Level During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(20,45,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E",
                              "gold3","#3E8300",
                              "#77B81C","darkorange2",
                              "#798897","#FD85AE"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#TREATMENT YEAR
#Create a New Data Set with an Average Anxiety Variable and Year
Mothership_Mean_CUXOS_Year<-Mothership_Long_Anx %>%
  group_by(TxDay,TxYear) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Anxiety Line Graph by Year
ggplot(data=subset(Mothership_Mean_CUXOS_Year,!is.na(TxYear)),
       aes(x=TxDay,y=mean_CUXOS,group=TxYear,color=TxYear))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Treatment Year During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(20,45,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E",
                              "gold3","#3E8300",
                              "#77B81C","darkorange2",
                              "#798897","#FD85AE","firebrick4"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

#SEXUALITY
#Create a New Data Set with an Average Anxiety Variable and Sexuality
Mothership_Mean_CUXOS_Sex<-Mothership_Long_Anx %>%
  group_by(TxDay,Sexuality_1) %>%
  summarise(mean_CUXOS=mean(CUXOS,na.rm=TRUE))

#Create the Depression Line Graph by Sexuality
ggplot(data=subset(Mothership_Mean_CUXOS_Sex,!is.na(Sexuality_1)),
       aes(x=TxDay,y=mean_CUXOS,group=Sexuality_1,color=Sexuality_1))+
  geom_line(size=1)+
  geom_point()+
  ggtitle("Anxiety Severity by Sexuality During Partial Hospitalization")+
  scale_x_discrete(name="Treatment Day",limits=c(1,2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(name="Anxiety Severity",limits=c(20,36,by=5))+
  scale_color_manual(values=c("#DA2E2E","#57423E","gold3","#77B81C"))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title=element_blank())

############################ RDQ Pre-Post ######################################

#Read in a New Data Set
Mothership_RDQ<-read.csv('Mothership_DV.csv',sep=",",quote = "",header=TRUE)
Mothership_RDQ<-select(Mothership,ID1:Sexuality_1,MDD_C:Day40_CUXOS)
Mothership_RDQ<-data.frame(sapply(Mothership_RDQ,FUN=as.numeric))

#Make This Data Set Long Data
library(tidyr)
Mothership_Long_RDQ <- gather(Mothership_RDQ,rm,RDQ,rdqPRE_coping:rdqPOST_sym,
                              factor_key = "T")
Mothership_Long_RDQ <- Mothership_Long_RDQ[order(Mothership_Long_RDQ$ID1),]

#Create a Time Variable
Mothership_Long_RDQ$PrePost <- NA
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPRE_coping"]<-"Pre"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPRE_pmh"]<-"Pre"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPRE_func"]<-"Pre"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPRE_wbs"]<-"Pre"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPRE_sym"]<-"Pre"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPOST_coping"]<-"Post"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPOST_pmh"]<-"Post"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPOST_func"]<-"Post"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPOST_wbs"]<-"Post"
Mothership_Long_RDQ$PrePost[Mothership_Long_RDQ$rm=="rdqPOST_sym"]<-"Post"

#Create a New Data Set with an Average Depression Variable and Race
Mothership_Mean_RDQ<-Mothership_Long_RDQ %>%
  group_by(rm,PrePost) %>%
  summarise(RDQ=mean(RDQ,na.rm=TRUE))

#Add a Paired Variable to the New Data Set
Mothership_Mean_RDQ<-mutate(Mothership_Mean_RDQ,
                            paired=case_when(rm=="rdqPRE_coping"~"1",
                                             rm=="rdqPOST_coping"~"1",
                                             rm=="rdqPRE_pmh"~"2",
                                             rm=="rdqPOST_pmh"~"2",
                                             rm=="rdqPRE_func"~"3",
                                             rm=="rdqPOST_func"~"3",
                                             rm=="rdqPRE_wbs"~"4",
                                             rm=="rdqPOST_wbs"~"4",
                                             rm=="rdqPRE_sym"~"5",
                                             rm=="rdqPOST_sym"~"5"))

#Pre Post RDQ Plot
ggplot(data=Mothership_Mean_RDQ,
       aes(x=factor(PrePost,level=c("Pre","Post")),y=RDQ,
           group=rm,color=paired,!is.na(PrePost)))+
  geom_point(size=2.5)+
  geom_line(group=Mothership_Mean_RDQ$paired,size=1.5)+
  scale_x_discrete(name="Time Point")+
  scale_y_continuous(name="RDQ Subscore")+
  ggtitle("Mean Pre and Post Scores of the Remission from Depression Questionnaire")+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.5,linetype = 2))+
  theme(panel.background=NULL)+
  theme(legend.title = element_blank())+
  scale_color_manual(values=c("1"="#8956BB","2"="#4C4452","3"="#B1A8B9",
                              "4"="#00BCA3","5"="#008570"),
                     labels=c("Coping Skills","Positive Mental Health",
                              "Functioning","Well-Being","Symptoms"))


