#load libraries
library(haven)
library(likert)
library(dplyr)

#set working directory
setwd("~/UM/Biostatistics/STATCOM/raw_data")

#read in data files
#user surveys- sent out to students taking classes at SPH
s_15 = read_sav("Website User Survey_May 22, 2020_15.01.sav")
ph200 = read_sav("Website User Survey - Public Health 200_May 22, 2020_15.02.sav")
ph514 = read_sav("Website User Survey - Public Health 514_May 22, 2020_15.03.sav")
ph516 = read_sav("Website User Survey - Public Health 516_May 22, 2020_15.04.sav")

#keep relevant variables
ph200 = select(ph200, Progress, Finished, ResponseId, Affiliation, School, Q1_1, Q2, Q3, Q4, Q6, Q7, Q8, Q9, Q10, Q11, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q13, Q14, Q15, Q16, Q17, Q17_1_TEXT, Q17_2_TEXT, Q17_3_TEXT, Q17_4_TEXT)
ph514 = select(ph514, Progress, Finished, ResponseId, Affiliation, Q1_1, Q2, Q3, Q4, Q6, Q7, Q8, Q9, Q10, Q11, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q13, Q14, Q15, Q16, Q17, Q17_1_TEXT, Q17_2_TEXT, Q17_3_TEXT, Q17_4_TEXT)
ph516 = select(ph516, Progress, Finished, ResponseId, Affiliation, Q1_1, Q2, Q3, Q4, Q6, Q7, Q8, Q9, Q10, Q11, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q13, Q14, Q15, Q16, Q17, Q17_1_TEXT, Q17_2_TEXT, Q17_3_TEXT, Q17_4_TEXT)
s_15 = select(s_15, Q2, Q13, Q14, Q15)

#create identifier
ph200$id = "ph200"
ph514$id = "ph514"
ph516$id = "ph516"
s_15$id = "s_15"

#create school variable for the datasets which don't have them
ph514$School = NA_real_
ph516$School = NA_real_
s_15$School = NA_real_

#create the missing variables in s_15 dataset
s_15$Progress = NA_real_
s_15$Finished = NA_real_
s_15$ResponseId = NA
s_15$Affiliation = 3
s_15$Q1_1 = NA_real_
s_15$Q3 = NA
s_15$Q4 = NA
s_15$Q6 = NA
s_15$Q7 = NA
s_15$Q8 = NA
s_15$Q9 = NA
s_15$Q10 = NA
s_15$Q11 = NA
s_15$Q12_1 = NA_real_
s_15$Q12_2 = NA_real_
s_15$Q12_3 = NA_real_
s_15$Q12_4 = NA_real_
s_15$Q12_5 = NA_real_
s_15$Q12_6 = NA_real_
s_15$Q12_7 = NA_real_
s_15$Q16 = NA
s_15$Q17 = NA_real_
s_15$Q17_1_TEXT = NA
s_15$Q17_2_TEXT = NA
s_15$Q17_3_TEXT = NA
s_15$Q17_4_TEXT = NA

#stack the datasets
classes = rbind(ph514, ph516, ph200, s_15)

#change affiliation to factor
classes$aff[classes$Affiliation == 1] = "Undergraduate"
classes$aff[classes$Affiliation == 2] = "Master's Student"
classes$aff[classes$Affiliation == 3] = "Unknown"

#Extract Data for Likert
sub=classes[,c("aff","Q12_1","Q12_2","Q12_3","Q12_4","Q12_5","Q12_6","Q12_7")]
num_data=classes[,c("Q12_1","Q12_2","Q12_3","Q12_4","Q12_5","Q12_6","Q12_7")]
names(num_data)=c("Degree Requirements","Research Areas/Areas of Study",
                  "Department-Specific Information","Career Outcomes","Faculty Information","Student Life",
                  "Financial Aid (Scholarships and Funding)")
affiliation=sub[complete.cases(sub),]$aff
num_data=as.data.frame(num_data[complete.cases(sub),])

#Create Factor Variables
for(i in 1:7) {
  num_data[,i]=factor(num_data[,i])
  levels(num_data[,i])=c("1","2","3","4","5","6","7")
}

#Overall Likert Plot
like_open=likert(num_data)
like_open_plot = (plot(like_open)
+ggtitle("When gathering information on schools, \nwhich information is the most important to you?")
+ guides(fill=guide_legend("Rank")))

#Students replied that degree requirements were the most important to them, followed by career outcomes, research areas/areas of study, financial aid, and department specific information. Student life and faculty information often ranked below a 4.

#Likert Plot By Affiliation
like_open_aff=likert(num_data,grouping=affiliation)
like_open_affiliation = (plot(like_open_aff)
  +ggtitle("When gathering information on schools, \nwhich information is the most important to you?")
  + guides(fill=guide_legend("Rank")))

#This analysis changes when we look at the student's affiliation. 
#Career outcomes were more important to Master's students than undergraduates, as were degree requirements and department-specific information.
#Faculty information was about equally important to undergraduates and Master's students.
#Financial aid information was more important to undergraduates than Master's students, as were research areas/areas of study and student life.

#Degree requirements, career outcomes, and department-specific information were the most important factors to Master's students.
#Student life and faculty information were the least important to Master's students.

#Research areas/areas of study, financial aid, degree requirements were the most important factors to undergraduates.
#Student life, faculty information, and department-specific information were the least important factors to undergraduates.

saveRDS(list(like_open_plot, like_open_affiliation), file="../results/Class_12.rds")
