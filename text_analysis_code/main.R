# run text analysis
library(ggplot2)
library(dplyr)


setwd('~/UM/Biostatistics/STATCOM/processed_data')

duderstadt = read.csv("Duderstadt_open_cleaned.csv",stringsAsFactors=FALSE)
sph = read.csv("SPH_open_cleaned.csv",stringsAsFactors=FALSE)

open_survey =  rbind(duderstadt, sph)

source("../summary_func.R")
source("../Render_Element_Gen.R")

# Open Survey

## Q1 from the open survey
open_Q1_result = full_analysis(open_survey$Q1, open_survey$Affiliation, topicnum = 3)

open_Q1_stmsum = open_Q1_result[[2]]

topics_Q1_open = c("Class Size", "Undergraduate", "Tediousness")
examples_Q1_open = c(open_Q1_stmsum$representative$Topic.1[3],
                     open_Q1_stmsum$representative$Topic.2[1],
                     open_Q1_stmsum$representative$Topic.3[1])

elements_open_1 = elements(open_Q1_result, topics_Q1_open, examples_Q1_open)

saveRDS(elements_open_1, file="../results/Open_Q1.rds")


## Q2 from the open survey
open_Q2_result = full_analysis(open_survey$Q2, open_survey$Affiliation, topicnum = 6)

open_Q2_stmsum = open_Q2_result[[2]]

topics_Q2_open = c("Financial Aid", "Faculty Research", "Page Layout", "Degree Info",
                   "Employment", "Course Offering")

examples_Q2_open = c(open_Q2_stmsum$representative$Topic.1[1],
                     open_Q2_stmsum$representative$Topic.2[5],
                     open_Q2_stmsum$representative$Topic.3[1],
                     open_Q2_stmsum$representative$Topic.4[2],
                     open_Q2_stmsum$representative$Topic.5[2],
                     open_Q2_stmsum$representative$Topic.6[7])


elements_open_2 = elements(open_Q2_result, topics_Q2_open, examples_Q2_open)

saveRDS(elements_open_2, file="../results/Open_Q2.rds")


##Q3 from open survey

open_Q3_result = full_analysis(open_survey$Q3, open_survey$Affiliation, topicnum = 6)

open_Q3_stmsum = open_Q3_result[[2]]

topics_Q3_open = c("Financial Aid", "Student Testimonial", "Page Organization",
                   "Program Experience", "Uniqueness", "Research & Intern")

examples_Q3_open = c(gsub("[\r\n]", "", open_Q3_stmsum$representative$Topic.1[3]),
                     open_Q3_stmsum$representative$Topic.2[2],
                     open_Q3_stmsum$representative$Topic.3[5],
                     open_Q3_stmsum$representative$Topic.4[2],
                     open_Q3_stmsum$representative$Topic.5[1],
                     open_Q3_stmsum$representative$Topic.6[1])

elements_open_3 = elements(open_Q3_result, topics_Q3_open, examples_Q3_open)

saveRDS(elements_open_3, file="../results/Open_Q3.rds")


##Q5 from Open Survey

open_Q5_result = full_analysis(open_survey$Q5, open_survey$Affiliation, topicnum = 3)

open_Q5_stmsum = open_Q5_result[[2]]

topics_Q5_open = c("Rankings", "Social Media", "Networking")

examples_Q5_open = c(open_Q5_stmsum$representative$Topic.1[4],
                     open_Q5_stmsum$representative$Topic.2[1],
                     open_Q5_stmsum$representative$Topic.3[3])

elements_open_5 = elements(open_Q5_result, topics_Q5_open, examples_Q5_open)

saveRDS(elements_open_5, file="../results/Open_Q5.rds")


## Q6 from Open Survey

open_Q6_result = full_analysis(open_survey$Q6, open_survey$Affiliation, topicnum = 6)

open_Q6_stmsum = open_Q6_result[[2]]

View(open_Q6_stmsum$topicwords)

topics_Q6_open = c("Program Requirements", "Networking", "Degree Program",
                   "Financial Aid", "Class Size", "Research")

examples_Q6_open = c(open_Q6_stmsum$representative$Topic.1[1],
                     open_Q6_stmsum$representative$Topic.2[1],
                     open_Q6_stmsum$representative$Topic.3[1],
                     open_Q6_stmsum$representative$Topic.4[2],
                     open_Q6_stmsum$representative$Topic.5[1],
                     open_Q6_stmsum$representative$Topic.6[2])

elements_open_6 = elements(open_Q6_result, topics_Q6_open, examples_Q6_open)

saveRDS(elements_open_6, file="../results/Open_Q6.rds")



# In class Surveys

PH200 = read.csv("PH200_cleaned.csv",stringsAsFactors=FALSE)
PH200 = PH200[, -c(2)] %>% subset(select = 1:22) %>%na.omit()
PH514 = read.csv("PH514_cleaned.csv",stringsAsFactors=FALSE)
PH514 = PH514[,c(1:22)] %>% na.omit()
PH516 = read.csv("PH516_cleaned.csv",stringsAsFactors=FALSE)
PH516 = PH516[,c(1:22)] %>% na.omit()

class_answers = rbind(PH200, PH514, PH516)

## Q2 from class survey

class_Q2_result = full_analysis(class_answers$Q2, class_answers$Affiliation,
                                topicnum = 4)

class_Q2_stmsum = class_Q2_result[[2]]

topics_Q2_class = c("Aesthetics", "Accessibility", "Aesthetics(Redundant)",
                   "Usefulness")


examples_Q2_class = c(class_Q2_stmsum$representative$Topic.1[2],
                      class_Q2_stmsum$representative$Topic.2[1],
                      class_Q2_stmsum$representative$Topic.3[2],
                      gsub("[\r\n]", "", class_Q2_stmsum$representative$Topic.4[1]))

elements_class_2 = elements(class_Q2_result, topics_Q2_class, examples_Q2_class)

saveRDS(elements_class_2, file="../results/Class_Q2.rds")


## Q3 from class survey

class_Q3_result = full_analysis(class_answers$Q3, class_answers$Affiliation,
                                topicnum = 3)

class_Q3_stmsum = class_Q3_result[[2]]

topics_Q3_class = c("Event Gallery", "Versatility", "Basic Info")

examples_Q3_class = c(class_Q3_stmsum$representative$Topic.1[1],
                      class_Q3_stmsum$representative$Topic.2[1],
                      class_Q3_stmsum$representative$Topic.3[2])

elements_class_3 = elements(class_Q3_result, topics_Q3_class, examples_Q3_class)

saveRDS(elements_class_3, file="../results/Class_Q3.rds")

## Q4 from class survey

class_Q4_result = full_analysis(class_answers$Q4, class_answers$Affiliation,
                                topicnum = 3)

class_Q4_stmsum = class_Q4_result[[2]]

topics_Q4_class = c("Comparison with web", "Layout", "No Comment")

examples_Q4_class = c(class_Q4_stmsum$representative$Topic.1[1],
                      class_Q4_stmsum$representative$Topic.2[1],
                      class_Q4_stmsum$representative$Topic.3[2])

elements_class_4 = elements(class_Q4_result, topics_Q4_class, examples_Q4_class)

saveRDS(elements_class_4, file="../results/Class_Q4.rds")



## Q6 from class survey

class_Q6_result = full_analysis(class_answers$Q6, class_answers$Affiliation,
                                topicnum = 3)

class_Q6_stmsum = class_Q6_result[[2]]

topics_Q6_class = c("Application Procedure", "Why Public Health", "Building & Faculty Info") 

examples_Q6_class = c(class_Q6_stmsum$representative$Topic.1[1],
                      gsub("[\r\n]", "", class_Q6_stmsum$representative$Topic.2[1]),
                      class_Q6_stmsum$representative$Topic.3[1])

elements_class_6 = elements(class_Q6_result, topics_Q6_class, examples_Q6_class)

saveRDS(elements_class_6, file="../results/Class_Q6.rds")


## Q7 from class survey

class_Q7_result = full_analysis(class_answers$Q7, class_answers$Affiliation,
                                 topicnum = 3)
elements_Q7_class = elements_simple(class_Q7_result,class_answers$Q7)
saveRDS(elements_Q7_class, file="../results/Class_Q7.rds")


## Q8 from class survey

class_Q8_result = full_analysis(class_answers$Q8, class_answers$Affiliation,
                                topicnum = 3)
elements_Q8_class = elements_simple(class_Q8_result,class_answers$Q8)
saveRDS(elements_Q8_class, file="../results/Class_Q8.rds")


## Q9 from class survey

class_Q9_result = full_analysis(class_answers$Q9, class_answers$Affiliation,
                                topicnum = 3)
elements_Q9_class = elements_simple(class_Q9_result,class_answers$Q9)
saveRDS(elements_Q9_class, file="../results/Class_Q9.rds")

## Q10 from class survey

class_Q10_result = full_analysis(class_answers$Q10, class_answers$Affiliation,
                                topicnum = 4)

class_Q10_stmsum = class_Q10_result[[2]]

topics_Q10_class = c("Nothing", "Popular Links and Events", "Layout", "Font")

examples_Q10_class = c(class_Q10_stmsum$representative$Topic.1[1],
                       class_Q10_stmsum$representative$Topic.2[1],
                       class_Q10_stmsum$representative$Topic.3[1],
                       class_Q10_stmsum$representative$Topic.4[1])

elements_class_10 = elements(class_Q10_result, topics_Q10_class, examples_Q10_class)

saveRDS(elements_class_10, file="../results/Class_Q10.rds")

## Q11 from class survey

class_Q11_result = full_analysis(class_answers$Q11, class_answers$Affiliation,
                                 topicnum = 4)

class_Q11_stmsum = class_Q11_result[[2]]

topics_Q11_class = c("Atmosphere", "Living Cost", "Housing", "Traffic")


examples_Q11_class = c(class_Q11_stmsum$representative$Topic.1[1],
                       class_Q11_stmsum$representative$Topic.2[4],
                       class_Q11_stmsum$representative$Topic.3[1],
                       gsub("[\r\n]", "", class_Q11_stmsum$representative$Topic.4[3]))

elements_class_11 = elements(class_Q11_result, topics_Q11_class, examples_Q11_class)

saveRDS(elements_class_11, file="../results/Class_Q11.rds")


## Q16 from class survey

class_Q16_result = full_analysis(class_answers$Q16, class_answers$Affiliation,
                                 topicnum = 4)

class_Q16_stmsum = class_Q16_result[[2]]

topics_Q16_class = c("Research Interest", "Accessibility", "Teaching", "Not Much")

examples_Q16_class = c(class_Q16_stmsum$representative$Topic.1[3],
                       class_Q16_stmsum$representative$Topic.2[1],
                       class_Q16_stmsum$representative$Topic.3[1],
                       class_Q16_stmsum$representative$Topic.4[1])

elements_class_16 = elements(class_Q16_result, topics_Q16_class, examples_Q16_class)

saveRDS(elements_class_16, file="../results/Class_Q16.rds")


