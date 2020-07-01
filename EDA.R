setwd("~/UM/Biostatistics/STATCOM")
library(boxr)
library(tidyverse); library(haven) 


# processing duderstadt questions
data1  = read_sav("Website Open Survey - Duderstadt_May 22, 2020_14.52.sav") %>%
  na_if(-1) %>% as_factor

meaningful1 = data1[,18:32]

duderstadt_questions = c()
for (i in 1:ncol(meaningful1)){
  duderstadt_questions = c(duderstadt_questions, 
                          attr(meaningful1[[i]], "label"))
}

fileconn <- file("Duderstadt_open_question.txt")
writeLines(duderstadt_questions, fileconn)
close(fileconn)

write.csv(meaningful1, "Duderstadt_open_cleaned.csv", row.names=FALSE)


# processing open questions in SPH
data2 = read_sav("Website Open Survey_May 22, 2020_14.57.sav") %>%
  na_if(-1) %>% as_factor

meaningful2 = data2[,18:32]

SPH_open_questions = c()
for (i in 1:ncol(meaningful2)){
  SPH_open_questions = c(SPH_open_questions, 
                           attr(meaningful2[[i]], "label"))
}

fileconn <- file("SPH_open_question.txt")
writeLines(SPH_open_questions, fileconn)
close(fileconn)

write.csv(meaningful2, "SPH_open_cleaned.csv", row.names=FALSE)

# processing questions from SPH website survey
data3 = read_sav("Website User Survey_May 22, 2020_15.01.sav") %>%
  na_if(-1) %>% as_factor

meaningful3 = data3[, 2:5]
website_questions = c()
for (i in 1:ncol(meaningful3)){
  website_questions = c(website_questions, 
                           attr(meaningful3[[i]], "label"))
}

fileconn <- file("website_question.txt")
writeLines(website_questions, fileconn)
close(fileconn)

write.csv(meaningful3, "SPH_website_cleaned.csv", row.names=FALSE)

# PH200
data4 = read_sav("Website User Survey - Public Health 200_May 22, 2020_15.02.sav") %>%
  na_if(-1) %>% as_factor

meaningful4 = data4[,c(18,19,22:47)]

PH200 = c()
for (i in 1:ncol(meaningful4)){
  PH200 = c(PH200, 
                        attr(meaningful4[[i]], "label"))
}

fileconn <- file("PH200_question.txt")
writeLines(PH200, fileconn)
close(fileconn)
write.csv(meaningful4, "PH200_cleaned.csv", row.names=FALSE)


# PH514
data5 = read_sav("Website User Survey - Public Health 514_May 22, 2020_15.03.sav") %>%
  na_if(-1) %>% as_factor

meaningful5 = data5[,c(18,21:46)]
PH514 = c()

for (i in 1:ncol(meaningful5)){
  PH514 = c(PH514, attr(meaningful5[[i]], "label"))
}

fileconn <- file("PH514_question.txt")
writeLines(PH514, fileconn)
close(fileconn)


write.csv(meaningful5, "PH514_cleaned.csv", row.names=FALSE)

# PH516
data6 = read_sav("Website User Survey - Public Health 516_May 22, 2020_15.04.sav") %>%
  na_if(-1) %>% as_factor

meaningful6 = data6[,c(18,21:46)]

PH516 = c()

for (i in 1:ncol(meaningful6)){
  PH516 = c(PH516, attr(meaningful6[[i]], "label"))
}

fileconn <- file("PH516_question.txt")
writeLines(PH516, fileconn)
close(fileconn)

write.csv(meaningful6, "PH516_cleaned.csv", row.names=FALSE)

