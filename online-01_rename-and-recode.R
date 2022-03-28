# library(readxl)
library(qualtRics)
library(tidyverse)
library(summarytools)

raw_data_folder <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/online survey/"

raw_data_file <- paste0(
  raw_data_folder,
  'Communication with social touch in Autism spectrum disorder_March 28, 2022_14.50.csv'
  )

raw_data <- read_survey(raw_data_file)

# Just get 100% completed data + remove preview
valid_data <- raw_data %>% 
  # SCREENING for those who...
  # are not spam
  filter(Status != "Spam") %>% 
  # completed the survey
  filter(Progress == 100) %>% 
  # were not part of the testing/preview phase
  filter(DistributionChannel != "preview") %>% 
  # give consent
  filter(Q2 == "Yes, I will participate.") %>% 
  # are aged 15+
  filter(Q3 != "under 15") %>% 
  # do not have a bipolar or psychosis diagnosis
  filter(Q6 == "No") %>% 
  # do not regularly take recreational drugs
  filter(Q8 == "No") %>% 
  # do not regularly drink 5 or more alcoholic drinks per day
  filter(Q10 == "No") %>% 
  # are not interested in participating in the in-person study
  filter(Q12 == "No") %>% 
  # can see the video
  filter(Q15 == "Yes") %>% 
  # remove variables used for screening
  select(-c(
    Status,
    Progress,
    DistributionChannel,
    Finished,
    Q2, Q3, Q6, Q8, Q10, Q12, Q15
  ))

# summary table
valid_data %>% 
  dfSummary() %>% view



# Create functions that replace value with 1 or 0
replace1_2 <- function(x) {
  if_else(condition = x == 1| x ==2, 
          true = 1, 
          false = 0)
}
replace3_4 <- function(x) {
  if_else(condition = x == 3| x == 4, 
          true = 1, 
          false = 0)
}

# function for reversing scores on BAPQ
reversedBAPQ <- function(x) {
  x=7-x
}

# function for reversing scores on STQ
reversedSTQ <- function(x) {
  x=6-x
}



# Apply the replace functions accross the concerned columns to get AQ score
df <- age_data %>% 
  mutate(across(c(Q24_2, Q24_4, Q24_5,Q24_6,Q24_7,Q24_9,Q24_12,Q24_13,
                  Q24_13,Q24_16,Q24_18,Q24_19,Q24_20,Q24_21,Q24_22,Q24_23
                  ,Q24_26,Q24_33,Q24_35,Q24_39,Q24_41,Q24_42,Q24_43,Q24_45
                  ,Q24_46),
                replace1_2)
  )%>%  
  mutate(across(c(Q24_1, Q24_3, Q24_8,Q24_10,Q24_11,Q24_14,Q24_15,Q24_17
                  ,Q24_24,Q24_25,Q24_27,Q24_28,Q24_29,Q24_30,Q24_31,Q24_32
                  ,Q24_34,Q24_36,Q24_37,Q24_38,Q24_40,Q24_44,Q24_47,Q24_48
                  ,Q24_49,Q24_50),
                replace3_4)
  )

# Sum up AQ score
AQ_sum <- df %>% 
  rowwise() %>% 
  mutate(AQ_result = sum(c(Q24_2, Q24_4, Q24_5,Q24_6,Q24_7,Q24_9,Q24_12,
                               Q24_13,Q24_13,Q24_16,Q24_18,Q24_19,Q24_20,Q24_21,
                               Q24_22,Q24_2,Q24_26,Q24_33,Q24_35,Q24_39,Q24_41,
                               Q24_42,Q24_43,Q24_45,Q24_46, Q24_1, Q24_3, Q24_8,
                               Q24_10,Q24_11,Q24_14,Q24_15,Q24_17,Q24_24,Q24_25,
                               Q24_27,Q24_28,Q24_29,Q24_30,Q24_31,Q24_32,Q24_34,
                               Q24_36,Q24_37,Q24_38,Q24_40,Q24_44,Q24_47,Q24_48
                               ,Q24_49,Q24_50 )))

# Visualize AQ results
table(df$AQ_result)
hist(df$AQ_result)
av <- c(11, 21)

# tried to split participants into groups depending on the division between
# degrees of AQ score 
average <- between(df$AQ_result, 11, 21)
above_av <- between(df$AQ_result, 22, 25)
bordeline <- between(df$AQ_result, 26, 31)
clinical <- df$AQ_result >= 32


# reverse score on "reverse items" for BAPQ
BAPQscoring <- AQ_sum %>% 
  mutate(across(c(Q25_1,Q25_3,Q25_7,Q25_9,Q25_12,Q25_15,Q25_16,Q25_19,Q25_21,
                  Q25_23,Q25_25,Q25_28,Q25_30,Q25_34,Q25_36),
                reversedBAPQ))

# Sum up BAPQ score for each participant
BAPQ_sum <- BAPQscoring %>% 
  rowwise() %>% 
  mutate(BAPQ_result = sum(c(Q25_1,Q25_2,Q25_3,Q25_4,Q25_5,Q25_6,Q25_7,Q25_8
                             ,Q25_9,Q25_10,Q25_11,Q25_12,Q25_13,Q25_14,Q25_15
                             ,Q25_16,Q25_17,Q25_18,Q25_19,Q25_20,Q25_21
                             ,Q25_22,Q25_23,Q25_24,Q25_25,Q25_26,Q25_27
                             ,Q25_28,Q25_29,Q25_30,Q25_31,Q25_32,Q25_33
                             ,Q25_34,Q25_35,Q25_36)))

# reverse score on "reverse items" for STQ

STQscoring <- BAPQ_sum %>% 
  mutate(across(c(Q26_1,Q26_4,Q26_6,Q26_9,Q26_11,Q26_12,Q26_14,Q26_15,Q26_18
                  ,Q26_20),
                reversedSTQ))
         
         
# Sum up STQ score for each participant 
STQ_sum <- STQscoring %>% 
  rowwise() %>% 
  mutate(STQ_result = sum(c(Q26_1, Q26_2, Q26_3, Q26_4, Q26_5, Q26_6, 
                            Q26_7,Q26_8, Q26_9, Q26_10, Q26_11, Q26_12, 
                            Q26_13,Q26_14, Q26_15, Q26_16, Q26_17, Q26_18, 
                            Q26_19,Q26_20))) 


# Sum up TAS score for each participant
df <- age_data %>% 
  rowwise() %>% 
  mutate(Tas_result = sum(c(Q29_1, Q29_2, Q29_3, Q29_4, Q29_5, Q29_6, 
                        Q29_7,Q29_8, Q29_9, Q29_10, Q29_11, Q29_12, 
                        Q29_13,Q29_14, Q29_15, Q29_16, Q29_17, Q29_18, 
                        Q29_19,Q29_20 )))

# Visualize TAS results
table(df$Tas_result)
hist(df$Tas_result)

#Get amout of high Tas (score >= 61)
High_Tas <- df$Tas_result >= 61
table(High_Tas)




  





