library(readxl)
library(tidyverse)
library(summarytools)

# Load data without names
my_data <- read_excel("Data_ASD_8juni.xlsx", 
                             range = "A3:FJ274", col_names = FALSE)

# Load data names
data_names <- read_excel("Data_ASD_8juni.xlsx", 
                             range = "A1:FJ1", col_names = FALSE)
# Add names to my_data
names(my_data) <- unlist(data_names[1, ])
View(my_data)

# Just get 100% completed data + remove preview
prog_data <- my_data %>% filter(Progress == 100)
prog_data <- prog_data %>% filter(DistributionChannel == "anonymous")


# Create data frame with sub with age
age_data <- prog_data[!is.na(prog_data$Q19),]


# Get summary of age, mean et + plot
age <- age_data$Q19
summary(age)
hist(age)
table(age)

# Get gender distribution
table(age_data$Q18)
#female <- subset(age_data, Q18 == "Female")

# Get duration times
time = age_data$`Duration (in seconds)`
summary(time)
meanT = mean(time)/60
meanT


# AQ score 
#Pseudo code: 
#if Q(n) == 3 or if Q(n) == 4: 
#  Q(n) = 1
#elif Q(n) == 1 or if Q(n) == 2: 
#  Q(n) = 0

#for each row:
#  add column after x
#   x+1 = summarise (column(n:x))

#list1 <- list("Q24_2", "Q24_4", "Q24_5","Q24_6")
#list2 <- list("Q24_1", "Q24_3", "Q24_8","Q24_10")

#for(i in list1) {
 # if(age_data$i == 1) {
 #   age_data$i = 1
 # } else {
 #     age_data$i = 0
 # }
#}

# TAS score
# Create new data frame with summarized tas results
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


  





