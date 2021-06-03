library(readxl)
library(tidyverse)
library(summarytools)

# Load data without names
my_data <- read_excel("Data_ASD_27maj.xlsx", 
                             range = "A3:FN279", col_names = FALSE)
# Load data names
data_names <- read_excel("Data_ASD_27maj.xlsx", 
                             range = "A1:FN1", col_names = FALSE)
# Add names to my_data
names(my_data) <- unlist(data_names[1, ])
View(my_data)

# Just get 100% completed data
prog_data <- my_data %>% filter(Progress == 100)

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


  





