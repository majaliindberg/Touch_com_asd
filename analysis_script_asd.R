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

age <- my_data$
Nfin <- my_data %>% filter(Progress == 100)
summary(Nfin)

dfSummary(Nfin)


