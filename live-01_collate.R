library(readr)
library(dplyr)
library(stringr)

raw.data.folder <- '~/OneDrive - Linköpings universitet/projects - in progress/Touch Comm ASD/Data/'

collate_data <- function(folder, pattern) {
  folder %>% 
    list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>% 
    read_csv(id = 'path', col_types = cols()) %>% 
    mutate(
      group = str_extract(path, '(ASD - )|(Controls - )') %>% str_replace_all('( - )|s', ''),
      PID = str_extract(path,'(asd[0-9]+)|(sub[0-9]+)') %>% as.factor() %>% as.numeric()
    ) %>% 
    select(-path)
}

if ( !dir.exists('data') ) { dir.create('data') }
if ( !dir.exists('data/primary') ) { dir.create('data/primary') }

raw.data.folder %>% 
  collate_data('comm.*data\\.csv') %>% 
  write_csv('data/live-comm_collated.csv')

raw.data.folder %>% 
  collate_data('pleas.*data\\.csv') %>% 
  write_csv('data/live-pleas_collated.csv')
