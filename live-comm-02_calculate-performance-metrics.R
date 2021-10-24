library(readr)
library(dplyr)

#### functions ####

tally_by <- function(.data, ...) {
  byVars <- enquos(..., .named = TRUE)
  xformula <- reformulate(termlabels = c(names(byVars)))
  .data %>%
    xtabs(formula = xformula) %>%
    as_tibble() %>%
    arrange(...)
}

calculate_performance_metrics <- function(.data, item, response, ...) {
  .data %>% 
    # count up responses for all combos of item and response
    tally_by(..., {{item}}, {{response}}) %>% 
    # calculate total number of trials/presentations
    group_by(...) %>% mutate(Total = sum(n)) %>% 
    # number of times each cue appeared
    group_by(...,{{item}}) %>% mutate(Present = sum(n)) %>%
    # number of times each response was made
    group_by(..., {{response}}) %>% mutate(Selected = sum(n)) %>%
    # get rid of unneeded rows (we're summarising, one line for each cue)
    filter({{item}} == {{response}}) %>% rename('Hits' = 'n') %>% # now counts are just hits
    # calculate performance variables
    ungroup() %>% 
    mutate(Misses = Present - Hits,
           FalseAlarms = Selected - Hits,
           CorrectRejections = Total - Present - FalseAlarms,
           Recall = Hits/Present, # p correct when cue present
           Precision = Hits/Selected, # p correct with this response
           Precision = if_else(is.na(Precision), 0, Precision), 
           F1 = 2*( (Precision*Recall) / (Precision+Recall)), 
           F1 = if_else(is.na(F1), 0, F1),
           F1chance = 2*( (Present/Total) / ((Present/Total)+1) ) # always give same answer
    )
}

if ( !dir.exists('data') ) { dir.create('data') }
if ( !dir.exists('data/processed') ) { dir.create('data/processed') }

read_csv('data/primary/live-comm_collated.csv', col_types = cols()) %>% 
  group_by(group) %>% 
  do(calculate_performance_metrics(., cued, response, PID)) %>% 
  write_csv('data/processed/live-comm_performance.csv')

