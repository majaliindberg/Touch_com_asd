library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(afex)
library(emmeans)
library(psych)
library(patchwork)
library(parallel)

source('comm_source.R')

#### read data ####
live.comm.data <- read_csv('data/primary/live-comm_collated.csv', col_types = cols()) 

#### read performance metrics ####

live.performance.data <- read_csv('data/processed/live-comm_performance.csv', col_types = cols()) %>% 
  mutate(cued = factor(cued, levels = orderedCues))

#### mixed effects models ####
set_sum_contrasts()
theme_set(theme_light())

mm <- mixed(F1 ~ group*cued + (1|PID),
            data = live.performance.data, 
            method = 'LRT',
            control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
            family = binomial, weights=live.performance.data$Total )
summary(mm)

# reported
anova(mm)

emmeans(mm, ~ cued, type = 'response')
emmeans(mm,  ~  group, type = 'response')
(emm <- emmeans(mm,  ~  group + cued))
plot(emm, comparisons = TRUE, adjust = 'holm', by = 'group') # quick look

# reported
pairs(emm, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)
emmeans(mm, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)


### figure compare ####

Ns <- live.performance.data %>% 
  group_by(group,PID) %>% 
  tally() %>% tally()

N.ASD <- Ns %>% filter(group == 'ASD') %>% pull(n)
N.Control <- Ns %>% filter(group == 'Control') %>% pull(n)

emmeans(mm,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = orderedCues)) %>% 
  ggplot(aes(y = prob, x = cued, colour = group, fill = group)) +
  geom_hline(yintercept = live.performance.data$F1chance[1], 
             colour = 'grey', linetype = 'dashed', size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.4, size = 1.3,
                position = position_dodge(0.2)) +
  geom_point(size = 6, shape = "\u2014", # unicode m-dash for horizontal line
             position = position_dodge(0.2)) +
  scale_color_manual(values = c(colour.ASD, colour.Control)) +
  scale_fill_manual(values = c(colour.ASD, colour.Control)) +
  scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = NULL, y = 'Performance F1', colour = NULL, fill = NULL) +
  theme_light() + theme_biggerfonts + theme_x45deg + theme_insidelegend(0.85,0.85) +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') -> compare.plot

#### confusion matrices ####

###. figure live ASD ####
live.comm.data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.ASD, ylabels = c(orderedCues,'other')) -> confmat.live.ASD

###. figure live ASD individual ####
orderedPIDs.liveASD <- live.comm.data %>% 
  filter(group == 'ASD') %>% 
  order_PIDs(PID)
live.comm.data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(exptPID = factor(PID, levels = orderedPIDs.liveASD)) %>% 
  confusion_matrix_individual_plot(colour.ASD, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ PID, ncol = 5) -> confmat.live.ASD.ind

###. figure live Control ####
live.comm.data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.Control, ylabels = c(orderedCues,'other')) -> confmat.live.Control

###. figure live Control individual ####
orderedPIDs.liveControl <- live.comm.data %>%
  filter(group == 'Control') %>% 
  order_PIDs(PID)
live.comm.data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(PID = factor(PID, levels = orderedPIDs.liveControl)) %>% 
  confusion_matrix_individual_plot(colour.Control, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ PID, ncol = 5) -> confmat.live.Control.ind

#### combine figures ####

if ( !dir.exists('figures') ) { dir.create('figures') }

design.compare = '
AAAB
AAAC
'

quartz(width = 10.5, height = 5.7); plot(1:10)
compare.plot + confmat.live.ASD + confmat.live.Control +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = design.compare) 
ggsave('figures/Compare_ASD-vs-Control.svg')
ggsave('figures/Compare_ASD-vs-Control.pdf')

