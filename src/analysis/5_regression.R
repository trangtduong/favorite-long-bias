##############################################################################################
# This file provides codes to do the regression in my thesis
# Alternative Specifications are noted with 'ALTERNATIVE SPECIFICATION'
# When alter these specifications by commenting/uncommenting, please re-run this whole file.
##############################################################################################

# ALTERNATIVE SPECIFICATION: Choose lag: Day -1, Day -2 ... Day -14 from below codes or 
# set a new value for lag from -1, -2, ... -14.
# lag <- -1 # Day -1
lag <- -7 # Day-7
# lag <- -14 # Day -14

# regression on Day -1:
by_bins_data %>% 
  filter(day_before == -lag) %>%
  lm(formula = result - subj_prob ~ subj_prob) %>%
  summary()

#For draw
by_bins_data %>% 
  filter(day_before == -lag) %>%
  filter(role == 'D') %>%
  lm(formula = result - subj_prob ~ subj_prob) %>%
  summary()
#For Away Win
by_bins_data %>% 
  filter(day_before == -lag) %>%
  filter(role == 'A') %>%
  lm(formula = result - subj_prob ~ subj_prob) %>%
  summary()
#For Home Win
by_bins_data %>% 
  filter(day_before == -lag) %>%
  filter(role == 'H') %>%
  lm(formula = result - subj_prob ~ subj_prob) %>%
  summary()

# Mixed:
by_bins_data %>% 
  filter(day_before == -lag) %>%
  filter(role != 'D') %>%
  lm(formula = result - subj_prob ~ subj_prob) %>%
  summary()

# control for home bias:
by_bins_data %>% 
  filter(day_before == -lag) %>%
  filter(role != 'D') %>%
  lm(formula = result - subj_prob ~ subj_prob + role + subj_prob * role) %>%
  summary()
