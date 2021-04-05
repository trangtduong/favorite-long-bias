################################################################################
# This file compares subjective probability and objective probabiltiy.
# Alternative Specifications are noted with 'ALTERNATIVE SPECIFICATION'.
# When alter these specifications, please re-run the whole file.
################################################################################

# Synthetic odds: either 'median' or 'average'
odds_type = 'median'

# Two odds grouping techniques were considered: 
#   1. Odds are divided with equal interval of subjective probability. The 'binwidth'
#      variable controls the interval. 
#   2. Odds are divided with equal number of observations in each group. The 
#      'n_groups" variable controls the number of groups. 
binwidth = 5
n_groups <- 20


select_synthetic_odds <- function(odds_type=c('median', 'average')) {
  ret <- data_by_day_before_match
  if (odds_type == 'median') {
    ret <- ret %>%
      select(id, match_id, nation, league, season, role, outcome, median_odds, day_before) %>%
      mutate(implied_prob = 1/median_odds *100)
  } else {
    ret <- ret %>%
      select(id, match_id, nation, league, season, role, outcome, avg_odds, day_before) %>%
      mutate(implied_prob = 1/avg_odds *100)
  }
  ret <- ret %>%
    group_by(id) %>%
    mutate(overround = sum(implied_prob) - 100) %>%
    ungroup() %>%
    mutate(subj_prob = 100*implied_prob/(100+overround),
           result = ifelse(role == outcome, 100, 0),
           type = recode(role, 'H' = 'HOME WIN', 'A' = 'AWAY WIN', 'D' = 'DRAW'))

  return ret
}
 
data_by_intervals <- select_synthetic_odds(odds_type)
data_by_intervals <- data_by_intervals %>%
  mutate(group = cut(subj_prob, breaks = seq(0,100, by = binwidth)))


data_by_groups <- data_by_intervals %>% filter(day_before  == 1) %>%
  mutate(group1 = cut_number(subj_prob, n = n_groups))
data_by_groups <- data_by_groups %>%
  group_by(type) %>%
  mutate(group2 = cut_number(subj_prob, n = n_groups)) %>%
  ungroup()

############################################################################################################
# CALCULATE OBJ. PROBS AND CONF. INT. FOR EACH GROUP FOR EACH TYPES/ALL (GROUPS OF EQUAL RANGE)
############################################################################################################
by_bins_results <- data_by_intervals %>%
  group_by(day_before, type, group) %>%
  summarise(subj_prob = mean(subj_prob),
            obj_prob = mean(result),
            win = sum(result)/100,
            n = n())

by_bins_results <- data_by_intervals %>%
  group_by(day_before, group) %>%
  summarise(subj_prob = mean(subj_prob),
            obj_prob = mean(result),
            win = sum(result)/100,
            n = n()) %>%
  rbind(by_bins_results) %>%
  mutate(type = replace_na(type, 'ALL'),
         type = factor(type, levels = c('HOME WIN', 'AWAY WIN', 'ALL', 'DRAW')))

by_bins_results$conf1 <- mapply(binomial.test.conf.int1, 
                                by_bins_results$win, 
                                by_bins_results$n,
                                conf.level = 0.95)
by_bins_results$conf2 <- mapply(binomial.test.conf.int2, 
                                by_bins_results$win, 
                                by_bins_results$n,
                                conf.level = 0.95)

by_bins_results$p.value.two.side <- mapply(binomial.test.p.value, 
                                           by_bins_results$win, 
                                           by_bins_results$n,
                                           by_bins_results$subj_prob/100,
                                           alternative  = 'two.sided')
by_bins_results <- by_bins_results %>%
  mutate(conf1 = 100*conf1, conf2 = 100*conf2) %>%
  select(day_before, type, group, subj_prob, obj_prob, win, n, conf1, conf2, p.value.two.side)

# FILTER RESULT BY day_before:
by_bins_results %>%
  filter(day_before == 1 | day_before == 7)


############################################################################################################
# GRAPH OF SUBJ. AND OBJ. PROBS ON DAY -1 (GROUPS OF EQUAL RANGE)
############################################################################################################
plot1 <- by_bins_results %>%
  filter(day_before == 1) %>%
  ggplot(aes(x = subj_prob, y = obj_prob, color = type)) +
  geom_point() +
  geom_linerange(aes(x = subj_prob, ymin = conf1, ymax = conf2))+
  geom_abline(slope = 1, intercept = 0, color = 'gray') + 
  facet_wrap(type ~. , ncol = 2) +
  scale_x_continuous(name = 'Subjective Probability', breaks = seq(0,100, 20), minor_breaks = seq(0, 100, binwidth)) +
  ylab('Objective Probability') +
  theme(legend.position = "none")

plot2 <- by_bins_results %>%
  filter(day_before == 1) %>%
  mutate(diff = subj_prob - obj_prob) %>%
  ggplot(aes(x = subj_prob, y = diff, color = type)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_linerange(aes(x = subj_prob, ymin = subj_prob - conf2, ymax = subj_prob - conf1)) +
  facet_wrap(type ~., ncol = 2)+
  scale_x_continuous(name = 'Subjective Probability', breaks = seq(0,100, 20), minor_breaks = seq(0, 100, binwidth)) +
  ylab('Differences between Subjective\nand Objective Probability') +
  theme(legend.position = "none")
plot1
plot2
multiplot(plot1, plot2)

############################################################################################################
# GRAPH OF SUBJ. AND OBJ. PROBS ON DAY -7 (GROUP OF EQUAL RANGE)
############################################################################################################
plot3 <- by_bins_results %>%
  filter(day_before == 7) %>%
  ggplot(aes(x = subj_prob, y = obj_prob, color = type)) +
  geom_point() +
  geom_linerange(aes(x = subj_prob, ymin = conf1, ymax = conf2))+
  geom_abline(slope = 1, intercept = 0, color = 'gray') + 
  facet_wrap(type ~. , ncol = 2) +
  scale_x_continuous(name = 'Subjective Probability',breaks = seq(0,100, 20), minor_breaks = seq(0, 100, binwidth)) +
  ylab('Objective Probability') +
  theme(legend.position = "none")

plot4 <- by_bins_results %>%
  filter(day_before == 7) %>%
  mutate(diff = subj_prob - obj_prob) %>%
  ggplot(aes(x = subj_prob, y = diff, color = type)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_linerange(aes(x = subj_prob, ymin = subj_prob - conf2, ymax = subj_prob - conf1)) +
  facet_wrap(type ~., ncol = 2)+
  scale_x_continuous(name = 'Subjective Probability',breaks = seq(0,100, 20), minor_breaks = seq(0, 100, binwidth)) +
  ylab('Differences between Subjective\nand Objective Probability') +
  theme(legend.position = "none")
plot3
plot4
multiplot(plot3, plot4)


############################################################################################################
# BINOMIAL TEST FOR DIFFERENCE BETWEEN SUBJECTIVE AND OBJECTIVE PROBABILITY (GROUPS OF EQUAL RANGE)
############################################################################################################

by_bins_results %>%
  filter(day_before == 1 | day_before == 7) %>%
  select(day_before, type, group, p.value.two.side) %>%
  unite(key, c(type, day_before)) %>%
  spread(key, value = p.value.two.side) %>%
  select(group, ALL_1, `HOME WIN_1`, `DRAW_1`, `AWAY WIN_1`, ALL_7, `HOME WIN_7`, `DRAW_7`, `AWAY WIN_7`)

by_bins_results %>%
  filter(day_before == 1) %>%
  select(day_before, type, group, p.value.two.side) %>%
  unite(key, c(type, day_before)) %>%
  spread(key, value = p.value.two.side) %>%
  select(group, ALL_1, `HOME WIN_1`, `DRAW_1`, `AWAY WIN_1`)



############################################################################################################
# CALCULATE OBJ. PROBS AND CONF. INT. FOR EACH GROUP FOR EACH TYPES/ALL (GROUPS OF EQUAL OBS)
############################################################################################################


by_bins_results_2 <- data_by_groups %>%
  group_by(day_before, type, group2) %>%
  summarise(subj_prob = mean(subj_prob),
            obj_prob = mean(result),
            win = sum(result)/100,
            n = n())

by_bins_results_2 <- data_by_groups %>%
  group_by(day_before, group1) %>%
  summarise(subj_prob = mean(subj_prob),
            obj_prob = mean(result),
            win = sum(result)/100,
            n = n()) %>%
  rbind(by_bins_results_2) %>%
  mutate(type = replace_na(type, 'ALL'),
         type = factor(type, levels = c('HOME WIN', 'DRAW', 'AWAY WIN', 'ALL')))

by_bins_results_2$conf1 <- mapply(binomial.test.conf.int1, 
                                  by_bins_results_2$win, 
                                  by_bins_results_2$n,
                                  conf.level = 0.95)
by_bins_results_2$conf2 <- mapply(binomial.test.conf.int2, 
                                  by_bins_results_2$win, 
                                  by_bins_results_2$n,
                                  conf.level = 0.95)

by_bins_results_2$p.value.two.side <- mapply(binomial.test.p.value, 
                                             by_bins_results_2$win, 
                                             by_bins_results_2$n,
                                             by_bins_results_2$subj_prob/100,
                                             alternative  = 'two.sided')
by_bins_results_2 <- by_bins_results_2 %>%
  mutate(conf1 = 100*conf1, conf2 = 100*conf2)
by_bins_results_2 <- by_bins_results_2 %>%
  mutate_at(c('conf1', 'conf2'), round, digits = 4) %>%
  mutate(CI = paste0('[', conf1, ';', conf2, ']' )) %>% ungroup() 

# Results: 
by_bins_results_2 %>%
  select(type, group, n, win, subj_prob, obj_prob, CI, p.value.two.side)
############################################################################################################
# GRAPH OF SUBJ. AND OBJ. PROBS ON DAY -1 (GROUPS OF EQUAL OBS)
############################################################################################################
# add breaks between groups as 'breaks'
by_bins_results_2 <- by_bins_results_2%>% 
  unite(group, group1, group2, sep = '') %>%
  mutate(group = str_replace(group, 'NA', ''))

plot5 <- by_bins_results_2 %>%
  filter(day_before == 1) %>%
  ggplot(aes(x = subj_prob, y = obj_prob, color = type)) +
  geom_abline(slope = 1, intercept = 0, color = 'gray') + 
  geom_line(linetype = 'solid') +
  geom_line(aes(y = conf1), linetype = 'dashed') +
  geom_line(aes(y = conf2), linetype = 'dashed')+
  facet_wrap(type ~. , ncol = 2) +
  xlab('Subjective Probability')+
  ylab('Objective Probability') +
  theme(legend.position = "none")

plot6 <- by_bins_results_2 %>%
  filter(day_before == 1) %>%
  mutate(diff = subj_prob - obj_prob) %>%
  ggplot(aes(x = subj_prob, y = diff, color = type)) +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_line(linetype = 'solid') +
  geom_line(aes(y = subj_prob - conf1), linetype = 'dashed') +
  geom_line(aes(y = subj_prob - conf2), linetype = 'dashed') +
  facet_wrap(type ~., ncol = 2)+
  scale_x_continuous(name = 'Subjective Probability', breaks = seq(0,100, 20), minor_breaks = seq(0, 100, binwidth)) +
  ylab('Differences between Subjective\nand Objective Probability') +
  theme(legend.position = "none")
plot5
plot6
multiplot(plot5, plot6)




