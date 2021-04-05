####################################################################
# This file creates a dataframe of data that is used in my thesis
# and descriptive statitstics of data                           
####################################################################

# Calculate median odds, average odds, max odds... for each row of dataframe 
# for decimal odds (each row is an observation of different odds quoted by 
# bookmakers simultaneously)
data <- decimal_data_with_completed_set %>% 
  select(id, role, V10:V9) %>%
  gather(key = 'bookie', value = 'odds', -c(id, role)) %>%
  group_by(id, role)  %>%
  summarise(n = sum(!is.na(odds)),
            avg_odds = mean(odds, na.rm = T),
            median_odds = median(odds, na.rm = T),
            max_odds = max(odds, na.rm = T),
            min_odds = min(odds, na.rm = T),
            sd = sd(odds, na.rm = T)) 

# Select data for each day before matches. choose the timestamp (ts) which has 
# highest number of odds quotes and lastest
data_by_day_before_match <- decimal_data_with_completed_set %>% 
  select(id:outcome) %>%
  inner_join(data, by = c("id", "role")) %>%
  mutate(day_before = as.numeric(ceiling(difftime(match_time, ts, units = 'days')))) %>%
  group_by(match_id,day_before) %>%
  filter(n == max(n)) %>%
  filter(ts - match_time == min(ts- match_time)) %>%
  ungroup() %>%
  filter(day_before <=14, day_before> 0) %>%
  mutate(day_before = factor(day_before))


# Table of number of match have odds quotes, average and SD of number of bookie 
#quotes on each day before match start
data_by_day_before_match %>%
  select(id, match_id, n, day_before) %>%
  distinct() %>%
  group_by(day_before) %>%
  summarise(n_match = n(), average_n_odds = mean(n), sd = sd(n))


# Graph histogram the number of bookies on day-1 and day-7 
# Full graph
data_by_day_before_match %>%
  select(id, match_id, n, day_before) %>%
  distinct() %>%
  group_by(day_before, n) %>%
  tally(name = 'count') %>% 
  filter(day_before == 1 | day_before == 7) %>%
  ggplot(aes(x = n, y = count, fill = day_before)) +
  geom_col(position = 'dodge', width = 0.9) +
  labs(x = 'No. bookmakers quote odds in a match',
       y = 'No. Matches') +
  scale_fill_discrete(name = "Period", labels = c("Day -1", "Day -7"))

# Larger bins
data_by_day_before_match %>%
  select(id, match_id, n, day_before) %>%
  distinct() %>%
  group_by(day_before, n) %>%
  tally(name = 'count') %>%
  mutate(bins = case_when(n < 5 ~ '1-5',
                          n < 10 ~ '6-10',
                          n < 15 ~ '11-15',
                          n < 20 ~ '16-20',
                          n < 25 ~ '21-25',
                          TRUE ~ '26-28'),
         bins = factor(bins, level = c('1-5', '6-10', '11-15', '16-20', '21-25', '26-28'))) %>%
  group_by(day_before, bins) %>%
  tally(wt = count) %>%
  filter(day_before == 1 | day_before == 7) %>%
  ggplot(aes(x = bins, y = n, fill = day_before)) +
  geom_col(position = 'dodge', width = 0.9) +
  labs(x = 'No. bookmakers quote odds for a match',
       y = 'No. Matches') +
  scale_fill_discrete(name = "Period", labels = c("Day -1", "Day -7"))


# Number of matches and odds quotes 
data_by_day_before_match <- data_by_day_before_match %>%
  mutate(league = factor(league, 
                        levels = c('champions-league', 'premier-league', 'championship',
                                  'league-one', 'league-two', 'national-league',
                                  'fa-cup','efl-cup','la-liga-primera', 
                                  'la-liga-segunda', 'copa-del-rey')),
         nation = factor(nation, levels = c('UEFA', 'England', 'Spain')))

tab_number_of_matches_and_odds_quoted <- data_by_day_before_match %>%
  select(match_id, nation, league) %>%
  distinct() %>%
  group_by(nation, league) %>%
  tally(name = 'match_played')

tab_number_of_matches_and_odds_quoted <- data_by_day_before_match %>%
  group_by(nation, league) %>%
  summarise(odds_quotes_all = sum(n)) %>%
  right_join(tab_number_of_matches_and_odds_quoted)

tab_number_of_matches_and_odds_quoted <- data_by_day_before_match %>%
  group_by(day_before, nation, league) %>%
  summarise(odds_quoted = sum(n)) %>% 
  spread(key = day_before, value = odds_quoted) %>%
  right_join(tab_number_of_matches_and_odds_quoted)

tab_number_of_matches_and_odds_quoted  <- tab_number_of_matches_and_odds_quoted %>%
  select(nation, league, match_played, odds_quotes_all, 
          odds_quotes_day_1 = `1`, odds_quotes_day_7 = `7`)

tab_number_of_matches_and_odds_quoted <- tab_number_of_matches_and_odds_quoted %>%
  ungroup() %>%
  summarise_if(is.integer, .funs = sum) %>%
  full_join(tab_number_of_matches_and_odds_quoted) %>%
  select(nation, league, everything()) %>%
  arrange(nation, league)

tab_number_of_matches_and_odds_quoted


# Descriptive statistics
tab_descriptive_statistics <- data_by_day_before_match %>%
  group_by(role) %>%
  summarise(avg_odds = mean(avg_odds), median_odds = mean(median_odds), 
            max_odds = mean(max_odds), sd = mean(sd, na.rm = T)) %>%
  mutate(set = 'All')
tab_descriptive_statistics <- data_by_day_before_match %>%
  summarise(avg_odds = mean(avg_odds), median_odds = mean(median_odds), 
            max_odds = mean(max_odds), sd = mean(sd, na.rm = T)) %>%
  mutate(role = 'All', set = 'All') %>%
  full_join(tab_descriptive_statistics)

tab_descriptive_statistics <- data_by_day_before_match %>%
  group_by(day_before, role) %>%
  summarise(avg_odds = mean(avg_odds), median_odds = mean(median_odds), 
            max_odds = mean(max_odds), sd = mean(sd, na.rm = T)) %>%
  rename(set = day_before) %>%
  full_join(tab_descriptive_statistics)

tab_descriptive_statistics <- data_by_day_before_match %>%
  group_by(day_before) %>%
  summarise(avg_odds = mean(avg_odds), median_odds = mean(median_odds), 
            max_odds = mean(max_odds), sd = mean(sd, na.rm = T)) %>%
  rename(set = day_before) %>%
  mutate(role = 'All') %>%
  full_join(tab_descriptive_statistics)

tab_descriptive_statistics <- tab_descriptive_statistics%>%
  mutate(role = recode(role, 'H' = 'Home Win', 'A' = 'Away Win', 'D' = 'Draw'), 
         role = factor(role, levels = c('Home Win', 'Draw', 'Away Win', 'All')),
         set = factor(set, levels = c(1:14, 'All'))) %>%
  arrange(set, role) %>%
  select(set, role, everything())

tab_descriptive_statistics %>%
  filter(set == '1' | set == '7' | set == 'All')

