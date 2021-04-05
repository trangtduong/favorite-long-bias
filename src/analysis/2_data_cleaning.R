##########################################################
#########      READ ODDS FILES   #########################
##########################################################

odds_dir <- '../data/odds'
match_result_file <- '../data/match_result.csv'

# Create dataframe that contains odds file names and file path:
odds_file <- data.frame(FilePath = dir(odds_dir, recursive = TRUE)) 
odds_file <- odds_file %>% mutate(file_id = as.numeric(row.names(odds_file)), FilePath = as.character(FilePath)) %>% select(file_id, everything()) %>%
  separate(FilePath, into = paste0('folder', 1:7), sep = '/', remove = F, extra = 'warn', fill = 'left') %>%
  select(file_id, FileName = folder7, FilePath) %>%
  mutate(FilePath = paste0(odds_dir, FilePath))
odds_file <- odds_file %>%
  separate(FilePath, into = paste0('folder', 1:4), sep = '/', extra = 'merge', remove = F) %>%
  select(file_id, FileName, folder = folder3, FilePath)

# Create read_plus function - customize function for reading files
read_plus <- function(flnm) {
  read.csv(flnm, header = FALSE, col.names = paste0('V', seq(1,30,by = 1)), sep = ';',colClasses = c('character'), na.strings = c('', 'NA', 'N/A'), fill = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, blank.lines.skip = TRUE) %>% 
    mutate(file_path = flnm)
}


# The below codes is commented out. 
# The original data contain '&nbsp;; ' AND '</span; ' which create problems
# to read data into dataframe. Those characters are removed in original files
# by below codes so that read function can create a correct dataframe. 
# #READ DATA
# data <- map_df(odds_file$FilePath, ~read_plus(.))
# data$V1 %>% as.factor() %>% levels()
# 
# #REMOVE '&nbsp;; ' AND '</span; '
# pathsContainWrongInputData <- data %>% filter(V1 == '&nbsp' | V1 == '</span') %>%
#   select(file_path) %>%
#   distinct() %>%
#   unlist() %>%
#   unname()
# pathsContainWrongInputData
# filesContainWrongInputData <- sapply(pathsContainWrongInputData, read_file)
# correctInput <- filesContainWrongInputData %>% sapply(str_remove_all, pattern = '&nbsp;; ') %>% sapply(str_remove_all, pattern = '</span; ')
# for (i in 1: length(pathsContainWrongInputData)) {
#   write_file(correctInput[i], path = pathsContainWrongInputData[i], append = F)
# }
# rm(list = c('pathsContainWrongInputData', 'filesContainWrongInputData', 'correctInput', 'i'))




#Read data
data <- map_df(odds_file$FilePath, ~read_plus(.))

data <- data %>% mutate(info_row = if_else(V1 == 'META', T, F, missing = F), 
                        id = cumsum(info_row), 
                        remove = F, 
                        row_name = as.numeric(row.names(data))) %>% 
  select(row_name, id, info_row, remove, everything())

##########################################################
#########      DATA CLEANING: INFO ROWS    ###############
##########################################################

# Check for wrong recorded info data
temp <- data %>%
  filter(info_row == T) %>%
  filter(!is.na(V7)) # 5 rows is recorded wrongly
data$V3[temp$row_name] <- data$V7[temp$row_name]
data$V4[temp$row_name] <- data$V8[temp$row_name]
data$V5[temp$row_name] <- data$V9[temp$row_name]
data[temp$row, c('V6', 'V7', 'V8', 'V9')] <- NA

# Make sure that V7:V30 is NA in info row
temp <- data %>% filter(info_row == T) %>%
  select(V7:V30) %>% 
  is.na.data.frame()
sum(1-temp) # == 0 is ok

# check V5 and V6 column
data %>% filter(info_row == TRUE) %>%
  select(V5, V6) %>%
  distinct() # Some row has NA data in V6 and computer name in V5
# recode V5 and V6
data$V6[data$V5 == 'e70300-dr003'] <- 'e70300-dr003'
data$V5[data$V5 == 'e70300-dr003'] <- 'Nothing'
# recheck V5 and V6 column
data %>% filter(info_row == TRUE) %>%
  select(V5, V6) %>%
  distinct() #ok

#Spread info data into every rows
data <- data %>%
  filter(info_row == T) %>%
  select(id, path = V2, ts = V3, match_time = V4, in_play_status = V5, computer = V6) %>%
  right_join(data, by = 'id') %>%
  filter(info_row == F) %>%
  select(-c(info_row))

data <- data %>%
  mutate(row_name = as.numeric(row.names(data))) %>%
  select(row_name, remove, everything())


##########################################################
#########      DATA CLEANING: TEAM NAMES    ##############
##########################################################

# Create a column 'n_odds' that calculate the number of odds for each id:
# The correct n_odds is 3 (odds for home team, away team and draw)
data <- data %>%
  add_count(id) %>%
  rename(n_odds = n)

# Recode wrong team names/draw in V1
# V1 should contain only the team name or 'Draw'
# recode 'The Draw' in V1 to 'Draw',
# recode w, l, d, 1 in V1 to NA
# remove ' d', ' l', ' w' append in team name
data <- data %>%
  mutate(V1 = recode(V1, 'The Draw' = 'Draw', 'w' = NA_character_, 'l' = NA_character_, 'd' = NA_character_, '1' = NA_character_)) %>%
  mutate(V1 = str_remove_all(V1, '[:space:]d|[:space:]l|[:space:]w|[:space:]B$'))

# Recode missing team names/draw to correct team names if possible:
# check NA in V1
temp <- data %>% filter(is.na(V1)) %>%
  select(id) %>%
  distinct() %>%
  left_join(data, by = 'id')
temp <- data %>% filter(is.na(V1)) %>%
  select(id) %>% left_join(data, by = 'id') %>%
  filter(!is.na(V1)) %>%
  select(id, path, file_path, V1) %>%
  group_by(id, path, file_path) %>%
  mutate(names = c('name1', 'name2')) %>%
  ungroup() %>%
  spread(key = names, value = V1)

temp2 <- temp %>%
  select(path, name1, name2) %>%
  distinct() %>%
  mutate(replace_na = c('Derby', 'Wolves', 'Wolves', 'Blackpool', 'Peterborough', 
                        'Shrewsbury', 'Accrington', 'Accrington', 'Coventry', 'Swindon',
                        'Bournemouth', 'Burnley', 'Liverpool', 'Newcastle', 'Swansea',
                        'Tottenham', 'Watford', 'Burnley', 'West Ham', 'Eibar',
                        'Girona', 'Barcelona', 'Draw', 'Burnley', 'Man Utd',
                        'Leicester', 'Man City', 'Draw', 'Watford', 'Betis'
  ))

temp3 <- data %>% filter(is.na(V1)) %>% 
  left_join(temp2, by = 'path') %>%
  select(id, row_name, V1, replace_na) %>% distinct()

data$V1[temp3$row_name] <- temp3$replace_na


# Recode team name in V1
data <- data %>% mutate(V1 = recode(V1, 'Cambridge United' = 'Cambridge Utd',
                                    'Real Betis' = 'Betis',
                                    'AFC Wimbledon' = 'Wimbledon',
                                    'Lincoln City FC' = 'Lincoln',
                                    'FC Barcelona' = 'Barcelona', 
                                    'Brimscombe and Thrupp FC' = 'Brimscombe and Thrupp',
                                    'UCAM Murcia CF' = 'UCAM Murcia',
                                    'Atleticoe Madrid' = 'Atletico Madrid',
                                    'PNE' = 'Preston', 
                                    'Gimnastice Tarragona' = 'Gimnastice De Tarragona', 
                                    'Dagenham & Redbridge' = 'Dagenham Redbridge',
                                    'Huddersfiel' = 'Huddersfield', 
                                    'Watfor' = 'Watford', 
                                    'Liverpoo'= 'Liverpool', 
                                    'Man Ut' = 'Man Utd', 
                                    'Arsena' = 'Arsenal'))

########################################################################
#########DATA CLEANING: REMOVE ROWS CONTAIN IRRELEVANT DATA #############
########################################################################

# Any rows that contains odds for others events is marked 
# in variable 'remove' = TRUE.
# Criteria: One id (for bets on 1 match at a moment) have
# only 1 row for draw odds and one id have only a set of 3 odds: 
# home, draw and away.

# Create n_draw variable that count number of draw odds for one match_id
data <- data %>% group_by(id) %>%
  mutate(n_draw = sum(V1 == 'Draw')) %>%
  ungroup() 

# Check rows where n_odds is different than 3:
### no draw odds:
data %>% filter(n_draw == 0) %>%
  distinct(n_odds) %>%
  arrange(n_odds)
data %>%
  filter(n_draw == 0) -> temp
View(temp)
### 1 & 2 draw odds:
data %>% filter(n_draw == 1) %>%
  distinct(n_odds) %>%
  arrange(n_odds) # in set of 1:7 odds
data %>% filter(n_draw == 2) %>%
  distinct(n_odds) %>%
  arrange(n_odds) # in set of 4 odds
data %>%
  filter(n_draw == 2) -> temp
View(temp)
data %>%
  filter(n_draw == 1, n_odds > 3) -> temp
View(temp)

# Collapsing the rows with odds for same team in each id
temp <- data %>% filter(n_draw == 2 | (n_draw == 1 & n_odds > 3))
my_list <- rep('', 29) %>% as.list()
names(my_list) <- paste0('V', 2:30)
temp <- temp %>% select(-row_name) %>% 
  group_by(remove, id, path, ts, match_time, in_play_status, computer, V1, file_path, n_odds, n_draw) %>%
  replace_na(my_list) %>%
  summarise_all(list(~trimws(paste(., collapse = ' ')))) %>%
  ungroup() %>%
  mutate_at(paste0('V', 2:30), .funs = ~if_else(str_detect(., ' '), '', .)) %>%
  mutate_at(paste0('V', 2:30),  .funs = ~na_if(.,""))

data <- data %>%
  select(-row_name) %>%
  anti_join(temp, by = 'id') %>%
  rbind(temp) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(n_draw = sum(V1 == 'Draw')) %>%
  ungroup() %>%
  select(-n_odds) %>%
  add_count(id, name = 'n_odds')

data <- data %>% mutate(row_name = as.numeric(row.names(data)))

rm(my_list)

# Special cases that contains a full set of 3 odds for home, away and draw
# and some irrelevant odds. The irrelevant odds is given new id, which will 
# be marked as removed = TRUE later
temp <- data %>% filter(n_draw == 1, n_odds == 7) 
data$id[temp$row_name[c(1,2,5,7)]] <- max(data$id) + 1
data$id[temp$row_name[c(8,9,12,14)]] <- max(data$id) + 1

temp <- data %>% filter(n_draw == 1, n_odds == 6)
data$id[temp$row_name[c(3,4,6)]] <- max(data$id) + 1

temp <- data %>% filter(n_draw == 1, n_odds == 5)
data$id[temp$row_name[c(3,5)]] <- max(data$id) + 1

temp <- data %>% filter(n_draw == 1, n_odds == 4)

# recalculate n_odds and n_draw after alter id for special cases
data <- data %>% group_by(id) %>%
  mutate(n_draw = sum(V1 == 'Draw')) %>%
  ungroup() %>%
  select(-n_odds) %>%
  add_count(id, name = 'n_odds')

# all id without draw odds is removed
data <- data %>% mutate(remove = if_else(n_draw == 0, T, remove))
sum(data$remove) # remove 32254 rows

# keep only id with 3 odds:
data <- data %>% mutate(remove = if_else(n_odds != 3, T, remove))

# Remove sets of odds for in play games
data <- data %>%
  mutate(remove = ifelse(in_play_status == 'In Play', T, remove))


########################################################################
#########        DATA CLEANING: RECODE TEAM NAMES       ################
########################################################################
# Create variables: nation, league, season
data <- data %>%
  mutate(season = 
           case_when(
             str_detect(file_path, '2016-17') ~ '16/17',
             str_detect(file_path, '2017-18') ~ '17/18',
             TRUE ~ NA_character_),
         league = str_extract(file_path, '[^/]+(?=/winner)')
  )

data <- data %>%
  mutate(league = recode(league, 'league-cup' = 'efl-cup'),
         nation = case_when(
           league == 'champions-league' ~ 'UEFA',
           league == 'championship' ~ 'England',
           league == 'efl-cup' ~ 'England',
           league == 'fa-cup' ~ 'England',
           league == 'league-1' ~ 'England',
           league == 'league-2' ~ 'England',
           league == 'national-league' ~ 'England',
           league == 'premier-league' ~ 'England',
           league == 'copa-del-rey' ~ 'Spain',
           league == 'la-liga-primera' ~ 'Spain',
           league == 'la-liga-segunda' ~ 'Spain',
         ))
temp <- data %>%
  filter(remove == F) %>%
  select(nation, league, season, V1) %>% distinct() %>%
  filter(V1 != 'Draw')
championship_16_17 <- temp %>% filter(league == 'championship', season == '16/17') %>% arrange(V1)
championship_16_17 %>% select(V1) %>% 
  unlist() %>% as.vector()
"na/na/na/na/na/na/na/na/na/Aston Villa/na/na/na/na/na/na/na/na/na/Barnsley FC/na/na/na/na/Birmingham City/Blackburn Rovers/na/na/na/na/na/na/Brentford FC/Brighton & Hove Albion/Bristol City/na/na/Burton Albion/na/na/na/Cardiff City/na/na/na/na/na/na/na/na/na/na/na/na/Derby County/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Fulham FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Huddersfield Town/na/na/na/Ipswich Town/na/na/na/na/na/na/na/na/Leeds United/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Newcastle United/na/na/na/na/Norwich City/Nottingham Forest/na/na/na/na/na/na/na/na/na/na/Preston North End/na/Queens Park Rangers/na/na/na/na/Reading FC/na/na/na/na/na/na/na/na/na/Rotherham United/na/na/na/na/na/na/na/na/na/na/Sheffield Wednesday/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Wigan Athletic/na/na/na/Wolverhampton Wanderers/na/na/na/na" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
championship_16_17 <- championship_16_17 %>% mutate(team = na_if(team, 'na'))

championship_17_18 <- temp %>% filter(league == 'championship', season == '17/18') %>% arrange(V1)
championship_17_18 %>% select(V1) %>% 
  unlist() %>% as.vector()

"Aston Villa/Barnsley FC/Birmingham City/Bolton Wanderers/Brentford FC/Bristol City/Burton Albion/Cardiff City/Derby County/Fulham FC/Hull City/Ipswich Town/Leeds United/na/Middlesbrough FC/Millwall FC/Norwich City/Nottingham Forest/Preston North End/Queens Park Rangers/Queens Park Rangers/Reading FC/Sheffield United/Sheffield Wednesday/Sunderland AFC/na/Wolverhampton Wanderers" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
championship_17_18 <- championship_17_18 %>% mutate(team = na_if(team, 'na'))

efl_cup_16_17 <- temp %>% filter(league == 'efl-cup', season == '16/17') %>% arrange(V1)
efl_cup_16_17 %>% select(V1) %>% 
  unlist() %>% as.vector()

"Accrington Stanley/Arsenal FC/Aston Villa/Barnet FC/Barnsley FC/Birmingham City/Blackburn Rovers/Blackpool FC/Bolton Wanderers/AFC Bournemouth/Bradford City/Brentford FC/Brighton & Hove Albion/Bristol City/Bristol Rovers/Burnley FC/Burton Albion/Bury FC/Cambridge United/Cardiff City/Carlisle United/Charlton Athletic/Chelsea FC/Cheltenham Town/Chesterfield FC/Colchester United/Coventry City/Crawley Town/Crewe Alexandra/Crystal Palace/Derby County/Doncaster Rovers/Everton FC/Exeter City/Fleetwood Town/Fulham FC/Gillingham FC/Grimsby Town/Hartlepool United/Huddersfield Town/Hull City/Ipswich Town/Leeds United/Leicester City/Leyton Orient/Liverpool FC/Luton Town/Manchester City/Manchester United/Mansfield Town/Middlesbrough FC/Millwall FC/Milton Keynes Dons/Morecambe FC/Newcastle United/Newport County/Northampton Town/Norwich City/Nottingham Forest/Notts County/Oldham Athletic/Oxford United/Peterborough United/Plymouth Argyle/Port Vale FC/Portsmouth FC/Preston North End/Queens Park Rangers/Reading FC/Rochdale AFC/Rotherham United/Scunthorpe United/Sheffield United/Sheffield Wednesday/Shrewsbury Town/Southampton FC/Southend United/Stevenage FC/Stoke City/Sunderland AFC/Swansea City/Swindon Town/Tottenham Hotspur/Walsall FC/Watford FC/West Bromwich Albion/West Ham United/Wigan Athletic/AFC Wimbledon/Wolverhampton Wanderers/Wycombe Wanderers/Yeovil Town" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
efl_cup_16_17 <- efl_cup_16_17 %>% mutate(team = na_if(team, 'na'))

fa_cup_16_17 <- temp %>% filter(league == 'fa-cup', season == '16/17') %>% arrange(V1)
fa_cup_16_17 %>% select(V1) %>% 
  unlist() %>% as.vector()
"Accrington Stanley/na/na/na/na/na/na/na/na/Alfreton Town/na/Altrincham FC/na/Arsenal FC/na/Aston Villa/na/na/Barnet FC/Barnsley FC/Barrow AFC/na/na/na/na/na/na/na/na/na/na/Birmingham City/na/na/Blackburn Rovers/Blackpool FC/na/na/Bolton Wanderers/Boreham Wood/na/AFC Bournemouth/Brackley Town/Brackley Town/Bradford City/na/Braintree Town/Brentford FC/na/na/Brighton & Hove Albion/na/na/Bristol City/Bristol Rovers/na/na/na/Burnley FC/na/Burton Albion/Bury FC/na/Cambridge United/na/Cambridge United/na/Cardiff City/Carlisle United/na/Charlton Athletic/na/na/Chelsea FC/Cheltenham Town/Chesham United/Chesham United/na/Chesterfield FC/na/na/na/na/Colchester United/na/na/na/na/Coventry City/Crawley Town/Crewe Alexandra/na/Crystal Palace/Curzon Ashton/Dagenham & Redbridge/na/Dartford FC/na/Derby County/na/Doncaster Rovers/Dover Athletic/na/na/na/na/Eastbourne Borough/Eastbourne Borough/Eastleigh FC/na/na/na/Everton FC/na/Exeter City/na/na/na/na/na/na/na/na/Fleetwood Town/na/na/Fulham FC/na/na/Gillingham FC/na/na/na/na/Grimsby Town/na/na/na/Halifax Town/na/na/na/na/na/Harrow Borough/Hartlepool United/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Huddersfield Town/Hull City/na/na/na/na/na/Ipswich Town/na/na/Kidderminster Harriers/na/na/na/na/na/Leeds United/na/Leicester City/na/na/Leyton Orient/Lincoln City/na/Liverpool FC/Luton Town/Macclesfield Town/na/Maidstone United/Manchester City/Manchester United/Mansfield Town/na/na/na/Merstham FC/na/na/na/Middlesbrough FC/Millwall FC/Milton Keynes Dons/Morecambe FC/na/na/na/Newcastle United/Newport County/na/na/Northampton Town/Norwich City/Nottingham Forest/Notts County/na/Oldham Athletic/na/Oxford United/na/Peterborough United/Plymouth Argyle/na/Port Vale FC/Portsmouth FC/na/na/Preston North End/Queens Park Rangers/Reading FC/na/Rochdale AFC/na/Rotherham United/na/na/na/na/na/Scunthorpe United/na/Sheffield United/Sheffield Wednesday/na/Shrewsbury Town/na/na/na/na/na/Solihull Moors/na/Southampton FC/Southend United/Southport FC/na/Spennymoor/St. Albans City/na/na/Stamford AFC/Stamford AFC/Stevenage FC/Stockport County/Stoke City/Stourbridge FC/Sunderland AFC/Sutton United/na/Sutton United/Sutton United/Swansea City/Swindon Town/na/na/Taunton Town/Taunton Town/na/na/na/na/na/na/Tottenham Hotspur/na/na/na/na/na/Walsall FC/na/na/Watford FC/na/na/West Bromwich Albion/West Ham United/Westfields FC/na/na/na/Whitehawk FC/na/Wigan Athletic/AFC Wimbledon/na/na/na/Woking FC/Wolverhampton Wanderers/na/na/na/na/Wycombe Wanderers/Yeovil Town/na" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
fa_cup_16_17 <- fa_cup_16_17 %>% mutate(team = team)

"na/AFC Dunstable/AFC Fylde/AFC Kempston Rovers/AFC Mansfield/AFC Portchester/AFC Portchester/AFC Sudbury/Aldershot Town/Alfreton Town/Alresford Town/Altrincham/na/na/Ashton Athletic/na/na/Banbury United/na/na/Barrow/Barton Rovers/Barwell/Basingstoke Town/Bath City/Beaconsfield SYCOB/Beaconsfield SYCOB/Bedworth United/Belper Town/Biggleswade Town/Billericay Town/na/Bishop Auckland/Bishop's Stortford/na/na/Blyth Spartans/Bognor Regis Town/na/Boreham Wood/Boston United/na/Brackley Town/Brackley Town/na/Bradford Park Avenue/Braintree Town/na/Bridlington Town/Bridlington Town/na/Brimscombe & Thrupp/Brislington/na/na/Bromley/na/Burgess Hill Town/na/Burscough/na/na/Cadbury Heath/na/Cambridge City/na/Canvey Island/na/na/na/na/Chasetown/Chelmsford City/na/na/Chesham United/Chesham United/Chester/na/Chippenham Town/Chorley/Cirencester Town/Coalville Town/na/Colliers Wood United/Concord Rangers/na/Consett/Coventry United/na/na/na/na/na/Dagenham & Redbridge/Darlington 1883/Dartford/na/na/Dereham Town/na/Dover Athletic/Dulwich Hamlet/Dunston UTS/Dunston UTS/East Thurrock United/Eastbourne Borough/Eastbourne Borough/Eastleigh/Ebbsfleet United/Egham Town/Egham Town/na/Evesham United/na/Farnborough/Farsley Celtic/Faversham Town/FC United of Manchester/FC United of Manchester/Felixstowe & Walton United/Felixstowe & Walton United/Fleet Town/na/Folkestone Invicta/Forest Green Rovers/na/Gainsborough Trinity/Gateshead/na/Gloucester City/Gosport Borough/Grantham Town/Gresley/na/Guiseley/Hadley/Halesowen Town/FC Halifax Town/Hampton & Richmond Borough/Handsworth Parramore/Hanwell Town/Harlow Town/Harrogate Town/Harrow Borough/na/Hastings United/Havant & Waterlooville/Hayes & Yeading United/Hemel Hempstead Town/Hendon/Hereford/Herne Bay/Heybridge Swifts/Highgate United/Highworth Town/Histon/Hitchin Town/Hitchin Town/AFC Hornchurch/na/na/Hungerford Town/Hyde United/Hythe Town/na/Ilkeston/na/AFC Kempston Rovers/Kettering Town/Kidderminster Harriers/Kidsgrove Athletic/Kings Langley/King's Lynn Town/Kirby Muxloe/Lancaster City/na/Leek Town/na/Leiston/Lewes/na/Lincoln City/Lincoln United/na/na/Macclesfield Town/Maidenhead United/Maidstone United/na/na/AFC Mansfield/Margate/Marine/Matlock Town/Merstham/Merthyr Town/Metropolitan Police/Mickleover Sports/na/na/na/na/Morpeth Town/na/Nantwich Town/na/na/North Ferriby United/North Leigh/na/na/na/na/Nuneaton Town/na/Ossett Town/na/Oxford City/na/na/Poole Town/na/na/Potters Bar Town/Potters Bar Town/na/na/na/na/na/na/na/Rugby Town/Rushall Olympic/AFC Rushden & Diamonds/Salford City/Salisbury/na/Sevenoaks Town/na/na/Shildon/na/Skelmersdale United/na/Slimbridge/Slimbridge/Slough Town/Solihull Moors/South Park/na/na/Southport/na/Spennymoor Town/St Albans City/Staines Town/Stalybridge Celtic/Stamford/Stamford/na/Stockport County/na/Stourbridge/na/Sutton United/Sutton Coldfield Town/Sutton United/Sutton United/na/na/Swindon Supermarine/Tamworth/Taunton Town/Taunton Town/AFC Telford United/na/Thamesmead Town/na/Tonbridge Angels/Torquay United/na/Trafford/Tranmere Rovers/Uxbridge/VCD Athletic/na/na/Waltham Forest/Walton Casuals/na/Wealdstone/Welling United/na/na/Westfields/Weston-super-Mare/Weymouth/Whitby Town/Whitehawk/Whyteleafe/na/na/Winchester City/Witham Town/Witton Albion/Woking/na/Worcester City/Workington/Worthing/Wrexham/na/na/York City" %>% 
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team2

fa_cup_16_17 <- fa_cup_16_17 %>% mutate(qualyfing_team = recode(team2,
                                                                'Altrincham' = 'Altrincham FC',
                                                                'Barrow' = 'Barrow AFC',
                                                                'Dartford' = 'Dartford FC',
                                                                'Eastleigh' = 'Eastleigh FC',
                                                                'FC Halifax Town' = 'Halifax Town',
                                                                'AFC Mansfield' = 'Mansfield Town',
                                                                'Merstham' = 'Merstham FC',
                                                                'Southport' = 'Southport FC',
                                                                'Spennymoor Town' = 'Spennymoor',
                                                                'St Albans City' = 'St. Albans City',
                                                                'Stamford' = 'Stamford AFC',
                                                                'Stourbridge' = 'Stourbridge FC',
                                                                'Westfields' = 'Westfields FC',
                                                                'Whitehawk' = 'Whitehawk FC',
                                                                'Woking' = 'Woking FC'))

fa_cup_16_17 <- fa_cup_16_17 %>% mutate(team = na_if(team, 'na'), qualyfing_team = na_if(qualyfing_team, 'na'), 
                                        team = coalesce(team, qualyfing_team)) %>% select(-qualyfing_team)


league_1_16_17 <- temp %>% filter(league == 'league-1', season == '16/17') %>% arrange(V1)
league_1_16_17 %>% select(V1) %>% 
  unlist() %>% as.vector()

'na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Bolton Wanderers/na/na/Bradford City/na/na/Bristol Rovers/na/Bury FC/na/na/na/na/na/na/na/Charlton Athletic/na/na/Chesterfield FC/na/na/na/na/Coventry City/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Fleetwood Town/na/na/na/na/na/na/na/na/na/na/Gillingham FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Millwall FC/na/Milton Keynes Dons/na/na/na/na/na/na/na/na/na/na/na/Northampton Town/na/na/na/Oldham Athletic/na/na/Oxford United/na/na/na/na/na/Peterborough United/na/na/na/Port Vale FC/na/na/na/na/na/Rochdale AFC/na/na/na/na/na/na/na/na/Scunthorpe United/na/na/na/Sheffield United/na/Shrewsbury Town/na/na/Southend United/na/na/na/na/na/na/na/na/Swindon Town/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Walsall FC/na/na/na/AFC Wimbledon/na/na/na'%>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
league_1_16_17 <- league_1_16_17 %>% mutate(team = na_if(team, 'na'))

league_1_17_18 <- temp %>% filter(league == 'league-1', season == '17/18') %>% arrange(V1)
league_1_17_18 %>% select(V1) %>% 
  unlist() %>% as.vector()

"Blackburn Rovers/Blackpool FC/Bradford City/Bristol Rovers/Bury FC/Charlton Athletic/na/Doncaster Rovers/Fleetwood Town/Gillingham FC/Milton Keynes Dons/Northampton Town/na/Oldham Athletic/Oxford United/Peterborough United/Plymouth Argyle/Portsmouth FC/Rochdale AFC/Rotherham United/Scunthorpe United/Shrewsbury Town/Southend United/Walsall FC/Wigan Athletic/AFC Wimbledon" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
league_1_17_18 <- league_1_17_18 %>% mutate(team = na_if(team, 'na'))

league_2_16_17 <- temp %>% filter(league == 'league-2', season == '16/17') %>% arrange(V1)
league_2_16_17 %>% select(V1) %>% 
  unlist() %>% as.vector()
team <- "na/Accrington Stanley/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Barnet FC/na/na/na/na/na/na/na/Blackpool FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Cambridge United/Cambridge United/na/na/Carlisle United/na/na/na/na/Cheltenham Town/na/na/Colchester United/na/Crawley Town/Crewe Alexandra/na/na/na/na/na/na/na/na/Doncaster Rovers/na/na/na/na/Exeter City/na/na/na/na/na/na/na/na/na/na/na/na/Grimsby Town/na/na/na/na/Hartlepool United/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Leyton Orient/na/na/na/Luton Town/na/na/na/na/na/Mansfield Town/na/na/na/na/na/na/na/na/na/na/na/Morecambe FC/na/na/na/na/Newport County/na/na/Notts County/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Plymouth Argyle/na/Portsmouth FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Stevenage FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Wycombe Wanderers/Yeovil Town/na/na/na/na"%>%
  strsplit(split = '/') %>% unlist() %>% as.vector() 
league_2_16_17 <- league_2_16_17 %>% mutate(team = na_if(team, 'na'))

league_2_17_18 <- temp %>% filter(league == 'league-2', season == '17/18') %>% arrange(V1)
league_2_17_18 %>% select(V1) %>% 
  unlist() %>% as.vector()
"Accrington Stanley/Barnet FC/Cambridge United/Cambridge United/Carlisle United/Cheltenham Town/Chesterfield FC/Colchester United/Coventry City/Crawley Town/Crewe Alexandra/Exeter City/Forest Green Rovers/Grimsby Town/Lincoln City/Luton Town/Mansfield Town/Morecambe FC/Newport County/Notts County/Port Vale FC/na/Stevenage FC/Swindon Town/na/Wycombe Wanderers/Yeovil Town"   %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
league_2_17_18 <- league_2_17_18 %>% mutate(team = na_if(team, 'na'))


national_league_16_17 <- temp %>% filter(league == 'national-league', season == '16/17') %>% arrange(V1)
national_league_16_17 %>% select(V1) %>% unlist() %>% as.vector()
"na/na/Aldershot Town/na/na/na/na/na/na/na/na/na/na/Barrow AFC/na/na/na/na/na/na/Boreham Wood/na/na/na/Braintree Town/Bromley FC/na/na/na/na/na/na/na/na/Chester FC/na/na/na/na/na/Dagenham & Redbridge/na/na/na/na/Dover Athletic/Eastleigh FC/na/na/na/na/na/na/na/Forest Green Rovers/na/na/na/Gateshead FC/na/na/na/na/Guiseley AFC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Lincoln City/na/na/na/Macclesfield Town/Maidstone United/na/na/na/na/na/na/na/na/na/na/North Ferriby United/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Solihull Moors/na/na/na/Southport FC/na/na/na/na/na/na/Sutton United/Sutton United/na/na/na/na/na/Torquay United/na/Tranmere Rovers/na/na/na/na/na/na/na/na/na/Woking FC/Wrexham AFC/York City/York City" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
national_league_16_17 <- national_league_16_17 %>% mutate(team = na_if(team, 'na'))

national_league_17_18 <- temp %>% filter(league == 'national-league', season == '17/18') %>% arrange(V1)
national_league_17_18 %>% select(V1) %>% unlist() %>% as.vector()
"AFC Fylde/Aldershot Town/Barrow AFC/Boreham Wood/Bromley FC/Chester FC/Dagenham & Redbridge/Dover Athletic/Eastleigh FC/Ebbsfleet United/Gateshead FC/Guiseley AFC/Halifax Town/Hartlepool United/Leyton Orient/Macclesfield Town/Maidenhead United/Maidstone United/na/Solihull Moors/Sutton United/Sutton United/Torquay United/Tranmere Rovers/Woking FC/Wrexham AFC/na" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
national_league_17_18 <- national_league_17_18 %>% mutate(team = na_if(team, 'na'))


premier_league_16_17 <- temp %>% filter(league == 'premier-league', season == '16/17') %>% arrange(V1)
premier_league_16_17 %>% select(V1) %>% unlist() %>% as.vector()
"na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Arsenal FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/AFC Bournemouth/na/na/na/na/Burnley FC/na/na/na/na/na/na/na/na/Chelsea FC/na/na/na/na/na/na/na/na/na/na/Crystal Palace/na/na/na/na/na/na/na/na/na/na/na/na/na/Everton FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Hull City/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Leicester City/na/na/na/na/Liverpool FC/na/na/na/na/na/Manchester City/Manchester United/na/na/na/Middlesbrough FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Southampton FC/na/na/na/na/na/Stoke City/na/Sunderland AFC/Swansea City/na/na/na/na/na/Tottenham Hotspur/na/na/na/na/na/na/na/na/na/na/na/Watford FC/na/West Bromwich Albion/West Ham United/na/na/na/na/na/na/na/na/"%>%  
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
premier_league_16_17 <- premier_league_16_17 %>% mutate(team = na_if(team, 'na'))

premier_league_17_18 <- temp %>% filter(league == 'premier-league', season == '17/18') %>% arrange(V1)
premier_league_17_18 %>% select(V1) %>% unlist() %>% as.vector()
"Arsenal FC/AFC Bournemouth/Brighton & Hove Albion/Burnley FC/Chelsea FC/Crystal Palace/Everton FC/Huddersfield Town/Leicester City/Liverpool FC/Manchester City/Manchester United/Newcastle United/na/Southampton FC/na/Stoke City/Swansea City/Tottenham Hotspur/Watford FC/West Bromwich Albion/West Ham United"%>%  
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
premier_league_17_18 <- premier_league_17_18 %>% mutate(team = na_if(team, 'na'))


copa_del_rey_16_17 <- temp %>% filter(league == 'copa-del-rey', season == '16/17') %>% arrange(V1)
copa_del_rey_16_17 %>% select(V1) %>% unlist() %>% as.vector()

'CD Alavés/Albacete/AD Alcorcón/CD Alcoyano/UD Almería/SD Amorebieta/Andorra CF/Arenas de Getxo/Athletic Bilbao/Atlético Madrid/Atlético Mancha Real/Atlético Saguntino/Atlético Sanluqueño CF/Barakaldo CF/FC Barcelona/Real Betis/Burgos CF/Cádiz CF/Caudal Deportivo/CD Boiro/CD Calahorra/Celta Vigo/Lorca Deportiva/na/CA Cirbonero/UB Conquense/Córdoba CF/Deportivo La Coruña/na/SD Eibar/Elche CF/Espanyol Barcelona/Extremadura UD/FC Cartagena/Sevilla FC/Racing Ferrol/SD Formentera/Getafe CF/Sporting Gijón/Gimnàstic/Gimnàstic/Girona FC/Granada CF/CD Guijuelo/Hércules CF/SD Huesca/SD Laredo/UD Las Palmas/na/CD Leganés/CD Leonesa/Levante UD/Lleida Esportiu/na/Lorca FC/CD Lugo/Málaga CF/RCD Mallorca/CD Mirandés/CD Numancia/CA Osasuna/Real Oviedo/na/AE Prat/Racing Santander/Rayo Vallecano/Real Madrid/Real Murcia/Real Sociedad/Real Unión/CF Reus Deportiu/na/UD SS Reyes/na/SD Formentera/Sestao River/Sevilla FC/UD Socuéllamos/CD Tenerife/na/CD Toledo/CD Tudelano/UCAM Murcia CF/UE Cornellà/UD Las Palmas/UD Logroñés/UE Cornellà/UE Llagostera/Valencia CF/Real Valladolid/UD Villa Santa Brígida/Villarreal CF/Zamora CF/SD Zamudio/Real Zaragoza'%>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
copa_del_rey_16_17 <- copa_del_rey_16_17 %>% mutate(team = na_if(team, 'na'))

primera_16_17 <- temp %>% filter(league == 'la-liga-primera', season == '16/17') %>% arrange(V1)
primera_16_17 %>% select(V1) %>% unlist() %>% as.vector()

"na/na/na/na/CD Alavés/na/na/na/na/na/na/Athletic Bilbao/na/Atlético Madrid/na/na/na/na/na/na/FC Barcelona/na/na/na/na/na/na/na/Real Betis/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Celta Vigo/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Deportivo La Coruña/na/na/na/na/na/na/na/na/na/SD Eibar/na/na/Espanyol Barcelona/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Sporting Gijón/Granada CF/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/UD Las Palmas/na/na/CD Leganés/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Málaga CF/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/CA Osasuna/na/na/na/na/na/na/na/na/na/na/na/na/na/Real Madrid/Real Sociedad/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Sevilla FC/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/na/Valencia CF/na/na/na/na/na/na/Villarreal CF/na/na/na/na/na/na/na/na/na/na/na/na/na/na" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
primera_16_17 <- primera_16_17 %>% mutate(team = na_if(team, 'na'))

primera_17_18 <- temp %>% filter(league == 'la-liga-primera', season == '17/18') %>% arrange(V1)
primera_17_18 %>% select(V1) %>% unlist() %>% as.vector()

'CD Alavés/Athletic Bilbao/Atlético Madrid/FC Barcelona/Real Betis/Celta Vigo/Deportivo La Coruña/SD Eibar/Espanyol Barcelona/Sevilla FC/na/Getafe CF/Girona FC/UD Las Palmas/CD Leganés/Levante UD/Málaga CF/na/Real Madrid/Real Sociedad/Sevilla FC/UD Las Palmas/Valencia CF/Villarreal CF' %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
primera_17_18 <- primera_17_18 %>% mutate(team = na_if(team, 'na'))

segunda_16_17 <- temp %>% filter(league == 'la-liga-segunda', season == '16/17') %>% arrange(V1)
segunda_16_17 %>% select(V1) %>% unlist() %>% as.vector()
"AD Alcorcón/na/UD Almería/na/na/na/na/na/na/na/na/na/na/Cádiz CF/na/na/na/na/na/na/na/na/na/na/Córdoba CF/na/na/na/na/na/Elche CF/na/na/na/na/na/na/na/na/na/na/na/Getafe CF/na/Gimnàstic/Gimnàstic/Girona FC/na/na/na/na/na/na/SD Huesca/na/na/na/na/na/na/na/na/Levante UD/na/na/na/CD Lugo/na/na/RCD Mallorca/na/na/na/CD Mirandés/na/na/na/na/na/na/na/na/CD Numancia/na/na/Real Oviedo/na/na/na/na/na/na/na/Rayo Vallecano/na/CF Reus Deportiu/na/na/na/na/na/na/na/na/na/SD Huesca/Sevilla Atlético/Sevilla Atlético/na/na/na/na/na/na/na/na/na/na/CD Tenerife/na/na/na/na/na/UCAM Murcia CF/na/na/na/Real Valladolid/na/na/na/Real Zaragoza/na/na"%>%  
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
segunda_16_17 <- segunda_16_17 %>% mutate(team = na_if(team, 'na'))

segunda_17_18 <- temp %>% filter(league == 'la-liga-segunda', season == '17/18') %>% arrange(V1)
segunda_17_18 %>% select(V1) %>% unlist() %>% as.vector()
"Albacete/AD Alcorcón/UD Almería/FC Barcelona B/Cádiz CF/Córdoba CF/Sporting Gijón/Gimnàstic/Gimnàstic/Granada CF/na/CD Leonesa/Lorca FC/CD Lugo/CD Numancia/CA Osasuna/Real Oviedo/na/Rayo Vallecano/CF Reus Deportiu/Gimnàstic/SD Huesca/Sevilla Atlético/CD Tenerife/UD Almería/Real Valladolid/Real Zaragoza/"%>%  
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
segunda_17_18 <- segunda_17_18 %>% mutate(team = na_if(team, 'na'))

champions_league_16_17 <- temp %>% filter(league == 'champions-league', season == '16/17') %>% arrange(V1)
champions_league_16_17 %>% select(V1) %>% unlist() %>% as.vector()

"AFC Ajax/FC Alashkert/RSC Anderlecht/APOEL Nikosia/Arsenal FC/Astra Giurgiu/Atlético Madrid/na/na/na/B36 Tórshavn/FC Barcelona/FC Basel/BATE Borisov/Bayer Leverkusen/Bayern München/SL Benfica/Beşiktaş/Borussia Dortmund/Bor. Mönchengladbach/Bor. Mönchengladbach/na/Celtic FC/na/Club Brugge KV/Crusaders FC/Crvena Zvezda/CSKA Moskva/na/na/Dinamo Zagreb/Dinamo Tbilisi/F91 Dudelange/na/Dundalk FC/Dinamo Kiev/na/FK Astana/FC København/FC Santa Coloma/FC Sheriff/Fenerbahçe/Ferencvárosi TC/Qarabağ FK/FK Liepāja/FK Rostov/Vardar Skopje/FC Flora/na/FH Hafnarfjörður/na/Hapoel Be'er Sheva/na/Juventus/FK Liepāja/na/Legia Warszawa/Leicester City/Lincoln Red Imps/PFC Ludogorets Razgrad/PFC Ludogorets Razgrad/PFC Ludogorets Razgrad/Olympique Lyon/Manchester City/na/Mladost Podgorica/AS Monaco/na/SSC Napoli/na/na/IFK Norrköping/Olimpija Ljubljana/Olympiakos Piräus/na/na/na/na/PAOK Saloniki/FK Partizani/Viktoria Plzeň/FC Porto/na/Paris Saint-Germain/PSV Eindhoven/na/Real Madrid/AS Roma/Rosenborg BK/FK Rostov/RB Salzburg/na/na/Sevilla FC/Shakhtar Donetsk/SJK Seinäjoki/SP Tre Penne/Sparta Praha/Sporting CP/Steaua Bucureşti/na/na/na/The New Saints/Tottenham Hotspur/FK AS Trenčín/na/na/na/Valletta FC/Villarreal CF/na/na/BSC Young Boys/na/FK Žalgiris/Zrinjski Mostar" %>%
  strsplit(split = '/') %>% unlist() %>% as.vector() -> team
champions_league_16_17 <- champions_league_16_17 %>% mutate(team = na_if(team, 'na'))

team_name <- rbind(champions_league_16_17, 
                   championship_16_17, championship_17_18, 
                   efl_cup_16_17, fa_cup_16_17, 
                   league_1_16_17, league_1_17_18, league_2_16_17, league_2_17_18,
                   national_league_16_17, national_league_17_18, 
                   premier_league_16_17, premier_league_17_18,
                   copa_del_rey_16_17, primera_16_17, primera_17_18, segunda_16_17, segunda_17_18)

list <- 'champions_league_16_17, championship_16_17, championship_17_18, efl_cup_16_17, fa_cup_16_17,league_1_16_17, league_1_17_18,league_2_16_17, league_2_17_18,national_league_16_17, national_league_17_18, premier_league_16_17, premier_league_17_18,copa_del_rey_16_17, primera_16_17, primera_17_18, segunda_16_17, segunda_17_18' %>% strsplit(split = ",") %>% unlist() %>% as.vector() %>% trimws()
rm(list = list)
rm(list)

data <- data %>% 
  left_join(team_name, by = c('nation', 'league', 'season','V1')) %>% 
  group_by(id) %>%
  mutate(n_team = sum(!is.na(team))) %>%
  ungroup()
# Keep only id that have 2 teams
data <- data %>%
  mutate(remove = if_else(n_team != 2, T, remove))
#recode 'Draw' to team
data <- data %>% mutate(team = ifelse(V1 == 'Draw', 'Draw', team))
data <- data %>% mutate_at(paste0('V', 2:30),.funs = ~if_else(str_detect(.,'/0+'), NA_character_, .))
data <- data %>% mutate(league = recode(league, "league-1" = "league-one", "league-2" = "league-two")) 



########################################################################
#########        DATA CLEANING: RECODE MATCH TIME       ################
########################################################################

data %>% filter(remove == F) %>% 
  summarise_all(.funs = ~sum(is.na(.))) %>% as.data.frame()

data <- data %>% mutate(ts = recode(ts, 
                                    '2017-03-26 03:45:42 GMT+2' = '2017-03-26 04:45:42 GMT+3', 
                                    '2017-03-26 03:45:44 GMT+2' = '2017-03-26 04:45:44 GMT+3'),
                        ts = ymd_hms(str_remove(ts, '\ GMT\\+\\d'), tz = 'EET'))

# recode NA in match_time
#count NA = 1224
data %>% filter(remove == F, is.na(match_time)) %>% count()
data %>% filter(remove == F, is.na(match_time)) %>%
  select(id, file_path) %>% distinct() #408 ids have missing match_time

#file contain NA in match_time = 303 file
data %>% filter(remove == F, is.na(match_time)) %>% select(file_path) %>% distinct() %>% count()

temp <- data %>% 
  filter(remove == F, is.na(match_time)) %>% 
  select(file_path) %>% distinct() %>%
  left_join(data, by = 'file_path') %>%
  filter(remove == F) %>%
  select(id, ts, match_time, file_path) %>% distinct()

temp2 <- temp %>% filter(!is.na(match_time)) %>% 
  group_by(match_time, file_path) %>%
  summarise(start = min(ts), end = max(ts), start_id = min(id), end_id = max(id)) %>%
  ungroup() %>%
  mutate(fake = 1)
temp3 <- temp %>% filter(is.na(match_time)) %>% mutate(fake = 1)
temp4 <- temp2 %>% full_join(temp3, by = 'fake') %>% select(-fake) %>%
  filter(id >= start_id, id <= end_id, ts >= start, ts <= end)

# find the ids that belong to files that have only 1 match_time
temp5 <- data %>% filter(remove == F, !is.na(match_time)) %>%
  select(file_path, match_time) %>% distinct() %>% 
  group_by(file_path) %>%
  add_count(file_path, name = 'n_time') %>%
  ungroup() %>%
  filter(n_time == 1) %>%
  select(file_path) %>%
  left_join(data, by = 'file_path') %>%
  select(id) %>% distinct()
# if no match_time in those id, recode it with the correct one.
temp6 <- data %>% filter(remove == F, is.na(match_time)) %>%
  select(id, match_time, file_path) %>% 
  semi_join(temp5, by = 'id') %>% 
  left_join(data, by = 'file_path') %>%
  select(id = id.x, match_time = match_time.y) %>%
  filter(!is.na(match_time)) %>%
  distinct()

temp4 %>% select(id, match_time = match_time.x) %>% full_join(temp6, by = 'id') -> temp7

data <- temp7 %>% 
  mutate(match_time = coalesce(match_time.x, match_time.y)) %>% 
  select(id, match_time) %>%
  right_join(data, by = 'id') %>%
  mutate(match_time = coalesce(match_time.x, match_time.y)) %>%
  select(remove, everything(), -match_time.x, -match_time.y)

# 3 special case is recode manually:
data <- data %>% mutate(match_time = case_when(id == 511509 ~ '2018-01-21 11:00',
                                               id == 511722 ~ '2018-01-21 15:15',
                                               id == 515005 ~ '2018-01-13 19:30',
                                               TRUE ~ match_time))



########################################################################
######### READ MATCH RESULTS AND MATCH RESULTS WITH odds ################
########################################################################
# Read data of match result
match_result <- read.csv(match_result_file, stringsAsFactors = F)

# Create matching dataframe which contain match_id from data of match result and id from data of betting odds
matching <- data %>% filter(remove == F) %>%
  select(id, nation, league, season, team, match_time) %>%
  filter(team != 'Draw') %>%
  group_by(id) %>%
  mutate(role = c('team1', 'team2')) %>%
  ungroup() %>%
  spread(key = role, value = team) %>%
  separate(match_time, into = c('date', 'time'), sep = ' ')

matching <- matching %>% left_join(match_result, by = c('nation', 'league', 'season', 'date')) %>%
  mutate(home1 = team1 == home,
         home2 = team2 == home,
         away1 = team1 == away,
         away2 = team2 == away,
         correct = home1 + home2 +away1 +away2) %>%
  filter(correct == 2) %>%
  select(id, match_id)



# Match result with odds to have the cleaned data:
data <- matching %>% left_join(match_result, by = 'match_id') %>%
  left_join(data, by = c('id', 'nation', 'league', 'season')) %>%
  mutate(role = case_when(team == home ~ 'home_team',
                          team == away ~ 'away_team',
                          team == 'Draw' ~ 'draw',
                          TRUE ~ NA_character_),
         match_time = ymd_hm(match_time, tz = 'UTC')) %>%
  select(id, match_id, ts, match_time, date, time, nation, league, season, team, role, outcome, computer, file_path, V2:V30)

data <- data %>% 
  mutate(time = ifelse(!is.na(time), time, substr(match_time, 12, 16))) %>%
  mutate(match_time = ymd_hm(paste(date, time, sep = ' '), tz = 'GMT'))

data <- data %>% gather(key  = 'bookie', value = 'odds', -c(id:file_path))
data <- data %>% filter(!is.na(odds))


# Transform fractional odds to decimal odds:
decimal_data <- data %>%
  separate(odds, into = c('num', 'den'), sep = '/', convert = T, fill = 'right') %>%
  mutate(odds = 1  + if_else(is.na(den), num*1.0, num*1.0/den)) %>%
  select(-num, -den) %>%
  mutate(match_time = ymd_hm(match_time), 
         role = recode(role, home_team = 'H', away_team = 'A', draw = 'D')) %>%
  spread(key = 'bookie', value = 'odds')

# Remove odds of a bookmaker if the bookmaker only provide 1 or 2 of 1X2 Bet.
decimal_data_with_completed_set <- decimal_data %>%
  gather(key  = 'bookie', value = 'odds', -c(id:file_path))
decimal_data_with_completed_set <- decimal_data_with_completed_set %>%
  select(id, match_id, role, bookie, odds) %>%
  spread(key = 'role', value = 'odds')
decimal_data_with_completed_set <- decimal_data_with_completed_set %>%
  filter(!is.na(A), !is.na(H), !is.na(D))
decimal_data_with_completed_set <- decimal_data_with_completed_set %>%
  gather(key = 'role', value = 'odds', -c(id:bookie))
decimal_data_with_completed_set <- decimal_data_with_completed_set %>%
  spread(key = 'bookie', value = 'odds')

decimal_data_with_completed_set <- decimal_data %>% 
  select(id:file_path) %>%
  inner_join(decimal_data_with_completed_set, by = c('id', 'match_id', 'role'))
                             
                             
                                                        
