api_flashlive <- data.table(read.table("api_flashlive_vars.txt", header = T))[name == "api_ya", value] # API key

host_flashlive <- data.table(read.table("api_flashlive_vars.txt", header = T))[name == "host", value] # Main host

endpoints <- jsonlite::read_json("endpoints.json", simplifyVector = T) # List needed endpoints

locale <- data.table(read.table("locale_vars.txt", header = T)) # Table of locales

standing_type <- data.table(read.table("standing_type_vars.txt", header = T)) # Table of standings

## Creation required variables for forecasting

list_indep_vars <- list(c('corner_kicks', 'fouls', 'free_kicks', 'blocked_shots', 'ball_possession',
                          'dangerous_attacks', 'shots_off_goal', 'shots_on_goal', 'offsides', 
                          'tackles', 'throw_ins', 'attacks', 'total_passes', 'expected_goals'),
                        c('assists', 'blocks', 'defensive_rebounds', 'offensive_rebounds',
                          'personal_fouls', 'steals', 'technical_fouls', 'turnovers',
                          'two_point_field_g._attempted', 'three_point_field_g._attempted', 'free_throws_attempted',
                          'two_point_field_goals_made', 'three_point_field_goals_made', 'free_throws_made'),
                        c('blocked_shots', 'empty_net_goals', 'faceoffs_won',
                          'giveaways', 'hits', 'penalties', 'power_play_goals', 
                          'shorthanded_goals', 'shots_off_goal', 'shots_on_goal'))

names(list_indep_vars) <- sport_list[id %in% c(1, 3, 4), name]

list_dep_vars <- list(NULL, 
                      NULL,
                      NULL)

names(list_dep_vars) <- sport_list[id %in% c(1, 3, 4), name]

list_result_vars <- list(c('home_score_full', 'away_score_full', 'goalkeeper_saves'), 
                         NULL,
                         c('home_score_full', 'away_score_full', 'goalkeeper_saves'))

names(list_result_vars) <- sport_list[id %in% c(1, 3, 4), name]

list_except_vars <- list(c('days_between_games', 'yellow_cards', 'goal_attempts'),
                         c('two_point_field_goals', 'three_point_field_goals',
                           'field_goals', 'field_goals_attempted', 'field_goals_made',
                           'free_throws', 'total_rebounds', 'days_between_games'),
                         c('pen._killing_pct', 'power_play_pct', 'saves_pct',
                           'shooting_pct', 'faceoffs', 'days_between_games', 'pim', 'takeaways'))

names(list_except_vars) <- sport_list[id %in% c(1, 3, 4), name]
