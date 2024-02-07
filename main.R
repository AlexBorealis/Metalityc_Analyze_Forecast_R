## Setting working directory ----
setwd(getwd())

## Loading workspace of project ----
load(paste0(getwd(), "/", list.files(getwd(), pattern = ".RData")))

## Loading need packages ----
source(paste0(getwd(), "/need_pckgs.R"), local = T)

## Loading data processing engine ----
source(paste0(getwd(), "/need_vars.R"), local = T)

## Loading data processing engine (main functions) ----
source(paste0(getwd(), "/flashlive_engine_(main_funs).R"), local = T)

## Loading data processing engine (grouping functions) ----
source(paste0(getwd(), "/flashlive_engine_(grouped_tbls).R"), local = T)

## Loading data processing engine (functions for pre-processing data) ----
source(paste0(getwd(), "/prep_data.R"), local = T)

## Loading data processing engine (functions for an analyzing) ----
source(paste0(getwd(), "/analyzing.R"), local = T)

## Loading data processing engine (functions for a forecasting) ----
source(paste0(getwd(), "/forecasting.R"), local = T)

#system.time(
  
#  fun_event_statistics_g(ev_id = need_events_id$event_id)
  
#)

#mean_table <- map_dfr(teams_id, \(i) mean_goals(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

#mean_int_table <- map_dfr(teams_id, \(i) mean_goals_int(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

## Creation main table ----
DT <- tab_for_an(team_name = team_name[2], st_name = '4th quarter',
                 side = 'away', sport = 3, part = 4, date = Sys.Date() - 200)

## Testing forecasting variables
models <- creation_models(tbl = DT,
                          names_of_vars = DT$independent_variables,
                          a = .05)

forecast_models <- forecasting_models(tbl = DT,
                                      h = max(cumsum(days_between_games_t2)),
                                      names_of_vars = DT$independent_variables,
                                      models = models)

## Forecasting variables
dep_models <- creation_models(tbl = DT,
                              names_of_vars = inc_names[c(2, 4, 6)],
                              indep_vars = F,
                              a = .05)

forecast_made_models <- forecasting_models(tbl = DT,
                                           models = dep_models,
                                           forecast_models1 = forecast_models$lower_values,
                                           names_of_vars = inc_names[c(2, 4, 6)],
                                           indep_vars = F,
                                           h = max(cumsum(days_between_games_t2)))

dt <- forecast_made_models$lower_values[days_between_games_t2] |>
  mutate_all(as.numeric) |>
  mutate(score_full = 2 * two_point_field_goals_made + 3 * three_point_field_goals_made + free_throws_made)

dt1 <- forecast_made_models$mean_values[days_between_games_t2] |>
  mutate_all(as.numeric) |>
  mutate(score_full = 2 * two_point_field_goals_made + 3 * three_point_field_goals_made + free_throws_made)

dt2 <- forecast_made_models$upper_values[days_between_games_t2] |>
  mutate_all(as.numeric) |>
  mutate(score_full = 2 * two_point_field_goals_made + 3 * three_point_field_goals_made + free_throws_made)

dt
dt1
dt2
#save.image(getwd(), "/", list.files(getwd(), pattern = ".RData"))