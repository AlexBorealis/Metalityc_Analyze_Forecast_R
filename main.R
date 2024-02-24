## Setting working directory ----
setwd(getwd())

## Loading workspace of project ----
load(paste0(getwd(), "/", list.files(getwd(), pattern = ".RData")))

## Loading need packages ----
source(paste0(getwd(), "/required_pckgs.R"), local = T)

## Loading data processing engine ----
source(paste0(getwd(), "/required_vars.R"), local = T)

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

sides <- c('home', 'away')

team_name <- c('Pittsburgh Penguins', 'Los Angeles Kings')

sp = 4

n = 1

days = 4

if (sp == 1) {
  
  a_cor = .2
  
  conf_int = 80
  
} else {
  
  a_cor = .1
  
  conf_int = 70
  
}

## 1) Getting list of events on day
# fun_events_list_g(days = 1, sport = sp)

## 2) Getting table of tournament in league
# fun_tournament_tables(sport = sp, t_season_id = 'jDTEm9zs', t_stage_id = 'I3O5jpB2')

## 3) Getting table of fixtures for team from league
# fun_teams_fixtures_events(sport = 4, tm_id = 'CIDo8i2o')

## 4) Getting table hth for team from league
# fun_event_hth_g(ev_id = 'QZZjDQzR', sport = sp)

# table_events_id <- need_ids(sport = sp) |>
#   group_by(event_id) |>
#   filter(row_number() == 1) |>
#   #group_by(tab_name, group_label) |>
#   #filter(row_number() %in% 1:10) |>
#   as.data.table() # Getting table of ids for sport
# 
# events_id <- table_events_id$event_id

# system.time(
# 
#   fun_event_statistics_g(ev_id = events_id[1:115], sport = sp)
# 
# )

#mean_table <- map_dfr(teams_id, \(i) mean_goals(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

#mean_int_table <- map_dfr(teams_id, \(i) mean_goals_int(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

## Creation main table ----

DT <- tab_for_an(team_name = team_name[n], st_name = 'match', side = sides[n], 
                 sport = sp, part = 2, date = Sys.Date() - 200)

DT_ts <- create_ts(DT, except_vars = list_except_vars, all_vars = T, sport = sp, days = 30)

DT_cor_matrix <- tab_cor(tbl = DT, indep_vars = list_indep_vars, except_vars = list_except_vars, sport = sp, a = a_cor)

## Testing forecasting variables
indep_models <- creation_models_arima(tbl = DT_ts,
                                      ts_vars = list_indep_vars,
                                      sport = sp,
                                      a = .05,
                                      lambda = 'auto',
                                      biasadj = T)

indep_models_f <- forecasting_models_arima(tbl = DT_ts,
                                           ts_vars = list_indep_vars,
                                           models = indep_models,
                                           sport = sp,
                                           level = conf_int,
                                           h = 20)

## Creation forecasting tables
if (sp == 3) {
  
  as.data.table(cbind(names(indep_models_f$mean_values),
                      t(indep_models_f$lower_values[days]),
                      t(indep_models_f$mean_values[days]),
                      t(indep_models_f$upper_values[days]))) |>
    rename_all(~ c('incidents', 'lower_value', 'mean_value', 'upper_value'))
  
} else {
  
  main_forecast_lower <- forecasting_models_rf(DT_ts, 
                                               indep_vars = list_indep_vars, dep_vars = list_result_vars,
                                               forecast_models = indep_models_f, sport = sp, level_value = 1,
                                               ntree = 250,
                                               side = sides[n])
  
  main_forecast_mean <- forecasting_models_rf(DT_ts, 
                                              indep_vars = list_indep_vars, dep_vars = list_result_vars,
                                              forecast_models = indep_models_f, sport = sp, level_value = 2,
                                              ntree = 250,
                                              side = sides[n])
  
  main_forecast_upper <- forecasting_models_rf(DT_ts, 
                                               indep_vars = list_indep_vars, dep_vars = list_result_vars,
                                               forecast_models = indep_models_f, sport = sp, level_value = 3,
                                               ntree = 250,
                                               side = sides[n])
  
  as.data.table(cbind(names(main_forecast_lower),
                      t(main_forecast_lower[days]),
                      t(main_forecast_mean[days]),
                      t(main_forecast_upper[days]))) |>
    rename_all(~ c('incidents', 'lower_value', 'mean_value', 'upper_value'))
  
}

#save.image(getwd(), "/", list.files(getwd(), pattern = ".RData"))