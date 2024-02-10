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
DT <- tab_for_an(team_name = team_name[1], st_name = '4th quarter', a = .01,
                 side = 'home', sport = 3, part = 4, date = Sys.Date() - 200)

## Testing forecasting variables
models <- creation_models(tbl = DT,
                          names_of_vars = DT$independent_variables,
                          a = .01,
                          spline = F)

forecast_models <- forecasting_models(tbl = DT,
                                      h = max(cumsum(days_between_games_t1)[1:3]),
                                      names_of_vars = DT$independent_variables,
                                      models = models)

## Forecasting variables
dep_models <- creation_models(tbl = DT,
                              names_of_vars = inc_names[c(1, 3, 5)],
                              indep_vars = F,
                              a = .01,
                              spline = F)

forecast_dep_models <- forecasting_models(tbl = DT,
                                          models = dep_models,
                                          forecast_models1 = forecast_models$mean_values,
                                          names_of_vars = inc_names[c(1, 3, 5)],
                                          indep_vars = F,
                                          h = max(cumsum(days_between_games_t1)[1:3]))

#save.image(getwd(), "/", list.files(getwd(), pattern = ".RData"))