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

sp = 3

d = 1:5

## 1) Getting list of events on day
fun_events_list_g(days = 1, sport = sp)

## 2) Getting table of tournament in league
# fun_tournament_tables(sport = sp, t_season_id = 'jDTEm9zs', t_stage_id = 'I3O5jpB2')

## 3) Getting table of fixtures for team from league
# fun_teams_fixtures_events(sport = 4, tm_id = 'CIDo8i2o')

## 4) Getting table hth for team from league
fun_event_hth_g(ev_id = 'QZZjDQzR', sport = sp)

table_events_id <- need_ids(sport = sp) |>
  group_by(event_id) |>
  filter(row_number() == 1) |>
  #group_by(tab_name, group_label) |>
  #filter(row_number() %in% 1:10) |>
  as.data.table() # Getting table of ids for sport

events_id <- table_events_id$event_id

# system.time(
# 
#   fun_event_statistics_g(ev_id = events_id[1:115], sport = sp)
# 
# )

#mean_table <- map_dfr(teams_id, \(i) mean_goals(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

#mean_int_table <- map_dfr(teams_id, \(i) mean_goals_int(table = table_tournament_result_soccer, sport = 1, table_team = table_team_data_soccer, i))

## Creation main table ----

DT <- tab_for_an(team_name = team_name[1], st_name = '4th quarter', side = 'home', 
                 sport = sp, part = 2, date = NULL)

DT_ts <- create_ts(DT, except_vars = list_except_vars, all_vars = T, sport = sp, days = 30)

DT_cor_matrix <- tab_cor(tbl = DT, indep_vars = list_indep_vars, except_vars = list_except_vars, sport = sp)

## Testing forecasting variables
models <- creation_models(tbl = DT_ts,
                          names_of_vars = list_indep_vars,
                          sport = sp,
                          a = .05,
                          lambda = NULL,
                          biasadj = T)

f_models <- forecasting_models(tbl = DT_ts,
                               names_of_vars = list_indep_vars,
                               models = models,
                               sport = sp,
                               h = 20)

dep_models <- creation_models(tbl = DT_ts,
                              names_of_vars = list_dep_vars,
                              indep_vars = list_indep_vars,
                              bool_indep_vars = F,
                              sport = sp,
                              a = .05,
                              lambda = NULL,
                              biasadj = T)

f_dep_models <- forecasting_models(tbl = DT_ts,
                                   names_of_vars = list_dep_vars,
                                   indep_vars = list_indep_vars,
                                   models = dep_models,
                                   forecast_models1 = f_models,
                                   sport = sp,
                                   bool_indep_vars = F,
                                   h = 20)

## Creation forecasting tables
getting_scores(DT_ts, sport = sp, level_value = 1, forecast_models1 = f_models, forecast_models2 = f_dep_models,
               indep_vars = c(list_indep_vars$BASKETBALL, list_dep_vars$BASKETBALL), dep_vars = list_result_vars)[8]
getting_scores(DT_ts, sport = sp, level_value = 2, forecast_models = f_models, forecast_models2 = f_dep_models, 
               indep_vars = c(list_indep_vars$BASKETBALL, list_dep_vars$BASKETBALL), dep_vars = list_result_vars)[8]
getting_scores(DT_ts, sport = sp, level_value = 3, forecast_models = f_models, forecast_models2 = f_dep_models, 
               indep_vars = c(list_indep_vars$BASKETBALL, list_dep_vars$BASKETBALL), dep_vars = list_result_vars)[8]

#save.image(getwd(), "/", list.files(getwd(), pattern = ".RData"))