tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       inc_name = NULL,
                       part = 2,
                       win_pts = 3,
                       a = .05,
                       b = .1,
                       env = parent.frame()) {
  
  # Getting table of event_id for a sending requests for a getting statistics
  events_id <- need_ids(date = date) |>
    group_by(event_id) |>
    filter(row_number() == 1) |>
    #group_by(tab_name, group_label) |>
    #filter(row_number() %in% 1:10) |>
    as.data.table()
  
  # In this stage need to use this variant - working with story statistics
  # In the production stage need to use variant 'update_stats = T'
  if (isTRUE(update_stats)) {
    
    fun_event_statistics_g(ev_id = events_id$event_id)
    
  } else {
    
    NULL
    
  }
  
  # First stage of preparing table for analyze:
  #1) choose one side parameter
  #2) repair some names in incidents
  #3) pivot table (wide format)
  if (side == 'home') {
    
    if (is.null(st_name)) {
      
      stats <- need_stats(ev_id = ev_id,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = ''))
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          st_name = st_name) |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = ''))
      
    }
    
    main_dt <- events_id[home_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full > away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', win_pts, ifelse(h_result == 'DRAW', 1, 0))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_home,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  } else {
    
    if (is.null(st_name)) {
      
      stats <- need_stats(ev_id = ev_id,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = ''))
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          st_name = st_name) |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = ''))
      
    }
    
    main_dt <- events_id[away_participant_name_one %ilike% team_name] |>
      mutate(h_result = ifelse(home_score_full < away_score_full, 'WIN', 
                               ifelse(home_score_full == away_score_full, 'DRAW', 'LOST'))) |>
      mutate(pts = ifelse(h_result == 'WIN', win_pts, ifelse(h_result == 'DRAW', 1, 0))) |>
      inner_join(stats, by = 'event_id') |>
      pivot_wider(names_from = incident_name,
                  values_from = value_away,
                  values_fn = c,
                  values_fill = NA) |>
      as.data.table() %>%
      .[order(start_time)]
    
  }
  
  # Second stage of preparing table for analyze:
  #1) remove character rows
  #2) remove NA values
  #3) interpolate/change time series of statistics parameters
  main_dt_num <- main_dt |>
    select(-c(group_label:home_participant_name_one,
              current_result, away_participant_name_one,
              h_result, stage_name)) |>
    mutate(home_score_full = home_score_full / part,
           away_score_full = away_score_full / part,
           pts = pts / part) |>
    mutate_if(is.character, as.numeric) %>%
    .[, map(.SD, zoo::na.approx, na.rm = F), .SDcols = colnames(.)]
  
  lst <- map(colnames(main_dt_num), \(i) {
    
    if (length(main_dt_num[[i]][ !is.na( main_dt_num[[i]]) ] ) == nrow(main_dt_num)) {
      
      if ( length(unique(main_dt_num[[i]]) ) > 7) {
        
        main_dt_num[[i]]
        
      } else {
        
        as.factor(main_dt_num[[i]])
        
      }
      
    } else {
      
      main_dt_num[[i]] <- NULL
      
    }
    
  })
  
  names(lst) <- colnames(main_dt_num)
  
  main_dt_no_na <- as.data.table(lst) |> na.omit()
  
  # Creation correlation matrix of statistic parameters
  rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na))
    
  cor <- ifelse(rcor$P < a, round(rcor$r, 3), NA)
  
  # Last stage of preparing table for analyze:
  #1) determine dependent statistic parameters
  #2) creation list of results
  #3) determine max quantity independent variables
  if (!is.null(inc_name)) {
    
    if (!is.null(main_dt_no_na[[inc_name]])) {
      
      correlations <- cor[, inc_name] %>% .[!is.na(.)]
      
      dep_vars <- names(correlations)
      
      dep_rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na |> select( all_of(dep_vars) ) ))
      
      dep_cor <- ifelse(dep_rcor$P > b, round(dep_rcor$r, 3), NA)
      
      col_indep_vars <- which.max( apply(dep_cor, 2, FUN = \(i) length( which( !is.na(i) ) ) ) )[1] |> names()
      
      new_dep_vars <- dep_cor[, col_indep_vars] %>% .[!is.na(.)]
      
      cor_data_vars <- c(inc_name, names(new_dep_vars))
      
      list('real_stats' = main_dt,
           'correlation_matrix' = cor,
           'indep_vars_correlation_matrix' = dep_cor,
           'correlations' = correlations,
           'dependent_variables' = names(new_dep_vars),
           'formula_with_intercept' = reformulate(names(new_dep_vars), response = inc_name, env = env),
           'formula_without_intercept' = reformulate(names(new_dep_vars), response = inc_name, env = env, intercept = F),
           'approximated_data' =  main_dt_no_na,
           'correlated_data' =  main_dt_no_na |> select( all_of(cor_data_vars) ) )
      
    } else {
      
      list('real_stats' = main_dt,
           'correlation_matrix' = cor,
           'approximated_data' =  main_dt_no_na)
      
    }
    
  } else {
    
    correlations <- cor[, 'pts'] %>% .[!is.na(.)]
    
    dep_vars <- names(correlations)
    
    dep_rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na |> select( all_of(dep_vars) ) ))
    
    dep_cor <- ifelse(dep_rcor$P > b, round(dep_rcor$r, 3), NA)
    
    col_indep_vars <- which.max( apply(dep_cor, 2, FUN = \(i) length( which( !is.na(i) ) ) ) )[1] |> names()
    
    new_dep_vars <- dep_cor[, col_indep_vars] %>% .[!is.na(.)]
    
    cor_data_vars <- c('pts', names(new_dep_vars))
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'indep_vars_correlation_matrix' = dep_cor,
         'correlations' = correlations,
         'dependent_variables' = names(new_dep_vars),
         'formula_with_intercept' = reformulate(names(new_dep_vars), response = 'pts', env = env),
         'formula_without_intercept' = reformulate(names(new_dep_vars), response = 'pts', env = env, intercept = F),
         'approximated_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select( all_of(cor_data_vars) ) )
    
  }
  
}

test_data <- function(cv_data,
                      probs = seq(.25, 1, .25)) {
  
  DT <- cv_data
  
  test.data <- data.table('quantiles' = c('25%', '50%', '75%', '100%'),
                          t(map_df(colnames(cr_dt$train_data), \(i) {
                            
                            quantile(cr_dt$train_data[[i]], probs = seq(.25, 1, .25))
                            
                            })))
  
  colnames(test.data) <- c('quantiles', colnames(train.data))
  
  results <- data.table(test.data[, .(quantiles)],
                        test.data[, ..inc_name],
                        predict_rf = predict(model_rf, new_test.data),
                        delta2_rf = (new_test.data[, ..inc_name] - predict(model_rf, new_test.data))^2,
                        predict_glm = predict(model_glm, new_test.data),
                        delta2_glm = (new_test.data[, ..inc_name] - predict(model_glm, new_test.data))^2,
                        predict_pcr = predict(model_pcr, new_test.data, 
                                              ncomp = which(as.numeric(R2_pcr$val) >= max(as.numeric(R2_pcr$val) ) * 3/4 )[1])[, 1, 1],
                        delta2_pcr = (new_test.data[, ..inc_name] - predict(model_pcr, new_test.data))^2)
  
  colnames(results) <- c('quantiles', inc_name,
                         'predict_rf', 'delta2_rf',
                         'predict_glm', 'delta2_glm',
                         'predict_pcr', 'delta2_pcr')
  
}