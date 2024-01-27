tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       inc_name = NULL,
                       win_pts = 3,
                       a = .05,
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
    
    stats <- need_stats(ev_id = ev_id,
                        st_name = st_name) |>
      select(-value_away) |>
      mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
               gsub(pattern = '_\\(xG\\)', replacement = ''))
    
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
    
    stats <- need_stats(ev_id = ev_id,
                        st_name = st_name) |>
      select(-value_home) |>
      mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
               gsub(pattern = '_\\(xG\\)', replacement = ''))
    
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
    mutate_if(is.character, as.numeric) %>%
    .[, map(.SD, zoo::na.approx, na.rm = F), .SDcols = colnames(.)]
  
  lst <- map(colnames(main_dt_num), \(i) {
    
    if (length(main_dt_num[[i]][!is.na(main_dt_num[[i]])]) < nrow(main_dt_num)) {
      
      main_dt_num[[i]] <- NULL
      
    } else {
      
      main_dt_num[[i]]
      
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
  if (!is.null(inc_name)) {
    
    dep_vars <- cor[, inc_name] %>% .[!is.na(.)] %>% names()
    
    cor_data_vars <- c(inc_name, dep_vars)
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, inc_name] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = inc_name, env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = inc_name, env = env, intercept = F),
         'approximated_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(cor_data_vars)))
    
  } else {
    
    dep_vars <- cor[, "pts"] %>% .[!is.na(.)] %>% names()
    
    cor_data_vars <- c('pts', dep_vars)
    
    list('real_stats' = main_dt,
         'correlation_matrix' = cor,
         'correlations' = cor[, "pts"] %>% .[!is.na(.)],
         'dependent_variables' = dep_vars,
         'formula_with_intercept' = reformulate(dep_vars, response = 'pts', env = env),
         'formula_without_intercept' = reformulate(dep_vars, response = 'pts', env = env, intercept = F),
         'approximated_data' =  main_dt_no_na,
         'correlated_data' =  main_dt_no_na |> select(all_of(cor_data_vars)))
    
  }
  
}

test_data <- function() {
  
  
  
}