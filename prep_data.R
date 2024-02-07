tab_for_an <- function(side = 'home',
                       update_stats = F,
                       team_name = NULL,
                       st_name = NULL,
                       ev_id = NULL,
                       date = NULL,
                       part = 2,
                       win_pts = 3,
                       sport = 1,
                       a = .05) {
  
  # Getting table of event_id for a sending requests for a getting statistics
  events_id <- need_ids(date = date,
                        sport = sport) |>
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
                          sport = sport,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        select(-value_away) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
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
                          sport = sport,
                          st_name = st_name) |>
        filter(stage_name != 'Match') |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
    } else {
      
      stats <- need_stats(ev_id = ev_id,
                          sport = sport,
                          st_name = st_name) |>
        select(-value_home) |>
        mutate(incident_name = gsub(incident_name, pattern = ' |-', replacement = '_') |>
                 gsub(pattern = '_\\(xG\\)', replacement = '') |>
                 gsub(pattern = '2', replacement = 'Two') |>
                 gsub(pattern = '3', replacement = 'Three') |>
                 gsub(pattern = '_%', replacement = '') |>
                 tolower() )
      
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
  
  days_between_games <- c(0, difftime(main_dt$start_time[-1], 
                                      main_dt$start_time[-length(main_dt$start_time)],
                                      units = 'days') |> round(0))
  
  main_dt <- main_dt |> mutate(days_between_games = days_between_games)
  
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
  
  #lst <- map(colnames(main_dt_num), \(i) {
    
  #  if (length(main_dt_num[[i]][ !is.na( main_dt_num[[i]]) ] ) == nrow(main_dt_num)) {
      
  #    if ( length(unique(main_dt_num[[i]]) ) > 7) {
        
  #      main_dt_num[[i]]
        
  #    } else {
        
  #      as.factor(main_dt_num[[i]])
        
  #    }
      
  #  } else {
      
  #    main_dt_num[[i]] <- NULL
      
  #  }
    
  #})
  
  #names(lst) <- colnames(main_dt_num)
  
  #if (sport %in% c(1, 4)) {
    
  #  main_dt_no_na <- as.data.table(lst) |> na.omit()
    
  #} else {
    
  #  main_dt_no_na <- as.data.table(lst) |>
  #    select(-pts) |>
  #    na.omit()
    
  #}
  
  if (sport %in% c(1, 4)) {
    
      main_dt_no_na <- main_dt_num |> na.omit()
    
    } else {
    
      main_dt_no_na <- main_dt_num |>
        select(-pts) |>
        na.omit()
    
    }
  
  vars_for_cor <- c('two_point_field_g._attempted', 'two_point_field_goals_made',
                    'three_point_field_g._attempted', 'three_point_field_goals_made',
                    'assists', 'blocks', 'defensive_rebounds',
                    'free_throws_attempted', 'free_throws_made',
                    'offensive_rebounds', 'personal_fouls',
                    'steals', 'technical_fouls', 
                    'turnovers')
  
  rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na[, ..vars_for_cor]))
  
  cor <- ifelse(rcor$P < a, round(rcor$r, 3), NA)
  
  indep_vars <- c('two_point_field_g._attempted', 'three_point_field_g._attempted', 'free_throws_attempted',
                  'assists', 'blocks', 'defensive_rebounds',
                  'offensive_rebounds', 'personal_fouls',
                  'steals', 'technical_fouls', 
                  'turnovers')
  
  indep_rcor <- Hmisc::rcorr(as.matrix(main_dt_no_na[, ..indep_vars]))
  
  indep_cor <- ifelse(indep_rcor$P > a, round(indep_rcor$r, 3), NA)
  
  # Last stage of preparing table for analyze:
  # creation list of results
  
  list('real_stats' = main_dt,
       'approximated_data' = main_dt_no_na,
       'main_correlation_matrix' = cor,
       'indep_vars_correlation_matrix' = indep_cor,
       'independent_variables' = colnames(indep_cor))
  
}

create_ts <- function(tbl,
                      inc_name,
                      spline = F) {
  
  num_observ <- cumsum(tbl$approximated_data[days_between_games < 20, days_between_games])
  
  observ <- tbl$approximated_data[days_between_games < 20][[inc_name]]
  
  if (isFALSE(spline)) {
    
    dt <- data.table(t = 1:num_observ[length(num_observ)]) |>
      left_join(data.table(t = num_observ,
                           event = observ),
                by = 't') %>%
      .[, map(.SD, \(i) round( zoo::na.approx(i, na.rm = F), 0) ), .SDcols = colnames(.)] |>
      na.omit()
    
  } else {
    
    dt <- data.table(t = 1:num_observ[length(num_observ)]) |>
      left_join(data.table(t = num_observ,
                           event = observ),
                by = 't') %>%
      .[, map(.SD, \(i) round( zoo::na.spline(i, na.rm = F), 0) ), .SDcols = colnames(.)] |>
      na.omit() |>
      filter(t > min(num_observ))
    
  }
  
  dt$event
  
}

dep_vars <- function(tbl,
                     names_of_vars) {
  
  indep_vars <- tbl$independent_variables
  
  lst <- map(names_of_vars, \(i) {
    
    vars <- tbl$main_correlation_matrix[, i] %>% .[!is.na(.)]
    
    names(vars)[!(names(vars) %ilike% 'made')]
    
  })
  
  names(lst) <- names_of_vars
  
  lst
  
}
