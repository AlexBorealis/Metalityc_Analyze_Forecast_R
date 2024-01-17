coefs_equations <- function(tbl,
                            ev_id = NULL,
                            inc_name = NULL,
                            side = 'home') {
  
  if (nrow(tbl[event_id == ev_id]) > 1) {
    
    if (side == 'home') {
      
      if (!is.null(inc_name)) {
        
        if (nrow(tbl[incident_name == inc_name][event_id == ev_id]) > 0) {
          
          vars <- tbl[incident_name == inc_name][event_id == ev_id, value_home]
          
          DT <- tbl[event_id == ev_id] |>
            mutate(clust_vars = vars)
          
          diag <- diag(ifelse(DT$value_home != 0, DT$value_home, 0.01))
          
          DT <- DT %>%
            mutate(coefs = round(ifelse(solve(diag, DT$clust_vars) < mean(solve(diag, DT$clust_vars)),
                                        solve(diag, DT$clust_vars),
                                        0), 3)) %>%
            .[!(incident_name == inc_name)]
          
          colnames(DT) <- c('group_label', 'event_name', 'tab_name', 'start_time', 'event_id', 'home_participant_name_one',
                            'home_score_full', 'current_result', 'away_score_full', 'away_participant_name_one', 
                            'h_result', 'incident_name', 'stage_name', 'value_home', 'value_away', inc_name, 'coefs')
          
          DT |>
            mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
          
        } else {
          
          NULL
          
        }
        
      } else {
        
        DT <- tbl[event_id == ev_id] |>
          mutate(delta = home_score_full - away_score_full) |>
          mutate(clust_vars = ifelse(delta > 0, 3, ifelse(delta == 0, 1, 0)))
        
        diag <- diag(ifelse(DT$value_home != 0, DT$value_home, 0.01))
        
        DT %>%
          mutate(coefs = round(ifelse(solve(diag, DT$clust_vars) < mean(solve(diag, DT$clust_vars)),
                                      solve(diag, DT$clust_vars),
                                      0), 3)) |>
          select(-delta) |>
          mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
        
      }
      
    } else {
      
      if (!is.null(inc_name)) {
        
        if (nrow(tbl[incident_name == inc_name][event_id == ev_id]) > 0) {
          
          vars <- tbl[incident_name == inc_name][event_id == ev_id, value_away]
          
          DT <- tbl[event_id == ev_id] |>
            mutate(clust_vars = vars)
          
          diag <- diag(ifelse(DT$value_away != 0, DT$value_away, 0.01))
          
          DT <- DT %>%
            mutate(coefs = round(ifelse(solve(diag, DT$clust_vars) < mean(solve(diag, DT$clust_vars)),
                                        solve(diag, DT$clust_vars),
                                        0), 3)) %>%
            .[!(incident_name == inc_name)]
          
          colnames(DT) <- c('group_label', 'event_name', 'tab_name', 'start_time', 'event_id', 'home_participant_name_one',
                            'home_score_full', 'current_result', 'away_score_full', 'away_participant_name_one', 
                            'h_result', 'incident_name', 'stage_name', 'value_home', 'value_away', inc_name, 'coefs')
          
          DT |>
            mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
          
        } else {
          
          NULL
          
        }
        
      } else {
        
        DT <- tbl[event_id == ev_id] |>
          mutate(delta = home_score_full - away_score_full) |>
          mutate(clust_vars = ifelse(delta > 0, 3, ifelse(delta == 0, 1, 0)))
        
        diag <- diag(ifelse(DT$value_away != 0, DT$value_away, 0.01))
        
        DT <- DT %>%
          mutate(coefs = round(ifelse(solve(diag, DT$clust_vars) < mean(solve(diag, DT$clust_vars)),
                                      solve(diag, DT$clust_vars),
                                      0), 3)) |> 
          select(-delta) |>
          mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
        
      }
      
    }
    
  } else {
    
    NULL
    
  }
  
}

coefs_equations_nm <- function(tbl,
                               ev_id = NULL,
                               inc_name = NULL,
                               side = 'home') {
  
  if (nrow(tbl[event_id == ev_id]) > 1) {
    
    if (side == 'home') {
      
      if (!is.null(inc_name)) {
        
        if (nrow(tbl[incident_name == inc_name][event_id == ev_id]) > 0) {
          
          vars <- tbl[incident_name == inc_name][event_id == ev_id, value_home]
          
          DT <- tbl[event_id == ev_id] |>
            mutate(clust_vars = vars)
          
          DT <- DT[!(incident_name == inc_name)]
          
          colnames(DT) <- c('group_label', 'event_name', 'tab_name', 'start_time', 'event_id', 'home_participant_name_one',
                            'home_score_full', 'current_result', 'away_score_full', 'away_participant_name_one', 
                            'h_result', 'incident_name', 'stage_name', 'value_home', 'value_away', inc_name)
          
          DT |>
            mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
          
        } else {
          
          NULL
          
        }
        
      } else {
        
        tbl[event_id == ev_id] |>
          mutate(delta = home_score_full - away_score_full) |>
          mutate(clust_vars = ifelse(delta > 0, 3, ifelse(delta == 0, 1, 0))) |>
          select(-delta) |>
          mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
        
      }
      
    } else {
      
      if (!is.null(inc_name)) {
        
        if (nrow(tbl[incident_name == inc_name][event_id == ev_id]) > 0) {
          
          vars <- tbl[incident_name == inc_name][event_id == ev_id, value_away]
          
          DT <- tbl[event_id == ev_id] |>
            mutate(clust_vars = vars)
          
          DT <- DT[!(incident_name == inc_name)]
          
          colnames(DT) <- c('group_label', 'event_name', 'tab_name', 'start_time', 'event_id', 'home_participant_name_one',
                            'home_score_full', 'current_result', 'away_score_full', 'away_participant_name_one', 
                            'h_result', 'incident_name', 'stage_name', 'value_home', 'value_away', inc_name)
          
          DT |>
            mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
          
        } else {
          
          NULL
          
        }
        
      } else {
        
        tbl[event_id == ev_id] |>
          mutate(delta = home_score_full - away_score_full) |>
          mutate(clust_vars = ifelse(delta > 0, 3, ifelse(delta == 0, 1, 0))) |>
          select(-delta) |>
          mutate(h_result = ifelse(clust_vars == 3, 'WIN', ifelse(clust_vars == 1, 'DRAW', 'LOST')))
        
      }
      
    }
    
  } else {
    
    NULL
    
  }
  
}

heatmap_cor <- function(m,
                        size = 10,
                        main = 'Correlation Heatmap',
                        ...) {
  
  pheatmap(
    
    m,
    breaks = seq(-1, 1, 0.02),
    cluster_rows = F,
    cluster_cols = F,
    fontsize_number = size,
    display_numbers = T,
    number_format = '%0.2f',
    main = main,
    ...
    
  )
  
}

cmeans_cluster <- function(side = 'home',
                           inc_name = NULL,
                           team_name,
                           st_name,
                           cc = 0.5,
                           cents = 6,
                           fuzz_deg = 1.5,
                           cntrl = NULL,
                           no_matrix = F) {
  
  need_events_id <- need_ids() |>
    group_by(event_id) |>
    filter(row_number() == 1) |>
    #group_by(tab_name, group_label) |>
    #filter(row_number() %in% 1:10) |>
    as.data.table()
  
  #get_stats <- fun_event_statistics_g(ev_id = need_events_id$event_id)
  
  stats <- need_stats()
  
  for_analyze <- need_events_id[stats, on = 'event_id'][stage_name %ilike% st_name]
  
  for_analyze_home <- for_analyze[home_participant_name_one %ilike% team_name]
  
  for_analyze_away <- for_analyze[away_participant_name_one %ilike% team_name]
  
  if (isFALSE(no_matrix)) {
    
    if (is.null(inc_name)) {
      
      if (side == 'home') {
        
        DT <- map_df(unique(for_analyze_home$event_id), \(i) coefs_equations(ev_id = i,
                                                                             tbl = for_analyze_home,
                                                                             side = 'home'))
        
      } else {
        
        DT <- map_df(unique(for_analyze_away$event_id), \(i) coefs_equations(ev_id = i,
                                                                             tbl = for_analyze_away,
                                                                             side = 'away'))
        
      }
      
      # Loading dataset
      dt_pre <- DT[order(-ymd_hms(start_time))] |>
        select(-all_of(matches(c('value_home', 'value_away')))) |>
        pivot_wider(names_from = incident_name,
                    values_from = coefs,
                    values_fn = c,
                    values_fill = NA) |>
        as.data.table()
      
      if (is_empty(dt_pre$`Red Cards`)) {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name)) |>
          mutate_if(is.character, as.numeric)
        
      } else {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name, `Red Cards`)) |>
          mutate_if(is.character, as.numeric)
        
      }
      
      dt_pre_no_na <- dt_pre_num[, map(.SD, zoo::na.approx, n = nrow(dt_pre_num)), .SDcols = names(dt_pre_num)]
      
      COR <- round(cor(dt_pre_no_na), 3) %>%
        ifelse(. != 1 & (. > cc | . < -cc), ., NA)
      
      dep_vars <- COR[, "clust_vars"] %>% .[!is.na(.)] %>% names()
      
      dt <- dt_pre_no_na |> select(all_of(dep_vars))
      
      result <- cmeans(dt, centers = cents, m = fuzz_deg, control = cntrl)
      
      list('cmeans_result' = result,
           'table_with_cmeans_result' = cbind(dt_pre |>
                                                select(c(group_label:stage_name, "clust_vars", all_of(dep_vars))),
                                              result$cluster,
                                              result$membership),
           'correlation_matrix' = COR,
           'dependent_variabes' = dep_vars)
      
    } else {
      
      if (side == 'home') {
        
        DT <- map_df(unique(for_analyze_home$event_id), \(i) coefs_equations(ev_id = i,
                                                                             tbl = for_analyze_home,
                                                                             inc_name = inc_name,
                                                                             side = 'home'))
        
      } else {
        
        DT <- map_df(unique(for_analyze_away$event_id), \(i) coefs_equations(ev_id = i,
                                                                             tbl = for_analyze_away,
                                                                             inc_name = inc_name,
                                                                             side = 'away'))
        
      }
      
      # Loading dataset
      dt_pre <- DT[order(-ymd_hms(start_time))] |>
        select(-all_of(matches(c('value_home', 'value_away')))) |>
        pivot_wider(names_from = incident_name,
                    values_from = coefs,
                    values_fn = c,
                    values_fill = NA) |>
        as.data.table()
      
      if (is_empty(dt_pre$`Red Cards`)) {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name)) |>
          mutate_if(is.character, as.numeric)
        
      } else {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name, `Red Cards`)) |>
          mutate_if(is.character, as.numeric)
        
      }
      
      dt_pre_no_na <- dt_pre_num[, map(.SD, zoo::na.approx, n = nrow(dt_pre_num)), .SDcols = names(dt_pre_num)]
      
      COR <- round(cor(dt_pre_no_na), 3) %>%
        ifelse(. != 1 & (. > cc | . < -cc), ., NA)
      
      dep_vars <- COR[, inc_name] %>% .[!is.na(.)] %>% names()
      
      dt <- dt_pre_no_na |> select(all_of(dep_vars))
      
      result <- cmeans(dt, centers = cents, m = fuzz_deg, control = cntrl)
      
      list('cmeans_result' = result,
           'table_with_cmeans_result' = cbind(dt_pre |>
                                                select(c(group_label:stage_name, inc_name, all_of(dep_vars))),
                                              result$cluster,
                                              result$membership),
           'correlation_matrix' = COR,
           'dependent_variabes' = dep_vars)
      
    }
    
  } else {
    
    if (is.null(inc_name)) {
      
      if (side == 'home') {
        
        DT <- map_df(unique(for_analyze_home$event_id), \(i) coefs_equations_nm(ev_id = i,
                                                                                tbl = for_analyze_home,
                                                                                side = 'home'))
        
        # Loading dataset
        dt_pre <- DT[order(-ymd_hms(start_time))] |>
          select(-matches(c('value_away'))) |>
          pivot_wider(names_from = incident_name,
                      values_from = value_home,
                      values_fn = c,
                      values_fill = NA) |>
          as.data.table()
        
      } else {
        
        DT <- map_df(unique(for_analyze_away$event_id), \(i) coefs_equations_nm(ev_id = i,
                                                                                tbl = for_analyze_away,
                                                                                side = 'away'))
        
        # Loading dataset
        dt_pre <- DT[order(-ymd_hms(start_time))] |>
          select(-matches(c('value_home'))) |>
          pivot_wider(names_from = incident_name,
                      values_from = value_away,
                      values_fn = c,
                      values_fill = NA) |>
          as.data.table()
        
      }
      
      if (is_empty(dt_pre$`Red Cards`)) {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name)) |>
          mutate_if(is.character, as.numeric)
        
      } else {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name, `Red Cards`)) |>
          mutate_if(is.character, as.numeric)
        
      }
      
      dt_pre_no_na <- dt_pre_num[, map(.SD, zoo::na.approx, n = nrow(dt_pre_num)), .SDcols = names(dt_pre_num)]
      
      COR <- round(cor(dt_pre_no_na), 3) %>%
        ifelse(. != 1 & (. > cc | . < -cc), ., NA)
      
      dep_vars <- COR[, 'clust_vars'] %>% .[!is.na(.)] %>% names()
      
      dt <- dt_pre_no_na |> select(all_of(dep_vars))
      
      result <- cmeans(dt, centers = cents, m = fuzz_deg, control = cntrl)
      
      list('cmeans_result' = result,
           'table_with_cmeans_result' = cbind(dt_pre |>
                                                select(c(group_label:stage_name, clust_vars, all_of(dep_vars))),
                                              result$cluster,
                                              result$membership),
           'correlation_matrix' = COR,
           'dependent_variabes' = dep_vars)
      
    } else {
      
      if (side == 'home') {
        
        DT <- map_df(unique(for_analyze_home$event_id), \(i) coefs_equations_nm(ev_id = i,
                                                                                tbl = for_analyze_home,
                                                                                inc_name = inc_name,
                                                                                side = 'home'))
        
        # Loading dataset
        dt_pre <- DT[order(-ymd_hms(start_time))] |>
          select(-matches(c('value_away'))) |>
          pivot_wider(names_from = incident_name,
                      values_from = value_home,
                      values_fn = c,
                      values_fill = NA) |>
          as.data.table()
        
      } else {
        
        DT <- map_df(unique(for_analyze_away$event_id), \(i) coefs_equations_nm(ev_id = i,
                                                                                tbl = for_analyze_away,
                                                                                inc_name = inc_name,
                                                                                side = 'away'))
        
        # Loading dataset
        dt_pre <- DT[order(-ymd_hms(start_time))] |>
          select(-matches(c('value_home'))) |>
          pivot_wider(names_from = incident_name,
                      values_from = value_away,
                      values_fn = c,
                      values_fill = NA) |>
          as.data.table()
        
      }
      
      if (is_empty(dt_pre$`Red Cards`)) {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name)) |>
          mutate_if(is.character, as.numeric)
        
      } else {
        
        dt_pre_num <- dt_pre |>
          select(-c(group_label:stage_name, `Red Cards`)) |>
          mutate_if(is.character, as.numeric)
        
      }
      
      dt_pre_no_na <- dt_pre_num[, map(.SD, zoo::na.approx, n = nrow(dt_pre_num)), .SDcols = names(dt_pre_num)]
      
      COR <- round(cor(dt_pre_no_na), 3) %>%
        ifelse(. != 1 & (. > cc | . < -cc), ., NA)
      
      dep_vars <- COR[, inc_name] %>% .[!is.na(.)] %>% names()
      
      dt <- dt_pre_no_na |> select(all_of(dep_vars))
      
      result <- cmeans(dt, centers = cents, m = fuzz_deg, control = cntrl)
      
      list('cmeans_result' = result,
           'table_with_cmeans_result' = cbind(dt_pre |>
                                                select(c(group_label:stage_name, inc_name, all_of(dep_vars))),
                                              result$cluster,
                                              result$membership),
           'correlation_matrix' = COR,
           'dependent_variabes' = dep_vars)
      
    }
    
  }
  
}
