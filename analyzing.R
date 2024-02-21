heatmap_cor <- function(m,
                        size = 10,
                        main = 'Correlation Heatmap',
                        ...) {
  
  pheatmap(
    
    m,
    breaks = seq(-1, 1, .02),
    cluster_rows = F,
    cluster_cols = F,
    fontsize_number = size,
    display_numbers = T,
    number_format = '%0.2f',
    main = main,
    ...
    
  )
  
}

tab_cor <- function(tbl,
                    indep_vars,
                    except_vars,
                    sport = 1,
                    a = .05,
                    b = .1) {
  
  vars_for_cor <- colnames(tbl$approximated_data |> select(-c(except_vars[[sport_list[id == sport, name]]])))
  
  rcor <- Hmisc::rcorr(as.matrix(tbl$approximated_data[, ..vars_for_cor]))
  
  cor <- ifelse(rcor$P < a, round(rcor$r, 3), NA)
  
  vars <- indep_vars[[sport_list[id == sport, name]]][indep_vars[[sport_list[id == sport, name]]] %in% colnames(tbl$approximated_data)]
  
  indep_rcor <- Hmisc::rcorr(as.matrix(tbl$approximated_data[, ..vars]))
  
  indep_cor <- ifelse(indep_rcor$P > b, round(indep_rcor$r, 3), NA)
  
  return(list('main_correlation_matrix' = cor,
              'indep_vars_correlation_matrix' = indep_cor))
  
}

dependent_vars <- function(tbl,
                           dep_vars,
                           indep_vars,
                           sport = 1) {
  
  if (is.list(dep_vars)) {
    
    map_args <- dep_vars[[sport_list[id == sport, name]]]
    
  } else {
    
    map_args <- dep_vars
    
  }
  
  lst <- map(map_args, \(i) {
    
    if (is.list(indep_vars)) {
      
      exists_variables <- indep_vars[[sport_list[id == sport, name]]][indep_vars[[sport_list[id == sport, name]]] %in% colnames(tbl)]
      
    } else {
      
      exists_variables <- indep_vars[indep_vars %in% colnames(tbl)]
      
    }
    
    initial_model <- lm(reformulate(exists_variables[!(exists_variables %in% i)], response = i, intercept = F),
                        data = tbl)
    
    sum_inital_model <- summary(initial_model)
    
    results_coef <- sum_inital_model$coefficients[, "Pr(>|t|)"] %>% .[. < .05]
    
    results_formula <- reformulate(names(results_coef), response = i, intercept = F)
    
    new_model <- lm(results_formula,
                    data = DT_ts)
    
    new_model_formula <- reformulate(rownames(summary(new_model)$coefficients), response = i, intercept = F)
    
    list('incidents' = rownames(summary(new_model)$coefficients),
         'formula' = new_model_formula,
         'model' = new_model)
    
  })
  
  names(lst) <- map_args
  
  return(lst)
  
}
