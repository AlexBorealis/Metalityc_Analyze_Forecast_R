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

modify_pca <- function(team_name,
                       inc_name,
                       st_name,
                       side = 'home',
                       cor_level = 0.2,
                       p = 0.8,
                       date = NULL) {
  
  DT <- tab_for_an(team_name = team_name, st_name = st_name, 
                   date = date, side = side, cor_level = cor_level,
                   inc_name = ifelse(is.null(inc_name), 'pts', inc_name))
  
  if (is.null(inc_name)) {
    
    training.samples <- createDataPartition(DT$repaired_data$pts, p = p, list = F)
    
  } else {
    
    training.samples <- createDataPartition(DT$repaired_data[[inc_name]], p = p, list = F)
    
  }
  
  train.data  <- DT$repaired_data[training.samples]
  
  test.data <- DT$repaired_data[-training.samples]
  
  list('model_pcr' = pcr(DT$formula_without_intercept, data = train.data, scale = T),
       'test_data' = test.data)
  
}
