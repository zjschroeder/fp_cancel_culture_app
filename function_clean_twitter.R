clean_twitter <- function(json_data){
  # SET UP FUNCTIONS
  tictoc::tic()
    gc() # Force a 'garbage collection' before running, prevents crashing
    df <- tibble(will_smith)
    name <- deparse(substitute(json_data)) # Captures the name put into the function
    future::plan(multisession, workers = 6) # Set the number of cores you'll use
  # CREATING NULL DFS
    loops <- tibble(NA)
    parallel_output <- tibble(NA)
    tibble_output <- tibble(NA)
    clean_output <- tibble(NA)
    data <- tibble(NA)
  
  # Creating grouping variables to ensure we don't lose data  while running computationally-heavy code
    df <- df %>% 
      mutate(
        group = groupdata2::group_factor(df,
                                       n = 10000,
                                       method = "greedy"))
    loops <- max(as.numeric(levels(df$group)))
    
  tictoc::toc()
  
  #### BEGIN LOOPING OVER GROUPS
    tictoc::tic()
    for(i in 1:loops){
      interim <- df %>% filter(group == i) 
      interim <- list(interim[,1])
      interim <- interim[[1]]
      # Three cleaning functions
      parallel_output <- json_to_nested_tibbles_parallel_process(interim)
      tibble_output <- tibble(parallel_output)
      data[[i]] <- tidyr::unnest_auto(tibble_output, col = parallel_output) %>% 
        list()
      paste0("finished ", i)
    }
  # Organizing list
    x <- tibble(data)
    y <- unnest_wider(x, data)
  # Saves the output as the name of the input + _clean
    assign(
      x = paste0(name, "_clean"),
      value = data,
      envir = .GlobalEnv)
  tictoc::toc()
}
clean_twitter(will_smith)

