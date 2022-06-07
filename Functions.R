# R Code Data Cleaning

## Functions for Tweet Cleaning

# NO PARALLEL PROCESSING
json_to_nested_tibbles <- function(json_data){
  map(json_data, ~tibble(
    # Variables present in all Tweets
    username = .x$author$username,
    tweet_date = .x$created_at %>% 
      lubridate::ymd_hms(),
    text = .x$text,
    retweet_count = .x$public_metrics$retweet_count,
    reply_count = .x$public_metrics$reply_count,
    like_count = .x$public_metrics$like_count,
    quote_count = .x$public_metrics$quote_count,
    followers_count = .x$author$public_metrics$followers_count,
    following_count = .x$author$public_metrics$following_count,
    tweet_count = .x$author$public_metrics$tweet_count,
    author_id = .x$author$id,
    verified = .x$author$verified,
    protected = .x$author$protected,
    account_created_at = .x$author$created_at %>% 
      lubridate::ymd_hms(),
    name = .x$author$name,
    account_description = .x$author$description,
    listed_count = .x$author$public_metrics$listed_count,
    url = .x$url,
    conversation_id = .x$conversation_id,
    # If statements for items that may or may not be present in tweet data
    location = if("location" %in% names(.x$author)){
      as.character(.x$author$location)
    }else{as.character(NA)},
    referenced_tweets = if("referenced_tweets" %in% names(.x)){
      list(.x$referenced_tweets)
    }else{list(NULL)},
    entities = if("entities" %in% names(.x)){
      list(.x$entities)
    }else{list(NULL)},
    in_reply_to_user_id = if("in_reply_to_user_id" %in% names(.x)){
      list(.x$in_reply_to_user_id)
    }else{list(NULL)}
  )
  )
}

# WITH PARALLEL PROCESSING
json_to_nested_tibbles_parallel_process <- function(json_data){
  furrr::future_map(json_data, ~tibble(
    # Variables present in all tweets
    username = .x$author$username,
    tweet_date = .x$created_at %>% 
      lubridate::ymd_hms(),
    text = .x$text,
    retweet_count = .x$public_metrics$retweet_count,
    reply_count = .x$public_metrics$reply_count,
    like_count = .x$public_metrics$like_count,
    quote_count = .x$public_metrics$quote_count,
    followers_count = .x$author$public_metrics$followers_count,
    following_count = .x$author$public_metrics$following_count,
    tweet_count = .x$author$public_metrics$tweet_count,
    author_id = .x$author$id,
    verified = .x$author$verified,
    protected = .x$author$protected,
    account_created_at = .x$author$created_at %>% 
      lubridate::ymd_hms(),
    name = .x$author$name,
    account_description = .x$author$description,
    listed_count = .x$author$public_metrics$listed_count,
    url = .x$url,
    conversation_id = .x$conversation_id,
    # If statements for items that may or may not be present in tweet data
    location = if("location" %in% names(.x$author)){
      as.character(.x$author$location)
    }else{as.character(NA)},
    referenced_tweets = if("referenced_tweets" %in% names(.x)){
      list(.x$referenced_tweets)
    }else{as.character(NA)},
    entities = if("entities" %in% names(.x)){
      list(.x$entities)
    }else{as.character(NA)},
    in_reply_to_user_id = if("in_reply_to_user_id" %in% names(.x)){
      list(.x$in_reply_to_user_id)
    }else{as.character(NA)}
  )
  )
}


# Function for all cleaning
clean_twitter <- function(json_data){
  # SET UP FUNCTIONS
  tictoc::tic()
  gc() # Force a 'garbage collection' before running, prevents (some) crashing
  name <- deparse(substitute(json_data)) # Captures the name put into the function
  future::plan(multisession, workers = 6) # Set the number of cores you'll use
  # CREATING NULL DFS
  loops <- tibble(NA)
  parallel_output <- tibble(NA)
  tibble_output <- tibble(NA)
  clean_output <- tibble(NA)
  data <- NA
  # Automatically calculating how many loops
  total_groups <- max(as.numeric(groupdata2::group_factor(json_data,
                                                          n = 10000,
                                                          method = "greedy")))
  loops <- total_groups - 1
  loop_call_left <- c(0:loops) * 10000 + 1
  loop_call_right <- c(0:(loops)) * 10000 + 10000
  # Final items; start with these because it sets up the tibble, and those items don't fit into the memory-saving bins
  parallel_output <- json_data[loop_call_right[loops]:length(json_data)] %>% 
    json_to_nested_tibbles_parallel_process()
  tibble_output <- tibble(parallel_output)
  data <- tidyr::unnest_wider(tibble_output, col = parallel_output)
  parallel_output <- tibble(NA)
  tibble_output <- tibble(NA)
  tictoc::toc()
  #### BEGIN LOOPING OVER GROUPS
  tictoc::tic()
  for(i in 1:loops){
    # Select 10000 at a time
    parallel_output <- json_data[loop_call_left[i]:loop_call_right[i]] %>% 
      json_to_nested_tibbles_parallel_process()
    tibble_output <- tibble(parallel_output)
    data <- rbind(data, tidyr::unnest_wider(tibble_output, col = parallel_output))
    parallel_output <- tibble(NA)
    tibble_output <- tibble(NA)
    paste0("finished ", i, " of ", loops)
  }
  # Saves the output as the name of the input + _clean
  assign(
    x = paste0(name, "_clean"),
    value = data,
    envir = .GlobalEnv)
  tictoc::toc()
}

# IMPORT AND CLEAN TWITTER DATA

import_and_clean_twitter <- function(json_tweets){
  x <- rjson::fromJSON(file = here::here(paste0("data/", json_tweets, ".json")))
  clean_twitter(x)
}


convert_to_data_frame <- function(formal_dfm){
  convert(formal_dfm, to = "data.frame")
}
mental_perception <- function(dfmat){
  dfm_lookup(dfmat, dictionary = dict_mpd, levels = 1) %>% 
    convert_to_data_frame()
}
sentiment <- function(dfmat){
  dfm_lookup(dfmat, dictionary = dict_posneg, levels = 1) %>% 
    convert_to_data_frame()
}
calculate_tweet_valence <- function(df){
  df$valence <- map2_dbl(.x = df$positive, 
                         .y = df$negative, 
                         ~ .x - .y)
}

df <- celeb_data_DI[[1]]



analyze_text <- function(df){
  name <- names(df) 
  for(i in seq_along(df)){
  corp <- corpus(df[[i]], text_field = 1)
  toks <- tokens(corp, remove_punct = T)
  dfmat <- dfm(toks)
  ment_perc <- mental_perception(dfmat)
  pos_neg <- sentiment(dfmat) 
  data <- cbind(df[[i]],
                ment_perc %>% dplyr::select(!doc_id),
                pos_neg %>% dplyr::select(!doc_id))
  calculate_tweet_valence(data)
  assign(
    x = paste0(name[i], "_analyzed"),
    value = data,
    envir = .GlobalEnv)
  assign(
    x = paste0(name[i], "_text_other"),
    value = list(corp, toks, dfmat, ment_perc, pos_neg),
    envir = .GlobalEnv)}
}


long_form_analyzed <- function(df){
  data <- df %>% 
    pivot_longer(cols = c("negative", "positive", "neg_positive", 
                          "neg_negative", "Experience", "Agency", 
                          "PatientEmotion", "AgentEmotion", "MindOverall"),
                 names_to = "text_measures", 
                 values_to = "text_score")
  name = deparse(substitute(df))
  assign(x = paste0(name, "_long"),
         value = data,
         envir = .GlobalEnv)}

deidentify <- function(df){
  value = df %>% select(text, tweet_date, retweet_count, reply_count, like_count, quote_count, 
                          followers_count, following_count, tweet_count, verified, protected)
  name = (deparse(substitute(df)))
  assign(x = paste0(name, "_DI"),
         value = value,
         envir = .GlobalEnv)
}

##### Debugging:
safe_analyze_text <- safely(analyze_text)
safe_analyze_text(willsmith_deidentified)