fp_cancel_culture_app


Hello Daniel!

Thank you so much for a great term. This project is (for me, at least) pretty complicated. So, to help you grade as quickly as possible, I've identified each part of the grading criteria and where you can find it below. Additionally, because of the constraints of these data (both in size and privacy), several of the graded functions are unable to be run by you, so I've included comments to clarify what each piece is doing - I hope that's alright!

Best of luck on your new position, UO will definitely miss you!


*More than one variant of purrr::map is used [5 points]*
I used the map_dbl on line 39 of the app.R file to analyze the correlations between negative sentiment and Twitter engagement. I will use this in future research as outcomes that will be part of data analysis.

*At least one {purrr} function outside the basic map family (walk_, reduce, modify_, etc.) [5 points]*
I used safely to test my functions - an example is included at the bottom of the Functions.R file. It was a true lifesaver as I debugged some heavy, complicated data from the Twitter API!

*At least one instance of parallel iteration (e.g., map2_, pmap_) [5 points]*
I used map2 to calculate the valence of the tweets which was built into a function on line 157 of the Functions.R file.

*At least one use case of purrr::nest %>% mutate() [5 points]*
I've used it on line 58 of the app.R to calculate levels of engagement for each degree of negativity. This will also be used in analysis when I write up these data.

*At least two custom functions [20 points; 10 points each]*
All functions are housed in the Functions.R file. They, in large part, each do one thing and then build into more complicated functions that I then implement in the data cleaning.rmd file. 

*Each function must do exactly one thing. The functions may replicate the behavior of a base function - as noted above this is about practicing the skills you learn in class*