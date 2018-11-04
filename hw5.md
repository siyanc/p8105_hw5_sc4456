hw5\_sc4456
================
Siyan Chen
11/3/2018

Problem 1
=========

read all data

``` r
file_list = list.files(path = "./data") 
df = str_c("./data/", file_list) 
# create correct subpath format
new_file_list= str_remove(file_list, "\\.csv$")
# remove .csv 
data_read = data.frame(new_file_list) %>% 
mutate(file_read = map(.x = df, ~read_csv(.x))) %>% 
  unnest() 
```

tidy data ?

``` r
tidy_data = data_read %>%
  janitor::clean_names() %>% 
  separate(new_file_list, into = c("group", "subject_id"), sep = "_") %>% 
  gather(key = "week", value = value, week_1:week_8) %>% 
  mutate(subject_id = as.numeric(subject_id), week = str_remove(week, "week\\_")) %>% 
  mutate(week = as.numeric(week), value = as.numeric(value))
```

Plot

``` r
tidy_data$participants = paste(tidy_data$group, tidy_data$subject_id)
# create a variable to make subject_id unique(control1 is not exp 1)
tidy_data %>% 
  ggplot(aes(x = week, y = value, color = participants)) + geom_line() + facet_grid(~group)
```

![](hw5_files/figure-markdown_github/unnamed-chunk-3-1.png)
