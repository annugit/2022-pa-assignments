Exercise_5\_trial_1
================

``` r
data_path <- "C:/Users/abhid/Documents/Courses/ORGB_690_People_Analytics/Exercise_5_AM/"
exercise_gs_1 <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 7
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): examiner_name_last, examiner_name_first, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Data cleaning

We changed the data by separating the name column into first and last
name using comma deliminator. From the data, we can see that majority of
the values associated with the maximum examiner grade is not available.
Therefore removing the rows with null values.

``` r
exercise_gs_1 <- na.omit(exercise_gs_1)
```

## Determine gender

The gender function returned a lot of null values and therefore removing
all the null values. Although the package could identify 10-15% of the
names and therefore the insights derived from these may not be
representative of the entire population

``` r
examiner_names <- exercise_gs_1 %>% 
  distinct(examiner_name_first)

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender
  )

exercise_gs_1 <- exercise_gs_1 %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
exercise_gs_1 <- na.omit(exercise_gs_1)
```

## 3. Determine race

``` r
examiner_surnames <- exercise_gs_1 %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 339
    ## surnames that could not be matched to Census list.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race <- examiner_race %>% 
  select(surname,race)
exercise_gs_1 <- exercise_gs_1 %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
```

## 4. Determine tenure

Calculating the number of days an examiner spends within a particular
examiner grade

``` r
exercise_gs_1 <- exercise_gs_1 %>% 
  mutate(start_date = as_date(mdy(start_date)), end_date = as_date(mdy(end_date)))

exercise_gs_1 <- exercise_gs_1 %>%
  mutate(tenure_days = interval(start_date, end_date) %/% days(1))
```

## 5. Determine the average tenure days

Determining the average tenure days for each examiner grade and
averaging the same across gender.

``` r
grade_level_gender_data <- exercise_gs_1 %>% 
  group_by(examiner_grade,gender) %>% 
  summarise(
    people_count = n(),
    start_date = min(start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    tenure_days = mean(tenure_days, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'examiner_grade'. You can override using
    ## the `.groups` argument.

``` r
ggplot(grade_level_gender_data) +
  geom_bar(
    aes(x=as_factor(examiner_grade),y=tenure_days, fill = gender), 
    position = "dodge", stat = "identity"
    ) + 
  xlab("Examiner grade")
```

![](exercise_5_AA_files/figure-gfm/avg%20tenure_gender-1.png)<!-- -->
Findings: The average number of days an examiner spends within each
grade before moving on to the next grade is nearly similar for both
males and females in the lower grades (5,7,9,11). However, for higher
grades (12-14), females seem to spend slightly more days in one grade
than males before progressing into the next grade. We also observed that
in the last grade, males stayed longer than females, which may imply
that females left the job/retired sooner than males.

## Determining avg tenure days across race

Determining the average tenure days for each examiner grade and
averaging the same across race.

``` r
grade_level_race_data <- exercise_gs_1 %>% 
  group_by(examiner_grade,race) %>% 
  summarise(
    start_date = min(start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    tenure_days = mean(tenure_days, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'examiner_grade'. You can override using
    ## the `.groups` argument.

``` r
ggplot(grade_level_race_data) +
  geom_bar(
    aes(x=as_factor(examiner_grade),y=tenure_days, fill = race), 
    position = "dodge", stat = "identity"
    ) + 
  xlab("Examiner grade")
```

![](exercise_5_AA_files/figure-gfm/avg%20tenure_race-1.png)<!-- -->

Findings: We can see that people of Asian and White race stayed for
similar period in the grades. People of black race stayed longer than
all other races in a grade before progressing to the next. Another
interesting fact is that Hispanics stayed in the lower grades for longer
duration that Asians and Whites but for shorter duration in the last
grade.
