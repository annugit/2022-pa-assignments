Exercise_5\_2
================

``` r
data_path <- "C:/Users/abhid/Documents/Courses/ORGB_690_People_Analytics/Exercise_5_AM/USPTO_data/"

app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

## 1. Adding gender, race and tenure data

``` r
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4711724 251.7    8232512 439.7  4731365 252.7
    ## Vcells 49693122 379.2   95642772 729.7 80008695 610.5

``` r
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
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

app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  5051534 269.8    8232512 439.7  5641617 301.3
    ## Vcells 53378729 407.3   95642772 729.7 94128278 718.2

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018) %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )

app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5066072 270.6   14692108  784.7  14692108  784.7
    ## Vcells 65758503 501.7  165561909 1263.2 137815860 1051.5

## 2. adding paygrade data

``` r
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 6
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): examiner_name, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
examiner_ids <- read_csv(paste0(data_path,"examiner_ids.csv"))
```

    ## Rows: 19454 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): examiner_name
    ## dbl (3): old_pid, new_pid, patex_id
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
examiner_gs <- examiner_gs %>% 
  left_join(examiner_ids) %>% 
  select(
    grade = examiner_grade,
    start_date,
    end_date,
    examiner_id = patex_id
  )
```

    ## Joining, by = c("examiner_name", "old_pid", "new_pid")

``` r
time_in_grade <- examiner_gs %>% 
  mutate(
    start_date = mdy(start_date), # converting into proper date type
    end_date = mdy(end_date), # converting into proper date type
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  group_by(examiner_id) %>% 
  filter(grade!=max(grade, na.rm = TRUE)) %>% # dropping the highest grade record
  summarise(mean_days_in_grade = mean(days_in_grade, na.rm = TRUE))

time_in_grade
```

    ## # A tibble: 10,860 x 2
    ##    examiner_id mean_days_in_grade
    ##          <dbl>              <dbl>
    ##  1       59012               356.
    ##  2       59015               783 
    ##  3       59016               341.
    ##  4       59018               368.
    ##  5       59019               293 
    ##  6       59025               485 
    ##  7       59027               364.
    ##  8       59030               493.
    ##  9       59033               258.
    ## 10       59035               308.
    ## # ... with 10,850 more rows

``` r
examiner_data <- app_data_sample %>% 
  filter(disposal_type!="PEND") %>% # here, we exclude in-process applications
  mutate(
    app_start_date = ymd(filing_date),
    app_end_date = case_when(
      disposal_type == "ISS" ~ ymd(patent_issue_date), # for issued patents
      disposal_type == "ABN" ~ ymd(abandon_date), # for abandoned applications
      TRUE ~ NA_Date_
    ),
    app_proc_days = interval(app_start_date, app_end_date) %/% days(1)) %>% 
  filter(app_proc_days>0 & app_proc_days < 3650) %>% # limit to 0-10 years
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE)
  )


examiner_data <- app_data_sample %>% 
  filter(disposal_type!="PEND") %>% # here, we exclude in-process applications
  mutate(
    app_start_date = ymd(filing_date),
    app_end_date = case_when(
      disposal_type == "ISS" ~ ymd(patent_issue_date), # for issued patents
      disposal_type == "ABN" ~ ymd(abandon_date), # for abandoned applications
      TRUE ~ NA_Date_
    ),
    app_proc_days = interval(app_start_date, app_end_date) %/% days(1)) %>% 
  filter(app_proc_days>0 & app_proc_days < 3650) %>% # limit to 0-10 years
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE)
  )

examiner_data
```

    ## # A tibble: 5,549 x 7
    ##    examiner_id app_count    tc gender race  tenure_days mean_app_proc_days
    ##          <dbl>     <int> <dbl> <chr>  <chr>       <dbl>              <dbl>
    ##  1       59012        84  1700 male   white        4013              1295.
    ##  2       59025        96  2400 male   Asian        2761              1152.
    ##  3       59030       358  2400 <NA>   black        4179              1008.
    ##  4       59040       233  1700 female Asian        3542              1305.
    ##  5       59052         8  2100 male   Asian        2017               535.
    ##  6       59054        10  2100 <NA>   Asian        5887              1297 
    ##  7       59055         2  2100 male   Asian        1149               932.
    ##  8       59056      1019  2100 male   Asian        6268              1077.
    ##  9       59074       166  2100 <NA>   white        6255              1579.
    ## 10       59081        48  2400 male   Asian        2220              1317.
    ## # ... with 5,539 more rows

``` r
examiner_data <- examiner_data %>% 
  left_join(time_in_grade)
```

    ## Joining, by = "examiner_id"

## Defining correlations

``` r
examiner_data_test1 <- examiner_data %>%
  filter(mean_days_in_grade>0)

cor(examiner_data_test1$mean_days_in_grade, examiner_data_test1$mean_app_proc_days)
```

    ## [1] -0.02941389

Findings: We observe that the correlation between average time in grade
and average application processing time is -0.029, which is greater than
-0.8 and hence significant.

``` r
scatterplot(mean_days_in_grade ~ mean_app_proc_days | gender, data = examiner_data_test1, 
            smoother = FALSE, grid = FALSE, frame = FALSE)
```

    ## Warning in plot.window(...): "smoother" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "smoother" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in title(...): "smoother" is not a graphical parameter

![](exercise_5_2_AA_files/figure-gfm/scatterplot%20gender-1.png)<!-- -->
We can observe that on an average, as the number of application
processing days increases, the number of days in a grade first increases
till around 1250-1500 and then decreases again. This can also be better
shown in a graph with x and y axes interchanged as shown below:

``` r
scatterplot(mean_app_proc_days ~ mean_days_in_grade | gender, data = examiner_data_test1, 
            smoother = FALSE, grid = FALSE, frame = FALSE)
```

    ## Warning in plot.window(...): "smoother" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "smoother" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in title(...): "smoother" is not a graphical parameter

![](exercise_5_2_AA_files/figure-gfm/scatterplot%20gender%20(axes%20interchanged)-1.png)<!-- -->

Also, although there is a slight difference only, we can observe from
the trend lines that for males, as the number of application processing
days increases, the number of days in a grade tend to decrease whereas
it remains fairly constant for females.

``` r
scatterplot(mean_days_in_grade ~ mean_app_proc_days | race, data = examiner_data_test1, 
            smoother = FALSE, grid = FALSE, frame = FALSE)
```

    ## Warning in plot.window(...): "smoother" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "smoother" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in title(...): "smoother" is not a graphical parameter

    ## Warning in smoother(.x[subs], .y[subs], col = col[i], log.x = logged("x"), :
    ## could not fit smooth

![](exercise_5_2_AA_files/figure-gfm/scatterplot%20race-1.png)<!-- -->

``` r
scatterplot(mean_app_proc_days ~ mean_days_in_grade | race, data = examiner_data_test1, 
            smoother = FALSE, grid = FALSE, frame = FALSE)
```

    ## Warning in plot.window(...): "smoother" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "smoother" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "smoother" is not a
    ## graphical parameter

    ## Warning in title(...): "smoother" is not a graphical parameter

    ## Warning in smoother(.x[subs], .y[subs], col = col[i], log.x = logged("x"), :
    ## could not fit smooth

![](exercise_5_2_AA_files/figure-gfm/scatterplot%20race%20(swapped%20axes)-1.png)<!-- -->
We can observe that as the average number of application processing days
increases, the average number of days in a grade tends to increase for
Hispanics, remains fairly constant for Asians, and slightly decreases
for white and black races.

## Regression analysis

In the first model, we will see the effect of application prosecution
time on time in grade. In the 2nd model, we will add gender to the
previous model, and in the 3rd model, we will further add race to the
2nd model.

``` r
library(modelsummary)
models <- list()
models[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days, data = examiner_data) 
models[['m2']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender),          data = examiner_data) 
models[['m3']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender) + as_factor(race), data = examiner_data) 

modelsummary(models)
```

|                         |     m1     |     m2     |     m3     |
|:------------------------|:----------:|:----------:|:----------:|
| (Intercept)             |  528.481   |  550.975   |  552.690   |
|                         |  (43.856)  |  (49.860)  |  (50.105)  |
| mean_app_proc_days      |   0.014    |   -0.004   |   -0.003   |
|                         |  (0.035)   |  (0.039)   |  (0.040)   |
| as_factor(gender)female |            |   -4.166   |   -4.611   |
|                         |            |  (23.854)  |  (23.871)  |
| as_factor(race)Asian    |            |            |   -9.874   |
|                         |            |            |  (25.693)  |
| as_factor(race)black    |            |            |   51.199   |
|                         |            |            |  (60.209)  |
| as_factor(race)Hispanic |            |            |  -46.899   |
|                         |            |            |  (53.547)  |
| as_factor(race)other    |            |            |  -81.593   |
|                         |            |            | (681.698)  |
| Num.Obs.                |    4503    |    3838    |    3838    |
| R2                      |   0.000    |   0.000    |   0.000    |
| R2 Adj.                 |   0.000    |   -0.001   |   -0.001   |
| AIC                     |  71176.4   |  60975.0   |  60981.3   |
| BIC                     |  71195.6   |  61000.0   |  61031.3   |
| Log.Lik.                | -35585.191 | -30483.507 | -30482.639 |
| F                       |   0.160    |   0.019    |   0.295    |
| RMSE                    |   654.48   |   681.30   |   681.50   |

From the above results, we observe that model 2 is the best in
explaining the outcome because of the following points: 1) F-statistic,
or p-value, is the lowest for model 2. Also, since it is lower than
0.05, it is statistically significant.  
2) AIC (Akaike’s Information Criteria) and BIC (Bayesian information
criteria) are the lowest for model 2. These metrics penalize the
inclusion of additional variables, so the lower the AIC and BIC values,
the better.

Hence, from the above results, we can say that the application
processing time and gender have an explainable effect on the number of
days in grade whereas race does not. Since the correlation of came out
to be -0.029, which is significant, we plotted scatterplots between the
average number of days in grade and application processing days to
visualize the correlation. We can observe that male gender is a bit
negatively correlated with the number of days in grade. This is also
proved by the negative correlation we got from cor function. However,
according to the regression model, race does not significantly explain
the outcome.

## Limitations & Threats to Inference

1.  We have a limited set of data because of omitting the rows with
    unidentified values while taking gender and race. Because of this,
    we will get skewed results and they will not be representative of
    the whole dataset. It is possible that they do not show actual
    impact of a variable of the number of days in a grade.
2.  The results imply that the processing time might impact the time in
    grade within a particular gender or race. But the underlying factors
    associated with these results will not be evident with the limited
    data set. As discussed above,since majority of the examiners are not
    identified, we might be excluding a lot of examiners which are not
    from US.
