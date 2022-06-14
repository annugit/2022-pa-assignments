Project
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
    ## Ncells  4712745 251.7    8233971 439.8  4732377 252.8
    ## Vcells 49695358 379.2   95645438 729.8 80010916 610.5

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
    ## Ncells  5052555 269.9    8233971 439.8  5642638 301.4
    ## Vcells 53380965 407.3   95645438 729.8 94130514 718.2

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
    ## Ncells  5067093 270.7   14694276  784.8  14694276  784.8
    ## Vcells 65760739 501.8  165566516 1263.2 137818096 1051.5

## 2. Adding paygrade data

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
  mutate(
    iss_days = if_else(disposal_type == "ISS", app_proc_days, NA_real_),
    abn_days = if_else(disposal_type == "ABN", app_proc_days, NA_real_)
  )%>%
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE),
    mean_iss_days = mean(iss_days, na.rm = TRUE),
    mean_abn_days = mean(abn_days, na.rm = TRUE),
    art_unit = min(examiner_art_unit, na.rm = TRUE)
  )

examiner_data <- examiner_data %>% 
  left_join(time_in_grade)
```

    ## Joining, by = "examiner_id"

## 3. Spliting Issued & Abandoned Applications Count

``` r
accepted <- app_data_sample %>% 
  group_by(examiner_id)%>%
  filter(disposal_type == "ISS") %>% 
  summarise(acc = n())

rejected <- app_data_sample %>% 
  group_by(examiner_id) %>%
  filter(disposal_type == "ABN") %>% 
  summarise(rej = n())

acc_rej_join <- accepted %>%
  left_join(rejected, by = c("examiner_id" = "examiner_id"))

acc_rej_join <- acc_rej_join %>%
 mutate(
   total = acc + rej
   ) 
```

## 4. Calculating Percent of Issued & Abandoned Applications

``` r
acc_rej_join <- acc_rej_join %>%
 mutate(
   acc_percent = (acc/total),
   rej_percent = (rej/total)
   )
```

## 5. Merging percent data with examiner_data

``` r
data_merge1 <- merge(examiner_data, acc_rej_join, by = c("examiner_id"))

data_merge1 <- subset (data_merge1, select = -total)
```

## 6. Relation between Avg time in grade & Avg App Processing Time

``` r
ggplot(data=data_merge1, 
       mapping=aes(x=mean_days_in_grade, 
                   y=mean_app_proc_days))+ 
  geom_point(size=2)+ 
  geom_smooth(method=lm, se = F)+ 
  xlab("Average Time in Grade")+
  ylab("Average Application Prosecution Time")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 863 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 863 rows containing missing values (geom_point).

![](final-2_files/figure-gfm/scatterplots-1.png)<!-- -->

``` r
data_merge1 %>%
  filter(mean_days_in_grade < 2000) %>%
  filter(0 < mean_days_in_grade) %>%
  ggplot(aes(x=mean_days_in_grade, 
                   y=mean_app_proc_days))+ 
  geom_point(size=2)+ 
  geom_smooth(method=lm, se = F)+ 
  xlab("Average Time in Grade")+
  ylab("Average Application Prosecution Time")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](final-2_files/figure-gfm/scatterplots-2.png)<!-- -->

``` r
data_merge1 %>%
  filter(mean_days_in_grade < 500) %>%
  filter(0 < mean_days_in_grade) %>%
  ggplot(aes(x=mean_days_in_grade,
                   y=mean_app_proc_days))+ 
  geom_point(size=2)+ 
  geom_smooth(method=lm, se = F)+ 
  xlab("Average Time in Grade")+
  ylab("Average Application Prosecution Time")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](final-2_files/figure-gfm/scatterplots-3.png)<!-- -->

## 7. Regression models for Q.1.

``` r
library(modelsummary)
colnames(data_merge1)
```

    ##  [1] "examiner_id"        "app_count"          "tc"                
    ##  [4] "gender"             "race"               "tenure_days"       
    ##  [7] "mean_app_proc_days" "mean_iss_days"      "mean_abn_days"     
    ## [10] "art_unit"           "mean_days_in_grade" "acc"               
    ## [13] "rej"                "acc_percent"        "rej_percent"

``` r
models <- list()
models[['m1']] <- lm(mean_app_proc_days ~ 1 + mean_days_in_grade + as_factor(gender) + as_factor(race) + as_factor(tc) + tenure_days + mean_iss_days + mean_abn_days + acc_percent, data = data_merge1) 

models[['m2']] <- lm(mean_app_proc_days ~ 1 + acc_percent, data = data_merge1)

models[['m3']] <- lm(mean_app_proc_days ~ 1 + mean_days_in_grade + as_factor(gender) + as_factor(tc) + tenure_days + acc_percent, data = data_merge1) 

models[['m4']] <- lm(mean_app_proc_days ~ 1 + mean_days_in_grade + as_factor(tc) + tenure_days + acc_percent, data = data_merge1) 

models[['m5']] <- lm(mean_app_proc_days ~ 1 + mean_days_in_grade + as_factor(tc)  + acc_percent, data = data_merge1) 

models[['m6']] <- lm(mean_app_proc_days ~ 1 + as_factor(tc) +  acc_percent, data = data_merge1) 

models[['m7']] <- lm(mean_app_proc_days ~ 1 + mean_days_in_grade + as_factor(gender) + as_factor(race) + as_factor(tc) + tenure_days + acc_percent, data = data_merge1) 

models[['m8']] <- lm(mean_app_proc_days ~ 1 + tenure_days, data = data_merge1)

models[['m9']] <- lm(mean_app_proc_days ~ 1 + tenure_days + as_factor(tc), data = data_merge1)

models[['m10']] <- lm(mean_app_proc_days ~ 1 + tenure_days + as_factor(tc) + acc_percent, data = data_merge1)


modelsummary(models)
```

|                         |     m1     |     m2     |     m3     |     m4     |     m5     |     m6     |     m7     |     m8     |     m9     |    m10     |
|:------------------------|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|
| (Intercept)             |  -53.104   |  1326.261  |  879.470   |  882.382   |  1275.528  |  1260.869  |  878.723   |  956.786   |  702.934   |  901.567   |
|                         |  (5.176)   |  (11.348)  |  (16.601)  |  (14.868)  |  (12.453)  |  (11.383)  |  (16.644)  |  (11.502)  |  (13.962)  |  (13.432)  |
| mean_days_in_grade      |   0.000    |            |   -0.011   |   -0.011   |   0.019    |            |   -0.011   |            |            |            |
|                         |  (0.001)   |            |  (0.005)   |  (0.005)   |  (0.005)   |            |  (0.005)   |            |            |            |
| as_factor(gender)female |   2.652    |            |   -1.311   |            |            |            |   -1.261   |            |            |            |
|                         |  (1.655)   |            |  (7.475)   |            |            |            |  (7.499)   |            |            |            |
| as_factor(race)Asian    |   -3.738   |            |            |            |            |            |   8.954    |            |            |            |
|                         |  (1.762)   |            |            |            |            |            |  (7.986)   |            |            |            |
| as_factor(race)black    |   -6.708   |            |            |            |            |            |  -22.411   |            |            |            |
|                         |  (4.101)   |            |            |            |            |            |  (18.594)  |            |            |            |
| as_factor(race)Hispanic |   7.781    |            |            |            |            |            |   1.410    |            |            |            |
|                         |  (3.644)   |            |            |            |            |            |  (16.527)  |            |            |            |
| as_factor(race)other    |  -82.508   |            |            |            |            |            |   32.892   |            |            |            |
|                         |  (44.803)  |            |            |            |            |            | (203.110)  |            |            |            |
| as_factor(tc)1700       |   0.817    |            |  124.908   |  128.655   |   59.584   |   48.655   |  124.560   |            |   72.815   |  105.739   |
|                         |  (2.312)   |            |  (10.248)  |  (9.499)   |  (10.798)  |  (10.002)  |  (10.255)  |            |  (10.221)  |  (8.839)   |
| as_factor(tc)2100       |   4.807    |            |  386.248   |  394.664   |  314.746   |  313.007   |  385.333   |            |  292.966   |  393.318   |
|                         |  (2.706)   |            |  (10.310)  |  (9.269)   |  (10.456)  |  (9.839)   |  (10.430)  |            |  (9.741)   |  (8.815)   |
| as_factor(tc)2400       |   0.466    |            |  374.167   |  379.762   |  213.237   |  217.405   |  373.191   |            |  287.897   |  381.641   |
|                         |  (3.048)   |            |  (12.104)  |  (10.823)  |  (11.475)  |  (10.901)  |  (12.234)  |            |  (11.492)  |  (10.367)  |
| tenure_days             |   0.001    |            |   0.094    |   0.092    |            |            |   0.094    |   0.048    |   0.065    |   0.086    |
|                         |  (0.001)   |            |  (0.003)   |  (0.002)   |            |            |  (0.003)   |  (0.002)   |  (0.002)   |  (0.002)   |
| mean_iss_days           |   0.480    |            |            |            |            |            |            |            |            |            |
|                         |  (0.005)   |            |            |            |            |            |            |            |            |            |
| mean_abn_days           |   0.492    |            |            |            |            |            |            |            |            |            |
|                         |  (0.005)   |            |            |            |            |            |            |            |            |            |
| acc_percent             |  137.322   |  -220.390  |  -588.152  |  -583.035  |  -393.196  |  -383.036  |  -589.121  |            |            |  -600.009  |
|                         |  (4.998)   |  (18.233)  |  (18.205)  |  (16.713)  |  (18.480)  |  (16.736)  |  (18.256)  |            |            |  (15.584)  |
| Num.Obs.                |    3668    |    5005    |    3668    |    4311    |    4311    |    5005    |    3668    |    5282    |    5282    |    5005    |
| R2                      |   0.972    |   0.028    |   0.423    |   0.426    |   0.230    |   0.239    |   0.423    |   0.075    |   0.259    |   0.421    |
| R2 Adj.                 |   0.972    |   0.028    |   0.421    |   0.425    |   0.230    |   0.238    |   0.421    |   0.075    |   0.258    |   0.421    |
| AIC                     |  38308.7   |  70235.4   |  49392.6   |  57941.2   |  59203.6   |  69018.8   |  49397.4   |  74425.2   |  73259.4   |  67649.0   |
| BIC                     |  38401.8   |  70255.0   |  49448.4   |  57992.2   |  59248.2   |  69057.9   |  49478.1   |  74444.9   |  73298.9   |  67694.6   |
| Log.Lik.                | -19139.331 | -35114.703 | -24687.286 | -28962.611 | -29594.786 | -34503.397 | -24685.698 | -37209.594 | -36623.715 | -33817.480 |
| F                       |  9744.343  |  146.112   |  382.656   |  532.503   |  257.829   |  392.482   |  243.741   |  427.246   |  460.929   |  728.188   |
| RMSE                    |   44.74    |   269.67   |   202.88   |   200.37   |   231.99   |   238.73   |   202.90   |   277.51   |   248.44   |   208.18   |

## 8. Regression models for Q.2.

``` r
models_2 <- list()
#colnames(data_merge1)
models_2[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender) + as_factor(race) + as_factor(tc) + tenure_days + acc_percent, data = data_merge1) 

models_2[['m2']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender) + as_factor(race) + as_factor(tc) + tenure_days + mean_abn_days + acc_percent, data = data_merge1) 

models_2[['m3']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender) + as_factor(race) + as_factor(tc) + tenure_days + mean_iss_days + acc_percent, data = data_merge1) 

models_2[['m4']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender) + as_factor(race) + tenure_days + mean_iss_days + acc_percent, data = data_merge1) 

models_2[['m5']] <- lm(mean_days_in_grade ~ 1 + tenure_days + mean_iss_days + mean_abn_days + acc_percent, data = data_merge1) 

models_2[['m6']] <- lm(mean_days_in_grade ~ 1 + tenure_days + mean_iss_days + mean_abn_days, data = data_merge1) 


modelsummary(models_2)
```

|                         |     m1     |     m2     |     m3     |     m4     |     m5     |     m6     |
|:------------------------|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|
| (Intercept)             |  243.972   |  195.246   |  280.635   |  302.554   |  304.837   |  241.709   |
|                         |  (72.868)  |  (68.768)  |  (77.251)  |  (72.306)  |  (63.246)  |  (50.375)  |
| mean_app_proc_days      |   -0.124   |            |            |            |            |            |
|                         |  (0.055)   |            |            |            |            |            |
| as_factor(gender)female |  -37.184   |  -37.931   |  -36.555   |  -35.953   |            |            |
|                         |  (24.763)  |  (24.780)  |  (24.757)  |  (24.314)  |            |            |
| as_factor(race)Asian    |   10.827   |   10.856   |   11.161   |   11.541   |            |            |
|                         |  (26.382)  |  (26.400)  |  (26.376)  |  (26.115)  |            |            |
| as_factor(race)black    |   68.625   |   70.579   |   68.694   |   69.400   |            |            |
|                         |  (61.415)  |  (61.431)  |  (61.394)  |  (61.137)  |            |            |
| as_factor(race)Hispanic |   14.289   |   13.617   |   13.269   |   12.883   |            |            |
|                         |  (54.586)  |  (54.610)  |  (54.572)  |  (54.423)  |            |            |
| as_factor(race)other    |  -78.157   |  -83.162   |  -49.782   |  -48.233   |            |            |
|                         | (670.857)  | (671.136)  | (670.781)  | (670.511)  |            |            |
| as_factor(tc)1700       |   -2.416   |   -7.415   |   -2.776   |            |            |            |
|                         |  (34.548)  |  (34.641)  |  (34.329)  |            |            |            |
| as_factor(tc)2100       |  -27.442   |  -45.944   |  -23.953   |            |            |            |
|                         |  (40.368)  |  (39.940)  |  (39.413)  |            |            |            |
| as_factor(tc)2400       |   28.982   |   12.839   |   30.479   |            |            |            |
|                         |  (45.256)  |  (45.438)  |  (44.171)  |            |            |            |
| tenure_days             |   0.099    |   0.095    |   0.100    |   0.098    |   0.091    |   0.088    |
|                         |  (0.010)   |  (0.010)   |  (0.010)   |  (0.008)   |  (0.007)   |  (0.007)   |
| acc_percent             |  -23.663   |   5.409    |  -69.226   |  -75.925   |  -91.039   |            |
|                         |  (68.347)  |  (67.508)  |  (74.818)  |  (62.714)  |  (55.169)  |            |
| mean_abn_days           |            |   -0.077   |            |            |   0.072    |   0.049    |
|                         |            |  (0.053)   |            |            |  (0.064)   |  (0.062)   |
| mean_iss_days           |            |            |   -0.129   |   -0.136   |   -0.173   |   -0.133   |
|                         |            |            |  (0.048)   |  (0.041)   |  (0.059)   |  (0.054)   |
| Num.Obs.                |    3668    |    3668    |    3668    |    3668    |    4311    |    4311    |
| R2                      |   0.039    |   0.038    |   0.039    |   0.039    |   0.038    |   0.037    |
| R2 Adj.                 |   0.036    |   0.035    |   0.036    |   0.036    |   0.037    |   0.036    |
| AIC                     |  58162.5   |  58165.6   |  58160.5   |  58157.4   |  67993.5   |  67994.2   |
| BIC                     |  58243.2   |  58246.3   |  58241.2   |  58219.4   |  68031.7   |  68026.0   |
| Log.Lik.                | -29068.257 | -29069.790 | -29067.253 | -29068.685 | -33990.729 | -33992.092 |
| F                       |   13.405   |   13.116   |   13.594   |   18.335   |   42.253   |   55.408   |
| RMSE                    |   670.17   |   670.45   |   669.98   |   669.97   |   643.09   |   643.22   |

## 9. Regression models for Q.2. with individual variables

``` r
models_3 <- list()
#colnames(data_merge1)
models_3[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days, data = data_merge1) 

models_3[['m2']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender), data = data_merge1) 

models_3[['m3']] <- lm(mean_days_in_grade ~ 1 + as_factor(race), data = data_merge1) 

models_3[['m4']] <- lm(mean_days_in_grade ~ 1 + tenure_days, data = data_merge1) 

models_3[['m5']] <- lm(mean_days_in_grade ~ 1 + as_factor(tc), data = data_merge1) 

models_3[['m6']] <- lm(mean_days_in_grade ~ 1 + mean_iss_days, data = data_merge1) 

models_3[['m7']] <- lm(mean_days_in_grade ~ 1 + mean_abn_days, data = data_merge1) 

models_3[['m8']] <- lm(mean_days_in_grade ~ 1 + acc_percent, data = data_merge1) 

models_3[['m9']] <- lm(mean_days_in_grade ~ 1 + rej_percent, data = data_merge1) 


modelsummary(models_3)
```

|                         |     m1     |     m2     |     m3     |     m4     |     m5     |     m6     |     m7     |     m8     |     m9     |
|:------------------------|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|:----------:|
| (Intercept)             |  515.951   |  546.314   |  548.794   |  174.064   |  626.503   |  563.788   |  481.119   |  477.837   |  596.319   |
|                         |  (44.613)  |  (13.286)  |  (12.531)  |  (32.197)  |  (22.291)  |  (41.268)  |  (43.932)  |  (30.483)  |  (22.753)  |
| mean_app_proc_days      |   0.024    |            |            |            |            |            |            |            |            |
|                         |  (0.036)   |            |            |            |            |            |            |            |            |
| as_factor(gender)female |            |   -4.376   |            |            |            |            |            |            |            |
|                         |            |  (23.853)  |            |            |            |            |            |            |            |
| as_factor(race)Asian    |            |            |  -11.046   |            |            |            |            |            |            |
|                         |            |            |  (21.649)  |            |            |            |            |            |            |
| as_factor(race)black    |            |            |   48.226   |            |            |            |            |            |            |
|                         |            |            |  (49.695)  |            |            |            |            |            |            |
| as_factor(race)Hispanic |            |            |  -40.476   |            |            |            |            |            |            |
|                         |            |            |  (49.822)  |            |            |            |            |            |            |
| as_factor(race)other    |            |            |  -81.594   |            |            |            |            |            |            |
|                         |            |            | (650.654)  |            |            |            |            |            |            |
| tenure_days             |            |            |            |   0.078    |            |            |            |            |            |
|                         |            |            |            |  (0.006)   |            |            |            |            |            |
| as_factor(tc)1700       |            |            |            |            |  -72.014   |            |            |            |            |
|                         |            |            |            |            |  (29.851)  |            |            |            |            |
| as_factor(tc)2100       |            |            |            |            |  -101.885  |            |            |            |            |
|                         |            |            |            |            |  (27.588)  |            |            |            |            |
| as_factor(tc)2400       |            |            |            |            |  -129.581  |            |            |            |            |
|                         |            |            |            |            |  (31.008)  |            |            |            |            |
| mean_iss_days           |            |            |            |            |            |   -0.014   |            |            |            |
|                         |            |            |            |            |            |  (0.031)   |            |            |            |
| mean_abn_days           |            |            |            |            |            |            |   0.057    |            |            |
|                         |            |            |            |            |            |            |  (0.037)   |            |            |
| acc_percent             |            |            |            |            |            |            |            |  118.482   |            |
|                         |            |            |            |            |            |            |            |  (49.255)  |            |
| rej_percent             |            |            |            |            |            |            |            |            |  -118.482  |
|                         |            |            |            |            |            |            |            |            |  (49.255)  |
| Num.Obs.                |    4419    |    3765    |    4419    |    4419    |    4419    |    4419    |    4311    |    4311    |    4311    |
| R2                      |   0.000    |   0.000    |   0.000    |   0.032    |   0.005    |   0.000    |   0.001    |   0.001    |   0.001    |
| R2 Adj.                 |   0.000    |   0.000    |   0.000    |   0.032    |   0.004    |   0.000    |   0.000    |   0.001    |   0.001    |
| AIC                     |  69793.9   |  59767.2   |  69798.3   |  69650.3   |  69778.2   |  69794.1   |  68151.1   |  68147.6   |  68147.6   |
| BIC                     |  69813.0   |  59785.9   |  69836.7   |  69669.5   |  69810.2   |  69813.3   |  68170.2   |  68166.8   |  68166.8   |
| Log.Lik.                | -34893.930 | -29880.620 | -34893.153 | -34822.155 | -34884.107 | -34894.062 | -34072.525 | -34070.823 | -34070.823 |
| F                       |   0.467    |   0.034    |   0.505    |  146.322   |   6.713    |   0.203    |   2.380    |   5.786    |   5.786    |
| RMSE                    |   650.43   |   677.05   |   650.53   |   639.95   |   649.13   |   650.45   |   655.19   |   654.93   |   654.93   |
