Untitled
================

## 1. Load data

``` r
library(arrow) # to be able to load data in the .parquet format
# read application data
data_path <- "C:/Users/abhid/Documents/Courses/ORGB_690_People_Analytics/Exercise_2_AM/exercise_2_AM/"
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

### Get gender for examiners

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
```

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
```

``` r
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
    ## Ncells  4711656 251.7    8071894 431.1  4731095 252.7
    ## Vcells 49690261 379.2   93217160 711.2 80005792 610.4

### Guess the examiner’s race

``` r
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 3,806 x 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # ... with 3,796 more rows

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 x 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # ... with 3,796 more rows

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
examiner_race
```

    ## # A tibble: 3,806 x 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # ... with 3,796 more rows

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  5081844 271.4    8071894 431.1  6143819 328.2
    ## Vcells 53444993 407.8   93217160 711.2 92761970 707.8

### Examiner’s tenure

``` r
library(lubridate) # to work with dates
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

    ## # A tibble: 2,018,477 x 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # ... with 2,018,467 more rows

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

    ## # A tibble: 5,649 x 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # ... with 5,639 more rows

``` r
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5096190 272.2   14323892  765.0  14323892  765.0
    ## Vcells 65824448 502.3  134408710 1025.5 134401236 1025.5

## 3. Descriptive statistics

Let’s look at distribution of gender, race and tenure, overall in the
organization, and by technology centers (TCs) and workgroups.

### Overall distributions of gender, race and tenure.

We can we can start with simple frequencies.

``` r
subset_app_data <- app_data_sample %>% 
  mutate(race = as_factor(race), gender = as_factor(gender)) %>% 
  select(gender, race, tenure_days) 
subset_app_data %>% 
  count(gender) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 3 x 3
    ##   gender       n   pct
    ##   <fct>    <int> <dbl>
    ## 1 female  571227 0.283
    ## 2 male   1143391 0.566
    ## 3 <NA>    303859 0.151

``` r
subset_app_data %>% 
  count(race) %>% 
  mutate(pct = n/sum(n))
```

    ## # A tibble: 5 x 3
    ##   race           n      pct
    ##   <fct>      <int>    <dbl>
    ## 1 white    1279353 0.634   
    ## 2 black      90027 0.0446  
    ## 3 Asian     588988 0.292   
    ## 4 Hispanic   59657 0.0296  
    ## 5 other        452 0.000224

We can also use library `skimr` to skim the data quickly.

``` r
library(skimr)
subset_app_data %>%  
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 2018477    |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 2          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                        |
|:--------------|----------:|--------------:|:--------|---------:|:--------------------------------------------------|
| gender        |    303859 |          0.85 | FALSE   |        2 | mal: 1143391, fem: 571227                         |
| race          |         0 |          1.00 | FALSE   |        5 | whi: 1279353, Asi: 588988, bla: 90027, His: 59657 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |      sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|--------:|----:|-----:|-----:|-----:|-----:|:------|
| tenure_days   |         0 |             1 | 5531.37 | 1100.91 |  27 | 4962 | 6090 | 6336 | 6518 | ▁▁▁▂▇ |

### Plot tenure by gender and TCs

``` r
person_level_data <- app_data_sample %>% 
  group_by(examiner_id) %>% 
  summarise(
    art_unit = min(examiner_art_unit, na.rm = TRUE),
    gender = min(gender, na.rm = TRUE),
    start_year = min(year(earliest_date), na.rm = TRUE),
    latest_date = max(latest_date, na.rm = TRUE),
    tenure_days = max(tenure_days, na.rm = TRUE)
  ) %>% 
  mutate(
    tc = floor(art_unit/100)*100,
    work_group = floor(art_unit/10)*10
  ) %>% 
  filter(!is.na(gender)) # dropping all records where we don't know the gender
person_level_data
```

    ## # A tibble: 4,849 x 8
    ##    examiner_id art_unit gender start_year latest_date tenure_days    tc
    ##          <dbl>    <dbl> <chr>       <dbl> <date>            <dbl> <dbl>
    ##  1       59012     1716 male         2004 2015-07-24         4013  1700
    ##  2       59025     2465 male         2009 2017-05-18         2761  2400
    ##  3       59040     1724 female       2007 2017-05-23         3542  1700
    ##  4       59052     2138 male         2001 2007-02-28         2017  2100
    ##  5       59055     2165 male         2004 2007-12-26         1149  2100
    ##  6       59056     2124 male         2000 2017-05-22         6268  2100
    ##  7       59081     2489 male         2011 2017-05-19         2220  2400
    ##  8       59086     2487 female       2010 2017-05-18         2527  2400
    ##  9       59096     1612 male         2000 2015-11-20         5800  1600
    ## 10       59117     2439 male         2009 2011-09-02          925  2400
    ## # ... with 4,839 more rows, and 1 more variable: work_group <dbl>

``` r
ggplot(person_level_data) +
  geom_boxplot(aes(x = tenure_days, color = gender))
```

![](exercise_2_2_AA_files/figure-gfm/plot-gender-2-1.png)<!-- -->

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(tc), fill = gender), 
    position = position_stack()
    ) +
  xlab("Technology Center")
```

![](exercise_2_2_AA_files/figure-gfm/plot-gender-3-1.png)<!-- -->

### Tiling multiple plots

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(work_group), fill = gender), 
    position = position_stack()
    ) +
  xlab("Work group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + #rotate labels
  facet_wrap(vars(tc), scales = "free")
```

![](exercise_2_2_AA_files/figure-gfm/plot-gender-4-1.png)<!-- --> \##
Gender distribution

Looking at the gender distribution across different technology centers
(TC’s), it is evident that the overall number of females are relatively
small proportion of the overall population, assuming that this
observation is true for the entire data set i.e also for the examiners
that the gender package could identify the gender. For tc’s 1600 and
1700, the female proportions are almost equal or close to male
proportions. From personal experience, similar trend can be observed in
few other settings. For example, in my undergrad university there were
higher percentage of females in Biotechnology and Chemistry departments
as compared to other disciplines. Also for tc’s 2100 where the
percentage of females are relatively lesser as compared to 1600’s and
1700’s, I personally have observed this while working since I was one of
the few females within my company in this sector. Now looking at the
tenure days, there seems to be a weak correlation between gender and
tenure days as the difference in median tenure days between males and
females is about 2 years, which when compared to the overall tenure is a
relatively smaller percentage

### Plot tenure by race and TCs

``` r
person_level_data <- app_data_sample %>% 
  group_by(examiner_id) %>% 
  summarise(
    art_unit = min(examiner_art_unit, na.rm = TRUE),
    race = min(race, na.rm = TRUE),
    start_year = min(year(earliest_date), na.rm = TRUE),
    latest_date = max(latest_date, na.rm = TRUE),
    tenure_days = max(tenure_days, na.rm = TRUE)
  ) %>% 
  mutate(
    tc = floor(art_unit/100)*100,
    work_group = floor(art_unit/10)*10
  ) %>% 
  filter(!is.na(race)) # dropping all records where we don't know the race
person_level_data
```

    ## # A tibble: 5,649 x 8
    ##    examiner_id art_unit race  start_year latest_date tenure_days    tc
    ##          <dbl>    <dbl> <chr>      <dbl> <date>            <dbl> <dbl>
    ##  1       59012     1716 white       2004 2015-07-24         4013  1700
    ##  2       59025     2465 Asian       2009 2017-05-18         2761  2400
    ##  3       59030     2441 black       2005 2017-05-22         4179  2400
    ##  4       59040     1724 Asian       2007 2017-05-23         3542  1700
    ##  5       59052     2138 Asian       2001 2007-02-28         2017  2100
    ##  6       59054     2122 Asian       2000 2016-12-23         5887  2100
    ##  7       59055     2165 Asian       2004 2007-12-26         1149  2100
    ##  8       59056     2124 Asian       2000 2017-05-22         6268  2100
    ##  9       59074     2131 white       2000 2017-03-17         6255  2100
    ## 10       59081     2489 Asian       2011 2017-05-19         2220  2400
    ## # ... with 5,639 more rows, and 1 more variable: work_group <dbl>

``` r
ggplot(person_level_data) +
  geom_boxplot(aes(x = tenure_days, color = race))
```

![](exercise_2_2_AA_files/figure-gfm/plot-race-2-1.png)<!-- -->

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(tc), fill = race), 
    position = position_stack()
    ) +
  xlab("Technology Center")
```

![](exercise_2_2_AA_files/figure-gfm/plot-race-3-1.png)<!-- -->

### Tiling multiple plots

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(work_group), fill = race), 
    position = position_stack()
    ) +
  xlab("Work group") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + #rotate labels
  facet_wrap(vars(tc), scales = "free")
```

![](exercise_2_2_AA_files/figure-gfm/plot-race-4-1.png)<!-- -->

## race distribution

Looking at the race distribution across different technology centers
(TC’s) and work group, it can be observed that the combined population
of all other races is almost equal to the white population for tc’s
2100-2400 and lower for tc’s 1600 and 1700 indicating predominantly
white population across the organization which is expected given
geography of operation for the organization. Although personally I have
not worked in an organization with enough racial diversity to comment on
the above observation, but looking at the data it could indicate that
the organization’s concern about lack of diversity could be warranted.
Now to evaluate its impact on the attrition, it could be observed that
the maximum tenure and the median tenure is highest for the white
population, which could be attributed to the fact that the earliest
employees as per the data were predominantly white. Therefore further
analysis could include looking at the earliest dates when people of
other race were included in the organization and thier progression or
movement across the grades, tc’s and workgroups.
