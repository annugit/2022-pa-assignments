Exercise_3
================

``` r
data_path <- "C:/Users/anuak/Documents/Course Work/Summer 2022/People Analytics"
setwd(data_path)
app_data_sample <- read_parquet("app_data_sample.parquet")
```

``` r
app_data_sample
```

    ## # A tibble: 2,018,477 x 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ... with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

### Get gender for examiners

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

    ## # A tibble: 2,595 x 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # ... with 2,585 more rows

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
examiner_names_gender
```

    ## # A tibble: 1,822 x 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # ... with 1,812 more rows

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
    ## Ncells  4551400 243.1    7796381 416.4  5084391 271.6
    ## Vcells 49526090 377.9   92424540 705.2 79841647 609.2

### Guess the examiner’s race

``` r
library(wru)
```

    ## Warning: package 'wru' was built under R version 4.1.3

``` r
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
    ## Ncells  4964022 265.2    7796381 416.4  7796381 416.4
    ## Vcells 53320479 406.9   92424540 705.2 90426939 690.0

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
    ## Ncells  4977939 265.9   14090052  752.5  14090052  752.5
    ## Vcells 65698980 501.3  133267337 1016.8 133267271 1016.8

## 3. Descriptive statistics

Let’s look at distribution of gender, race and tenure, overall in the
organization, and by technology centers (TCs) and workgroups.

### Overall distributions of gender, race and tenure.

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

![](Exercise_3_AA_files/figure-gfm/plot-gender-2-1.png)<!-- -->

``` r
ggplot(person_level_data) +
  geom_bar(
    aes(x=as_factor(tc), fill = gender), 
    position = position_stack()
    ) +
  xlab("Technology Center")
```

![](Exercise_3_AA_files/figure-gfm/plot-gender-3-1.png)<!-- -->

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

![](Exercise_3_AA_files/figure-gfm/plot-gender-4-1.png)<!-- -->

## 1. Art unit size

We have calculated art unit size by performing group-by on examiner art
unit and taking the count of examiners for each art unit.

``` r
art_unit_size <- app_data_sample %>% 
  group_by(examiner_art_unit) %>% summarise(count=n())
```

## 2. Art unit gender ratio

We have calculated art unit gender ratio by performing group-by on
examiner_art_unit, gender, and race, i.e, we have taken the count of
each unique combination of examiner_art_unit, gender, and race. Then, we
joined this table and the table that has art unit size. In the resulting
table, we took the ratio of art unit and gender, i.e., we calculated how
many people of a particular gender are there in each art unit.

Issues faced: The gender package did not classify some of the names as
discussed in class, because the package is based on the birth
certificates names from US and thus the database will not classify some
names outside US. For our analysis, we have not excluded the
unclassified names. Rather, we calculated the gender ratio by dividing
each gender group within each of the art units by the total size of the
art unit(Male, female, NA). A better way for predicting the tenure could
include removing the art units with a high NA gender ratio (maybe \>0.5)
to create a linear model.

``` r
art_unit_gender_count <- app_data_sample %>% 
  group_by(examiner_art_unit,gender,race) %>% summarise(count=n())
```

    ## `summarise()` has grouped output by 'examiner_art_unit', 'gender'. You can
    ## override using the `.groups` argument.

``` r
art_unit_gender_ratio <- art_unit_gender_count %>% left_join(art_unit_size, by = "examiner_art_unit")

art_unit_gender_ratio <- art_unit_gender_ratio %>% 
  mutate(
    art_gender_ratio = count.x/count.y
  )
```

## 3. Tenure_days for each combination of examiner’s art_unit, gender, & race

We have taken the mean of tenure days for all the people of a particular
art unit, gender, and race using aggregate function. Then, we joined the
resulting table with “art_unit_gender_ratio” table.

Issues faced: The tenure days for each art unit has been aggregated for
the available number of tenure days. We could have evaluated the spread
of the tenure days within a particular art unit to determine a
confidence interval for the tenure days. We are unsure if having
sub-models for the tenure days would be too extensive for a linear
regression or that aggregating it has any effect on its correlation with
other variables.

``` r
test <- aggregate(tenure_days~examiner_art_unit+gender+race, app_data_sample, mean)
art_unit_gender_ratio <- test %>% left_join(art_unit_gender_ratio, by = "examiner_art_unit")
```

## 4. Correlation between tenure_days and numeric variables

We have calculated the correlation between tenure_days and the two
numeric variables - art unit size, which is characterized by count.y,
and art unit-gender ratio. We observed that both art unit size and art
unit gender ratio have a negligible correlation with tenure_days (0.23
and -0.011 respectively).

``` r
cor(art_unit_gender_ratio$tenure_days, art_unit_gender_ratio$count.y)
```

    ## [1] 0.227077

``` r
cor(art_unit_gender_ratio$tenure_days,art_unit_gender_ratio$art_gender_ratio)
```

    ## [1] -0.0111859

## 5. Regressions

We ran regressions between tenure_days and each of the 4 variables -
gender, art unit size, race, and art unit gender ratio. t-statistic is
calculated by dividing the beta coefficient (b) by the standard error
(SE), i.e., it measures the number of standard deviations that b is away
from 0. Thus, a large t-statistic will produce a small p-value, and the
lower the p value, the more statistically significant the variable. So,
we are looking for p values of 0.05 or below.

## 5a. Regressing tenure_days with art unit gender ratio

We observed that regressing tenure_days with art unit gender ratio
generates a p value of 0.306 which is higher than 0.05, and hence, we
consider this variable to be not significant.

``` r
model <- lm(tenure_days~art_gender_ratio, data=art_unit_gender_ratio)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ art_gender_ratio, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5076.1  -463.3   382.8   910.9  1317.7 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       5105.22      15.35 332.512   <2e-16 ***
    ## art_gender_ratio   -96.95      79.39  -1.221    0.222    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1260 on 11919 degrees of freedom
    ## Multiple R-squared:  0.0001251,  Adjusted R-squared:  4.124e-05 
    ## F-statistic: 1.492 on 1 and 11919 DF,  p-value: 0.222

``` r
modelsummary(model)
```

|                  |   Model 1   |
|:-----------------|:-----------:|
| (Intercept)      |  5105.224   |
|                  |  (15.354)   |
| art_gender_ratio |   -96.955   |
|                  |  (79.387)   |
| Num.Obs.         |    11921    |
| R2               |    0.000    |
| R2 Adj.          |    0.000    |
| AIC              |  204047.4   |
| BIC              |  204069.5   |
| Log.Lik.         | -102020.680 |
| F                |    1.492    |
| RMSE             |   1260.43   |

## 5b. Regressing tenure_days with art unit size

We observed that regressing tenure_days with art unit size produces a p
value of 2 times 10 to the power of -16, which is much lower than 0.05,
and hence, we consider race variable to be significant. The correlation
result does not agree with this result, i.e., we found a correlation
coefficient for art unit size that was not significant.

``` r
model1 <- lm(tenure_days~count.y, data=art_unit_gender_ratio)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ count.y, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4916.0  -421.3   331.8   855.0  1703.1 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.631e+03  2.134e+01  217.05   <2e-16 ***
    ## count.y     6.185e-02  2.430e-03   25.46   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1228 on 11919 degrees of freedom
    ## Multiple R-squared:  0.05156,    Adjusted R-squared:  0.05148 
    ## F-statistic:   648 on 1 and 11919 DF,  p-value: < 2.2e-16

``` r
modelsummary(model1)
```

|             |   Model 1   |
|:------------|:-----------:|
| (Intercept) |  4631.238   |
|             |  (21.337)   |
| count.y     |    0.062    |
|             |   (0.002)   |
| Num.Obs.    |    11921    |
| R2          |    0.052    |
| R2 Adj.     |    0.051    |
| AIC         |  203417.7   |
| BIC         |  203439.9   |
| Log.Lik.    | -101705.872 |
| F           |   648.005   |
| RMSE        |   1227.58   |

## 5c. Regressing tenure_days with race

We observed that regressing tenure_days with race produces p values of
0.599, 0.631, 0.347, and 0.582 for black, Hispanic, other, and white
category, each of which are higher than 0.05, and hence, we consider
race variable to be not significant.

``` r
model3 <- lm(tenure_days~race.y, data=art_unit_gender_ratio)
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ race.y, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5073.7  -464.5   381.3   912.2  1268.3 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5081.74      19.51 260.442   <2e-16 ***
    ## race.yblack       18.91      36.02   0.525    0.599    
    ## race.yHispanic    17.72      36.93   0.480    0.631    
    ## race.yother      265.63     282.55   0.940    0.347    
    ## race.ywhite       15.03      27.27   0.551    0.582    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1261 on 11916 degrees of freedom
    ## Multiple R-squared:  0.0001085,  Adjusted R-squared:  -0.0002272 
    ## F-statistic: 0.3232 on 4 and 11916 DF,  p-value: 0.8626

``` r
modelsummary(model3)
```

|                |   Model 1   |
|:---------------|:-----------:|
| (Intercept)    |  5081.744   |
|                |  (19.512)   |
| race.yblack    |   18.914    |
|                |  (36.016)   |
| race.yHispanic |   17.719    |
|                |  (36.933)   |
| race.yother    |   265.627   |
|                |  (282.553)  |
| race.ywhite    |   15.029    |
|                |  (27.272)   |
| Num.Obs.       |    11921    |
| R2             |    0.000    |
| R2 Adj.        |    0.000    |
| AIC            |  204053.6   |
| BIC            |  204097.9   |
| Log.Lik.       | -102020.780 |
| F              |    0.323    |
| RMSE           |   1260.60   |

## 5d. Regressing tenure_days with gender

We observed that regressing tenure_days with gender produces p values of
0.2, which is higher than 0.05, and hence, we consider it to be not
significant.

``` r
model4 <- lm(tenure_days~gender.y, data=art_unit_gender_ratio)
summary(model4)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ gender.y, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5045.2  -462.2   382.4   915.8  1277.8 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5107.59      20.46 249.595   <2e-16 ***
    ## gender.ymale   -35.38      27.70  -1.277    0.202    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1264 on 8400 degrees of freedom
    ##   (3519 observations deleted due to missingness)
    ## Multiple R-squared:  0.0001941,  Adjusted R-squared:  7.509e-05 
    ## F-statistic: 1.631 on 1 and 8400 DF,  p-value: 0.2016

``` r
modelsummary(model4)
```

|              |  Model 1   |
|:-------------|:----------:|
| (Intercept)  |  5107.589  |
|              |  (20.463)  |
| gender.ymale |  -35.380   |
|              |  (27.704)  |
| Num.Obs.     |    8402    |
| R2           |   0.000    |
| R2 Adj.      |   0.000    |
| AIC          |  143868.4  |
| BIC          |  143889.5  |
| Log.Lik.     | -71931.219 |
| F            |   1.631    |
| RMSE         |  1264.44   |

## 5e. Regressing tenure_days with all the variables together

By performing this, we do not get any different result than already
discussed above.

``` r
model2 <- lm(tenure_days~count.y+art_gender_ratio+gender.y+race.y, data=art_unit_gender_ratio)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ count.y + art_gender_ratio + gender.y + 
    ##     race.y, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4963.5  -436.6   333.0   850.7  1715.8 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.613e+03  3.600e+01 128.113   <2e-16 ***
    ## count.y           6.283e-02  2.938e-03  21.382   <2e-16 ***
    ## art_gender_ratio  1.211e+02  1.183e+02   1.024    0.306    
    ## gender.ymale     -2.092e+01  3.005e+01  -0.696    0.486    
    ## race.yblack       4.905e+01  4.380e+01   1.120    0.263    
    ## race.yHispanic   -1.957e+01  4.188e+01  -0.467    0.640    
    ## race.yother       1.520e+02  2.774e+02   0.548    0.584    
    ## race.ywhite      -6.255e+00  3.758e+01  -0.166    0.868    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1232 on 8394 degrees of freedom
    ##   (3519 observations deleted due to missingness)
    ## Multiple R-squared:  0.05225,    Adjusted R-squared:  0.05146 
    ## F-statistic: 66.11 on 7 and 8394 DF,  p-value: < 2.2e-16

``` r
modelsummary(model2)
```

|                  |  Model 1   |
|:-----------------|:----------:|
| (Intercept)      |  4612.700  |
|                  |  (36.005)  |
| count.y          |   0.063    |
|                  |  (0.003)   |
| art_gender_ratio |  121.058   |
|                  | (118.274)  |
| gender.ymale     |  -20.923   |
|                  |  (30.054)  |
| race.yblack      |   49.055   |
|                  |  (43.797)  |
| race.yHispanic   |  -19.574   |
|                  |  (41.880)  |
| race.yother      |  151.964   |
|                  | (277.352)  |
| race.ywhite      |   -6.255   |
|                  |  (37.579)  |
| Num.Obs.         |    8402    |
| R2               |   0.052    |
| R2 Adj.          |   0.051    |
| AIC              |  143431.2  |
| BIC              |  143494.5  |
| Log.Lik.         | -71706.583 |
| F                |   66.112   |
| RMSE             |  1231.52   |

## 5f. Regressing tenure_days with art unit size and art unit gender ratio

By performing this, we do not get any different result than discussed
above.

``` r
model5 <- lm(tenure_days~count.y+art_gender_ratio, data=art_unit_gender_ratio)
summary(model5)
```

    ## 
    ## Call:
    ## lm(formula = tenure_days ~ count.y + art_gender_ratio, data = art_unit_gender_ratio)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4916.7  -421.8   331.8   854.8  1707.8 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.632e+03  2.387e+01 194.024   <2e-16 ***
    ## count.y           6.184e-02  2.432e-03  25.424   <2e-16 ***
    ## art_gender_ratio -6.084e+00  7.740e+01  -0.079    0.937    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1228 on 11918 degrees of freedom
    ## Multiple R-squared:  0.05156,    Adjusted R-squared:  0.05141 
    ## F-statistic:   324 on 2 and 11918 DF,  p-value: < 2.2e-16

``` r
modelsummary(model5)
```

|                  |   Model 1   |
|:-----------------|:-----------:|
| (Intercept)      |  4632.079   |
|                  |  (23.874)   |
| count.y          |    0.062    |
|                  |   (0.002)   |
| art_gender_ratio |   -6.084    |
|                  |  (77.404)   |
| Num.Obs.         |    11921    |
| R2               |    0.052    |
| R2 Adj.          |    0.051    |
| AIC              |  203419.7   |
| BIC              |  203449.3   |
| Log.Lik.         | -101705.869 |
| F                |   323.978   |
| RMSE             |   1227.63   |

## Recommendations

Based on the trial models as above we recommend to proceed with the
below two models to estimate the tenure days: 1. Art unit Size - Its P
value was lower than 0.05. Thus it is significant 2. All variables

Issues faced: For univariate regressions, the output is easier to
analyze. But for multivariate regressions the output is difficult to
understand because the values of the coefficients and P-Value are
separate and not together. The P values also seem to be not analogues
with the correlation values.

    ## `geom_smooth()` using formula 'y ~ x'

![](Exercise_3_AA_files/figure-gfm/ggplots-1.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](Exercise_3_AA_files/figure-gfm/ggplots-2.png)<!-- -->
