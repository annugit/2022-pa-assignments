---
output:
  pdf_document: default
  html_document: default
---
Untitled
================

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

`{r cars} summary(cars)`

## Including Plots

You can also embed plots, for example:

`{r pressure, echo=FALSE} plot(pressure)`

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

    library(arrow)
    library(gender)
    library(wru)
    library(lubridate)
    library(tidyverse)

    data_path <- "C:/Users/anuak/OneDrive/Documents/Course Work/Summer 2022/People Analytics"
    setwd(data_path)

    app_data_sample <- read_parquet("app_data_sample.parquet")
    gender2 <- app_data_sample$examiner_name_first

    names=unique(gender2)

    gender4=gender(names)

    app_data_sample1 <- read_parquet("app_data_sample.parquet")

    colnames(app_data_sample1)
    names(app_data_sample1)[names(app_data_sample1) == "examiner_name_last"] <- "surname"

    new_table <- app_data_sample1 %>% mutate( race1 = predict_race(voter.file=app_data_sample1, surname.only=T))
    race1 = predict_race(voter.file=app_data_sample1, surname.only=T)

    race1 <- race1 %>% unite("full_name", surname:examiner_name_first, remove = FALSE)

    table_tenure <- race1 %>% group_by(full_name) %>% max(as.numeric(race1$filing_date))-min(as.numeric(race1$filing_date))

    race_temp <- race1 %>% mutate( race = max(c(race1$pred.whi, race1$pred.bla, race1$pred.his, race1$pred.asi, race1$pred.oth)))
    
    race_max <- race1 %>% mutate( race = max(c(race1$pred.whi, race1$pred.bla, race1$pred.his, race1$pred.asi, race1$pred.oth)))

    work_group <- race_max %>% mutate( wc = floor(examiner_art_unit/10)*10 )

