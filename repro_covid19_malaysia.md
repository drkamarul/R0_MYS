---
title: "Reproduction Number for Malaysia"
author: "Kamarul Imran Musa"
date: "6/16/2020 updated on 2020-07-16"
output: 
  html_document: 
    keep_md: yes
---



# Motivation

To model the time-dependent reproduction number of COVID-19 for Malaysia

# Methods

- Reproduce methods proposed in EpiEstim package
- Reference  https://cran.r-project.org/web/packages/EpiEstim/vignettes/demo.html

Data from

- WHO from github
- Crowd-sourcing data from data world

# Prepare environment


```r
library(tidyverse)
```

```
## -- Attaching packages -------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ----------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(EpiEstim)
library(here)
```

```
## here() starts at D:/R0_MYS
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```


# Data

## From github, save and read again

and save locally as covid
and read


```r
# covid <- read_csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv')
# write_csv(covid, 'covid.csv')
covid <- read_csv('covid.csv')
```

```
## Parsed with column specification:
## cols(
##   date = col_date(format = ""),
##   province = col_logical(),
##   country = col_character(),
##   lat = col_double(),
##   long = col_double(),
##   type = col_character(),
##   cases = col_double()
## )
```

## Malaysia data

Malaysia data from github

mys starts from first local case (Feb 4th)


```r
mys <- covid %>% 
  select(date, country, type , cases) %>% 
  filter(country == 'Malaysia', type == 'confirmed') %>% clean_names() %>%
  rename(Time = date, I = cases) %>% filter(Time > '2020-02-3') %>%
  data.frame()
```

Malaysia data from dataworld


```r
library("httr")
library("readxl")
GET("https://query.data.world/s/dl2knkmq7y2erjews5hnsc6vjbr3pc", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
```

```
## Response [https://download.data.world/file_download/erhanazrai/httpsdocsgooglecomspreadsheetsd15a43eb68lt7ggk9vavy/Covid19-KKM.xlsx?auth=eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmtpbTQ1OTciLCJpc3MiOiJhZ2VudDpraW00NTk3Ojo0Y2JiNWNhZi0yYmQ4LTQxNjAtOTVlYi00NzkzNmIzYmQwNTIiLCJpYXQiOjE1OTMyNjYzNTksInJvbGUiOlsidXNlciIsInVzZXJfYXBpX2FkbWluIiwidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6ZmFsc2UsInVybCI6IjNlNjU2OTA0ZDk5Nzk0YzY4NDc2NmE5Y2NkMjg0MmZlOTFmYjU0YjQifQ.yd0caZAlJKAFaMelFfRE-68UjkRNV1Z7uZSj5i7TeCldQZagrXUFjvJHbac7Cbr__2SHUQIe451R7d86BOIyNQ]
##   Date: 2020-07-16 15:34
##   Status: 200
##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
##   Size: 566 kB
## <ON DISK>  C:\Users\DRKAMA~1\AppData\Local\Temp\Rtmpofss3E\file512c62c25781.xlsx
```

```r
mys_dw <- read_excel(tf, sheet = 'd(Base)') %>% clean_names() %>%
  select(Time = date, mco_phase, cum_pos = positive, I = d_positive) %>%
  mutate_if(is.POSIXt, as.Date) %>% 
  filter(Time < '2020-06-07') %>%
  data.frame()
```

```
## New names:
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
```

```r
mys_dw_state <- read_excel(tf, sheet = 'State') %>% clean_names() %>%
  select(Time = date, perlis:total, east_malaysia, west_malaysia) %>%
  mutate_if(is.POSIXt, as.Date) %>% filter(Time > '2020-02-3') %>%
  data.frame()
```

```
## New names:
## * `` -> ...23
## * `` -> ...24
```

# Calculate R

## Based on Wuhan

Setting the mean_si = 7.5 and std_si = 3.4. What would be the mean_si and std_si?

The problem is R is estimated too early.

What would be the earliest time to start and time to end?

t_start = need to adjust
t_end = need to adjust

For Malaysia data John Hopkins 


```r
mys_parametric_si <- estimate_R(mys, 
                                method = "parametric_si",
                                config = make_config(list(mean_si = 7.5, 
                                                          std_si = 3.4)))
```

```
## Default config will estimate R on weekly sliding windows.
##     To change this change the t_start and t_end arguments.
```

```
## Warning in estimate_R_func(incid = incid, method = method, si_sample = si_sample, : You're estimating R too early in the epidemic to get the desired
##             posterior CV.
```

```r
glimpse(mys_parametric_si)
```

```
## List of 8
##  $ R         :'data.frame':	136 obs. of  11 variables:
##   ..$ t_start          : num [1:136] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ t_end            : num [1:136] 8 9 10 11 12 13 14 15 16 17 ...
##   ..$ Mean(R)          : num [1:136] 3.5 2.01 1.8 1.5 1.17 ...
##   ..$ Std(R)           : num [1:136] 1.168 0.761 0.636 0.531 0.442 ...
##   ..$ Quantile.0.025(R): num [1:136] 1.602 0.809 0.776 0.649 0.47 ...
##   ..$ Quantile.0.05(R) : num [1:136] 1.827 0.945 0.894 0.747 0.548 ...
##   ..$ Quantile.0.25(R) : num [1:136] 2.661 1.462 1.338 1.118 0.848 ...
##   ..$ Median(R)        : num [1:136] 3.37 1.92 1.72 1.44 1.11 ...
##   ..$ Quantile.0.75(R) : num [1:136] 4.2 2.46 2.18 1.82 1.43 ...
##   ..$ Quantile.0.95(R) : num [1:136] 5.62 3.41 2.95 2.47 1.98 ...
##   ..$ Quantile.0.975(R): num [1:136] 6.14 3.76 3.24 2.71 2.18 ...
##  $ method    : chr "parametric_si"
##  $ si_distr  : Named num [1:144] 0 0.00127 0.02002 0.06267 0.10375 ...
##   ..- attr(*, "names")= chr [1:144] "t0" "t1" "t2" "t3" ...
##  $ SI.Moments:'data.frame':	1 obs. of  2 variables:
##   ..$ Mean: num 7.5
##   ..$ Std : num 3.42
##  $ dates     : int [1:143] 1 2 3 4 5 6 7 8 9 10 ...
##  $ I         : num [1:143] 2 2 0 0 4 0 2 0 0 1 ...
##  $ I_local   : num [1:143] 0 2 0 0 4 0 2 0 0 1 ...
##  $ I_imported: num [1:143] 2 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "class")= chr "estimate_R"
```
### Plot

- plots the incidence
- plots the serial interval distribution
- plot of R


```r
p_I <- plot(mys_parametric_si, "incid") 
p_I + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
p_SI <- plot(mys_parametric_si, "SI")  
p_SI + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
- the 7-day sliding window estimates of instantaneous $R_e$


```r
p_Ri <- plot(mys_parametric_si, "R")
p_Ri + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The $R_t$ is very high nearly day 50. Reasons:

- One possible explanation is that COVID-19 is transmissible before the onset of symptoms, resulting in much shorter serial intervals than expected, possibly shorter than the incubation period
- Alternatively, and very likely, there may be non-symptomatic, sub-clinical spreaders of the disease, who are undetected.
- some cases transmitting the disease very soon after infection, possibly before the onset of symptoms (so-called super-spreaders), and some cases being sub-clinical, and thus undetected, spreading the disease as well, while other cases have a serial interval more consistent with that of MERS or SARS, with a mean around 8 days.

### Calculate R with uncertainty

incorporate this uncertainty around the serial interval distribution by allowing specification of a distribution of distributions of serial intervals. So let’s 

- retain the mean SI estimated by Li et al of 7.5 days, with an SD of 3.4
- but let’s also allow that mean SI to vary between 2.3 and 8.4 
- Use a truncated normal distribution with an SD of 2.0.
- We’ll also allow the SD or the SD to vary between 0.5 and 4.0.


```r
# mys_res_uncertain_si <- estimate_R(mys, method = "uncertain_si", 
#                                   config = make_config(list(mean_si = 7.5, std_si = 3.4, 
#                                                             std_mean_si = 2, 
#                                                             min_mean_si = 2.3, max_mean_si = 8.4, 
#                                                             std_std_si = 2, 
#                                                             min_std_si = 0.5, max_std_si = 4, 
#                                                             n1 = 1000, n2 = 1000)))

# plot_Ri(mys_res_uncertain_si)
```


## Based on Du, Z., et al.

The serial interval of COVID-19 from publicly reported confirmed cases. medRxiv, 2020

mean = 3.96, SD = 4.75


```r
mys_parametric_si_du <- estimate_R(mys, 
                                method = "parametric_si",
                                config = make_config(list(mean_si = 3.96, 
                                                          std_si = 4.75)))
```

```
## Default config will estimate R on weekly sliding windows.
##     To change this change the t_start and t_end arguments.
```

```
## Warning in estimate_R_func(incid = incid, method = method, si_sample = si_sample, : You're estimating R too early in the epidemic to get the desired
##             posterior CV.
```

```r
glimpse(mys_parametric_si_du)
```

```
## List of 8
##  $ R         :'data.frame':	136 obs. of  11 variables:
##   ..$ t_start          : num [1:136] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ t_end            : num [1:136] 8 9 10 11 12 13 14 15 16 17 ...
##   ..$ Mean(R)          : num [1:136] 1.292 0.995 1.249 1.227 1.051 ...
##   ..$ Std(R)           : num [1:136] 0.431 0.376 0.442 0.434 0.397 ...
##   ..$ Quantile.0.025(R): num [1:136] 0.591 0.4 0.539 0.53 0.423 ...
##   ..$ Quantile.0.05(R) : num [1:136] 0.674 0.467 0.621 0.611 0.493 ...
##   ..$ Quantile.0.25(R) : num [1:136] 0.982 0.722 0.93 0.914 0.763 ...
##   ..$ Median(R)        : num [1:136] 1.245 0.948 1.197 1.177 1.002 ...
##   ..$ Quantile.0.75(R) : num [1:136] 1.55 1.22 1.51 1.49 1.29 ...
##   ..$ Quantile.0.95(R) : num [1:136] 2.07 1.68 2.05 2.02 1.78 ...
##   ..$ Quantile.0.975(R): num [1:136] 2.26 1.86 2.25 2.21 1.96 ...
##  $ method    : chr "parametric_si"
##  $ si_distr  : Named num [1:144] 0 0.361 0.204 0.1032 0.0695 ...
##   ..- attr(*, "names")= chr [1:144] "t0" "t1" "t2" "t3" ...
##  $ SI.Moments:'data.frame':	1 obs. of  2 variables:
##   ..$ Mean: num 3.96
##   ..$ Std : num 4.76
##  $ dates     : int [1:143] 1 2 3 4 5 6 7 8 9 10 ...
##  $ I         : num [1:143] 2 2 0 0 4 0 2 0 0 1 ...
##  $ I_local   : num [1:143] 0 2 0 0 4 0 2 0 0 1 ...
##  $ I_imported: num [1:143] 2 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "class")= chr "estimate_R"
```
### Plot

- plots the incidence
- plots the serial interval distribution
- plot of R


```r
p_I_du <- plot(mys_parametric_si_du, "incid") 
p_I_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
p_SI_du <- plot(mys_parametric_si_du, "SI")  
p_SI_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

- the 7-day sliding window estimates of instantaneous $R_e$


```r
p_Ri_du <- plot(mys_parametric_si_du, "R")
p_Ri_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



# Analysis from data world

## Wuhan SI

mean SI = 7.5, SD SI = 3.4 


```r
mys_dw <- mys_dw %>% select(Time, I)
mys_dw_parametric_si <- estimate_R(mys_dw, 
                                method = "parametric_si",
                                config = make_config(list(mean_si = 7.5, 
                                                          std_si = 3.4)))
```

```
## Default config will estimate R on weekly sliding windows.
##     To change this change the t_start and t_end arguments.
```

```
## Warning in estimate_R_func(incid = incid, method = method, si_sample = si_sample, : You're estimating R too early in the epidemic to get the desired
##             posterior CV.
```

```r
glimpse(mys_dw_parametric_si)
```

```
## List of 8
##  $ R         :'data.frame':	127 obs. of  11 variables:
##   ..$ t_start          : num [1:127] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ t_end            : num [1:127] 8 9 10 11 12 13 14 15 16 17 ...
##   ..$ Mean(R)          : num [1:127] 2.28 1.46 1.18 1.45 1.16 ...
##   ..$ Std(R)           : num [1:127] 0.933 0.651 0.53 0.549 0.474 ...
##   ..$ Quantile.0.025(R): num [1:127] 0.838 0.473 0.385 0.584 0.426 ...
##   ..$ Quantile.0.05(R) : num [1:127] 0.995 0.574 0.467 0.681 0.506 ...
##   ..$ Quantile.0.25(R) : num [1:127] 1.606 0.981 0.798 1.054 0.817 ...
##   ..$ Median(R)        : num [1:127] 2.16 1.36 1.11 1.38 1.1 ...
##   ..$ Quantile.0.75(R) : num [1:127] 2.83 1.83 1.49 1.77 1.44 ...
##   ..$ Quantile.0.95(R) : num [1:127] 4 2.67 2.17 2.46 2.03 ...
##   ..$ Quantile.0.975(R): num [1:127] 4.44 2.98 2.43 2.71 2.26 ...
##  $ method    : chr "parametric_si"
##  $ si_distr  : Named num [1:135] 0 0.00127 0.02002 0.06267 0.10375 ...
##   ..- attr(*, "names")= chr [1:135] "t0" "t1" "t2" "t3" ...
##  $ SI.Moments:'data.frame':	1 obs. of  2 variables:
##   ..$ Mean: num 7.5
##   ..$ Std : num 3.42
##  $ dates     : int [1:134] 1 2 3 4 5 6 7 8 9 10 ...
##  $ I         : num [1:134] 3 1 0 0 3 1 0 0 0 0 ...
##  $ I_local   : num [1:134] 0 1 0 0 3 1 0 0 0 0 ...
##  $ I_imported: num [1:134] 3 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "class")= chr "estimate_R"
```

### Plot

- plots the incidence
- plots the serial interval distribution
- plot of R


```r
p_I_dw <- plot(mys_dw_parametric_si, "incid") 
p_I_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



```r
p_SI_dw <- plot(mys_dw_parametric_si, "SI")  
p_SI_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

- the 7-day sliding window estimates of instantaneous $R_e$


```r
p_Ri_dw <- plot(mys_dw_parametric_si, "R")
p_Ri_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


## Based on Du et al

### Du et al SI

mean SI = 7.5, SD SI = 3.4 


```r
mys_dw <- mys_dw %>% select(Time, I)
mys_dw_parametric_si_du <- estimate_R(mys_dw, 
                                method = "parametric_si",
                                config = make_config(list(mean_si = 3.96, 
                                                          std_si = 4.75)))
```

```
## Default config will estimate R on weekly sliding windows.
##     To change this change the t_start and t_end arguments.
```

```
## Warning in estimate_R_func(incid = incid, method = method, si_sample = si_sample, : You're estimating R too early in the epidemic to get the desired
##             posterior CV.
```

```r
glimpse(mys_dw_parametric_si_du)
```

```
## List of 8
##  $ R         :'data.frame':	127 obs. of  11 variables:
##   ..$ t_start          : num [1:127] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ t_end            : num [1:127] 8 9 10 11 12 13 14 15 16 17 ...
##   ..$ Mean(R)          : num [1:127] 0.973 0.909 1.034 1.537 1.165 ...
##   ..$ Std(R)           : num [1:127] 0.397 0.406 0.463 0.581 0.476 ...
##   ..$ Quantile.0.025(R): num [1:127] 0.357 0.295 0.336 0.618 0.427 ...
##   ..$ Quantile.0.05(R) : num [1:127] 0.424 0.358 0.408 0.721 0.507 ...
##   ..$ Quantile.0.25(R) : num [1:127] 0.684 0.612 0.697 1.116 0.819 ...
##   ..$ Median(R)        : num [1:127] 0.919 0.849 0.966 1.464 1.101 ...
##   ..$ Quantile.0.75(R) : num [1:127] 1.2 1.14 1.3 1.88 1.44 ...
##   ..$ Quantile.0.95(R) : num [1:127] 1.7 1.66 1.89 2.6 2.04 ...
##   ..$ Quantile.0.975(R): num [1:127] 1.89 1.86 2.12 2.87 2.27 ...
##  $ method    : chr "parametric_si"
##  $ si_distr  : Named num [1:135] 0 0.361 0.204 0.1032 0.0695 ...
##   ..- attr(*, "names")= chr [1:135] "t0" "t1" "t2" "t3" ...
##  $ SI.Moments:'data.frame':	1 obs. of  2 variables:
##   ..$ Mean: num 3.96
##   ..$ Std : num 4.76
##  $ dates     : int [1:134] 1 2 3 4 5 6 7 8 9 10 ...
##  $ I         : num [1:134] 3 1 0 0 3 1 0 0 0 0 ...
##  $ I_local   : num [1:134] 0 1 0 0 3 1 0 0 0 0 ...
##  $ I_imported: num [1:134] 3 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "class")= chr "estimate_R"
```

### Plot

- plots the incidence
- plots the serial interval distribution
- plot of R


```r
p_I_dw_du <- plot(mys_dw_parametric_si_du, "incid") 
p_I_dw_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-19-1.png)<!-- -->



```r
p_SI_dw_du <- plot(mys_dw_parametric_si_du, "SI")  
p_SI_dw_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

- the 7-day sliding window estimates of instantaneous $R_e$


```r
p_Ri_dw_du <- plot(mys_dw_parametric_si_du, "R")
p_Ri_dw_du + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
# Values of R at MCO 1, 2 and 3

- First case local Malaysia on Feb 4 (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7293423/)
- MCO 1 on 18 March - 31 March
- MCO 2 on 1 April - 14 April
- MCO 3 on 15 April - 28 April
- MCO 4 on 29 April - 12 May

Calculate difference in days


```r
first_mco1 <- interval(as_date('2020-02-04'), as_date('2020-03-17'))
first_mco1 <- first_mco1 / ddays(1)

mco1_mco2 <- interval(as_date('2020-03-18'), as_date('2020-03-31'))
mco1_mco2 <- mco1_mco2 / ddays(1)

mco2_mco3 <- interval(as_date('2020-04-01'), as_date('2020-04-14'))
mco2_mco3 <- mco2_mco3 / ddays(1)

mco3_mco4 <- interval(as_date('2020-04-15'), as_date('2020-04-28'))
mco3_mco4 <- mco3_mco4 / ddays(1)

mco4_mco5 <- interval(as_date('2020-04-29'), as_date('2020-05-12'))
mco4_mco5 <- mco4_mco5 / ddays(1)

first_mco1 ; mco1_mco2 ; mco2_mco3 ; mco3_mco4 ; mco4_mco5
```

```
## [1] 42
```

```
## [1] 13
```

```
## [1] 13
```

```
## [1] 13
```

```
## [1] 13
```
In the table \@ref(tab:si), we see the estimated R value from the first day until . 


```r
mys_dw_parametric_si$R
```

```
##     t_start t_end    Mean(R)     Std(R) Quantile.0.025(R) Quantile.0.05(R)
## 1         2     8  2.2845115 0.93264793       0.838375467      0.994910384
## 2         3     9  1.4558525 0.65107703       0.472711341      0.573649432
## 3         4    10  1.1845666 0.52975430       0.384625561      0.466754687
## 4         5    11  1.4517604 0.54871384       0.583682964      0.681355875
## 5         6    12  1.1611411 0.47403388       0.426118330      0.505679815
## 6         7    13  1.3156446 0.49726691       0.528957358      0.617472542
## 7         8    14  1.3000052 0.49135579       0.522669522      0.610132506
## 8         9    15  1.6602492 0.55341639       0.759171643      0.866138628
## 9        10    16  1.8262649 0.57751567       0.875765005      0.990822798
## 10       11    17  1.9655042 0.59262182       0.981172633      1.102291814
## 11       12    18  1.5407773 0.51359243       0.704541484      0.803811101
## 12       13    19  1.1232796 0.42455978       0.451616652      0.527189721
## 13       14    20  0.8946137 0.36522451       0.328307458      0.389606461
## 14       15    21  0.8428603 0.34409627       0.309314873      0.367067729
## 15       16    22  0.9539108 0.36054439       0.383521608      0.447699722
## 16       17    23  0.8181679 0.33401567       0.300253217      0.356314151
## 17       18    24  0.6972820 0.31183398       0.226405562      0.274749959
## 18       19    25  0.7212678 0.32256075       0.234193680      0.284201075
## 19       20    26  0.7527398 0.33663546       0.244412559      0.296601992
## 20       21    27  0.6342692 0.31713462       0.172817020      0.216653432
## 21       22    28  0.6725682 0.33628409       0.183252194      0.229735571
## 22       23    29  0.1792441 0.17924410       0.004538068      0.009194020
## 23       24    30  0.1923717 0.19237170       0.004870430      0.009867378
## 24       25    31  0.2097461 0.20974605       0.005310310      0.010758566
## 25       26    32  0.2360415 0.23604150       0.005976053      0.012107346
## 26       27    33  0.2758199 0.27581989       0.006983155      0.014147711
## 27       28    34  0.3339152 0.33391523       0.008454002      0.017127612
## 28       29    35  1.6657904 0.83289520       0.453871820      0.569000019
## 29       30    36  2.1213895 1.06069477       0.578007252      0.724623390
## 30       31    37  5.2784153 1.86620162       2.278845066      2.626554476
## 31       32    38  5.8478559 2.06752930       2.524689127      2.909909772
## 32       33    39 10.0082295 2.58411374       5.601530068      6.169293162
## 33       34    40 14.8997731 2.76681840       9.978611866     10.658977687
## 34       35    41 12.2584600 2.10230854       8.489339894      9.017220996
## 35       36    42 13.8028692 1.79698051      10.507383550     10.985947752
## 36       37    43 10.1736966 1.22476914       7.915743199      8.246368380
## 37       38    44  6.6668824 0.79121338       5.206890496      5.420979131
## 38       39    45  5.4837040 0.58127146       4.403864597      4.563885459
## 39       40    46  4.0307167 0.41573668       3.257229365      3.372121637
## 40       41    47  3.1743701 0.31743701       2.582794228      2.670892077
## 41       42    48  2.5699922 0.25200847       2.099869841      2.169991765
## 42       43    49  2.3095275 0.21536455       1.906751852      1.967063615
## 43       44    50  2.4724444 0.20462095       2.087667065      2.145771708
## 44       45    51  4.8506662 0.26702038       4.341374028      4.419970655
## 45       46    52  5.6473131 0.27014762       5.130177724      5.210427054
## 46       47    53  5.9934314 0.25673039       5.500744625      5.577491295
## 47       48    54  5.5934226 0.22075478       5.169061407      5.235333497
## 48       49    55  4.8718986 0.17873262       4.527840378      4.581684293
## 49       50    56  4.0776989 0.14119917       3.805613290      3.848259651
## 50       51    57  3.5148599 0.11427795       3.294418869      3.329025285
## 51       52    58  2.5569300 0.08624311       2.390668443      2.416745621
## 52       53    59  2.2689372 0.07300186       2.128093705      2.150209927
## 53       54    60  1.8654379 0.06045919       1.748806794      1.767117790
## 54       55    61  1.7020363 0.05363572       1.598522138      1.614784687
## 55       56    62  1.6989391 0.05049572       1.601398372      1.616742957
## 56       57    63  1.5447957 0.04591429       1.456104747      1.470057130
## 57       58    64  1.4346770 0.04252879       1.352522493      1.365447353
## 58       59    65  1.3698432 0.04013358       1.292302344      1.304504509
## 59       60    66  1.2261381 0.03681914       1.155026718      1.166211020
## 60       61    67  1.1981405 0.03543926       1.129678937      1.140450096
## 61       62    68  1.1161783 0.03345692       1.051558894      1.061722561
## 62       63    69  1.0519012 0.03191979       0.990262010      0.999954264
## 63       64    70  1.1068751 0.03231838       1.044430722      1.054257985
## 64       65    71  1.0786166 0.03161481       1.017534990      1.027146970
## 65       66    72  1.0918106 0.03161019       1.030727087      1.040341934
## 66       67    73  1.0581446 0.03096162       0.998323547      1.007737519
## 67       68    74  1.0748069 0.03105289       1.014798490      1.024244533
## 68       69    75  1.0768182 0.03093079       1.017040787      1.026451640
## 69       70    76  0.9795831 0.02936255       0.922871691      0.931791553
## 70       71    77  0.8842740 0.02776949       0.830677411      0.839098364
## 71       72    78  0.9071448 0.02802180       0.853047463      0.861550353
## 72       73    79  0.8829917 0.02762048       0.829679506      0.838056555
## 73       74    80  0.8905457 0.02781598       0.836854956      0.845291778
## 74       75    81  0.9008559 0.02813801       0.846543547      0.855078045
## 75       76    82  0.8511992 0.02755859       0.798035317      0.806382213
## 76       77    83  0.8661296 0.02802730       0.812061141      0.820550178
## 77       78    84  0.8356727 0.02776337       0.782136336      0.790536395
## 78       79    85  0.7287364 0.02616011       0.678358672      0.686247281
## 79       80    86  0.6770363 0.02546257       0.628043669      0.635705523
## 80       81    87  0.5970449 0.02419348       0.550561806      0.557815242
## 81       82    88  0.5016048 0.02252272       0.458426603      0.465142008
## 82       83    89  0.4858841 0.02262988       0.442536899      0.449270009
## 83       84    90  0.4694192 0.02285097       0.425694872      0.432475679
## 84       85    91  0.5247567 0.02498841       0.476916895      0.484341930
## 85       86    92  0.5640341 0.02695059       0.512441946      0.520448370
## 86       87    93  0.5502313 0.02779088       0.497103611      0.505331185
## 87       88    94  0.6064447 0.03047499       0.548178285      0.557203430
## 88       89    95  0.6160388 0.03202633       0.554859926      0.564323607
## 89       90    96  0.7453285 0.03663090       0.675253311      0.686116732
## 90       91    97  0.7735481 0.03867740       0.699589714      0.711047658
## 91       92    98  0.7848514 0.04020916       0.708011820      0.719904923
## 92       93    99  0.9440690 0.04526464       0.857425057      0.870869381
## 93       94   100  1.1740519 0.05153516       1.075204250      1.090589554
## 94       95   101  1.2456287 0.05390362       1.142205926      1.158310905
## 95       96   102  1.2620638 0.05466605       1.157180321      1.173512259
## 96       97   103  1.1403058 0.05183208       1.040966341      1.056409877
## 97       98   104  1.0741494 0.04975901       0.978824767      0.993634348
## 98       99   105  1.0409643 0.04827356       0.948487655      0.962854224
## 99      100   106  0.9026406 0.04436236       0.817775084      0.830931384
## 100     101   107  0.7685044 0.04056011       0.691054573      0.703027909
## 101     102   108  0.7942778 0.04107113       0.715809861      0.727950551
## 102     103   109  0.7684856 0.04050275       0.691142465      0.703099984
## 103     104   110  0.7653040 0.04079084       0.687434156      0.699467600
## 104     105   111  0.7891342 0.04200138       0.708950398      0.721342147
## 105     106   112  0.7419377 0.04141093       0.662984686      0.675161945
## 106     107   113  0.6800793 0.04035528       0.603276304      0.615089738
## 107     108   114  0.5932011 0.03837099       0.520373024      0.531528630
## 108     109   115  0.5568473 0.03788866       0.485058002      0.496025701
## 109     110   116  0.6383512 0.04146537       0.559660983      0.571711984
## 110     111   117  0.6559658 0.04315938       0.574095834      0.586625598
## 111     112   118  0.7270876 0.04683583       0.638181113      0.651802429
## 112     113   119  0.9092060 0.05404669       0.806351459      0.822170732
## 113     114   120  1.0705739 0.06041598       0.955422401      0.973174232
## 114     115   121  1.2600136 0.06715895       1.131806960      1.151619091
## 115     116   122  1.7587704 0.08052857       1.604458135      1.628441815
## 116     117   123  2.3223541 0.09274589       2.144108738      2.171935298
## 117     118   124  2.2009842 0.08904229       2.029901061      2.056599013
## 118     119   125  1.9182470 0.08027614       1.764114351      1.788141811
## 119     120   126  1.8012083 0.07378037       1.659484787      1.681592634
## 120     121   127  1.5541260 0.06464312       1.429993415      1.449347939
## 121     122   128  1.3887532 0.05791501       1.277546448      1.294884175
## 122     123   129  0.9740420 0.04638295       0.885242807      0.899024996
## 123     124   130  0.5646924 0.03411432       0.499802490      0.509775231
## 124     125   131  0.6928171 0.03692728       0.622322853      0.633216532
## 125     126   132  1.2001749 0.04823910       1.107477597      1.121946108
## 126     127   133  1.0546546 0.04559669       0.967168217      0.980791999
## 127     128   134  1.0997053 0.04723639       1.009060180      1.023178864
##     Quantile.0.25(R)  Median(R) Quantile.0.75(R) Quantile.0.95(R)
## 1         1.60647208  2.1589248        2.8262080        4.0028582
## 2         0.98083705  1.3600309        1.8269291        2.6652347
## 3         0.79806632  1.1066006        1.4864962        2.1685906
## 4         1.05411427  1.3832450        1.7749776        2.4560458
## 5         0.81651626  1.0973096        1.4364674        2.0345195
## 6         0.95528143  1.2535531        1.6085572        2.2257691
## 7         0.94392578  1.2386519        1.5894359        2.1993109
## 8         1.26135497  1.5991799        1.9927500        2.6627906
## 9         1.41095158  1.7657634        2.1757839        2.8681885
## 10        1.54020659  1.9062751        2.3263766        3.0308467
## 11        1.17058759  1.4841026        1.8493513        2.4711756
## 12        0.81560639  1.0702667        1.3733644        1.9003316
## 13        0.62909375  0.8454340        1.1067418        1.5675175
## 14        0.59270068  0.7965256        1.0427168        1.4768366
## 15        0.69262874  0.9088912        1.1662877        1.6137984
## 16        0.57533698  0.7731907        1.0121695        1.4335714
## 17        0.46977287  0.6513881        0.8750095        1.2765168
## 18        0.48593257  0.6737952        0.9051089        1.3204276
## 19        0.50713590  0.7031958        0.9446027        1.3780436
## 20        0.40201890  0.5822688        0.8101882        1.2294765
## 21        0.42629393  0.6174278        0.8591096        1.3037157
## 22        0.05156531  0.1242425        0.2484851        0.5369673
## 23        0.05534189  0.1333419        0.2666838        0.5762941
## 24        0.06034018  0.1453849        0.2907698        0.6283430
## 25        0.06790491  0.1636115        0.3272230        0.7071171
## 26        0.07934844  0.1911838        0.3823676        0.8262826
## 27        0.09606143  0.2314524        0.4629048        1.0003206
## 28        1.05582802  1.5292209        2.1278088        3.2289917
## 29        1.34460045  1.9474678        2.7097715        4.1121315
## 30        3.92985265  5.0601854        6.3898055        8.6751506
## 31        4.35380905  5.6060832        7.0791440        9.6110344
## 32        8.16591716  9.7867245       11.6094603       14.6029982
## 33       12.95222088 14.7288657       16.6611676       19.7236525
## 34       10.78272150 12.1384907       13.6035083       15.9089869
## 35       12.55211666 13.7249657       14.9687311       16.8855496
## 36        9.32302379 10.1245907       10.9708561       12.2685417
## 37        6.11754407  6.6356088        7.1821399        8.0194707
## 38        5.08125947  5.4631795        5.8637802        6.4735374
## 39        3.74306527  4.0164324        4.3028003        4.7380393
## 40        2.95488890  3.1637952        3.3823260        3.7139221
## 41        2.39582614  2.5617597        2.7351858        2.9980756
## 42        2.16084993  2.3028367        2.4509128        2.6748155
## 43        2.33152653  2.4668018        2.6072122        2.8183650
## 44        4.66799292  4.8457674        5.0279995        5.2980721
## 45        5.46282964  5.6430061        5.8271016        6.0988911
## 46        5.81832941  5.9897661        6.1645379        6.4218743
## 47        5.44298515  5.5905187        5.7406945        5.9614172
## 48        4.75018356  4.8697131        4.9912312        5.1695679
## 49        3.98159405  4.0760693        4.1720273        4.3126971
## 50        3.43712040  3.5136215        3.5912494        3.7049189
## 51        2.49824348  2.5559604        2.6145596        2.7004217
## 52        2.21928077  2.2681543        2.3177402        2.3903350
## 53        1.82431061  1.8647848        1.9058532        1.9659859
## 54        1.66555901  1.7014729        1.7378994        1.7912096
## 55        1.66461304  1.6984388        1.7327198        1.7828416
## 56        1.51358399  1.5443408        1.5755115        1.6210858
## 57        1.40576727  1.4342567        1.4631286        1.5053399
## 58        1.34256408  1.3694512        1.3966950        1.4365187
## 59        1.20110714  1.2257695        1.2507673        1.2873222
## 60        1.17405044  1.1977911        1.2218497        1.2570227
## 61        1.09343343  1.1158440        1.1385588        1.1717742
## 62        1.03019931  1.0515784        1.0732512        1.1049495
## 63        1.08490867  1.1065606        1.1284988        1.1605652
## 64        1.05712771  1.0783077        1.0997688        1.1311398
## 65        1.07032683  1.0915056        1.1129618        1.1443198
## 66        1.03709999  1.0578427        1.0788601        1.1095818
## 67        1.05370221  1.0745078        1.0955856        1.1263893
## 68        1.05579739  1.0765220        1.0975161        1.1281949
## 69        0.95962173  0.9792898        0.9992247        1.0283754
## 70        0.86538867  0.8839833        0.9028424        0.9304411
## 71        0.88809039  0.9068563        0.9258847        0.9537234
## 72        0.86420836  0.8827038        0.9014612        0.9289092
## 73        0.87162964  0.8902562        0.9091462        0.9367875
## 74        0.88172083  0.9005630        0.9196717        0.9476331
## 75        0.83245263  0.8509018        0.8696215        0.8970306
## 76        0.84706435  0.8658273        0.8848654        0.9127403
## 77        0.81678279  0.8353653        0.8542275        0.8818577
## 78        0.71092517  0.7284234        0.7462064        0.7722932
## 79        0.65969249  0.6767171        0.6940321        0.7194558
## 80        0.58055340  0.5967181        0.6131802        0.6373891
## 81        0.48623532  0.5012677        0.5166069        0.5392173
## 82        0.47043502  0.4855328        0.5009502        0.5236964
## 83        0.45381107  0.4690485        0.4846233        0.5076274
## 84        0.50769306  0.5243601        0.5413880        0.5665243
## 85        0.54562979  0.5636049        0.5819705        0.6090838
## 86        0.53124038  0.5497635        0.5687123        0.5967272
## 87        0.58562090  0.6059343        0.6267122        0.6574270
## 88        0.59414558  0.6154839        0.6373272        0.6696469
## 89        0.72030513  0.7447284        0.7696978        0.8065869
## 90        0.74712113  0.7729035        0.7992724        0.8382471
## 91        0.75736952  0.7841649        0.8115850        0.8521399
## 92        0.91315699  0.9433457        0.9741925        1.0197360
## 93        1.13889324  1.1732980        1.2083888        1.2600862
## 94        1.20885986  1.2448512        1.2815500        1.3355984
## 95        1.22477451  1.2612746        1.2984927        1.3533073
## 96        1.10493072  1.1395205        1.1748249        1.2268802
## 97        1.04018182  1.0733811        1.1072795        1.1572850
## 98        1.00801030  1.0402181        1.0731049        1.1216194
## 99        0.87233572  0.9019139        0.9321534        0.9768285
## 100       0.74077200  0.7677909        0.7954590        0.8364145
## 101       0.76620344  0.7935700        0.8215806        0.8630194
## 102       0.74079294  0.7677741        0.7954027        0.8362980
## 103       0.73741026  0.7645794        0.7924078        0.8336121
## 104       0.76041322  0.7883892        0.8170430        0.8594677
## 105       0.71360249  0.7411674        0.7694333        0.8113412
## 106       0.65244251  0.6792812        0.7068461        0.7477910
## 107       0.56688911  0.5923739        0.6186114        0.6576950
## 108       0.53084513  0.5559882        0.5819131        0.6205995
## 109       0.60991549  0.6374536        0.6658085        0.7080523
## 110       0.62636240  0.6550195        0.6845377        0.7285340
## 111       0.69497318  0.7260822        0.7581061        0.8058024
## 112       0.87219196  0.9081353        0.9450530        0.9998936
## 113       1.02922828  1.0694377        1.1106810        1.1718496
## 114       1.21408873  1.2588206        1.3046380        1.3724775
## 115       1.70380566  1.7575415        1.8123956        1.8932910
## 116       2.25914327  2.3211196        2.3842193        2.4769841
## 117       2.14028956  2.1997836        2.2603700        2.3494649
## 118       1.86350854  1.9171273        1.9717649        2.0521716
## 119       1.75091033  1.8002010        1.8504082        1.9242599
## 120       1.51005013  1.5532298        1.5972249        1.6619610
## 121       1.34926369  1.3879482        1.4273651        1.4853681
## 122       0.94236885  0.9733059        1.0049127        1.0515701
## 123       0.54132362  0.5640056        0.5873126        0.6219525
## 124       0.66756540  0.6921612        0.7173538        0.7546553
## 125       1.16729544  1.1995287        1.2323499        1.2806081
## 126       1.02355240  1.0539975        1.0850405        1.1307583
## 127       1.06748697  1.0990291        1.1311865        1.1785386
##     Quantile.0.975(R)
## 1           4.4427399
## 2           2.9820485
## 3           2.4263688
## 4           2.7084610
## 5           2.2580967
## 6           2.4545180
## 7           2.4253406
## 8           2.9078691
## 9           3.1201377
## 10          3.2860293
## 11          2.6986182
## 12          2.0956344
## 13          1.7397749
## 14          1.6391290
## 15          1.7796533
## 16          1.5911092
## 17          1.4282551
## 18          1.4773856
## 19          1.5418502
## 20          1.3902029
## 21          1.4741472
## 22          0.6612099
## 23          0.7096360
## 24          0.7737279
## 25          0.8707286
## 26          1.0174663
## 27          1.2317730
## 28          3.6511098
## 29          4.6497004
## 30          9.5161087
## 31         10.5427160
## 32         15.6726346
## 33         20.7917578
## 34         16.7090991
## 35         17.5409785
## 36         12.7106631
## 37          8.3045682
## 38          6.6801657
## 39          4.8853694
## 40          3.8260349
## 41          3.0868931
## 42          2.7503224
## 43          2.8892851
## 44          5.3877966
## 45          6.1889244
## 46          6.5069473
## 47          6.0342860
## 48          5.2283767
## 49          4.3590456
## 50          3.7423387
## 51          2.7287015
## 52          2.4142297
## 53          1.9857806
## 54          1.8087520
## 55          1.7993226
## 56          1.6360715
## 57          1.5192194
## 58          1.4496112
## 59          1.2993437
## 60          1.2685876
## 61          1.1826973
## 62          1.1153752
## 63          1.1711070
## 64          1.1414534
## 65          1.1546276
## 66          1.1196817
## 67          1.1365147
## 68          1.1382785
## 69          1.0379617
## 70          0.9395224
## 71          0.9628817
## 72          0.9379405
## 73          0.9458822
## 74          0.9568331
## 75          0.9060531
## 76          0.9219160
## 77          0.8909562
## 78          0.7808928
## 79          0.7278427
## 80          0.6453848
## 81          0.5466984
## 82          0.5312275
## 83          0.5152504
## 84          0.5748502
## 85          0.6180652
## 86          0.6060175
## 87          0.6676116
## 88          0.6803711
## 89          0.8188134
## 90          0.8511691
## 91          0.8655925
## 92          1.0348234
## 93          1.2771842
## 94          1.3534695
## 95          1.3714320
## 96          1.2441075
## 97          1.1738398
## 98          1.1376808
## 99          0.9916355
## 100         0.8500084
## 101         0.8767680
## 102         0.8498716
## 103         0.8472915
## 104         0.8735519
## 105         0.8252682
## 106         0.7614173
## 107         0.6707294
## 108         0.6335186
## 109         0.7221421
## 110         0.7432133
## 111         0.8217074
## 112         1.0181449
## 113         1.1921826
## 114         1.3949996
## 115         1.9200663
## 116         2.5076151
## 117         2.3788903
## 118         2.0787427
## 119         1.9486559
## 120         1.6833513
## 121         1.5045345
## 122         1.0670245
## 123         0.6334854
## 124         0.7670391
## 125         1.2965446
## 126         1.1458747
## 127         1.1941935
```


In the table \@ref(tab:mco), we show for each MCO phase, the difference in the mean R value and the percentage of difference in means between

1. day 10 and day 5
2. day 5 and day 1 . 


```r
MCO <- mys_dw_parametric_si$R %>% 
  filter(t_start %in% c(2, 7, 12, 55, 60, 65, 68, 73, 78, 80, 85, 88))
MCO
```

```
##    t_start t_end   Mean(R)     Std(R) Quantile.0.025(R) Quantile.0.05(R)
## 1        2     8 2.2845115 0.93264793         0.8383755        0.9949104
## 2        7    13 1.3156446 0.49726691         0.5289574        0.6174725
## 3       12    18 1.5407773 0.51359243         0.7045415        0.8038111
## 4       55    61 1.7020363 0.05363572         1.5985221        1.6147847
## 5       60    66 1.2261381 0.03681914         1.1550267        1.1662110
## 6       65    71 1.0786166 0.03161481         1.0175350        1.0271470
## 7       68    74 1.0748069 0.03105289         1.0147985        1.0242445
## 8       73    79 0.8829917 0.02762048         0.8296795        0.8380566
## 9       78    84 0.8356727 0.02776337         0.7821363        0.7905364
## 10      80    86 0.6770363 0.02546257         0.6280437        0.6357055
## 11      85    91 0.5247567 0.02498841         0.4769169        0.4843419
## 12      88    94 0.6064447 0.03047499         0.5481783        0.5572034
##    Quantile.0.25(R) Median(R) Quantile.0.75(R) Quantile.0.95(R)
## 1         1.6064721 2.1589248        2.8262080        4.0028582
## 2         0.9552814 1.2535531        1.6085572        2.2257691
## 3         1.1705876 1.4841026        1.8493513        2.4711756
## 4         1.6655590 1.7014729        1.7378994        1.7912096
## 5         1.2011071 1.2257695        1.2507673        1.2873222
## 6         1.0571277 1.0783077        1.0997688        1.1311398
## 7         1.0537022 1.0745078        1.0955856        1.1263893
## 8         0.8642084 0.8827038        0.9014612        0.9289092
## 9         0.8167828 0.8353653        0.8542275        0.8818577
## 10        0.6596925 0.6767171        0.6940321        0.7194558
## 11        0.5076931 0.5243601        0.5413880        0.5665243
## 12        0.5856209 0.6059343        0.6267122        0.6574270
##    Quantile.0.975(R)
## 1          4.4427399
## 2          2.4545180
## 3          2.6986182
## 4          1.8087520
## 5          1.2993437
## 6          1.1414534
## 7          1.1365147
## 8          0.9379405
## 9          0.8909562
## 10         0.7278427
## 11         0.5748502
## 12         0.6676116
```

```r
MCO_type <- c(rep('MCO1',3), 
              rep('MCO2',3),
              rep('MCO3',3),
              rep('MCO4',3)) 
data_mco <- data.frame(day =  c('D1', 'D5' , 'D10'), MCO_type)
data_mco <- bind_cols(data_mco, MCO) %>% rename(meanR = "Mean(R)")
data_mco
```

```
##    day MCO_type t_start t_end     meanR     Std(R) Quantile.0.025(R)
## 1   D1     MCO1       2     8 2.2845115 0.93264793         0.8383755
## 2   D5     MCO1       7    13 1.3156446 0.49726691         0.5289574
## 3  D10     MCO1      12    18 1.5407773 0.51359243         0.7045415
## 4   D1     MCO2      55    61 1.7020363 0.05363572         1.5985221
## 5   D5     MCO2      60    66 1.2261381 0.03681914         1.1550267
## 6  D10     MCO2      65    71 1.0786166 0.03161481         1.0175350
## 7   D1     MCO3      68    74 1.0748069 0.03105289         1.0147985
## 8   D5     MCO3      73    79 0.8829917 0.02762048         0.8296795
## 9  D10     MCO3      78    84 0.8356727 0.02776337         0.7821363
## 10  D1     MCO4      80    86 0.6770363 0.02546257         0.6280437
## 11  D5     MCO4      85    91 0.5247567 0.02498841         0.4769169
## 12 D10     MCO4      88    94 0.6064447 0.03047499         0.5481783
##    Quantile.0.05(R) Quantile.0.25(R) Median(R) Quantile.0.75(R)
## 1         0.9949104        1.6064721 2.1589248        2.8262080
## 2         0.6174725        0.9552814 1.2535531        1.6085572
## 3         0.8038111        1.1705876 1.4841026        1.8493513
## 4         1.6147847        1.6655590 1.7014729        1.7378994
## 5         1.1662110        1.2011071 1.2257695        1.2507673
## 6         1.0271470        1.0571277 1.0783077        1.0997688
## 7         1.0242445        1.0537022 1.0745078        1.0955856
## 8         0.8380566        0.8642084 0.8827038        0.9014612
## 9         0.7905364        0.8167828 0.8353653        0.8542275
## 10        0.6357055        0.6596925 0.6767171        0.6940321
## 11        0.4843419        0.5076931 0.5243601        0.5413880
## 12        0.5572034        0.5856209 0.6059343        0.6267122
##    Quantile.0.95(R) Quantile.0.975(R)
## 1         4.0028582         4.4427399
## 2         2.2257691         2.4545180
## 3         2.4711756         2.6986182
## 4         1.7912096         1.8087520
## 5         1.2873222         1.2993437
## 6         1.1311398         1.1414534
## 7         1.1263893         1.1365147
## 8         0.9289092         0.9379405
## 9         0.8818577         0.8909562
## 10        0.7194558         0.7278427
## 11        0.5665243         0.5748502
## 12        0.6574270         0.6676116
```


Calculate the difference


```r
data_mco %>%
    group_by(MCO_type) %>%
    mutate(mean_diff = meanR - lag(meanR),
           mean_diff_percent = (meanR - lag(meanR) / meanR)*100) %>% 
  select(day, MCO_type, meanR, mean_diff, mean_diff_percent, everything())
```

```
## # A tibble: 12 x 15
## # Groups:   MCO_type [4]
##    day   MCO_type meanR mean_diff mean_diff_perce~ t_start t_end `Std(R)`
##    <chr> <chr>    <dbl>     <dbl>            <dbl>   <dbl> <dbl>    <dbl>
##  1 D1    MCO1     2.28    NA                 NA          2     8   0.933 
##  2 D5    MCO1     1.32    -0.969            -42.1        7    13   0.497 
##  3 D10   MCO1     1.54     0.225             68.7       12    18   0.514 
##  4 D1    MCO2     1.70    NA                 NA         55    61   0.0536
##  5 D5    MCO2     1.23    -0.476            -16.2       60    66   0.0368
##  6 D10   MCO2     1.08    -0.148             -5.82      65    71   0.0316
##  7 D1    MCO3     1.07    NA                 NA         68    74   0.0311
##  8 D5    MCO3     0.883   -0.192            -33.4       73    79   0.0276
##  9 D10   MCO3     0.836   -0.0473           -22.1       78    84   0.0278
## 10 D1    MCO4     0.677   NA                 NA         80    86   0.0255
## 11 D5    MCO4     0.525   -0.152            -76.5       85    91   0.0250
## 12 D10   MCO4     0.606    0.0817           -25.9       88    94   0.0305
## # ... with 7 more variables: `Quantile.0.025(R)` <dbl>,
## #   `Quantile.0.05(R)` <dbl>, `Quantile.0.25(R)` <dbl>, `Median(R)` <dbl>,
## #   `Quantile.0.75(R)` <dbl>, `Quantile.0.95(R)` <dbl>,
## #   `Quantile.0.975(R)` <dbl>
```



```r
sessionInfo()
```

```
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19041)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] readxl_1.3.1    httr_1.4.1      lubridate_1.7.9 janitor_2.0.1  
##  [5] here_0.1        EpiEstim_2.2-3  forcats_0.5.0   stringr_1.4.0  
##  [9] dplyr_1.0.0     purrr_0.3.4     readr_1.3.1     tidyr_1.1.0    
## [13] tibble_3.0.1    ggplot2_3.3.2   tidyverse_1.3.0
## 
## loaded via a namespace (and not attached):
##  [1] jsonlite_1.6.1        splines_4.0.2         modelr_0.1.8         
##  [4] assertthat_0.2.1      blob_1.2.1            cellranger_1.1.0     
##  [7] yaml_2.2.1            pillar_1.4.4          backports_1.1.7      
## [10] lattice_0.20-41       quantreg_5.55         glue_1.4.1           
## [13] digest_0.6.25         rvest_0.3.5           snakecase_0.11.0     
## [16] colorspace_1.4-1      htmltools_0.5.0       Matrix_1.2-18        
## [19] plyr_1.8.6            pkgconfig_2.0.3       broom_0.5.6          
## [22] SparseM_1.78          haven_2.3.1           coarseDataTools_0.6-5
## [25] scales_1.1.1          incidence_1.7.1       MatrixModels_0.4-1   
## [28] farver_2.0.3          generics_0.0.2        ellipsis_0.3.1       
## [31] withr_2.2.0           cli_2.0.2             survival_3.2-3       
## [34] magrittr_1.5          crayon_1.3.4          mcmc_0.9-7           
## [37] evaluate_0.14         fs_1.4.1              fansi_0.4.1          
## [40] nlme_3.1-148          MASS_7.3-51.6         xml2_1.3.2           
## [43] tools_4.0.2           fitdistrplus_1.1-1    hms_0.5.3            
## [46] lifecycle_0.2.0       MCMCpack_1.4-8        munsell_0.5.0        
## [49] reprex_0.3.0          compiler_4.0.2        rlang_0.4.6          
## [52] grid_4.0.2            rstudioapi_0.11       labeling_0.3         
## [55] rmarkdown_2.3         gtable_0.3.0          DBI_1.1.0            
## [58] curl_4.3              reshape2_1.4.4        R6_2.4.1             
## [61] gridExtra_2.3         knitr_1.29            utf8_1.1.4           
## [64] rprojroot_1.3-2       stringi_1.4.6         Rcpp_1.0.4.6         
## [67] vctrs_0.3.1           dbplyr_1.4.4          tidyselect_1.1.0     
## [70] xfun_0.15             coda_0.19-3
```

