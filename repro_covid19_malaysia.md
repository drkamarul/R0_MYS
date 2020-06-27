---
title: "Reproduction Number for Malaysia"
author: "Kamarul Imran Musa"
date: "6/16/2020"
output: 
  html_document: 
    keep_md: yes
---



# Motivation

To model the time-dependent reproduction number of COVID-19 for Malaysia

# Methods

- Reproduce methods proposed in EpiEstim package
- Reference  https://cran.r-project.org/web/packages/EpiEstim/vignettes/demo.html

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


# Get data

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


```r
mys <- covid %>% 
  select(date, country, type , cases) %>% 
  filter(country == 'Malaysia', type == 'confirmed') %>% clean_names() %>%
  rename(Time = date, I = cases) %>% data.frame()
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
##   Date: 2020-06-27 15:15
##   Status: 200
##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
##   Size: 566 kB
## <ON DISK>  C:\Users\DRKAMA~1\AppData\Local\Temp\RtmpIts8Pd\file441458391368.xlsx
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
  mutate_if(is.POSIXt, as.Date) %>% data.frame()
```

```
## New names:
## * `` -> ...23
## * `` -> ...24
```

# Calculate R

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
##  $ R         :'data.frame':	149 obs. of  11 variables:
##   ..$ t_start          : num [1:149] 2 3 4 5 6 7 8 9 10 11 ...
##   ..$ t_end            : num [1:149] 8 9 10 11 12 13 14 15 16 17 ...
##   ..$ Mean(R)          : num [1:149] 9.44 6.73 4.69 2.28 1.46 ...
##   ..$ Std(R)           : num [1:149] 3.339 2.245 1.563 0.933 0.651 ...
##   ..$ Quantile.0.025(R): num [1:149] 4.077 3.08 2.143 0.838 0.473 ...
##   ..$ Quantile.0.05(R) : num [1:149] 4.7 3.513 2.445 0.995 0.574 ...
##   ..$ Quantile.0.25(R) : num [1:149] 7.031 5.117 3.561 1.606 0.981 ...
##   ..$ Median(R)        : num [1:149] 9.05 6.49 4.52 2.16 1.36 ...
##   ..$ Quantile.0.75(R) : num [1:149] 11.43 8.08 5.63 2.83 1.83 ...
##   ..$ Quantile.0.95(R) : num [1:149] 15.52 10.8 7.52 4 2.67 ...
##   ..$ Quantile.0.975(R): num [1:149] 17.03 11.8 8.21 4.44 2.98 ...
##  $ method    : chr "parametric_si"
##  $ si_distr  : Named num [1:157] 0 0.00127 0.02002 0.06267 0.10375 ...
##   ..- attr(*, "names")= chr [1:157] "t0" "t1" "t2" "t3" ...
##  $ SI.Moments:'data.frame':	1 obs. of  2 variables:
##   ..$ Mean: num 7.5
##   ..$ Std : num 3.42
##  $ dates     : int [1:156] 1 2 3 4 5 6 7 8 9 10 ...
##  $ I         : num [1:156] 0 0 0 3 1 0 0 3 1 0 ...
##  $ I_local   : num [1:156] 0 0 0 3 1 0 0 3 1 0 ...
##  $ I_imported: num [1:156] 0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "class")= chr "estimate_R"
```
# Plot

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

# Calculate R with uncertainty

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


# Analysis from data world



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

# Plot

- plots the incidence
- plots the serial interval distribution
- plot of R


```r
p_I_dw <- plot(mys_dw_parametric_si, "incid") 
p_I_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



```r
p_SI_dw <- plot(mys_dw_parametric_si, "SI")  
p_SI_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

- the 7-day sliding window estimates of instantaneous $R_e$


```r
p_Ri_dw <- plot(mys_dw_parametric_si, "R")
p_Ri_dw + theme_bw()
```

![](repro_covid19_malaysia_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



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
## [61] gridExtra_2.3         knitr_1.29            rprojroot_1.3-2      
## [64] stringi_1.4.6         Rcpp_1.0.4.6          vctrs_0.3.1          
## [67] dbplyr_1.4.4          tidyselect_1.1.0      xfun_0.15            
## [70] coda_0.19-3
```

