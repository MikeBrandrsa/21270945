Financial Econometrics Practical

# Outline

This Readme will outline all the following questions by means of
explaining, interpreting and reasoning through all the necessary code
and functions, whereafter a simplified output will be reproduced within
each respective question folders.

First, I sanitize my working environment and source all the necessary
functions that will be incorporated into our analyses.

Now I procceed with the respective questions.

# Question 1: Yield Spread.

Since the beginning of 2020 the current Yield spreads in local mid to
longer dated bond yields have been the highest in decades. This is
conventionally expressed as the difference in these yields of these
instruments in percentage points or basis points.

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# bond_Adj <- 
# SA_bonds %>%  gather(Bond, Yield, -date) %>% 

    
    
    
#arrange(date) 
# Now calculate ordinary returns per stock:
#group_by(Stocks) %>% 
#mutate(Returns = Price/lag(Price) - 1) %>% ungroup()
```
