# CovidiaInR
Nate Silver's Covidia model ported to R

This repository contains an R function of [the spreadsheet model introduced here.](https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/)

```R
case = Covidia538()
```
The function arguments are the named parameters in column B of the worksheets. The default values correspond to Case1.
There is one additional argument, generations. The default value is 36.
```R
args(Covidia538)

function (Ro_uncontrolled = 2.7, Ro_intermediate = 1.4, Ro_lockdown = 0.7, 
    Cluster_value = "Yes, slightly", Begin_intermediate = 11, 
    Begin_lockdown = 15, Pct_asy = 0.3, Pct_mild = 0.6, Zero_date = "01/01/2020", 
    Serial = 5, Population = 1e+07, Initial_cases = 1, Faux_severe = 0.001, 
    Faux_mild = 0.025, Desire_severe = 1, Desire_mild = 0.5, 
    Desire_asy = 0.02, Initial_tests = 1000, Ramp_period = 3, 
    Test_gowth_rate = 0.5, Tests_max = 1e+07, Rationed_tests = 0.75, 
    False_negative = 0.2, False_positive = 0.005, Delay = 2, 
    generations = 36)
```
The function returns a list with two items. `case$params` shows the list of arguments and values.
`case$output` is a data frame containing the model output. The column names are similar to those in the original spreadsheet.

```R
library(reshape2)
library(ggplot2)
library(dplyr)
library(scales)
switch_colors = scale_color_manual(values=c("#00BFC4", "#F8766D"))

c1 <- case1$output %>%
  melt(id="Date") %>%
  mutate(value= replace(value, value == 0, NA) )

plotit <- function(x) {
x[x$variable %in% c("New_detected_cases","New_Infections"),] %>%
  ggplot() + 
    geom_line(aes(x=Date,y=value, group=variable, color=variable)) +
    scale_y_continuous(trans='log10', labels=comma) +
    switch_colors
}

plotit(c1)
```
## Case 1: actual cases vs detected cases
![Case 1 actual cases vs detected cases](https://github.com/samalcolm/CovidiaInR/blob/master/case1.png "Logo Title Text 1")

```R
case2 = Covidia538(Initial_tests=100, Ramp_period=6, Test_gowth_rate=2, Tests_max=100000)

case2$output %>%
  melt(id="Date") %>%
  mutate(value= replace(value, value == 0, NA) ) %>%
  plotit()
```
## Case 2: actual cases vs detected cases
![Case 2 actual cases vs detected cases](https://github.com/samalcolm/CovidiaInR/blob/master/case2.png "Logo Title Text 1")

```R
case3 = Covidia538(Initial_tests=10000, Ramp_period=2, Test_gowth_rate=0.03, Tests_max=20000, Rationed_tests = 1)

case3$output %>%
  melt(id="Date") %>%
  mutate(value= replace(value, value == 0, NA) ) %>%
  plotit()
```
## Case 3: actual cases vs detected cases
![Case 3 actual cases vs detected cases](https://github.com/samalcolm/CovidiaInR/blob/master/case3.png "Logo Title Text 1")

```R
case4 = Covidia538(Begin_lockdown=19, Initial_tests=10000, Ramp_period=10,T est_gowth_rate=-0.2, Tests_max=10000, Rationed_tests = 1)
case4$output %>%
  melt(id="Date") %>%
  mutate(value= replace(value, value == 0, NA) ) %>%
  plotit()
```
## Case 4: actual cases vs detected cases
![Case 4 actual cases vs detected cases](https://github.com/samalcolm/CovidiaInR/blob/master/case4.png "Logo Title Text 1")
