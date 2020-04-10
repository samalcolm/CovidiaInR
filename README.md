# CovidiaInR
Nate Silver's Covidia model ported to R

This repository contains an R function of [the spreadsheet model introduced here.](https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/)

```R
case = Covidia538(...)
```
```R
args(Covidia538)
```

```R
case$params
```

```R
case$output
```
```R
library(ggplot2)
library(dplyr)
library(scales)
switch_colors = scale_color_manual(values=c("#00BFC4", "#F8766D"))

c1 <- case1$output %>%
  reshape2::melt(id="Date") %>%
  mutate(value= replace(value, value == 0, NA) )
ggplot(c1[c1$variable %in% c("New_detected_cases","New_Infections"),]) + 
  geom_line(aes(x=Date,y=value, group=variable, color=variable)) +
  scale_y_continuous(trans='log10', labels=comma) +
  switch_colors
```
##Case 1: actual cases vs detected cases
![Case 1 actual cases vs detected cases](https://github.com/samalcolm/CovidiaInR/blob/master/case1.png "Logo Title Text 1")
