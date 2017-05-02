# It is not dependence






The formal statement establishes that, if there are no biases, then it is implausible that these observations are the result of chance. But it does not establish that it is a "True" effect. The observed unusual result could be caused by other things, with the truth being much less unusual. The analysis now moves to testing the data to prove that the observed effect is not other things, ruling out alternative hypotheses.

Earthquakes can occur in swarms- in particular where a large earthquake triggers aftershocks. In this sense, some of the earthquakes are dependent on other earthquakes, so the time the first earthquake occurs at potentially influences latter earthquakes.

In speaking with geologists, this was not generally a major concern as a single night (or day) is a very short amount of time in geological terms. If a large event happens the aftershocks soon start to cross back and forth between day and night as time goes on, and should balance out in the long run. So earthquakes would seem to be technically, but not practically, dependent. 

A subtler version of this problem is that the data is dependent enough that a few big events occurring at day, or in the night, bias the data set enough to make what are only moderately unlikely events (a bunch of large earthquakes that occur in the day or night) seem to be much more unlikely (the numerous aftershocks occurring at times conditioned by those few big events).

To test this, we can find the earthquakes that are not part of a swarm, and check the time of day they occurred at. To define a swarm I do so calculations to work out how far apart earthquakes in the same region are.

Using the 50 kilometres per side gridded areas I defined earlier, I calculate the amount of time in hours between earthquakes. This is based on the assumption that I can define a swarm as earthquakes close in both time and space.


Table: (\#tab:unnamed-chunk-2)Time from previous earthquake (hours) per 50km sq. grid

 median   mean   earthquakes
-------  -----  ------------
     28    216         99902

The median time between earthquake events in an area is 28 hours. This seems to offer a more useful measure than the mean, which is skewed by some very large gaps. 28 hours seems long enough to me that the events are not part of a swarm that is biasing results by occurring in a single day or night. Using this I can test the data based on the events that occurred in the non-swarm periods, being greater than or equal to 28 hours since the last earthquake.




Among earthquake events that show the least characteristics of being part of a swarm, 55.6% of the 50385 earthquakes occur at night, which is essentially identical to the overall earthquake catalogue result, and nowhere near 50%. On this basis, the evidence is that the non-swarm earthquakes are the same as the overall population

Another way to consider potential swarm dependencies is to exclude those swarms that would have been generated by aftershocks. To do this I calculate the night time event percentage for events only for months that occurred in grid areas that did not have any magnitude 5+ earthquake events.



Using "not aftershocks in grid zones" as the definition of earthquakes not in swarms, 56.4% of 89279 earthquake events occurred at night. This is fractionally more (but essentially identical to) the general number of night time earthquakes.

Since restricting the data in two different ways to minimise dependence is not changing the results, I can conclude that dependence between earthquake events is not significantly influencing the observed percentage of earthquake events at night.

## Formal Statement

Dependencies in the data caused by earthquakes triggering additional earthquakes are not influencing the results.

## Chapter Code


```r
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warnings=FALSE)
knitr::opts_chunk$set(errors=FALSE)
knitr::opts_chunk$set(message=FALSE)
knitr::opts_chunk$set(dpi = 150)
knitr::opts_chunk$set(fig.width =  6)
knitr::opts_chunk$set(fig.height =  4)

## ------------------------------------------------------------------------
Sys.setenv(TZ = "UTC") 
library(dplyr)
library(lubridate)
# Assumes there is eqnz_processed data created in chapter 2
load("eqdata/eqnz_processed.RData")

## ------------------------------------------------------------------------
eqnz %>% group_by(eq_roundedlat, eq_roundedlong) %>% mutate(timediff = floor(as.numeric(c(NA,diff(time_UTC)))/360)) %>%
ungroup() %>% filter(!is.na(timediff)) %>% summarise(`median` = median(timediff), `mean` = round(mean(timediff),0), `earthquakes`=n()) %>%
  knitr::kable(caption="Time from previous earthquake (hours) per 50km sq. grid")

## ------------------------------------------------------------------------
noswarm <- eqnz %>% group_by(eq_roundedlat, eq_roundedlong) %>% mutate(timediff = floor(as.numeric(c(NA,diff(time_UTC)))/360)) %>%
ungroup() %>% filter(!is.na(timediff) & timediff >=28) %>%
  summarise(night = sum(eq_is_night), totalevents=n(), percent = round(100 * sum(eq_is_night) / n(),1))

## ------------------------------------------------------------------------
tinymag <- eqnz %>% group_by(eq_roundedlat, eq_roundedlong, month(time_UTC), year(time_UTC)) %>% 
  mutate(monthTopMag = max(magnitude)) %>%
  ungroup() %>% filter(monthTopMag < 5) %>%
  summarise(night = sum(eq_is_night), totalevents=n(), percent = round(100 * sum(eq_is_night) / n(),1))
```