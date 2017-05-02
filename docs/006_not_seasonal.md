# It is not seasonal






The amount of night within a 24 hour period changes over the course of a year, so a possible explanation is that if earthquakes are more likely to occur in winter, and winter nights are longer, then there would overall be more earthquakes at night. This would be an interesting finding in itself, but it would make the daily pattern the result of a seasonal one, and in that case I would need to go back and consider the position of the sun on a seasonal north-south in addition to the daily east-west basis.

In calculating new variables from the data, I derived the length of night for each 24 hour period that an earthquake took place in. I can test this amount of night against the number of earthquakes that occurred in the available night.

<div class="figure">
<img src="006_not_seasonal_files/figure-epub3/unnamed-chunk-1-1.png" alt="Proportion of night earthquakes by length of night. n = 100279"  />
<p class="caption">(\#fig:unnamed-chunk-1)Proportion of night earthquakes by length of night. n = 100279</p>
</div>

As (for all hours we can be confident about) there is a consistent level of nightly earthquake oversupply, I can rule out seasonal differences being the cause.


Table: (\#tab:unnamed-chunk-2)Length of night and number of earthquakes

 hours of night   night quakes   total quakes   % hours of night   % quakes at night
---------------  -------------  -------------  -----------------  ------------------
              7              4              5                 29                  80
              8           1976           5003                 33                  39
              9          10067          21281                 38                  47
             10           6513          12899                 42                  50
             11           6555          11904                 46                  55
             12           7591          12734                 50                  60
             13          10150          16181                 54                  63
             14          13006          19724                 58                  66
             15            384            548                 62                  70

This is not a formal statistical test, because I don't think it needs it. The oversupply of nighttime earthquakes is neither rising or falling across the season's change of night lengths, so there is no evidence of an association with seasonal change.

## Formal statement

There is no evidence that the occurrence of night earthquakes are influenced by seasonal changes. As a consequence this analysis focuses on change within a 24 hour period.


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

## ----c002_libraries------------------------------------------------------
Sys.setenv(TZ = "UTC") 
library(dplyr)
library(ggplot2)
library(lubridate)
library(maptools)
library(binom)
library(parallel)
library(plotrix)
library(solidearthtide)
library(tidyr)
# Assumes there is eqnz_processed data created in chapter 2
load("eqdata/eqnz_processed.RData")
old_par <- par()

lbls <- c(
 "1 sigma", "2 sigma",
 "3 sigma", "4 sigma",
 "5 sigma", "6 sigma",
 "7 sigma")
typs <- c(1,1,1,1,1,1,1)
weights <- c(3,3,3,3,3,3,3)
clrs <- rev(c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'))



## ---- fig.cap="Proportion of night earthquakes by length of night. n = 100279"----
hrs <-eqnz %>% mutate(hour_band=floor(eq_nightlength)) %>% 
  group_by(hour_band) %>% 
  summarise(eq_nt = sum(eq_is_night), eq_all = n()) %>%
  mutate(nt_prop = hour_band/24, eq_prop = eq_nt/ eq_all)

old_par=par()
grf <- hrs
poly_conf_int <- function(success, trials, aa, stepsize, sigma, colr){
  ci <- binom.confint(success, trials, method=c("wilson"), conf.level = sigma)
  lower <- ci[1,5] 
  upper <- ci[1,6] 
  a <- polygon(x=c(aa,aa+stepsize,aa+stepsize,aa), y=c(upper,upper,lower,lower),
               col=colr, border=NA)
}


plot7sighr <- function(success, trials, aa, stepsize, expected){
  library(binom)
  #bands <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
  bands <- rev(c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'))
  sigmas <- c(0.682689492137086,
              0.954499736103642,
              0.997300203936740,
              0.999936657516334,
              0.999999426696856,
              0.999999998026825,
              0.999999999997440)
  sapply(7:1, function(x){poly_conf_int(success, trials, aa, stepsize, sigmas[x], bands[x])})
  a <- lines(c(aa, aa + stepsize), c(success/trials, success/trials), lwd=2)
  a <- lines(c(aa, aa + stepsize), c(expected,expected), col="#FFFFFF")
  a <- lines(c(aa, aa + stepsize), c(expected,expected), lty=2, col="#777777")
  
}


lbls <- c(
  "1 sigma", "2 sigma",
  "3 sigma", "4 sigma",
  "5 sigma", "6 sigma",
  "7 sigma")
typs <- c(1,1,1,1,1,1,1)
weights <- c(3,3,3,3,3,3,3)
clrs <- rev(c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'))
#clrs <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')

layout(matrix(c(1,1,1,2), ncol=4))
plot(x=c(min(grf$hour_band),max(grf$hour_band)+1), y=c(0,1), type="n", bty="n", xlab="Hours of night", ylab="Proportion of earthquakes at night")
a <- apply(grf,1,function(x){plot7sighr(x[2],x[3],x[1],1,x[4])})


par(mar=c(0,0,0,0))
plot(x=c(0,10), y=c(0,10), type="n", bty="n", axes=FALSE)
legend(0,5, legend=lbls, lty=typs, lwd=weights, col=clrs, bty="n", xjust=0, title="Confidence\nIntervals:", cex=0.9)
lbls=c("Proportion of night\nin 24 hours", "Night earthquakes\nProportion")
typs=c(2,1)
weights=c(1,2)
legend(0,9, legend=lbls, lty=typs, lwd=weights, bty="n", xjust=0, title="Legend", y.intersp=1.2)


par(mar=old_par$mar)
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
hrs %>% mutate(`% hours of night` = 100 * round(nt_prop,2),
               `% quakes at night` = 100 * round(eq_prop,2)) %>%
  select(`hours of night`=hour_band,
               `night quakes` = eq_nt,
               `total quakes` = eq_all,
               `% hours of night`,
               `% quakes at night`) %>%
  knitr::kable(caption="Length of night and number of earthquakes")
```
