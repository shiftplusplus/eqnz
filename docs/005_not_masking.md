# It is not masking






The proportion of earthquakes occurring at night changes as the magnitude of the earthquakes changes.

<div class="figure">
<img src="005_not_masking_files/figure-epub3/unnamed-chunk-1-1.png" alt="Proportion of earthquakes occurring at night by magnitude (0.5 steps)"  />
<p class="caption">(\#fig:unnamed-chunk-1)Proportion of earthquakes occurring at night by magnitude (0.5 steps)</p>
</div>

The size of the confidence intervals around the proportions is determined by the number of earthquake observations within that magnitude. Among magnitude bands with a large number of earthquakes, so small confidence intervals, the proportion of earthquakes is clearly high then falling towards fifty percent.

As the proportion of earthquake events occurring at night decreased with earthquake magnitude, one possible explanation is an unknown mechanism that limits the ability to detect low magnitude earthquakes during the day, creating an illusory difference in overall earthquakes. Atef, Liu, and Gao report cultural noise in seismographic data matching freeway activity and other urban activity [Link 1](#links), leading to more earthquakes recorded at night. Similarly, Iwata attributes the masking effects to the decrease in earthquakes in the early and late part of the day and the recovery around noon [Link 2](#links).

To see if the masking interpretation is valid, I need to establish a test where the Null Hypothesis is that the difference is caused by masking, and if that test is failed then the effect cannot be caused by masking. After some thinking, the New Zealand data can be limited by sensor location to those seismographs in remote wilderness areas and lightly populated offshore islands, all of which lack freeways and urban areas within sensor range for low magnitude earthquakes. If there is a masking pattern caused by human activity in the day, the sensors in the remote location would be expected to have a 50/50 balance of night and day earthquakes.

For New Zealand Earthquakes, the places an earthquake was detected from can be obtained from a second data source at Geonet [Link 3](#links) by making a separate query for each earthquake.
 


The two seismographs with the least cultural noise to potentially interfere with records are Raoul Island and Puyesgur Point.

The Raoul Island seismograph is located on an island group with no permanent inhabitants, no urban areas or roads, and around 1000km from both Tonga and the New Zealand mainland [Link 4](#links).

Puyesgur Point is an automated lighthouse in a remote corner of the Fiordland National park, approximately 150 kilometres from any city greater than 50,000 people and approximately 550 kilometres from any city with a population greater than 120,000 people. The 2013 census records less than 10 people within 60 kilometres of the seismograph in the roadless national park  [Link 5](#links).

Selecting events detected by the Raoul Island (RIZ) Puyesgur Point (PYZ) yields a set of 2090 events, 1136 of which occurred at night. From this we can reject (95% (2 Sigma) confidence interval 0.522 to 0.565) the hypothesis that among seismographs in areas with no cultural activity there are as many day earthquakes as night. So, in turn, we reject the hypothesis that cultural activities are responsible for the difference between day and night activities. The 2 sigma level of confidence reflects that, though the result is not 0.5, I cannot be as confident with this amount of data (though I am still confident)

Though that rules out cultural effects masking earthquakes, there still might be mysterious, unknown daytime masking effect. While it is impossible to prove the existence of such a mechanism, after some thought it is possible to disprove its existence. To test if the difference between day and night can be explained by an unknown confounder affecting the range at which events are detected, I can limit the data set to only those events that must have been detectable during the day. The median distance to the closest seismograph an event was detected at during the day for events of magnitude 0.3 to 0.5 (inclusive) gives a distance that events, if they were of greater magnitude and occurred within that distance to a seismograph, should be equally detectable in day or night- If earthquakes are stronger and closer than typically detected daytime earthquakes, they can be assumed to not be masked.




By discarding all events outside that distance as potentially unable to be detected in the day, of the 9127 events that remained, 5059 occurred at night. From that we can strongly reject (5 Sigma confidence interval of the proportion is 0.544 to 0.565) the hypothesis that equal numbers of earthquakes occur in the day and at night among earthquakes at a range that can be detected in the day.

## Formal Statement

The hypothesis that daytime masking is causing differences in the measured number of night and day earthquakes is rejected on two grounds. Seismographs in remote locations show no evidence of a lack of daytime masking. 



## Links

Link 1 - Atef, A. H., Liu, K. H. & Gao, S. S. Apparent Weekly and Daily Earthquake Periodicities in the Western United States. Bulletin of the Seismological Society of America 99, 2273–2279 (2009).

Link 2 - Iwata, T. in Earthquake Research and Analysis - New Advances in Seismology (ed. Damico, S.) (InTech, 2013).

Link 3 - Geonet Simple Queries http://info.geonet.org.nz/display/appdata/Simple+Queries

Link 4 - Raoul Island https://en.wikipedia.org/wiki/Raoul_Island

Link 5 - Puysegur Point https://en.wikipedia.org/wiki/Puysegur_Point and the 2013 census results http://www.stats.govt.nz/StatsMaps/Home/People%20and%20households/2013-census-quickstats-about-a-place-map.aspx



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
# Assumes there is eqnz_processed data created in chapter 2
load("eqdata/eqnz_processed.RData")
eq_national <- eqnz

## ---- fig.cap="Proportion of earthquakes occurring at night by magnitude (0.5 steps)"----
old_par=par()
grf <- eq_national %>% mutate(floored_mag = floor(magnitude*2)/2) %>%
  group_by(floored_mag) %>% summarise(successes = sum(eq_is_night), trials=n())

poly_conf_int <- function(success, trials, aa, stepsize, sigma, colr){
  ci <- binom.confint(success, trials, method=c("wilson"), conf.level = sigma)
  lower <- ci[1,5] 
  upper <- ci[1,6] 
  a <- polygon(x=c(aa,aa+stepsize,aa+stepsize,aa), y=c(upper,upper,lower,lower),
               col=colr, border=NA)
}


plot7sig <- function(success, trials, aa, stepsize){
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
plot(x=c(0,max(grf$floored_mag)+0.5), y=c(0,1), type="n", bty="n", xlab="Magnitude (0.5 steps)", ylab="Proportion of earthquakes at night")
a <- apply(grf,1,function(x){plot7sig(x[2],x[3],x[1],0.5)})
lines(c(0,10), c(.5,.5), col="#FFFFFF")
lines(c(0,10), c(.5,.5), lty=2, col="#777777")

par(mar=c(0,0,0,0))
plot(x=c(0,10), y=c(0,10), type="n", bty="n", axes=FALSE)
legend(0,5, legend=lbls, lty=typs, lwd=weights, col=clrs, bty="n", xjust=0, title="Confidence
Intervals:", cex=0.9)
lbls=c("Expected Proportion", "Actual Proportion")
typs=c(2,1)
weights=c(1,2)
legend(0,7, legend=lbls, lty=typs, lwd=weights, bty="n", xjust=0, title="Legend", y.intersp=1.2)


par(mar=old_par$mar)
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
## Becuase this involved 100000 downloads (though very small ones) from geonet, I only wanted to do it once
## hence the only downloading if saved file of results does not exist.
## there are getting on for 2.368 million rows in the final data set and it takes many hours to assemble (though is only 3.9 MB in the end)
## if you want to replicate this and save time, and trust me, you can load the shortlists later instead of running this section.
if (!(file.exists("eqdata/nz_detection_sites.RData"))){
get_pick_data <- function(x){
  csv_url <- paste("http://quakeml.geonet.org.nz/csv/1.0.0/", x, "/picks", sep="")
  eq_data <- read.csv(csv_url, stringsAsFactors = FALSE)
  station_data <- data.frame(station= unique(eq_data$station), stringsAsFactors=FALSE)
  station_data$eq_id <- x
  return(station_data)
}

# when I checked, 4 earthquakes had no pick information
no_pick_info <- c("3571857", "2012p569703", "2012p813511", "2013p613798")
eq_ids <- eqnz$publicid[!(eqnz$publicid %in% no_pick_info)]
nz_station_info <- bind_rows(lapply(eq_ids, get_pick_data))
save(nz_station_info, file="eqdata/nz_detection_sites.RData")
}
rm(list=ls())
load("eqdata/eqnz_processed.RData")
load("eqdata/nz_detection_sites.RData")

RIZ_list <- nz_station_info$eq_id[nz_station_info$station == "RIZ"]
PYZ_list <- nz_station_info$eq_id[nz_station_info$station == "PYZ"]

no_cultural_influence <- unique(c(RIZ_list, PYZ_list))
tot_qt_nt <- sum(eqnz$eq_is_night[eqnz$publicid %in% no_cultural_influence])
tot_qt <- length(no_cultural_influence)
bt <- binom.test(tot_qt_nt, tot_qt)


## ------------------------------------------------------------------------
threshold <- median(eqnz$minimumdistance[eqnz$magnitude >= 0.3 & eqnz$magnitude <= 0.5])
detectable <- eqnz %>% filter(magnitude > 0.5, minimumdistance <= threshold) %>%
  summarise(at_night = sum(eq_is_night), total=n(), proportion= at_night/total)
bt <-binom.test(detectable$at_night, detectable$total)
```
