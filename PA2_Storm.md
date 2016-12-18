#Title : 
##Impacts of Storms and other severe weather events on public health and economic problems for communities and municipalities

#Synopsis :
##This analysis tries to  explor the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.


```r
library(knitr)
library(markdown)
```

```
## Warning: package 'markdown' was built under R version 3.3.2
```

```r
library(rmarkdown)
```

```
## Warning: package 'rmarkdown' was built under R version 3.3.2
```

```r
library(plyr) # for count & aggregate method
library(stats)
library(RCurl) # for loading external dataset (getBinaryURL)
```

```
## Loading required package: bitops
```

```r
library(R.utils) # for bunzip2
```

```
## Warning: package 'R.utils' was built under R version 3.3.2
```

```
## Loading required package: R.oo
```

```
## Warning: package 'R.oo' was built under R version 3.3.2
```

```
## Loading required package: R.methodsS3
```

```
## Warning: package 'R.methodsS3' was built under R version 3.3.2
```

```
## R.methodsS3 v1.7.1 (2016-02-15) successfully loaded. See ?R.methodsS3 for help.
```

```
## R.oo v1.21.0 (2016-10-30) successfully loaded. See ?R.oo for help.
```

```
## 
## Attaching package: 'R.oo'
```

```
## The following object is masked from 'package:RCurl':
## 
##     clone
```

```
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
```

```
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
```

```
## R.utils v2.5.0 (2016-11-07) successfully loaded. See ?R.utils for help.
```

```
## 
## Attaching package: 'R.utils'
```

```
## The following object is masked from 'package:RCurl':
## 
##     reset
```

```
## The following object is masked from 'package:utils':
## 
##     timestamp
```

```
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

#Data Processing:
## The data is first unzipped and loaded into a data frame for further analysis and observations

```r
setwd("C://Users//spg//Downloads//documents//data science//data")
bunzip2("repdata%2Fdata%2FStormData.csv.bz2","repdata%2Fdata%2FStormData.csv")

storm <- read.csv(file = "repdata%2Fdata%2FStormData.csv", header = TRUE, sep = ",")
dim(storm)
```

```
## [1] 902297     37
```

```r
names(storm)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
str(storm)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436774 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

```r
ReqNedeed <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm <- storm[ReqNedeed]
dim(storm)
```

```
## [1] 902297      7
```

```r
names(storm)
```

```
## [1] "EVTYPE"     "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP"
## [6] "CROPDMG"    "CROPDMGEXP"
```

```r
str(storm)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
unique(storm$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
storm$PROPDMGEXP <- mapvalues(storm$PROPDMGEXP, from = c("K", "M","", "B", "m", "+", "0", "5", "6", "?", "4", "2", "3", "h", "7", "H", "-", "1", "8"), to = c(10^3, 10^6, 1, 10^9, 10^6, 0,1,10^5, 10^6, 0, 10^4, 10^2, 10^3, 10^2, 10^7, 10^2, 0, 10, 10^8))
storm$PROPDMGEXP <- as.numeric(as.character(storm$PROPDMGEXP))
storm$PROPDMGTOTAL <- (storm$PROPDMG * storm$PROPDMGEXP)/1000000000

unique(storm$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
storm$CROPDMGEXP <- mapvalues(storm$CROPDMGEXP, from = c("","M", "K", "m", "B", "?", "0", "k","2"), to = c(1,10^6, 10^3, 10^6, 10^9, 0, 1, 10^3, 10^2))
storm$CROPDMGEXP <- as.numeric(as.character(storm$CROPDMGEXP))
storm$CROPDMGTOTAL <- (storm$CROPDMG * storm$CROPDMGEXP)/1000000000

sumFatalities <- aggregate(FATALITIES ~ EVTYPE, data = storm,  FUN="sum")
dim(sumFatalities)  ## 985 observations
```

```
## [1] 985   2
```

```r
fatalities10events <- sumFatalities[order(-sumFatalities$FATALITIES), ][1:10, ]
dim(fatalities10events)
```

```
## [1] 10  2
```

```r
fatalities10events
```

```
##             EVTYPE FATALITIES
## 834        TORNADO       5633
## 130 EXCESSIVE HEAT       1903
## 153    FLASH FLOOD        978
## 275           HEAT        937
## 464      LIGHTNING        816
## 856      TSTM WIND        504
## 170          FLOOD        470
## 585    RIP CURRENT        368
## 359      HIGH WIND        248
## 19       AVALANCHE        224
```

```r
sumInjuries <- aggregate(INJURIES ~ EVTYPE, data = storm,  FUN="sum")
dim(sumInjuries)  ## 985 observations
```

```
## [1] 985   2
```

```r
injuries10events <- sumInjuries[order(-sumInjuries$INJURIES), ][1:10, ]
dim(injuries10events)
```

```
## [1] 10  2
```

```r
injuries10events
```

```
##                EVTYPE INJURIES
## 834           TORNADO    91346
## 856         TSTM WIND     6957
## 170             FLOOD     6789
## 130    EXCESSIVE HEAT     6525
## 464         LIGHTNING     5230
## 275              HEAT     2100
## 427         ICE STORM     1975
## 153       FLASH FLOOD     1777
## 760 THUNDERSTORM WIND     1488
## 244              HAIL     1361
```

```r
sumPropertyDamage <- aggregate(PROPDMGTOTAL ~ EVTYPE, data = storm,  FUN="sum")
dim(sumPropertyDamage)  ## 985 observations
```

```
## [1] 985   2
```

```r
propdmg10Total <- sumPropertyDamage[order(-sumPropertyDamage$PROPDMGTOTAL), ][1:10, ]
propdmg10Total
```

```
##                EVTYPE PROPDMGTOTAL
## 170             FLOOD   144.657710
## 411 HURRICANE/TYPHOON    69.305840
## 834           TORNADO    56.947381
## 670       STORM SURGE    43.323536
## 153       FLASH FLOOD    16.822674
## 244              HAIL    15.735268
## 402         HURRICANE    11.868319
## 848    TROPICAL STORM     7.703891
## 972      WINTER STORM     6.688497
## 359         HIGH WIND     5.270046
```

```r
sumCropDamage <- aggregate(CROPDMGTOTAL ~ EVTYPE, data = storm,  FUN="sum")
dim(sumCropDamage) ## 985 observations
```

```
## [1] 985   2
```

```r
cropdmg10Total <- sumCropDamage[order(-sumCropDamage$CROPDMGTOTAL), ][1:10, ]
cropdmg10Total
```

```
##                EVTYPE CROPDMGTOTAL
## 95            DROUGHT    13.972566
## 170             FLOOD     5.661968
## 590       RIVER FLOOD     5.029459
## 427         ICE STORM     5.022113
## 244              HAIL     3.025954
## 402         HURRICANE     2.741910
## 411 HURRICANE/TYPHOON     2.607873
## 153       FLASH FLOOD     1.421317
## 140      EXTREME COLD     1.292973
## 212      FROST/FREEZE     1.094086
```

#Results

##4.1. Injuries vs. Fatalities
##Juxtaposed the injuries and fatalaties for each major weather event, orderd by number of injuries You can see that the top 5 for both contain the same events. But Heat & Drought has more casulties than Tornado's, by far the #1 in injuries. High seas have almost as much casulties as injuries, so it has the worst odds of survival of the list.


```r
par(mfrow = c(1,1), mar = c(12, 6, 3, 2), mgp = c(4, 1, 0), cex = 0.8)
barplot(injuries10events$INJURIES, names.arg = injuries10events$EVTYPE, las = 3, main = "10 Injuries Highest Events", ylab = "Number of Injuries")
```

![](PA2_Storm_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, "injuries-events.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
par(mfrow = c(1,1), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(fatalities10events$FATALITIES, names.arg = fatalities10events$EVTYPE, las = 3, main = "10 Fatalities Highest Events", ylab = "Number of Fatalities")
```

![](PA2_Storm_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
dev.copy(png, "fatalities-events.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

#4.2. Economic Damage

##Crop damage is hardly a factor in comparission to the total economic cost of certain weather events, except for Heat & Drought, where it effects more than 90% The real interesting ones are Wind & Storm and Flooding & High Surf covering together more than 80% of all economic damage over all the years. Tornado's,Thunderstorms and Snow & Ice, which have high impact in human costs, hardly matter in economic damages


```r
par(mfrow = c(1,1), mar = c(10, 6, 3, 2), mgp = c(3, 1, 0), cex = 0.6)
barplot(cropdmg10Total$CROPDMGTOTAL, names.arg = cropdmg10Total$EVTYPE, las = 2, main = "10 Crop Damages Highest Events", ylab = "Damage Crop Values (in Billions) ")
```

![](PA2_Storm_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, "cropdmg-total.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off() 
```

```
## png 
##   2
```
