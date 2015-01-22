---
title: "Storm_Impact"
author: "Jinesh Panchal"
date: "Tuesday, January 21, 2015"
output: html_document
---



##SYNOPSIS
    Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.  
    This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.  
    We will display this analyis, project in sequence.Sequenc start with,  
    1. Preprocessing of data
    2. Processing of data(Computation of data for Fatalities, Injuries, Property loss)    
    3. Result display in form of numerics and tables  
    4. Result display in form of figure  
let's start working on it.    


##DATA PROCESSING
###Preprocessing of data  
    
   First of all we need to get the data from zip folder.Simply by **unzip** functoin we wouldnt be able to unzip **bzip2** type data, so here we have to install package called **r.utils** and Loading all the required packages.
  

```r
library(ggplot2)
library(plyr)
library(R.utils, warn.conflicts=FALSE)
```

  After loading package we needs to unzip data using **bunzip** function.  We are storing whole datasets or table in variable called as **storm**.


```r
bunzip2("repdata-data-StormData.csv.bz2")
```

```
## Error in bunzip2.default("repdata-data-StormData.csv.bz2"): File already exists: repdata-data-StormData.csv
```

```r
storm <- read.csv("repdata-data-StormData.csv",header = TRUE)
dim(storm)
```

```
## [1] 902297     37
```
Now we have raw data with some extra column which is not required for computation.    By simply doing some basic levlels of preprocessing on raw data.

```r
#Header of table convert it into lower alphabets
names(storm) <- tolower(names(storm))
#Extract only important column and stored it into vector **imp**.
imp <- c(8,23,24,25,27)
#extract important datasets from original datasets
storm <- storm[,imp]
#Lets summarize the column we extracted from orignal datasets
summary(storm)
```

```
##                evtype         fatalities          injuries        
##  HAIL             :288661   Min.   :  0.0000   Min.   :   0.0000  
##  TSTM WIND        :219940   1st Qu.:  0.0000   1st Qu.:   0.0000  
##  THUNDERSTORM WIND: 82563   Median :  0.0000   Median :   0.0000  
##  TORNADO          : 60652   Mean   :  0.0168   Mean   :   0.1557  
##  FLASH FLOOD      : 54277   3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##  FLOOD            : 25326   Max.   :583.0000   Max.   :1700.0000  
##  (Other)          :170878                                         
##     propdmg           cropdmg       
##  Min.   :   0.00   Min.   :  0.000  
##  1st Qu.:   0.00   1st Qu.:  0.000  
##  Median :   0.00   Median :  0.000  
##  Mean   :  12.06   Mean   :  1.527  
##  3rd Qu.:   0.50   3rd Qu.:  0.000  
##  Max.   :5000.00   Max.   :990.000  
## 
```

```r
#Now we have same dataset "storm" with only required column.
#Prprocessing of data has been done.
```
 
###COMPUTATION
Now we have to start doing processing on accquired data.  
first we will load package called **plyr**, which we already did at the very begining.  Now we will summarize this data on basis of total injuries,total fatalities and total property loss which is affected by several type of event
 

```r
#we will summarize with help of injuries and fatalities column from above extracted datasets called as storm
health.aff.data <- ddply(storm,.(evtype),summarize,total.injuries=sum(injuries,na.rm=T),total.fatalities=sum(fatalities,na.rm=T))
summary(health.aff.data)
```

```
##                    evtype    total.injuries    total.fatalities 
##     HIGH SURF ADVISORY:  1   Min.   :    0.0   Min.   :   0.00  
##   COASTAL FLOOD       :  1   1st Qu.:    0.0   1st Qu.:   0.00  
##   FLASH FLOOD         :  1   Median :    0.0   Median :   0.00  
##   LIGHTNING           :  1   Mean   :  142.7   Mean   :  15.38  
##   TSTM WIND           :  1   3rd Qu.:    0.0   3rd Qu.:   0.00  
##   TSTM WIND (G45)     :  1   Max.   :91346.0   Max.   :5633.00  
##  (Other)              :979
```

Now we have data which display the effect of various events on fatalities and injuries.  
But we have to select only topmost (12) data of **Fatalities and Injuries**   

###Fatalities data computation 


```r
#Top 12 fatalities Data
top.fatalities <- head(arrange(health.aff.data,desc(total.fatalities)),12)
top.fatalities <- top.fatalities[,c(1,3)]  
summary(top.fatalities)
```

```
##             evtype  total.fatalities
##  AVALANCHE     :1   Min.   : 204.0  
##  EXCESSIVE HEAT:1   1st Qu.: 242.0  
##  FLASH FLOOD   :1   Median : 487.0  
##  FLOOD         :1   Mean   :1040.9  
##  HEAT          :1   3rd Qu.: 947.2  
##  HIGH WIND     :1   Max.   :5633.0  
##  (Other)       :6
```

```r
#We will display this data at time of result section
```
###Injuries data computation    

```r
#Top 12 Injuries Data
top.injuries <- head(arrange(health.aff.data,desc(total.injuries)),12)
top.injuries <- top.injuries[,1:2]
summary(top.injuries)
```

```
##                evtype  total.injuries 
##  EXCESSIVE HEAT   :1   Min.   : 1275  
##  FLASH FLOOD      :1   1st Qu.: 1456  
##  FLOOD            :1   Median : 2038  
##  HAIL             :1   Mean   :10679  
##  HEAT             :1   3rd Qu.: 6591  
##  HURRICANE/TYPHOON:1   Max.   :91346  
##  (Other)          :6
```

```r
#We will display this data at time of result section
```
###Property loss data computation  
We have done computation based on **fatalities and injuries**, now we are going to do the computation for **Property loss** which includes column **Propdmg and cropdmg** because of several events.

```r
property.aff.data <- ddply(storm,.(evtype),summarize,total.dmg = sum(propdmg,cropdmg,na.rm=T))
#Top 12 Property Loss data
top.property <- head(arrange(property.aff.data,desc(total.dmg)),12)
#Total property(loss/1000000 for easy depiction on graph) due to diffrent type of event.  
top.property$total.dmg <- top.property$total.dmg/1000000
summary(top.property)
```

```
##          evtype    total.dmg      
##  FLASH FLOOD:1   Min.   :0.08882  
##  FLOOD      :1   1st Qu.:0.29019  
##  HAIL       :1   Median :0.77528  
##  HEAVY SNOW :1   Mean   :0.94988  
##  HIGH WIND  :1   3rd Qu.:1.31251  
##  LIGHTNING  :1   Max.   :3.31228  
##  (Other)    :6
```


##RESULTS  
Now we are going to display the result of above computation.    

###1. Top Events which affected on Human Health(Fatalities)  

```r
#Top 12 data on Fatalities due to diffrent event type
top.fatalities
```

```
##            evtype total.fatalities
## 1         TORNADO             5633
## 2  EXCESSIVE HEAT             1903
## 3     FLASH FLOOD              978
## 4            HEAT              937
## 5       LIGHTNING              816
## 6       TSTM WIND              504
## 7           FLOOD              470
## 8     RIP CURRENT              368
## 9       HIGH WIND              248
## 10      AVALANCHE              224
## 11   WINTER STORM              206
## 12   RIP CURRENTS              204
```
###2. Top Events which affected on Human Health(Injuries)  

```r
#Top 12 data on injuries due to diffrent event type
top.injuries
```

```
##               evtype total.injuries
## 1            TORNADO          91346
## 2          TSTM WIND           6957
## 3              FLOOD           6789
## 4     EXCESSIVE HEAT           6525
## 5          LIGHTNING           5230
## 6               HEAT           2100
## 7          ICE STORM           1975
## 8        FLASH FLOOD           1777
## 9  THUNDERSTORM WIND           1488
## 10              HAIL           1361
## 11      WINTER STORM           1321
## 12 HURRICANE/TYPHOON           1275
```
###3. Top Events which has greatest economic consequences  

```r
#Top 12 on property loss/1000000 due to diffrent event type
top.property
```

```
##                evtype  total.dmg
## 1             TORNADO 3.31227668
## 2         FLASH FLOOD 1.59932505
## 3           TSTM WIND 1.44516821
## 4                HAIL 1.26828966
## 5               FLOOD 1.06797636
## 6   THUNDERSTORM WIND 0.94363562
## 7           LIGHTNING 0.60693239
## 8  THUNDERSTORM WINDS 0.46497811
## 9           HIGH WIND 0.34201477
## 10       WINTER STORM 0.13469958
## 11         HEAVY SNOW 0.12441771
## 12           WILDFIRE 0.08882354
```


##FIGURES
####1. This figure depict the total fatalities cause by different type of events and shows which events are the culprit among all.


```r
#Plotting data for top 12 fatalities by diffrent eventype.
plot.fatalities <- ggplot(top.fatalities,aes(evtype,total.fatalities,fill=evtype))+geom_bar(stat = "identity")+scale_fill_discrete("Event Type")+theme(legend.title = element_text(colour="blue", size=16, face="bold"))+ggtitle("Most harmful (Top-12) event affected on human health - FATALITIES")+xlab("Event Type")+ylab("Total Fatalities")+ggsave("fatalities.png",width = 20,height = 10)
```

<img src="figure/unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

```r
plot.fatalities
```

<img src="figure/unnamed-chunk-11-2.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

####2. This figure depict the total number of injuries cause by different type of events and shows which events are the culprit among all.


```r
plot.injuries <- ggplot(top.injuries,aes(evtype,total.injuries,fill=evtype))+geom_bar(stat = "identity")+scale_fill_discrete("Event Type")+theme(legend.title = element_text(colour="blue", size=16, face="bold"))+ggtitle("Most harmful (Top-12) event affected on human health - INJURIES")+xlab("Event Type")+ylab("Total Injuries")+ggsave("injuries.png",width = 20,height = 10)
```

<img src="figure/unnamed-chunk-12-1.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />

```r
plot.injuries
```

<img src="figure/unnamed-chunk-12-2.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />


####3. This figure depict the total property loss(Propdmg+cropdmg) cause by different type of events and shows which events are the culprit among all.


```r
plot.property <- ggplot(top.property,aes(evtype,total.dmg,fill=evtype))+geom_bar(stat = "identity")+scale_fill_discrete("Event Type")+theme(legend.title = element_text(colour="blue", size=16, face="bold"))+ggtitle("Most harmful (Top-12) event which have greatest economic consequences - (Propdmg + Cropdmg)")+xlab("Event Type")+ylab("Total Economic loss in millions-(propdmg+cropdmg)/1000000")+ggsave("Propertyloss.png",width = 20,height = 10)
```

<img src="figure/unnamed-chunk-13-1.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />

```r
plot.property
```

<img src="figure/unnamed-chunk-13-2.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />

####**Note** :- Incase figure is not clearly visible, go to my repository ("https://github.com/Jineshpanchal/RepData_PeerAssessment2") and have a look at figures.       

####We are done with Preprocessing, computation,display and picturization.

####Hope you find it useful and valid.
