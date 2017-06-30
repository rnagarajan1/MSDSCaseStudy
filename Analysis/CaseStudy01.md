# CaseStudy01
Rangaswamy Nagarajan  
June 30, 2017  




```r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
getwd()
```

```
## [1] "C:/Users/swara/OneDrive/Documents/MSDS/Data Science/CaseStudy/MSDSCaseStudy01/Analysis"
```

```r
setwd("C:/Users/swara/OneDrive/Documents/MSDS/Data Science/CaseStudy/MSDSCaseStudy01/Analysis")
```

## Beer

Beer is the world's oldest and most widely consumed alcoholic drink. It is the third most popular drink overall, after water and tea.
This case study is about the analysis of data collected about Beers and Breweries in the United States of America. The data set contains a list of 2,410 US beers and 557 US breweries. 



```r
beers <- read.csv("C:/Users/swara/OneDrive/Documents/MSDS/Data Science/CaseStudy/MSDSCaseStudy01/Data/Beers.csv",header=TRUE)
breweries <- read.csv("C:/Users/swara/OneDrive/Documents/MSDS/Data Science/CaseStudy/MSDSCaseStudy01/Data/Breweries.csv", header=TRUE)

names(beers)[1] <- paste("Beer_Name")
names(breweries)[2] <- paste("Brewery_Name")
```


## Number of breweries in each State.


```r
NoOfBreweriesPerState <- data.frame(table(breweries$State))
names(NoOfBreweriesPerState)[1]<-paste("State")
names(NoOfBreweriesPerState)[2]<-paste("Count")
NoOfBreweriesPerState
```

```
##    State Count
## 1     AK     7
## 2     AL     3
## 3     AR     2
## 4     AZ    11
## 5     CA    39
## 6     CO    47
## 7     CT     8
## 8     DC     1
## 9     DE     2
## 10    FL    15
## 11    GA     7
## 12    HI     4
## 13    IA     5
## 14    ID     5
## 15    IL    18
## 16    IN    22
## 17    KS     3
## 18    KY     4
## 19    LA     5
## 20    MA    23
## 21    MD     7
## 22    ME     9
## 23    MI    32
## 24    MN    12
## 25    MO     9
## 26    MS     2
## 27    MT     9
## 28    NC    19
## 29    ND     1
## 30    NE     5
## 31    NH     3
## 32    NJ     3
## 33    NM     4
## 34    NV     2
## 35    NY    16
## 36    OH    15
## 37    OK     6
## 38    OR    29
## 39    PA    25
## 40    RI     5
## 41    SC     4
## 42    SD     1
## 43    TN     3
## 44    TX    28
## 45    UT     4
## 46    VA    16
## 47    VT    10
## 48    WA    23
## 49    WI    20
## 50    WV     1
## 51    WY     4
```

## Merged Beers and Breweries data.

```r
beerBrewery<-merge(breweries, beers, by.x="Brew_ID", by.y="Brewery_id", all=TRUE)
head(beerBrewery)
```

```
##   Brew_ID       Brewery_Name        City State     Beer_Name Beer_ID   ABV
## 1       1 NorthGate Brewing  Minneapolis    MN       Pumpion    2689 0.060
## 2       1 NorthGate Brewing  Minneapolis    MN    Stronghold    2688 0.060
## 3       1 NorthGate Brewing  Minneapolis    MN   Parapet ESB    2687 0.056
## 4       1 NorthGate Brewing  Minneapolis    MN  Get Together    2692 0.045
## 5       1 NorthGate Brewing  Minneapolis    MN Maggie's Leap    2691 0.049
## 6       1 NorthGate Brewing  Minneapolis    MN    Wall's End    2690 0.048
##   IBU                               Style Ounces
## 1  38                         Pumpkin Ale     16
## 2  25                     American Porter     16
## 3  47 Extra Special / Strong Bitter (ESB)     16
## 4  50                        American IPA     16
## 5  26                  Milk / Sweet Stout     16
## 6  19                   English Brown Ale     16
```

```r
tail(beerBrewery)
```

```
##      Brew_ID                  Brewery_Name          City State
## 2405     556         Ukiah Brewing Company         Ukiah    CA
## 2406     557       Butternuts Beer and Ale Garrattsville    NY
## 2407     557       Butternuts Beer and Ale Garrattsville    NY
## 2408     557       Butternuts Beer and Ale Garrattsville    NY
## 2409     557       Butternuts Beer and Ale Garrattsville    NY
## 2410     558 Sleeping Lady Brewing Company     Anchorage    AK
##                      Beer_Name Beer_ID   ABV IBU                   Style
## 2405             Pilsner Ukiah      98 0.055  NA         German Pilsener
## 2406         Porkslap Pale Ale      49 0.043  NA American Pale Ale (APA)
## 2407           Snapperhead IPA      51 0.068  NA            American IPA
## 2408         Moo Thunder Stout      50 0.049  NA      Milk / Sweet Stout
## 2409  Heinnieweisse Weissebier      52 0.049  NA              Hefeweizen
## 2410 Urban Wilderness Pale Ale      30 0.049  NA        English Pale Ale
##      Ounces
## 2405     12
## 2406     12
## 2407     12
## 2408     12
## 2409     12
## 2410     12
```

## Number of NA's in each column.
<!--The below code is used to identify "" and replcae it with NA. 
There were 5 observations with style as "". the below code is used to replace "" with NA --> 

```r
beerBrewery$Style<-ifelse(beerBrewery[,9]=="", NA, beerBrewery[,9])
```
<!--The below code is used to count the number of NA is each column. -->

```r
na_count <-sapply(beerBrewery, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count
```

```
##              na_count
## Brew_ID             0
## Brewery_Name        0
## City                0
## State               0
## Beer_Name           0
## Beer_ID             0
## ABV                62
## IBU              1005
## Style               5
## Ounces              0
```


## Median alcohol content for each state.

```r
MedABVByState <- aggregate(ABV~State, FUN = median, beerBrewery, na.rm = TRUE)
```

## Bar Chart for Median alcohol content by each state.


```r
ggplot(MedABVByState, aes(x=State, y=ABV, fill=ABV))+
  geom_bar(stat="identity")+
  ylab("Median Alcohol Cotent")+
  ggtitle("Medain Alcohol Content by State")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  theme(axis.text = element_text(size = 8, face="bold"))+
  theme(axis.title = element_text(size = 10, face="bold"))+
  scale_fill_gradient(low="#FF9999",high="#000099")
```

![](CaseStudy01_files/figure-html/MedABVPlot-1.png)<!-- -->


## Median international bitterness unit for each state.

```r
MedIBUByState <- aggregate(IBU~State, FUN = median, beerBrewery, na.rm = TRUE)
```

## Bar Chart for Median international bitterness unit by each state.

```r
ggplot(MedIBUByState, aes(x=State, y=IBU, fill=IBU))+
  geom_bar(stat="identity")+
  ylab("Median International Bitterness Unit")+
  ggtitle("Medain International Bitterness Unit by State")+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"))+
  theme(axis.text = element_text(size = 8, face="bold"))+
  theme(axis.title = element_text(size = 10, face="bold"))+
  scale_fill_gradient(low="#FF9999",high="#000099")
```

![](CaseStudy01_files/figure-html/MedIBUPlot-1.png)<!-- -->

## State that has the most alcoholic beer.

```r
maxAlcoholRow<-head(beerBrewery[order(-beerBrewery$ABV),],1)
maxAlcoholState<-as.character(maxAlcoholRow$State)
maxAlcoholState
```

```
## [1] " CO"
```


## State that has the most bitter beer.

```r
maxBitterRow<-head(beerBrewery[order(-beerBrewery$IBU),],1)
maxBitterState<-as.character(maxBitterRow$State)
maxBitterState
```

```
## [1] " OR"
```

## Summary statistics for ABV (Alcohol by volume) variable.

```r
summary(beerBrewery$ABV)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.00100 0.05000 0.05600 0.05977 0.06700 0.12800      62
```

##  Scatter plot to identify relationship between the bitterness of the beer and its alcoholic content

```r
ggplot(data=na.omit(beerBrewery),aes(x=IBU,y=ABV))+geom_point(size=1.5,alpha=.8)
```

![](CaseStudy01_files/figure-html/Scatter-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
