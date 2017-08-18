STA-380-Assignment-2
================
Jess Chung, Dani Diehl, Gihani Dissanayake, Chloe Kwon
August 16, 2017

Question 1
==========

### Flights at ABIA

Our goal here is to establish an understanding of the cause behind late departures from Austin. Is the late departure caused by the plane's late arrival? Or did the airport itself extend the delay?

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.1

``` r
library(ggmap)
```

    ## Warning: package 'ggmap' was built under R version 3.4.1

``` r
library(maps)
```

    ## Warning: package 'maps' was built under R version 3.4.1

``` r
library(mapdata)
```

    ## Warning: package 'mapdata' was built under R version 3.4.1

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 3.4.1

``` r
abia <- read.csv("https://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv")
abia$Turnaround <- abia$ArrDelay - abia$DepDelay
abia[is.na(abia)] <- 0
abiaorigin <- filter(abia, Origin =="AUS")
```

    ## Warning: package 'bindrcpp' was built under R version 3.4.1

``` r
abiaorigin <- abiaorigin[(abiaorigin$Dest != "DSM")&(abiaorigin$Dest != "DTW"),]

#abiadest <- filter(abia, Dest =="AUS")
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = FALSE)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")


airportlatlon <- subset(airports, select = c(IATA_FAA, lat, lon))

abiaorigin = merge(abiaorigin, airportlatlon, by.x = "Dest", by.y = "IATA_FAA")
#abiadest = merge(abiadest, airportlatlon, by.x = "Origin", by.y = "IATA_FAA")
origindata = abiaorigin[,c("lat","lon","Turnaround","Month")]
average = aggregate(origindata[, 3], list(origindata$lat, origindata$lon), mean)
origindataturnaround = merge(origindata, average, by.x = c("lat","lon"), by.y = c("Group.1","Group.2"))

abialatlon <- filter(airportlatlon, IATA_FAA=="AUS") #separate df for abia

usa <- map_data("usa")
origin <- gather(data = origindataturnaround, -lon, -lat, -x, -Turnaround, key = "var", value = "value")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + coord_fixed(1.3) + 
geom_curve(data=origin, aes(x = lon, y = lat, xend = abialatlon$lon, yend = abialatlon$lat, col = x), size = .01, curvature = .2) + 
 geom_point(data=origin, aes(x = lon, y = lat), col = "lightgreen", shape = "x") + scale_colour_gradient2()# + facet_wrap(~value, scales = "free")
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

By adding turn around time (the difference between the departure delay and the arrival delay) as a variable, we were able to view average delays with regards to different flights and then use that additional information to determine how much the Austin airport contributes to the delay of a flight. We wanted to excuse airports from flights that arrive late and then depart late, but the airport did not extend the delay further.

The graph shows the culmination of our analysis, which is the number of minutes the airport contributes to the delay. The redder routes have a slower turnaround time at the Austin airport, and blue routes have a faster turnaround time. For example, AUS was able to speed up the average turnaround tme for the flights to SEA, MSP, and CLT, while the flights to PHL on average was slowed down at AUS, and thus had a slower turnaround time.

We removed outliers such as th single flight from Des Moines, and the resulting range is fairly small: from the Austin airport extending the delay by 20 min to reducing the delay by 8 minutes. This small range is not surprising, as the internal processes for processing a plane through an airport is fairly consistent. Maybe we would see a broader range or some outliers in the data if international flights were taken into account, since those are a little more complicated and could thus extend delays.

Though the above is very interesting and useful for holding airlines and airports accountable, it is more relevent to actual fliers what their delay will likely be, not necessarily what the cause of that delay is.

``` r
#create a figure or series of figures that tell interesting story about data
#What is the best time of day to fly to minimize delays?
#What is the best time of year to fly to minimize delays?
#How do patterns of flights to different destinations or parts of the country change over the course of the year?
#What are the bad airports to fly to?

library(ggplot2)
library(lattice)
library(grid)
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.4.1

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
airline<- read.csv('https://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv',header=TRUE)
names(airline)
```

    ##  [1] "Year"              "Month"             "DayofMonth"       
    ##  [4] "DayOfWeek"         "DepTime"           "CRSDepTime"       
    ##  [7] "ArrTime"           "CRSArrTime"        "UniqueCarrier"    
    ## [10] "FlightNum"         "TailNum"           "ActualElapsedTime"
    ## [13] "CRSElapsedTime"    "AirTime"           "ArrDelay"         
    ## [16] "DepDelay"          "Origin"            "Dest"             
    ## [19] "Distance"          "TaxiIn"            "TaxiOut"          
    ## [22] "Cancelled"         "CancellationCode"  "Diverted"         
    ## [25] "CarrierDelay"      "WeatherDelay"      "NASDelay"         
    ## [28] "SecurityDelay"     "LateAircraftDelay"

``` r
attach(airline)

###departure delays###
dest_dep_delay<-aggregate(DepDelay~Origin, airline, mean)

dest_dep_delay[order(dest_dep_delay$DepDelay),]
```

    ##    Origin  DepDelay
    ## 35    OAK  2.508475
    ## 21    IND  2.660465
    ## 51    TUL  3.157303
    ## 28    MAF  3.236674
    ## 26    LBB  4.118841
    ## 22    JAX  4.136564
    ## 49    STL  4.382022
    ## 50    TPA  4.531335
    ## 25    LAX  4.852326
    ## 1     ABQ  4.943794
    ## 34    MSY  5.092971
    ## 6     BOS  5.193989
    ## 3     AUS  7.424662
    ## 18    HRL  7.454039
    ## 8     CLE  7.485333
    ## 30    MCO  7.541139
    ## 14    ELP  7.790022
    ## 46    SJC  7.962726
    ## 29    MCI  8.109620
    ## 40    PHX  8.356603
    ## 10    CVG  8.596923
    ## 47    SLC  8.670310
    ## 33    MSP  8.703704
    ## 32    MEM  8.846814
    ## 42    SAN  8.862937
    ## 20    IAH  8.999726
    ## 12    DEN  9.323996
    ## 52    TUS  9.397380
    ## 11    DAL  9.440563
    ## 45    SFO  9.614876
    ## 16    FLL  9.745833
    ## 24    LAS 10.084622
    ## 13    DFW 11.677383
    ## 17    HOU 11.902246
    ## 7     BWI 12.079670
    ## 48    SNA 12.361789
    ## 37    ONT 13.003289
    ## 44    SEA 13.020690
    ## 2     ATL 14.507879
    ## 9     CLT 14.852584
    ## 23    JFK 16.519274
    ## 31    MDW 17.404795
    ## 15    EWR 17.531183
    ## 38    ORD 18.179794
    ## 27    LGB 19.646091
    ## 5     BNA 20.155975
    ## 36    OKC 20.620690
    ## 39    PHL 20.672414
    ## 43    SAT 25.500000
    ## 41    RDU 27.934498
    ## 19    IAD 28.395498
    ## 4     BHM 38.000000
    ## 53    TYS 68.333333

``` r
top20worst_depdelays<-tail(dest_dep_delay[order(dest_dep_delay$DepDelay),],20)
top20worst_depdelays$Origin <- factor(top20worst_depdelays$Origin, levels = top20worst_depdelays$Origin[order(top20worst_depdelays$DepDelay)])

ggplot(top20worst_depdelays, aes(Origin, DepDelay)) + geom_bar(stat='identity')
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Based on the information above, if the plane's origin is Birmingham or Knoxville, you should expect greater delays. Both of these are also partially military airports, which counld account for the additional depay. However, this table doesn't take seasonal changes into account, thus we consider a month to month breakdown below.

``` r
####loop for all the months###  
p<-list()
for (i in 1:12) {
  dest_dep_delay<-aggregate(DepDelay~Origin, subset(airline, airline['Month']==i), mean)
  #sort(dest_dep_delay$DepDelay, decreasing=TRUE)
  dest_dep_delay[order(dest_dep_delay$DepDelay),]
  
  top20worst_depdelays<-tail(dest_dep_delay[order(dest_dep_delay$DepDelay),],10)
  top20worst_depdelays$Origin <- factor(top20worst_depdelays$Origin, levels = top20worst_depdelays$Origin[order(top20worst_depdelays$DepDelay)])
  
  par(mfrow=c(3,4))
  p[[i]]<-ggplot(top20worst_depdelays, aes(Origin, DepDelay)) + geom_bar(stat='identity')+ggtitle(i)+scale_fill_brewer(palette='Blues')
}
do.call(grid.arrange,p)
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

Each of the above charts each correspond to a single month. - TYS had the greatest departure delay overall, but it seems like the majority of their extreme delays are in January or March. This seasonality might relate to military operations. - IAD is the worst origin in terms of departure delay in October, May, and September, but it also appears in the high frequency

There are many conclusiosn we can draw from this, and two high level conclusions stand out. First, airports that have a military component often experience higher delays.

Question 2
==========

Author Attribution
------------------

First, we use Naive Bayes to predict the author of the test data.

``` r
library(tm) 
```

    ## Warning: package 'tm' was built under R version 3.4.1

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(magrittr)
```

    ## Warning: package 'magrittr' was built under R version 3.4.1

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## The following object is masked from 'package:ggmap':
    ## 
    ##     inset

``` r
library(class)
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.4.1

``` r
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.4.1

``` r
readerPlain = function(fname){
                readPlain(elem=list(content=readLines(fname)), 
                            id=fname, language='en') }

train = "C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50train"
test = "C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50test"
file_list = Sys.glob(paste0(train,'/*/*.txt'))
file_list_test = Sys.glob(paste0(test,'/*/*.txt'))
authornames = list.dirs("C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50train", full.names = FALSE)[-1]
#authornames_test = list.dirs("C:/users/jessc/documents/STA-380-Part-2--Exercises-2/data/ReutersC50/C50test", full.names = FALSE)[-1]
classificationnames = rep(authornames, each=50)
authors = lapply(file_list, readerPlain) 
authors_test = lapply(file_list_test,readerPlain)

mynames = file_list %>%
    { strsplit(., '/', fixed=TRUE) } %>%
    { lapply(., tail, n=2) } %>%
    { lapply(., paste0, collapse = '') } %>%
    unlist
    
names(authors) = mynames
my_documents = Corpus(VectorSource(authors))

mynamestest = file_list_test %>%
    { strsplit(., '/', fixed=TRUE) } %>%
    { lapply(., tail, n=2) } %>%
    { lapply(., paste0, collapse = '') } %>%
    unlist
    
names(authors_test) = mynamestest
my_documents_test = Corpus(VectorSource(authors_test))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus

my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
my_documents <- tm_map(my_documents, removeWords, c("character"))

my_documents_test = tm_map(my_documents_test, content_transformer(tolower)) # make everything lowercase
my_documents_test = tm_map(my_documents_test, content_transformer(removeNumbers)) # remove numbers
my_documents_test = tm_map(my_documents_test, content_transformer(removePunctuation)) # remove punctuation
my_documents_test = tm_map(my_documents_test, content_transformer(stripWhitespace)) ## remove excess white-space
my_documents_test = tm_map(my_documents_test, content_transformer(removeWords), stopwords("en"))
my_documents_test <- tm_map(my_documents_test, removeWords, c("character"))

## create a doc-term-matrix
DTM_authors = DocumentTermMatrix(my_documents)
DTM_authors = removeSparseTerms(DTM_authors, 0.95)

## create a doc-term-matrix for test
DTM_authors_test = DocumentTermMatrix(my_documents_test)
DTM_authors_test = removeSparseTerms(DTM_authors_test, 0.95)

#Normalize word frequency
X = as.matrix(DTM_authors)
X = X/rowSums(X)  # term-frequency weighting

X_test = as.matrix(DTM_authors_test)
X_test = X_test/rowSums(X_test)


# Transform dtm to matrix to data frame - df is easier to work with and create target column
mat.df <- as.data.frame(X, stringsAsfactors = FALSE)
mat.df$categorynb <- as.factor(classificationnames)

# Create training 90% and test 10%
set.seed(1)
test = sample(1:nrow(mat.df),nrow(mat.df)/10)
train =(-test)

# Transform dtm to matrix to data frame - df is easier to work with and create target column
mat.df.test <- as.data.frame(X_test, stringsAsfactors = FALSE)
mat.df.test$categorynb <- as.factor(classificationnames)

#Create model, use laplace = 1 to smooth
model <- naiveBayes(categorynb ~ ., data = mat.df[train,], laplace = 1)
#run model against withheld validation data
preds <- predict(model, newdata = mat.df[test,])
conf.mat <- confusionMatrix(preds, mat.df[test,]$categorynb)
conf.mat$overall['Accuracy']
```

    ## Accuracy 
    ##    0.716

``` r
#run model against test data
preds_test <- predict(model, newdata = mat.df.test)
conf.mat.test <- confusionMatrix(preds_test, mat.df.test$categorynb)
conf.mat.test$overall['Accuracy']
```

    ## Accuracy 
    ##   0.4996

Using Naive Bayes, we get a in-sample (train) accuracy of 72%, but our out-of-sample (test) accuracy is about 50%. This is fairly accurate, considering authors can write about any number of things, and every article adds dimensionality. We prefer this model, as it is significantly more accurate than randomly guessing or KNN, which we found works with the train data, but not so much the test data.

Second, we tried PCA to predict the author names.

``` r
library(ggplot2)
set.seed(1)

# Now PCA on term frequencies
X = as.matrix(DTM_authors)
X = X/rowSums(X)  # term-frequency weighting
X=cbind(X,classificationnames)

##Make test data
X_test <- as.data.frame(X_test, stringsAsfactors = FALSE)
X_test=cbind(X_test,classificationnames)
Z_test=X_test[,1:800] #taking out author
Z_test<-mapply(Z_test,FUN=as.numeric)
Z_test<-matrix(data=Z_test, ncol=800, nrow=2500)
Z_test <- as.data.frame(Z_test, stringsAsfactors = FALSE)

Z=X[,1:800] #taking out author
Z<-mapply(Z,FUN=as.numeric)
Z<-matrix(data=Z, ncol=800, nrow=2500)

pc = prcomp(Z, scale.=TRUE)
names(pc)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
#summary(pc) #801 pc's
plot(pc)
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
biplot(pc)
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-2.png)

``` r
loadings = pc$rotation
scores = pc$x
qplot(scores[,1], scores[,2], color=X[,801], xlab='Component 1', ylab='Component 2')
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-3.png) As you can see, PCA is an excellent tool for visualizing the complex nature of having so many authors and so many text options. It is not surprising that the first two, or even first five, principle components are not enough to clearly distinguish between authors.

``` r
library(rpart)
```

    ## Warning: package 'rpart' was built under R version 3.4.1

``` r
#rpart.model <- rpart(Z_test$Author ~ .,data = Z_test, method = "anova")
#rpart.model

#transform test into PCA
#test.data <- predict(pc, newdata = Z_test)
#test.data <- as.data.frame(test.data)

#select the first 30 components
#test.data <- test.data[,1:30]

#make prediction on test data
#rpart.prediction <- predict(rpart.model, test.data)
```

We tried to make the prediction work in many different ways. We could not. However, we could explain many many things that did not work.

Question 3
==========

### Practice With Association Rule Mining

Question: Pick your own thresholds for lift and confidence; just be clear what these thresholds are and how you picked them. Do your discovered item sets make sense? Present your discoveries in an interesting and concise way.

Answer:

``` r
library(arules)
```

    ## Warning: package 'arules' was built under R version 3.4.1

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:tm':
    ## 
    ##     inspect

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Warning: package 'arulesViz' was built under R version 3.4.1

``` r
grocery <- read.transactions('https://raw.githubusercontent.com/jgscott/STA380/master/data/groceries.txt', sep=',')
summary(grocery)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

``` r
inspect(grocery[1:5])
```

    ##     items                     
    ## [1] {citrus fruit,            
    ##      margarine,               
    ##      ready soups,             
    ##      semi-finished bread}     
    ## [2] {coffee,                  
    ##      tropical fruit,          
    ##      yogurt}                  
    ## [3] {whole milk}              
    ## [4] {cream cheese,            
    ##      meat spreads,            
    ##      pip fruit,               
    ##      yogurt}                  
    ## [5] {condensed milk,          
    ##      long life bakery product,
    ##      other vegetables,        
    ##      whole milk}

``` r
itemFrequencyPlot(grocery, topN = 20) 
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
rules <- apriori(grocery, parameter=list(support=0.01, confidence=0.5, maxlen=6))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5    0.01      1
    ##  maxlen target   ext
    ##       6  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 98 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [88 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [15 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
inspect(rules[1:5])
```

    ##     lhs                     rhs             support confidence     lift
    ## [1] {curd,                                                             
    ##      yogurt}             => {whole milk} 0.01006609  0.5823529 2.279125
    ## [2] {butter,                                                           
    ##      other vegetables}   => {whole milk} 0.01148958  0.5736041 2.244885
    ## [3] {domestic eggs,                                                    
    ##      other vegetables}   => {whole milk} 0.01230300  0.5525114 2.162336
    ## [4] {whipped/sour cream,                                               
    ##      yogurt}             => {whole milk} 0.01087951  0.5245098 2.052747
    ## [5] {other vegetables,                                                 
    ##      whipped/sour cream} => {whole milk} 0.01464159  0.5070423 1.984385

``` r
inspect(subset(rules, subset=confidence > 0.8))
summary(rules)
```

    ## set of 15 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##  3 
    ## 15 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       3       3       3       3       3       3 
    ## 
    ## summary of quality measures:
    ##     support          confidence          lift      
    ##  Min.   :0.01007   Min.   :0.5000   Min.   :1.984  
    ##  1st Qu.:0.01174   1st Qu.:0.5151   1st Qu.:2.036  
    ##  Median :0.01230   Median :0.5245   Median :2.203  
    ##  Mean   :0.01316   Mean   :0.5411   Mean   :2.299  
    ##  3rd Qu.:0.01403   3rd Qu.:0.5718   3rd Qu.:2.432  
    ##  Max.   :0.02227   Max.   :0.5862   Max.   :3.030  
    ## 
    ## mining info:
    ##     data ntransactions support confidence
    ##  grocery          9835    0.01        0.5

After reading in the grocery dataset, we first look at the item frequency plot to look at frequent items. We randomly chosen support of 0.01 and confidence of 0.5 values to start off, but we will try different values pairs to see which balance of values will result in the number of rules that will be most beneficial for our analysis.

The items with the highest relative frequency of occurance are not surprising, as they are all items that are commonly purchased. Additionally, adise from drinks and shopping bags, all of the items with the highest frequencies are pershable, and thus need to be replaced more often.

Support = Number of transactions with both A and B / Total number of transactions=P(Aâ©B)

Confidence = Number of transactions with both A and B / Total number of transactions with A=P(Aâ©B) / P(A)

Expected Confidence = Number of transactions with B / Total number of transactions=P(B)

Lift=Confidence / Expected Confidence = P(Aâ©B) / P(A)\*P(B)

### Interactive Inspect With Datatable

Here we can experiment with different Support and Confidence values. By maintaining the Data Table form, we can see different sets for market basket analysis. When we set rules3 with support of 0.01 and confidence of 0.5, we can see 15 entries that show LHS of curd, yogurt, butter, eggs, whipped/sour cream, before buying whole milk (RHS), which makes sense as they all need to be refrigerated and therefore probably located closer together. Rules4 with support of 0.001 and confidence of 0.8 reveal much bigger datatable with 410 entries that reveal liquor, red/blush wine purchased with bottled beer which are all alcohol and make sense to buy them together and also reveals cereal as part of the pairs before buying whole milk.

As we perform more visualizations to find patterns within these baskets as well as setting different rules, we will re-adjust our confidence and support values to maximize the effectiveness of our analysis.

``` r
rules3 <- apriori(grocery, parameter=list(support=0.01, confidence=0.5))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5    0.01      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 98 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.02s].
    ## sorting and recoding items ... [88 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [15 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
rules4 <- apriori(grocery, parameter=list(support=0.001, confidence=0.8))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.8    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.01s].
    ## writing ... [410 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
#inspectDT(rules3) #if we want highest support 15 entries 
#inspectDT(rules4) #if we want wider association with 410 entries
```

### Visualizations

``` r
rules_sorted <- sort(rules, by='confidence', decreasing=TRUE)
#matrix representation
#plot(rules[1:20], method = 'matrix', control = list(reorder=TRUE))

#Interactive Scatterplot 
plotly_arules(rules)
```

<!--html_preserve-->

<script type="application/json" data-for="366032b7a76">{"x":{"visdat":{"36606c594fb5":["function () ","plotlyVisDat"]},"cur_data":"36606c594fb5","attrs":{"36606c594fb5":{"x":[0.0146415861718353,0.0222674123029995,0.0151499745805796,0.0135231316725979,0.012709710218607,0.0108795119471276,0.0123029994916116,0.0145399084900864,0.011997966446365,0.0114895780376207,0.0100660904931368,0.0129130655821047,0.0122013218098627,0.0123029994916116,0.0103711235383833],"y":[0.507042253521127,0.51288056206089,0.517361111111111,0.517509727626459,0.523012552301255,0.524509803921569,0.552511415525114,0.562992125984252,0.570048309178744,0.573604060913706,0.582352941176471,0.5,0.502092050209205,0.584541062801932,0.586206896551724],"hoverinfo":"text","text":["[5]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;whipped/sour cream}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0146 <BR>confidence: 0.507 <BR>lift: 1.98","[15]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0223 <BR>confidence: 0.513 <BR>lift: 2.01","[10]<BR> <B>{tropical fruit,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0151 <BR>confidence: 0.517 <BR>lift: 2.02","[6]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;pip fruit}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0135 <BR>confidence: 0.518 <BR>lift: 2.03","[14]<BR> <B>{rolls/buns,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0127 <BR>confidence: 0.523 <BR>lift: 2.05","[4]<BR> <B>{whipped/sour cream,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0109 <BR>confidence: 0.525 <BR>lift: 2.05","[3]<BR> <B>{domestic eggs,<BR>&nbsp;&nbsp;other vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0123 <BR>confidence: 0.553 <BR>lift: 2.16","[12]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0145 <BR>confidence: 0.563 <BR>lift: 2.2","[9]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;tropical fruit}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.012 <BR>confidence: 0.57 <BR>lift: 2.23","[2]<BR> <B>{butter,<BR>&nbsp;&nbsp;other vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0115 <BR>confidence: 0.574 <BR>lift: 2.24","[1]<BR> <B>{curd,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0101 <BR>confidence: 0.582 <BR>lift: 2.28","[11]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0129 <BR>confidence: 0.5 <BR>lift: 2.58","[13]<BR> <B>{rolls/buns,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0122 <BR>confidence: 0.502 <BR>lift: 2.59","[8]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;tropical fruit}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0123 <BR>confidence: 0.585 <BR>lift: 3.02","[7]<BR> <B>{citrus fruit,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0104 <BR>confidence: 0.586 <BR>lift: 3.03"],"mode":"markers","marker":{"colorbar":{"title":"lift"}},"color":[1.98438542116207,2.00723451168677,2.02476980810894,2.02535144098935,2.04688756541299,2.05274728275711,2.16233576270971,2.20335358498015,2.23096900945999,2.24488497377091,2.27912502048173,2.58407777193904,2.59488981282582,3.0209991343442,3.02960842227336],"colors":["#EEEEEEFF","#EE0000FF"],"alpha":1,"sizes":[10,100],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"hovermode":"closest","xaxis":{"domain":[0,1],"title":"support"},"yaxis":{"domain":[0,1],"title":"confidence"},"showlegend":false,"legend":{"y":0.5,"yanchor":"top"}},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"x":[0.0146415861718353,0.0222674123029995,0.0151499745805796,0.0135231316725979,0.012709710218607,0.0108795119471276,0.0123029994916116,0.0145399084900864,0.011997966446365,0.0114895780376207,0.0100660904931368,0.0129130655821047,0.0122013218098627,0.0123029994916116,0.0103711235383833],"y":[0.507042253521127,0.51288056206089,0.517361111111111,0.517509727626459,0.523012552301255,0.524509803921569,0.552511415525114,0.562992125984252,0.570048309178744,0.573604060913706,0.582352941176471,0.5,0.502092050209205,0.584541062801932,0.586206896551724],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["[5]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;whipped/sour cream}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0146 <BR>confidence: 0.507 <BR>lift: 1.98","[15]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0223 <BR>confidence: 0.513 <BR>lift: 2.01","[10]<BR> <B>{tropical fruit,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0151 <BR>confidence: 0.517 <BR>lift: 2.02","[6]<BR> <B>{other vegetables,<BR>&nbsp;&nbsp;pip fruit}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0135 <BR>confidence: 0.518 <BR>lift: 2.03","[14]<BR> <B>{rolls/buns,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0127 <BR>confidence: 0.523 <BR>lift: 2.05","[4]<BR> <B>{whipped/sour cream,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0109 <BR>confidence: 0.525 <BR>lift: 2.05","[3]<BR> <B>{domestic eggs,<BR>&nbsp;&nbsp;other vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0123 <BR>confidence: 0.553 <BR>lift: 2.16","[12]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0145 <BR>confidence: 0.563 <BR>lift: 2.2","[9]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;tropical fruit}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.012 <BR>confidence: 0.57 <BR>lift: 2.23","[2]<BR> <B>{butter,<BR>&nbsp;&nbsp;other vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0115 <BR>confidence: 0.574 <BR>lift: 2.24","[1]<BR> <B>{curd,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{whole milk}<\/B> <BR><BR>support: 0.0101 <BR>confidence: 0.582 <BR>lift: 2.28","[11]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;yogurt}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0129 <BR>confidence: 0.5 <BR>lift: 2.58","[13]<BR> <B>{rolls/buns,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0122 <BR>confidence: 0.502 <BR>lift: 2.59","[8]<BR> <B>{root vegetables,<BR>&nbsp;&nbsp;tropical fruit}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0123 <BR>confidence: 0.585 <BR>lift: 3.02","[7]<BR> <B>{citrus fruit,<BR>&nbsp;&nbsp;root vegetables}<\/B><BR>&nbsp;&nbsp; => <B>{other vegetables}<\/B> <BR><BR>support: 0.0104 <BR>confidence: 0.586 <BR>lift: 3.03"],"mode":"markers","marker":{"colorbar":{"title":"lift"},"cmin":1.98438542116207,"cmax":3.02960842227336,"colorscale":[["0","rgba(238,238,238,1)"],["0.0127519544874511","rgba(239,236,235,1)"],["0.0246565947180603","rgba(240,234,232,1)"],["0.0344429493065644","rgba(241,232,230,1)"],["0.0388225905194702","rgba(241,231,228,1)"],["0.0391471967006774","rgba(241,231,228,1)"],["0.0494957363013387","rgba(242,229,226,1)"],["0.0602650859793804","rgba(243,227,223,1)"],["0.0635353627022466","rgba(243,227,222,1)"],["0.0916158384204912","rgba(245,222,216,1)"],["0.152776579465907","rgba(248,211,201,1)"],["0.186602381137442","rgba(250,205,193,1)"],["0.209494207059423","rgba(251,201,187,1)"],["0.22490622688305","rgba(251,198,184,1)"],["0.238133790988559","rgba(252,195,181,1)"],["0.245900215798774","rgba(252,194,179,1)"],["0.260148218345124","rgba(252,191,175,1)"],["0.279257404193576","rgba(253,188,171,1)"],["0.427866564907998","rgba(255,161,137,1)"],["0.574607862193664","rgba(254,133,104,1)"],["0.580642004040823","rgba(254,132,102,1)"],["0.686008364991","rgba(252,110,79,1)"],["0.9238176847452","rgba(242,49,25,1)"],["0.995195202727067","rgba(238,6,2,1)"],["1","rgba(238,0,0,1)"]],"showscale":false,"color":[1.98438542116207,2.00723451168677,2.02476980810894,2.02535144098935,2.04688756541299,2.05274728275711,2.16233576270971,2.20335358498015,2.23096900945999,2.24488497377091,2.27912502048173,2.58407777193904,2.59488981282582,3.0209991343442,3.02960842227336],"line":{"color":"transparent"}},"type":"scatter","xaxis":"x","yaxis":"y","frame":null},{"x":[0.0100660904931368,0.0222674123029995],"y":[0.5,0.586206896551724],"type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"colorbar":{"title":"lift","len":0.5,"y":1,"lenmode":"fraction","yanchor":"top"},"cmin":1.98438542116207,"cmax":3.02960842227336,"colorscale":[["0","rgba(238,238,238,1)"],["0.0127519544874511","rgba(239,236,235,1)"],["0.0246565947180603","rgba(240,234,232,1)"],["0.0344429493065644","rgba(241,232,230,1)"],["0.0388225905194702","rgba(241,231,228,1)"],["0.0391471967006774","rgba(241,231,228,1)"],["0.0494957363013387","rgba(242,229,226,1)"],["0.0602650859793804","rgba(243,227,223,1)"],["0.0635353627022466","rgba(243,227,222,1)"],["0.0916158384204912","rgba(245,222,216,1)"],["0.152776579465907","rgba(248,211,201,1)"],["0.186602381137442","rgba(250,205,193,1)"],["0.209494207059423","rgba(251,201,187,1)"],["0.22490622688305","rgba(251,198,184,1)"],["0.238133790988559","rgba(252,195,181,1)"],["0.245900215798774","rgba(252,194,179,1)"],["0.260148218345124","rgba(252,191,175,1)"],["0.279257404193576","rgba(253,188,171,1)"],["0.427866564907998","rgba(255,161,137,1)"],["0.574607862193664","rgba(254,133,104,1)"],["0.580642004040823","rgba(254,132,102,1)"],["0.686008364991","rgba(252,110,79,1)"],["0.9238176847452","rgba(242,49,25,1)"],["0.995195202727067","rgba(238,6,2,1)"],["1","rgba(238,0,0,1)"]],"showscale":true,"color":[1.98438542116207,3.02960842227336]},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script>
<!--/html_preserve-->
``` r
#plot(rules, method = 'graph', interactive=TRUE, shading=NA)
subrules <- head(sort(rules, by='lift'),10) #Graph just 10 rules by 10 highest lifts 
plot(subrules, method='graph')
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-2.png)

``` r
plot(rules, method='grouped') #Grouped Matrix shows LHS and RHS 
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-3.png)

``` r
plot(subrules,method='paracoord', control=list(reorder=TRUE)) #Parallel Coordinates plot for 10 rules 
```

![](STA_380_Assignment_2_Final_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-4.png)

The lift for {tropical fruit, root vegetables}--&gt;{other vegetables} is very high. This means there is a strong association between these things. If we know a person bought tropical fruit and root vegetables, the person is much more likley to buy other vegetables. The support for {other vegetables,yogurt}--&gt;{whole milk} is very high. This means there is a large proportion of all baskets that had all three of these items.

People tend to buy semi finished breads and margarine before ready soups so we should put those items close to the ready soups isle. Also, people tend to buy citrus fruit, soda, canned/bottled beer, and shopping bags before they buy margarine and ready soups.

When they buy whole milk together with ready soups, they tend to buy other vegetables. Also, people tend to buy baking powder, sugar, flour, and eggs before buying margarine, which sounds like they are buying items for baking.

### Getting the product recommendation rules

``` r
rules_conf <- sort(rules, by='confidence', decreasing=TRUE)
inspect(head(rules_conf)) #High-confidence rules
```

    ##     lhs                   rhs                   support confidence     lift
    ## [1] {citrus fruit,                                                         
    ##      root vegetables}  => {other vegetables} 0.01037112  0.5862069 3.029608
    ## [2] {root vegetables,                                                      
    ##      tropical fruit}   => {other vegetables} 0.01230300  0.5845411 3.020999
    ## [3] {curd,                                                                 
    ##      yogurt}           => {whole milk}       0.01006609  0.5823529 2.279125
    ## [4] {butter,                                                               
    ##      other vegetables} => {whole milk}       0.01148958  0.5736041 2.244885
    ## [5] {root vegetables,                                                      
    ##      tropical fruit}   => {whole milk}       0.01199797  0.5700483 2.230969
    ## [6] {root vegetables,                                                      
    ##      yogurt}           => {whole milk}       0.01453991  0.5629921 2.203354

``` r
rules_lift <- sort(rules, by='lift', decreasing=TRUE)
inspect(head(rules_lift)) #High lift rules 
```

    ##     lhs                   rhs                   support confidence     lift
    ## [1] {citrus fruit,                                                         
    ##      root vegetables}  => {other vegetables} 0.01037112  0.5862069 3.029608
    ## [2] {root vegetables,                                                      
    ##      tropical fruit}   => {other vegetables} 0.01230300  0.5845411 3.020999
    ## [3] {rolls/buns,                                                           
    ##      root vegetables}  => {other vegetables} 0.01220132  0.5020921 2.594890
    ## [4] {root vegetables,                                                      
    ##      yogurt}           => {other vegetables} 0.01291307  0.5000000 2.584078
    ## [5] {curd,                                                                 
    ##      yogurt}           => {whole milk}       0.01006609  0.5823529 2.279125
    ## [6] {butter,                                                               
    ##      other vegetables} => {whole milk}       0.01148958  0.5736041 2.244885

The rules with confidence of 1 imply that, whenever the LHS item was purchased, the RHS item was also purchased 100% of the time. So in our grocery rules, if one buys citrus fruit and root vegetables, there's 58.6% chance they will buy other vegetables.

A rule with a lift of 3 implies that, the items in LHS and RHS are 3 times more likely to be purchased together compared to the purchases when they are assumed to be unrelated, which is for the same LHS-RHS pair of {citrus fruit, root vegetables} -&gt; {other vegetables}.

### Targeting Items

-   What are customers likely to buy before or after this item? What are people buying before they buy margarine?

Tend to buy bottled water, eggs, and tropic fruit. Flour and tropical fruit as lhs scored higher on support and slightly less confidence, so we consider this as well when placing items on isles or for target coupon marketing.

``` r
rules <- apriori(data=grocery, parameter=list(supp=0.001, conf=0.08), appearance = list(default = 'lhs', rhs = 'margarine'), control=list(verbose=F))
rules <- sort(rules, decreasing=TRUE, by='confidence')
inspect(rules[1:5])
```

    ##     lhs                   rhs             support confidence     lift
    ## [1] {bottled water,                                                  
    ##      domestic eggs,                                                  
    ##      tropical fruit}   => {margarine} 0.001016777  0.4545455 7.761206
    ## [2] {flour,                                                          
    ##      tropical fruit}   => {margarine} 0.001423488  0.4375000 7.470161
    ## [3] {flour,                                                          
    ##      whole milk,                                                     
    ##      yogurt}           => {margarine} 0.001016777  0.4000000 6.829861
    ## [4] {bottled water,                                                  
    ##      flour}            => {margarine} 0.001016777  0.3703704 6.323945
    ## [5] {flour,                                                          
    ##      other vegetables,                                               
    ##      yogurt}           => {margarine} 0.001016777  0.3703704 6.323945

What are people buying after they buy margarine?

``` r
rules2 <- apriori(data=grocery, parameter=list(supp=0.01, conf=0.1), appearance = list(default = 'rhs', lhs = 'margarine'), control=list(verbose=F))
rules2 <- sort(rules2, by='confidence', decreasing=TRUE)
inspect(rules2)
```

    ##      lhs            rhs                support    confidence lift     
    ## [1]  {margarine} => {whole milk}       0.02419929 0.4131944  1.6170980
    ## [2]  {margarine} => {other vegetables} 0.01972547 0.3368056  1.7406635
    ## [3]  {}          => {whole milk}       0.25551601 0.2555160  1.0000000
    ## [4]  {margarine} => {rolls/buns}       0.01474326 0.2517361  1.3686151
    ## [5]  {margarine} => {yogurt}           0.01423488 0.2430556  1.7423115
    ## [6]  {}          => {other vegetables} 0.19349263 0.1934926  1.0000000
    ## [7]  {margarine} => {root vegetables}  0.01108287 0.1892361  1.7361354
    ## [8]  {}          => {rolls/buns}       0.18393493 0.1839349  1.0000000
    ## [9]  {margarine} => {bottled water}    0.01026945 0.1753472  1.5865133
    ## [10] {}          => {soda}             0.17437722 0.1743772  1.0000000
    ## [11] {margarine} => {soda}             0.01016777 0.1736111  0.9956066
    ## [12] {}          => {yogurt}           0.13950178 0.1395018  1.0000000
    ## [13] {}          => {bottled water}    0.11052364 0.1105236  1.0000000
    ## [14] {}          => {root vegetables}  0.10899847 0.1089985  1.0000000
    ## [15] {}          => {tropical fruit}   0.10493137 0.1049314  1.0000000

They tend to buy whole milk, other vegetables, rolls/buns, and yogurt after buying margarine. Whole milk and yogurt should be placed in the dairy section near margarine, so this chain association does make sense.
