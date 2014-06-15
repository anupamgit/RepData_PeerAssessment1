Reproducible Research: Assignment 1
========================================================



Read the input file

```r
orig_input <- read.csv("activity.csv", stringsAsFactors=FALSE)
input <- orig_input[!is.na(orig_input$steps),]
```


```r
agg <- aggregate(input$steps, list(Date = input$date), sum)
names(agg) <- c("date", "steps")
hist(agg$steps)
```

![plot of chunk question1](figure/question1.png) 

```r
mean(agg$steps)
```

```
## [1] 10766
```

```r
median(agg$steps)
```

```
## [1] 10765
```


```r
agg1 <- aggregate(steps ~ interval, data=input, FUN=mean)
names(agg1) <- c("interval", "steps")
agg1[agg1$steps==max(agg1$steps),]$interval
```

```
## [1] 835
```

```r
plot(agg1$interval, agg1$steps, type='l')
```

![plot of chunk question2](figure/question2.png) 


```r
sum(is.na(orig_input))
```

```
## [1] 2304
```

```r
new_input <- orig_input
for(i in seq(1,nrow(new_input))) {
   if(is.na(new_input[i,"steps"])) {
        new_input[i,"steps"] <- agg1[agg1$interval == new_input[i,"interval"],"steps"]
    }
}
summary(new_input$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0    37.4    27.0   806.0
```

```r
agg2 <- aggregate(new_input$steps, list(Date = new_input$date), sum)
names(agg2) <- c("date", "steps")
hist(agg2$steps)
```

![plot of chunk question3](figure/question3.png) 

```r
mean(agg2$steps)
```

```
## [1] 10766
```

```r
median(agg2$steps)
```

```
## [1] 10766
```


```r
res <- (weekdays(as.Date(new_input$date, "%Y-%m-%d")) == "Sunday"  | weekdays(as.Date(new_input$date, "%Y-%m-%d")) == "Saturday")
new_input$day <- "weekday"
new_input[res,"day"] <- "weekend"
library(lattice)
agg1 <- aggregate(steps ~ interval + day, data=new_input, FUN=mean)
##http://www.statmethods.net/advgraphs/trellis.html
xyplot(agg1$steps ~ agg1$interval | agg1$day, xlab="Interval", ylab="Number of steps", type="l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

