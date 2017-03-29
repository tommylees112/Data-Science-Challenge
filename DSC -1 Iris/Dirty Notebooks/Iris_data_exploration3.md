Iris DataExploration
================
thomas.lees
21/03/2017

Install the required packages
-----------------------------

Import the required packages
----------------------------

<https://cran.r-project.org/web/packages/ggforce/vignettes/Visual_Guide.html> - ggforce
---------------------------------------------------------------------------------------

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.3.2

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'ggplot2' was built under R version 3.3.2

    ## Warning: package 'tidyr' was built under R version 3.3.2

    ## Warning: package 'readr' was built under R version 3.3.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(grid)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(forcats)
```

    ## Warning: package 'forcats' was built under R version 3.3.2

``` r
library(modelr)
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.3.2

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(kknn)
```

    ## 
    ## Attaching package: 'kknn'

    ## The following object is masked from 'package:caret':
    ## 
    ##     contr.dummy

``` r
#update.packages(checkBuilt = TRUE)
```

Data cleaning
-------------

load iris data into a tibble for more intuitive exploration and manipulation

WHAT: perform some quick raw iris data overview and then transform it to create the long version. Then we coerce some character variables to R factors for better graphic analysis later. Then we proceed onto performing some housekeeping in which we check for missing and special values.

``` r
iris = as_tibble(iris)
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

Have a SECOND LONG FORMAT tibble (<https://www.kaggle.com/edmwanza1/d/uciml/iris/descriptive-analytics-demo-using-iris>)

long\_iris is longer with 600 observations & 4 variables. Don't be confused, the data is the same just formatted differently.

``` r
long_iris <- iris %>% 
  gather(key= 'part', value = 'value', Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  separate(part,c('part','measure'), sep = '\\.')
```

gather() - key = the column names you want / value = the value names / the columns you want under the key separate() - split the 'part' column at the '.' into two columns - 'part' and 'measure' note: needs to be \\. to escape the first  and then .

``` r
long_iris2 <- iris %>%
  gather(key='part',value='value',Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  separate(part,into = c('part','measure'), sep='\\.')
```

coerce character variables into factors for ease of analysis

``` r
factors <- c('part','measure')
long_iris[factors] <- lapply(long_iris[factors],as.factor)
```

early exploration
-----------------

use sapply() to apply a function to each variable

``` r
sapply(iris,class)
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    ##    "numeric"    "numeric"    "numeric"    "numeric"     "factor"

``` r
sapply(long_iris,class)
```

    ##   Species      part   measure     value 
    ##  "factor"  "factor"  "factor" "numeric"

``` r
sapply(long_iris2,class)
```

    ##     Species        part     measure       value 
    ##    "factor" "character" "character"   "numeric"

check for missing values

``` r
# CREATE A FUNCTION
na_data <- function(x){
  sum(is.na(x))/length(x)*100
}
#WHY DOES IT NEED THE /LENGTH*100??

na_data2 <- function(x){
  sum(is.na(x))
}

#loop it over our dataset
apply(long_iris,2,na_data2)
```

    ## Species    part measure   value 
    ##       0       0       0       0

map() series to loop over variables
-----------------------------------

map is super useful, just input a tibble and then the function that you want to be repeated over the data

``` r
map(iris,sd)
```

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm = na.rm): Calling var(x) on a factor x is deprecated and will become an error.
    ##   Use something like 'all(duplicated(x)[-1L])' to test for a constant vector.

    ## $Sepal.Length
    ## [1] 0.8280661
    ## 
    ## $Sepal.Width
    ## [1] 0.4358663
    ## 
    ## $Petal.Length
    ## [1] 1.765298
    ## 
    ## $Petal.Width
    ## [1] 0.7622377
    ## 
    ## $Species
    ## [1] 0.8192319

``` r
map(iris,mean)
```

    ## Warning in mean.default(.x[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## $Sepal.Length
    ## [1] 5.843333
    ## 
    ## $Sepal.Width
    ## [1] 3.057333
    ## 
    ## $Petal.Length
    ## [1] 3.758
    ## 
    ## $Petal.Width
    ## [1] 1.199333
    ## 
    ## $Species
    ## [1] NA

check for other values (non numeric)

is\_special &lt;- function(x){ if(is.numeric(x)) !is.finite(x) else is.na(x) }

sapply(long\_iris,is\_special)

Visualise the data
------------------

Some initial plots of individual dimensions

``` r
ggplot(iris) +
  geom_point(mapping=aes(x=Sepal.Length,y=Sepal.Width,col=Species,shape=Species))
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggplot(iris,mapping=aes(x=iris$Sepal.Width,y=iris$Sepal.Length,col=Species)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
ggplot(iris, mapping = aes())+
  geom_bar(mapping = aes(x=Species,fill=Species))
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
ggplot(iris, mapping = aes(fill=Species)) +
  geom_boxplot(mapping = aes(x=iris$Species,y=iris$Sepal.Width))
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-13-1.png)

More complicated plots (and multiple plots)
-------------------------------------------

### Imported Function

Super useful function = plotting multiple ggplots in a grid 'multiplot'

``` r
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

``` r
P1 <- ggplot(iris, mapping = aes(x = Sepal.Length, color = Species, fill=Species)) +
  geom_freqpoly(binwidth=0.3) +
  theme(legend.position = 'none')

P2 <- ggplot(iris, mapping = aes(x = Sepal.Width, color = Species, fill=Species)) +
  geom_freqpoly(binwidth=0.3) +
  theme(legend.position = 'none')

P3 <- ggplot(iris, mapping = aes(x = Petal.Length, color = Species, fill=Species)) +
  geom_freqpoly(binwidth=0.3) +
  theme(legend.position = 'none')

P4 <- ggplot(iris, mapping = aes(x = Petal.Width, color = Species, fill=Species)) +
  geom_freqpoly(binwidth=0.3)

multiplot(P1, P2, P3, P4, cols=2)
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Or you can do this with grid.arrange()

``` r
P1 <- ggplot(iris, mapping = aes(x = Sepal.Length, color = Species, fill=Species)) +
  geom_density(alpha=0.3) +
  theme(legend.position = 'none')

P2 <- ggplot(iris, mapping = aes(x = Petal.Length, color = Species, fill=Species)) +
  geom_density(alpha=0.3) +
  theme(legend.position = 'none')

P3 <- ggplot(iris, mapping = aes(x = Sepal.Width, color = Species, fill=Species)) +
  geom_density(alpha=0.3) +
  theme(legend.position = 'none')

P4 <- ggplot(iris, mapping = aes(x = Petal.Width, color = Species, fill=Species)) +
  geom_density(alpha=0.3) +
  theme(legend.position = 'none')

grid.arrange(P1,
             P2,
             P3,
             P4,
             nrow = 2,
             top = textGrob("Iris Density Plot", gp=gpar(fontsize=10)))
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-16-1.png)

Working with the long\_iris dataframe can help visualise the data much easier by having columns to split the data up

``` r
head(long_iris)
```

    ## # A tibble: 6 x 4
    ##   Species   part measure value
    ##    <fctr> <fctr>  <fctr> <dbl>
    ## 1  setosa  Sepal  Length   5.1
    ## 2  setosa  Sepal  Length   4.9
    ## 3  setosa  Sepal  Length   4.7
    ## 4  setosa  Sepal  Length   4.6
    ## 5  setosa  Sepal  Length   5.0
    ## 6  setosa  Sepal  Length   5.4

Let's draw boxplots ontop of underlying points to show how values vary for each species

``` r
ggplot(data= long_iris, mapping= aes(x = Species, y = value, col = Species)) +
  geom_jitter(alpha=0.3, size=0.8) + 
  stat_boxplot(alpha = 0.5) + 
  facet_grid(part~measure) +
  ggtitle('Iris: Feature Exploration')
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
iris_mean <- long_iris %>%
  group_by(Species,part,measure) %>%
  summarize(
    mean = mean(value)
  ) 
iris_mean
```

    ## Source: local data frame [12 x 4]
    ## Groups: Species, part [?]
    ## 
    ##       Species   part measure  mean
    ##        <fctr> <fctr>  <fctr> <dbl>
    ## 1      setosa  Petal  Length 1.462
    ## 2      setosa  Petal   Width 0.246
    ## 3      setosa  Sepal  Length 5.006
    ## 4      setosa  Sepal   Width 3.428
    ## 5  versicolor  Petal  Length 4.260
    ## 6  versicolor  Petal   Width 1.326
    ## 7  versicolor  Sepal  Length 5.936
    ## 8  versicolor  Sepal   Width 2.770
    ## 9   virginica  Petal  Length 5.552
    ## 10  virginica  Petal   Width 2.026
    ## 11  virginica  Sepal  Length 6.588
    ## 12  virginica  Sepal   Width 2.974

``` r
ggplot(data= iris_mean, mapping= aes(x=measure,y=mean,fill=Species)) +
  geom_bar(stat='identity',position='dodge')
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-19-1.png)

Let's build some error bars (calculate the standard error for the plots)

``` r
iris_mean_se <- long_iris %>%
  group_by(Species,part,measure) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    ymax = mean(value) + sd(value),
    ymin = mean(value) - sd(value)
  )
iris_mean_se
```

    ## Source: local data frame [12 x 7]
    ## Groups: Species, part [?]
    ## 
    ##       Species   part measure  mean        sd      ymax      ymin
    ##        <fctr> <fctr>  <fctr> <dbl>     <dbl>     <dbl>     <dbl>
    ## 1      setosa  Petal  Length 1.462 0.1736640 1.6356640 1.2883360
    ## 2      setosa  Petal   Width 0.246 0.1053856 0.3513856 0.1406144
    ## 3      setosa  Sepal  Length 5.006 0.3524897 5.3584897 4.6535103
    ## 4      setosa  Sepal   Width 3.428 0.3790644 3.8070644 3.0489356
    ## 5  versicolor  Petal  Length 4.260 0.4699110 4.7299110 3.7900890
    ## 6  versicolor  Petal   Width 1.326 0.1977527 1.5237527 1.1282473
    ## 7  versicolor  Sepal  Length 5.936 0.5161711 6.4521711 5.4198289
    ## 8  versicolor  Sepal   Width 2.770 0.3137983 3.0837983 2.4562017
    ## 9   virginica  Petal  Length 5.552 0.5518947 6.1038947 5.0001053
    ## 10  virginica  Petal   Width 2.026 0.2746501 2.3006501 1.7513499
    ## 11  virginica  Sepal  Length 6.588 0.6358796 7.2238796 5.9521204
    ## 12  virginica  Sepal   Width 2.974 0.3224966 3.2964966 2.6515034

``` r
ggplot(iris_mean_se, mapping = aes(x=measure,y=mean)) +
  geom_bar(aes(fill = Species) ,stat='identity',position='dodge') + 
  geom_errorbar(mapping = aes(ymin= ymin, ymax= ymax), width = 0.2)
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
#NOTE you need to set the positition dodge to the same to get them to line up (0.9 = default value for bars)
#also necessary to place fill=Species in the entire ggplot mapping, otherwise only in local environment and not passed to 
#geom_errorbar

#USE MULTIPLOT to have multiple plots of the different subsets of teh data 
dodge <- position_dodge(width = 0.9)

petal <- ggplot(data = subset(iris_mean_se, part=='Petal'), mapping = aes(x=measure,y=mean,fill = Species)) +
  geom_bar(stat='identity', position= dodge) + 
  geom_errorbar(mapping = aes(ymin= ymin, ymax= ymax), position= dodge, width = 0.2) +
  theme(legend.position = 'none')+
  ggtitle("Iris: Petal")

sepal <- ggplot(data = subset(iris_mean_se, part=='Sepal'), mapping = aes(x=measure,y=mean,fill = Species)) +
  geom_bar(stat='identity', position= dodge) + 
  geom_errorbar(mapping = aes(ymin= ymin, ymax= ymax), position= dodge, width = 0.2) +
  ggtitle("Iris: Sepal")

multiplot(petal, sepal, cols=2)
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-20-2.png)

<https://www.r-bloggers.com/building-barplots-with-error-bars/>

``` r
limits <- aes(ymax = iris_mean_se$ymax,
              ymin = long_iris$ymin)
dodge <- position_dodge(width = 0.9)

ggplot(data = iris_mean_se, mapping = aes(x=measure,y=mean, fill = Species)) +
  geom_bar(stat='identity', position= dodge) +
  geom_errorbar(mapping=aes(ymin= ymin, ymax = ymax), position = dodge, width = 0.2)
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
#BUT HOW DO I SUBSET BY THE PART OF THE PLANT?

ggplot(data = iris_mean_se, mapping = aes(x=measure,y=mean, fill = Species)) +
  geom_bar(stat='identity', position= dodge) +
  geom_errorbar(mapping=aes(ymin= ymin, ymax = ymax), position = dodge, width = 0.2)
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-21-2.png)

Model the dataset
-----------------

``` r
library(modelr)
options(na.action = na.warn)

#allows base R modelling functions to be wrapped into a piped operations
```

I want to: 1) create a linear regression which predicts sepal length from sepal width 2) repeat for sepal length vs. petal width // sepal width vs. petal width // petal width vs. petal length etc. 3) try and predict whether the input value is of a sepal or a petal? 4) classify data and predict species (setosa, versicolor, virginica)

Give each flower a unique ID so they can be subset from the long dataframe - tidy the dataset (EACH VARIABLE has to be a column) - linear regression of length and width of each specie - need WIDTH and LENGTH as columns in dataframe

``` r
# first we must number the flowers from 1: end (number of rows in the dataset)
iris$Flower <- 1:nrow(iris)
iris
```

    ## # A tibble: 150 x 6
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species Flower
    ##           <dbl>       <dbl>        <dbl>       <dbl>  <fctr>  <int>
    ## 1           5.1         3.5          1.4         0.2  setosa      1
    ## 2           4.9         3.0          1.4         0.2  setosa      2
    ## 3           4.7         3.2          1.3         0.2  setosa      3
    ## 4           4.6         3.1          1.5         0.2  setosa      4
    ## 5           5.0         3.6          1.4         0.2  setosa      5
    ## 6           5.4         3.9          1.7         0.4  setosa      6
    ## 7           4.6         3.4          1.4         0.3  setosa      7
    ## 8           5.0         3.4          1.5         0.2  setosa      8
    ## 9           4.4         2.9          1.4         0.2  setosa      9
    ## 10          4.9         3.1          1.5         0.1  setosa     10
    ## # ... with 140 more rows

``` r
#original data
iris
```

    ## # A tibble: 150 x 6
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species Flower
    ##           <dbl>       <dbl>        <dbl>       <dbl>  <fctr>  <int>
    ## 1           5.1         3.5          1.4         0.2  setosa      1
    ## 2           4.9         3.0          1.4         0.2  setosa      2
    ## 3           4.7         3.2          1.3         0.2  setosa      3
    ## 4           4.6         3.1          1.5         0.2  setosa      4
    ## 5           5.0         3.6          1.4         0.2  setosa      5
    ## 6           5.4         3.9          1.7         0.4  setosa      6
    ## 7           4.6         3.4          1.4         0.3  setosa      7
    ## 8           5.0         3.4          1.5         0.2  setosa      8
    ## 9           4.4         2.9          1.4         0.2  setosa      9
    ## 10          4.9         3.1          1.5         0.1  setosa     10
    ## # ... with 140 more rows

``` r
#stage1

wide_iris <- iris %>%
  gather(key='key', value='value', -Species, -Flower) 

wide_iris
```

    ## # A tibble: 600 x 4
    ##    Species Flower          key value
    ##     <fctr>  <int>        <chr> <dbl>
    ## 1   setosa      1 Sepal.Length   5.1
    ## 2   setosa      2 Sepal.Length   4.9
    ## 3   setosa      3 Sepal.Length   4.7
    ## 4   setosa      4 Sepal.Length   4.6
    ## 5   setosa      5 Sepal.Length   5.0
    ## 6   setosa      6 Sepal.Length   5.4
    ## 7   setosa      7 Sepal.Length   4.6
    ## 8   setosa      8 Sepal.Length   5.0
    ## 9   setosa      9 Sepal.Length   4.4
    ## 10  setosa     10 Sepal.Length   4.9
    ## # ... with 590 more rows

``` r
#stage2

wide_iris <- iris %>%
  gather(key='key', value='value', -Species, -Flower) %>%
  separate(key, c('Part','Measure'), sep= "\\.")

wide_iris
```

    ## # A tibble: 600 x 5
    ##    Species Flower  Part Measure value
    ## *   <fctr>  <int> <chr>   <chr> <dbl>
    ## 1   setosa      1 Sepal  Length   5.1
    ## 2   setosa      2 Sepal  Length   4.9
    ## 3   setosa      3 Sepal  Length   4.7
    ## 4   setosa      4 Sepal  Length   4.6
    ## 5   setosa      5 Sepal  Length   5.0
    ## 6   setosa      6 Sepal  Length   5.4
    ## 7   setosa      7 Sepal  Length   4.6
    ## 8   setosa      8 Sepal  Length   5.0
    ## 9   setosa      9 Sepal  Length   4.4
    ## 10  setosa     10 Sepal  Length   4.9
    ## # ... with 590 more rows

``` r
#stage3

wide_iris <- iris %>%
  gather(key='key', value='value', -Species, -Flower) %>%
  separate(key, c('Part','Measure'), sep= "\\.") %>%
  spread(Measure, value)

wide_iris
```

    ## # A tibble: 300 x 5
    ##    Species Flower  Part Length Width
    ## *   <fctr>  <int> <chr>  <dbl> <dbl>
    ## 1   setosa      1 Petal    1.4   0.2
    ## 2   setosa      1 Sepal    5.1   3.5
    ## 3   setosa      2 Petal    1.4   0.2
    ## 4   setosa      2 Sepal    4.9   3.0
    ## 5   setosa      3 Petal    1.3   0.2
    ## 6   setosa      3 Sepal    4.7   3.2
    ## 7   setosa      4 Petal    1.5   0.2
    ## 8   setosa      4 Sepal    4.6   3.1
    ## 9   setosa      5 Petal    1.4   0.2
    ## 10  setosa      5 Sepal    5.0   3.6
    ## # ... with 290 more rows

### what are gather, separate and spread doing?

<http://r4ds.had.co.nz/tidy-data.html#spreading-and-gathering> gather() - select a key which is the NAME OF THE COLUMNS in the untidy data (current variables) - the value for that variable -keep only SPECIES, FLOWER columns and then the variable being measured (key=) and then value for that observation (value=) separate() -split a column into two -split the key column, into 'part' and 'measure' at the '.' sign spread() -then spread the measure column into its separate components (length and width) -taking the value column as the valu for each of those cells! WE have tidy data :)

1 The relationship between length and width by species
------------------------------------------------------

``` r
library(ggplot2)

lm_iris <- ggplot(data = wide_iris, mapping= aes(x= Width, y= Length, col=Species))

#how does length and width correlate for each species? (on separate charts)
lm_iris1 <- lm_iris+
  geom_jitter(alpha= 0.4, size= 0.8) +
  facet_grid(. ~ Species) +
  stat_smooth(method= 'lm', se=F) + 
  ggtitle("Iris: Length vs. Width by Species")

lm_iris1
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
#how does length and width correlate for each part of the flower?
lm_iris2 <- lm_iris +
  geom_jitter(alpha=0.4, size=0.8) +
  facet_grid(.~ Part) +
  ggtitle("Iris: Length vs. Width by Flower Part")
  
lm_iris2
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-25-2.png)

``` r
#how does length and width correlate for each species (on a single chart)
lm_iris3 <- lm_iris +
  geom_point(alpha = 0.4, size= 0.8) + stat_smooth(method = 'lm', fullrange= T, size= 0.5) +
  ggtitle("Iris: Length vs. Width by Species 2")
  
lm_iris3
```

![](Iris_data_exploration3_files/figure-markdown_github/unnamed-chunk-25-3.png)

k nearest neighbours algorithm
------------------------------

<https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.arwbRyA>

to check if packages installed: - any(grepl("<name of your package>", installed.packages()))

### Normalisation

When? In short: when you suspect that the data is not consistent. You can easily see this when you go through the results of the summary() function. Look at the minimum and maximum values of all the (numerical) attributes. If you see that one attribute has a wide range of values, you will need to normalize your dataset, because this means that the distance will be dominated by this feature. For example, if your dataset has just two attributes, X and Y, and X has values that range from 1 to 1000, while Y has values that only go from 1 to 100, then Y's influence on the distance function will usually be overpowered by X's influence. When you normalize, you actually adjust the range of all features, so that distances between variables with larger ranges will not be over-emphasised.

Why? k-nearest neighbors with an Euclidean distance measure because we want all features to contribute equally

what? Example normalisation - adjustment of each example individually Feature normalisation - adjust each feature in the same way across all examples

How? create normalisation function

``` r
normalise <- function(x) {
  numerator <- x - min(x)
  denominator <- max(x) - min(x)
  return(numerator/denominator)
}

#zVar <- (myVar - mean(myVar)) / sd(myVar)
```

or use the scale() function <https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/> x = a numeric object (eg column in data frame) center = TRUE the means subtracted to center around 0 scale = TRUE the centered column values divided by standard deviation (both true use the root mean square & a z score is calculated)

<http://stackoverflow.com/questions/15215457/standardize-data-columns-in-r>

``` r
iris_norm <- iris %>%
  mutate_each_(funs(scale(.) %>% as.vector),
               vars= c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))

summary(iris_norm)
```

    ##   Sepal.Length       Sepal.Width       Petal.Length      Petal.Width     
    ##  Min.   :-1.86378   Min.   :-2.4258   Min.   :-1.5623   Min.   :-1.4422  
    ##  1st Qu.:-0.89767   1st Qu.:-0.5904   1st Qu.:-1.2225   1st Qu.:-1.1799  
    ##  Median :-0.05233   Median :-0.1315   Median : 0.3354   Median : 0.1321  
    ##  Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.67225   3rd Qu.: 0.5567   3rd Qu.: 0.7602   3rd Qu.: 0.7880  
    ##  Max.   : 2.48370   Max.   : 3.0805   Max.   : 1.7799   Max.   : 1.7064  
    ##        Species       Flower      
    ##  setosa    :50   Min.   :  1.00  
    ##  versicolor:50   1st Qu.: 38.25  
    ##  virginica :50   Median : 75.50  
    ##                  Mean   : 75.50  
    ##                  3rd Qu.:112.75  
    ##                  Max.   :150.00

``` r
#recreate long form
long_iris_norm <- iris_norm %>% 
  gather(key= 'part', value = 'value', Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  separate(part,c('part','measure'), sep = '\\.')

#coerce characters into factors
factors <- c('part','measure')
long_iris[factors] <- lapply(long_iris[factors],as.factor)

#recreate wide form
wide_iris_norm <- iris_norm %>%
  gather(key='key', value='value', -Species, -Flower) %>%
  separate(key, c('Part','Measure'), sep= "\\.") %>%
  spread(Measure, value)
```

#### Z score normalisation

See <http://sebastianraschka.com/Articles/2014_about_feature_scaling.html> - sigma = 1, mu = 0 - features rescaled so they have properties of standard normal distribution -

other use cases: - logistic regression, SVMs, perceptrons, neural networks etc. if you are using gradient descent/ascent-based optimization, otherwise some weights will update much faster than others -Principal Component Analysis (PCA) as an example where standardization is crucial, since it is “analyzing” the variances of the different features.

Note: - think about whether we want to “standardize” or “normalize” (here: scaling to \[0, 1\] range) our data. - Some algorithms assume that our data is centered at 0

Equations: Standardization: z=x−μ/σ

with mean: μ=1/N \* ∑i=1N(xi)

and standard deviation: σ= sqrt(1/N \*∑i=1N (xi−μ)^2)

#### Minmax scaling (normalisation)

-   data is scaled to a fixed range - usually 0 to 1.
-   xnorm = x-xmin / xmax-xmin

use case: A popular application is image processing, where pixel intensities have to be normalized to fit within a certain range (i.e., 0 to 255 for the RGB color range). Also, typical neural network algorithm require data that on a 0-1 scale.

Separate into Training & Test Data
----------------------------------

We will put 67% data into training set, 33% into test set.

N.B: instances of all three species needs to be present at more or less the same ratio as in your original data set.

Therefore, we randomly sample a equal ratio from each species

### Randomly Sample

``` r
#set seed to make work reproducible
set.seed(12345)

#create sample vector (probabilities set @ 67% : 33%)
sample_set <- sample(2, nrow(iris_norm), replace=TRUE, prob= c(0.67, 0.33))

#add to tibble
iris_norm['sample_set'] <- sample_set
iris_norm
```

    ## # A tibble: 150 x 7
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species Flower
    ##           <dbl>       <dbl>        <dbl>       <dbl>  <fctr>  <int>
    ## 1    -0.8976739  1.01560199    -1.335752   -1.311052  setosa      1
    ## 2    -1.1392005 -0.13153881    -1.335752   -1.311052  setosa      2
    ## 3    -1.3807271  0.32731751    -1.392399   -1.311052  setosa      3
    ## 4    -1.5014904  0.09788935    -1.279104   -1.311052  setosa      4
    ## 5    -1.0184372  1.24503015    -1.335752   -1.311052  setosa      5
    ## 6    -0.5353840  1.93331463    -1.165809   -1.048667  setosa      6
    ## 7    -1.5014904  0.78617383    -1.335752   -1.179859  setosa      7
    ## 8    -1.0184372  0.78617383    -1.279104   -1.311052  setosa      8
    ## 9    -1.7430170 -0.36096697    -1.335752   -1.311052  setosa      9
    ## 10   -1.1392005  0.09788935    -1.279104   -1.442245  setosa     10
    ## # ... with 140 more rows, and 1 more variables: sample_set <int>

N.B: the 'replace =' argument is set to TRUE: this means that you assign a 1 or a 2 to EVERY ROW (if it was false you wouldn't be able to reapply 1/2 to the rows).

you assign a 1 or a 2 to a certain row and then reset the vector of 2 to its original state. This means that, for the next rows in your data set, you can either assign a 1 or a 2, each time again. The probability of choosing a 1 or a 2 should not be proportional to the weights amongst the remaining items, so you specify probability weights.

### Subset training & test data

``` r
#training and test variables taken (remove labels)
iris_training <- iris_norm[sample_set==1,1:4]
iris_test <- iris_norm[sample_set==2,1:4]

#training and test LABELS extracted
iris_training_labels <- iris_norm[sample_set==1,5]
iris_test_labels <- iris_norm[sample_set==2,5]

count(iris_training)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    90

``` r
count(iris_test)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    60

``` r
iris_training_labels
```

    ## # A tibble: 90 x 1
    ##    Species
    ##     <fctr>
    ## 1   setosa
    ## 2   setosa
    ## 3   setosa
    ## 4   setosa
    ## 5   setosa
    ## 6   setosa
    ## 7   setosa
    ## 8   setosa
    ## 9   setosa
    ## 10  setosa
    ## # ... with 80 more rows

``` r
iris_training
```

    ## # A tibble: 90 x 4
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width
    ##           <dbl>       <dbl>        <dbl>       <dbl>
    ## 1   -1.01843718   1.2450302    -1.335752   -1.311052
    ## 2   -0.53538397   1.9333146    -1.165809   -1.048667
    ## 3   -1.50149039   0.7861738    -1.335752   -1.179859
    ## 4   -1.01843718   0.7861738    -1.279104   -1.311052
    ## 5   -0.53538397   1.4744583    -1.279104   -1.311052
    ## 6   -1.25996379   0.7861738    -1.222456   -1.311052
    ## 7   -1.86378030  -0.1315388    -1.505695   -1.442245
    ## 8   -0.05233076   2.1627428    -1.449047   -1.311052
    ## 9   -0.17309407   3.0804554    -1.279104   -1.048667
    ## 10  -0.53538397   1.9333146    -1.392399   -1.048667
    ## # ... with 80 more rows

### Build

``` r
#library(class)

#iris_pred <- knn(train= iris_training, test= iris_test, cl= iris_training_labels, k=3)
```

``` r
#install.packages('kknn')
#library(kknn)

iris_norm_train <- iris_norm[sample_set==1,1:5]
iris_norm_test <- iris_norm[sample_set==2,1:5]

#training and test variables taken
iris_training <- iris_norm[sample_set==1,1:4]
iris_test <- iris_norm[sample_set==2,1:4]

#training and test LABELS extracted
iris_training_labels <- iris_norm[sample_set==1,5]
iris_test_labels <- iris_norm[sample_set==2,5]
```

BUILD THE FRIGGIN MODEL :O :O :O
--------------------------------

``` r
#train the model using the 60% of data 
iris_train <- train(Species ~ ., iris_norm_train,
                   method = "knn",
                   trControl = trainControl(method = "cv")
)

#NOTE: the function loops through to find the best value for k (k = 9)

#test using new data to get predictions
iris_pred <- predict(iris_train, newdata = iris_norm_test)

#compare with the 'true' values to see your accuracy
confusionMatrix(iris_pred, iris_norm_test$Species)
```

    ## Confusion Matrix and Statistics
    ## 
    ##             Reference
    ## Prediction   setosa versicolor virginica
    ##   setosa         18          0         0
    ##   versicolor      0         18         0
    ##   virginica       0          2        22
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9667          
    ##                  95% CI : (0.8847, 0.9959)
    ##     No Information Rate : 0.3667          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9497          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: setosa Class: versicolor Class: virginica
    ## Sensitivity                    1.0            0.9000           1.0000
    ## Specificity                    1.0            1.0000           0.9474
    ## Pos Pred Value                 1.0            1.0000           0.9167
    ## Neg Pred Value                 1.0            0.9524           1.0000
    ## Prevalence                     0.3            0.3333           0.3667
    ## Detection Rate                 0.3            0.3000           0.3667
    ## Detection Prevalence           0.3            0.3000           0.4000
    ## Balanced Accuracy              1.0            0.9500           0.9737

### GET the overall accuracy

note: because the boolean statement evaluates to TRUE if the predictions match the observed cases

``` r
mean(iris_pred == iris_norm_test$Species)
```

    ## [1] 0.9666667

NOW: - I want to know about ROC Curves - Understand what the algorithm is actually doing - better understand how to use the INTERNAL normalisation functions (I did it using )

------------------------------------------------------------------------

``` r
#create training tibble
iris_training <- iris_norm %>%
  filter(sample_set == 1)

#create test tibble
iris_test <- iris_norm %>%
  filter(sample_set == 2)

summary(iris_training)
```

    ##   Sepal.Length       Sepal.Width         Petal.Length     
    ##  Min.   :-1.86378   Min.   :-1.966964   Min.   :-1.50569  
    ##  1st Qu.:-0.89767   1st Qu.:-0.819823   1st Qu.:-1.26494  
    ##  Median :-0.17309   Median :-0.131539   Median : 0.22206  
    ##  Mean   :-0.07917   Mean   :-0.006628   Mean   :-0.05992  
    ##  3rd Qu.: 0.55149   3rd Qu.: 0.786174   3rd Qu.: 0.74605  
    ##  Max.   : 2.24217   Max.   : 3.080455   Max.   : 1.77987  
    ##   Petal.Width             Species       Flower         sample_set
    ##  Min.   :-1.44224   setosa    :32   Min.   :  5.00   Min.   :1   
    ##  1st Qu.:-1.17986   versicolor:30   1st Qu.: 36.75   1st Qu.:1   
    ##  Median : 0.13207   virginica :28   Median : 74.50   Median :1   
    ##  Mean   :-0.06181                   Mean   : 73.68   Mean   :1   
    ##  3rd Qu.: 0.78803                   3rd Qu.:108.50   3rd Qu.:1   
    ##  Max.   : 1.70638                   Max.   :150.00   Max.   :1

``` r
summary(iris_test)
```

    ##   Sepal.Length      Sepal.Width         Petal.Length     
    ##  Min.   :-1.7430   Min.   :-2.425820   Min.   :-1.56234  
    ##  1st Qu.:-0.8977   1st Qu.:-0.360967   1st Qu.:-1.22246  
    ##  Median : 0.2496   Median :-0.131539   Median : 0.44865  
    ##  Mean   : 0.1188   Mean   : 0.009942   Mean   : 0.08988  
    ##  3rd Qu.: 0.8534   3rd Qu.: 0.384674   3rd Qu.: 0.88767  
    ##  Max.   : 2.4837   Max.   : 2.621599   Max.   : 1.60993  
    ##   Petal.Width             Species       Flower         sample_set
    ##  Min.   :-1.44224   setosa    :18   Min.   :  1.00   Min.   :2   
    ##  1st Qu.:-1.08146   versicolor:20   1st Qu.: 40.25   1st Qu.:2   
    ##  Median : 0.26326   virginica :22   Median : 79.00   Median :2   
    ##  Mean   : 0.09271                   Mean   : 78.23   Mean   :2   
    ##  3rd Qu.: 0.82083                   3rd Qu.:118.75   3rd Qu.:2   
    ##  Max.   : 1.57519                   Max.   :149.00   Max.   :2

NOTE: there are differnet numbers of species within test data & training data

Resources:
==========

<http://michael.hahsler.net/SMU/EMIS7332/R/regression.html> <https://rpubs.com/lwaldron/iris_regression> <https://www.kaggle.com/mgabrielkerr/d/uciml/iris/visualizing-knn-svm-and-xgboost-on-iris-dataset> <https://www.kaggle.com/edmwanza1/d/uciml/iris/descriptive-analytics-demo-using-iris>
