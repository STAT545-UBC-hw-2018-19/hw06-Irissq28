Data wrangling wrap up
================
Irissq28
5th November, 2018

-   [Load packages](#load-packages)
-   [Part 2: Writing functions](#part-2-writing-functions)
    -   [Create a data frame to work with](#create-a-data-frame-to-work-with)
    -   [Get some code that works](#get-some-code-that-works)
    -   [Turn working code into a function](#turn-working-code-into-a-function)
    -   [Test on other data and in a clean workspace](#test-on-other-data-and-in-a-clean-workspace)
-   [Part 4: Work with the singer data](#part-4-work-with-the-singer-data)
    -   [Introduction to Singer](#introduction-to-singer)
        -   [Familiar with singer\_locations](#familiar-with-singer_locations)
        -   [Filtering data](#filtering-data)
    -   [Geocoding API](#geocoding-api)
    -   [Tasks](#tasks)
        -   [TASK 1](#task-1)
        -   [TASK 2](#task-2)
        -   [TASK 3 : Data visualization](#task-3-data-visualization)

Load packages
=============

``` r
## load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gridBase))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(repurrrsive))
```

Part 2: Writing functions
=========================

In this part, I will write some functions that do something useful to pieces of Gampminder data, the detailed instructions listed [here](http://stat545.com/Classroom/assignments/hw06/hw06.html).

The workflow can be divided into 4 steps, basically followerd the instrcution of [this link](http://stat545.com/block012_function-regress-lifeexp-on-year.html).

Create a data frame to work with
--------------------------------

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(ggplot2))
```

I filter Canada from the data frame.

``` r
country_filtered <- "Canada"
country_data <- gapminder %>%
  filter(country == country_filtered)
knitr::kable(country_data, align = 'c')
```

| country | continent | year | lifeExp |    pop   | gdpPercap |
|:-------:|:---------:|:----:|:-------:|:--------:|:---------:|
|  Canada |  Americas | 1952 |  68.750 | 14785584 |  11367.16 |
|  Canada |  Americas | 1957 |  69.960 | 17010154 |  12489.95 |
|  Canada |  Americas | 1962 |  71.300 | 18985849 |  13462.49 |
|  Canada |  Americas | 1967 |  72.130 | 20819767 |  16076.59 |
|  Canada |  Americas | 1972 |  72.880 | 22284500 |  18970.57 |
|  Canada |  Americas | 1977 |  74.210 | 23796400 |  22090.88 |
|  Canada |  Americas | 1982 |  75.760 | 25201900 |  22898.79 |
|  Canada |  Americas | 1987 |  76.860 | 26549700 |  26626.52 |
|  Canada |  Americas | 1992 |  77.950 | 28523502 |  26342.88 |
|  Canada |  Americas | 1997 |  78.610 | 30305843 |  28954.93 |
|  Canada |  Americas | 2002 |  79.770 | 31902268 |  33328.97 |
|  Canada |  Americas | 2007 |  80.653 | 33390141 |  36319.24 |

Write a function can generate linear regression plot, and then plot the data of Canada.

``` r
lin_fit_plot <- function(dat, offset = 1952, country){
  ggplot(dat,aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Linear regression of", country , "life expectancy from 1952 to 2007")) +
  # the classic dark-on-light ggplot2 theme
  theme_bw() +
  # title is centerd, title size is adjusted
  theme(plot.title = element_text(size = 12,hjust = 0.5))  
}
lin_fit_plot(country_data, country = "Canada")
```

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-4-1.png)

Get some code that works
------------------------

In this part, I will fit the linear model by using `lm()`, here is the [lm() documentation](https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lm), and estimate coefficients by using `coef()`, here is the [coef() documentation](https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/coef). To make more sense of the intercept, I restrict the earliest year is 1952 by using `I()` funciton here, which inhibits interpretation/conversion of objects.

``` r
earliestyear <- 1952
country_fit <- lm(lifeExp ~ I(year - earliestyear), country_data)
# a named vector of coefficients
coef(country_fit)
```

    ##            (Intercept) I(year - earliestyear) 
    ##             68.8838462              0.2188692

Thus the intercept value is 68.8838462 and the slope is 0.2188692.

Turn working code into a function
---------------------------------

In this part, we turn the code from last part into a function, which can estimate the coefficients of a model. To make the column name more readable, here I used `setNames`.

``` r
# set a default year = 1952
country_linfit <- function(dat, offset = 1952) {
  fit <- lm(lifeExp ~ I(year - offset), dat )
  setNames(coef(fit), c("intercept", "slope"))
}
# function testing
country_linfit(country_data)
```

    ##  intercept      slope 
    ## 68.8838462  0.2188692

Here we get the exactly same result as before, which means the funciton works, then we can go on to next step!

Test on other data and in a clean workspace
-------------------------------------------

I cleared workspace first, [reference here](http://stat545.com/block012_function-regress-lifeexp-on-year.html), to avoid the mistake that accidentally relying on objects that were lying around in the workspace during development but that are not actually defined in your function nor passed as formal arguments.

Now let's choose other countries to test the funciton. I created a *countrytest\_data* function to filter the country data from Gapminder.

``` r
countrytest_data <- function(country_test){
  gapminder %>% filter(country == country_test)
}
# CHoose Australia
countrytest1_data <- countrytest_data("Australia")
knitr::kable(countrytest1_data, align = 'c')
```

|  country  | continent | year | lifeExp |    pop   | gdpPercap |
|:---------:|:---------:|:----:|:-------:|:--------:|:---------:|
| Australia |  Oceania  | 1952 |  69.120 |  8691212 |  10039.60 |
| Australia |  Oceania  | 1957 |  70.330 |  9712569 |  10949.65 |
| Australia |  Oceania  | 1962 |  70.930 | 10794968 |  12217.23 |
| Australia |  Oceania  | 1967 |  71.100 | 11872264 |  14526.12 |
| Australia |  Oceania  | 1972 |  71.930 | 13177000 |  16788.63 |
| Australia |  Oceania  | 1977 |  73.490 | 14074100 |  18334.20 |
| Australia |  Oceania  | 1982 |  74.740 | 15184200 |  19477.01 |
| Australia |  Oceania  | 1987 |  76.320 | 16257249 |  21888.89 |
| Australia |  Oceania  | 1992 |  77.560 | 17481977 |  23424.77 |
| Australia |  Oceania  | 1997 |  78.830 | 18565243 |  26997.94 |
| Australia |  Oceania  | 2002 |  80.370 | 19546792 |  30687.75 |
| Australia |  Oceania  | 2007 |  81.235 | 20434176 |  34435.37 |

Testing on the plot linear regression function and get the coefficients function.

``` r
lin_fit_plot(countrytest1_data, country = "Australia")
```

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
country_linfit(countrytest1_data)
```

    ##  intercept      slope 
    ## 68.4005128  0.2277238

The linear fit is good for the previous example. Let's try another country, such as Zimbabwe.

``` r
# country data filter function
countrytest2_data <- countrytest_data("Zimbabwe")
knitr::kable(countrytest2_data, align = "c")
```

|  country | continent | year | lifeExp |    pop   | gdpPercap |
|:--------:|:---------:|:----:|:-------:|:--------:|:---------:|
| Zimbabwe |   Africa  | 1952 |  48.451 |  3080907 |  406.8841 |
| Zimbabwe |   Africa  | 1957 |  50.469 |  3646340 |  518.7643 |
| Zimbabwe |   Africa  | 1962 |  52.358 |  4277736 |  527.2722 |
| Zimbabwe |   Africa  | 1967 |  53.995 |  4995432 |  569.7951 |
| Zimbabwe |   Africa  | 1972 |  55.635 |  5861135 |  799.3622 |
| Zimbabwe |   Africa  | 1977 |  57.674 |  6642107 |  685.5877 |
| Zimbabwe |   Africa  | 1982 |  60.363 |  7636524 |  788.8550 |
| Zimbabwe |   Africa  | 1987 |  62.351 |  9216418 |  706.1573 |
| Zimbabwe |   Africa  | 1992 |  60.377 | 10704340 |  693.4208 |
| Zimbabwe |   Africa  | 1997 |  46.809 | 11404948 |  792.4500 |
| Zimbabwe |   Africa  | 2002 |  39.989 | 11926563 |  672.0386 |
| Zimbabwe |   Africa  | 2007 |  43.487 | 12311143 |  469.7093 |

Testing on the plot linear regression function and get the coefficients function.

``` r
lin_fit_plot(countrytest2_data, country = "Zimbabwe")
```

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
country_linfit(countrytest2_data)
```

    ##   intercept       slope 
    ## 55.22124359 -0.09302098

From this plot, it can be seen that this linear fit is comically bad. Let's try quadratic regression instead, the quadratic regression performs better than linear regression in most cases.

The same as previous step, make a function which can get the coefficients first.

``` r
le_qua_fit <- function(dat, offset = 1952){
  the_quafit <- lm(lifeExp ~ I(year - offset) + I((year - offset)^2), dat)
  setNames(coef(the_quafit), c("intercept","poly 1","poly 2"))
}
le_qua_fit(countrytest2_data)
```

    ##   intercept      poly 1      poly 2 
    ## 45.69740659  1.04983946 -0.02077928

After that create a function which can plot the quadratic regression.

``` r
qua_fit_plot <- function(dat, offset = 1952, country){
  ggplot(dat,aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y ~ I(x - offset) + I((x - offset)^2), se = FALSE) +
  ggtitle(paste("Quadraticr regression of", country , "life expectancy from 1952 to 2007")) +
  # the classic dark-on-light ggplot2 theme
  theme_bw() +
  # title is centerd, title size is adjusted
  theme(plot.title = element_text(size = 12,hjust = 0.5))  
}
qua_fit_plot(countrytest2_data, country = "Zimbabwe")
```

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-12-1.png)

After comparison with linear regression, the quadratic regression works better.

Part 4: Work with the singer data
=================================

Introduction to Singer
----------------------

Singer is a data package that contains an excerpt from the [Million Song Dataset](https://labrosa.ee.columbia.edu/millionsong/) desighed for teaching purpose.

``` r
## installation
# install.packages("devtools")
## install singer from github
# devtools::install_github("JoeyBernhardt/singer")
## load singer
suppressPackageStartupMessages(library(singer))
## install ggmap
# install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
suppressPackageStartupMessages(library(ggmap))
```

### Familiar with singer\_locations

The singer\_locations dataframe in the singer package contains geographical information stored in two different formats: (1). as a (dirty!) variable named city; (2). as a latitude / longitude pair (stored in latitude, longitude respectively).

Let's familiar with the raw data first.

``` r
knitr::kable(head(singer_locations))
```

| track\_id          | title                 | song\_id           | release             | artist\_id         | artist\_name                   |  year|  duration|  artist\_hotttnesss|  artist\_familiarity|  latitude|  longitude| name          | city         |
|:-------------------|:----------------------|:-------------------|:--------------------|:-------------------|:-------------------------------|-----:|---------:|-------------------:|--------------------:|---------:|----------:|:--------------|:-------------|
| TRWICRA128F42368DB | The Conversation (Cd) | SOSURTI12A81C22FB8 | Even If It Kills Me | ARACDPV1187FB58DF4 | Motion City Soundtrack         |  2007|  170.4485|           0.6410183|            0.8230522|        NA|         NA| NA            | NA           |
| TRXJANY128F42246FC | Lonely Island         | SODESQP12A6D4F98EF | The Duke Of Earl    | ARYBUAO1187FB3F4EB | Gene Chandler                  |  2004|  106.5530|           0.3937627|            0.5700167|  41.88415|  -87.63241| Gene Chandler | Chicago, IL  |
| TRIKPCA128F424A553 | Here's That Rainy Day | SOQUYQD12A8C131619 | Imprompture         | AR4111G1187B9B58AB | Paul Horn                      |  1998|  527.5947|           0.4306226|            0.5039940|  40.71455|  -74.00712| Paul Horn     | New York, NY |
| TRYEATD128F92F87C9 | Rego Park Blues       | SOEZGRC12AB017F1AC | Still River         | ARQDZP31187B98D623 | Ronnie Earl & the Broadcasters |  1995|  695.1179|           0.3622792|            0.4773099|        NA|         NA| NA            | NA           |
| TRBYYXH128F4264585 | Games                 | SOPIOCP12A8C13A322 | Afro-Harping        | AR75GYU1187B9AE47A | Dorothy Ashby                  |  1968|  237.3220|           0.4107520|            0.5303468|  42.33168|  -83.04792| Dorothy Ashby | Detroit, MI  |
| TRKFFKR128F9303AE3 | More Pipes            | SOHQSPY12AB0181325 | Six Yanks           | ARCENE01187B9AF929 | Barleyjuice                    |  2006|  192.9400|           0.3762635|            0.5412950|  40.99471|  -77.60454| Barleyjuice   | Pennsylvania |

``` r
dim(singer_locations)
```

    ## [1] 10100    14

``` r
summary(singer_locations)
```

    ##    track_id            title             song_id         
    ##  Length:10100       Length:10100       Length:10100      
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##    release           artist_id         artist_name             year     
    ##  Length:10100       Length:10100       Length:10100       Min.   :   0  
    ##  Class :character   Class :character   Class :character   1st Qu.:1994  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2002  
    ##                                                           Mean   :1979  
    ##                                                           3rd Qu.:2006  
    ##                                                           Max.   :2010  
    ##                                                                         
    ##     duration         artist_hotttnesss artist_familiarity    latitude     
    ##  Min.   :   0.6004   Min.   :0.0000    Min.   :0.0000     Min.   :-45.87  
    ##  1st Qu.: 184.0518   1st Qu.:0.3644    1st Qu.:0.5164     1st Qu.: 35.15  
    ##  Median : 231.3791   Median :0.4098    Median :0.5960     Median : 40.72  
    ##  Mean   : 248.3994   Mean   :0.4149    Mean   :0.5976     Mean   : 40.05  
    ##  3rd Qu.: 288.4567   3rd Qu.:0.4673    3rd Qu.:0.6781     3rd Qu.: 50.88  
    ##  Max.   :2149.3285   Max.   :1.0213    Max.   :1.0000     Max.   : 69.65  
    ##                                                           NA's   :5968    
    ##    longitude            name               city          
    ##  Min.   :-155.434   Length:10100       Length:10100      
    ##  1st Qu.: -90.200   Class :character   Class :character  
    ##  Median : -74.727   Mode  :character   Mode  :character  
    ##  Mean   : -53.632                                        
    ##  3rd Qu.:  -1.465                                        
    ##  Max.   : 175.471                                        
    ##  NA's   :5968

### Filtering data

As we can see the data above is a little messy, because it contains lots of NA in each column, To clean up this dataset, Let's do filtering first, use `drop_na()` to drop rows containing missing values

``` r
singerloc_filtered <- singer_locations %>%
  # filter the NA data
  drop_na() %>%
  select(latitude, longitude, name, city)
knitr::kable(head(singerloc_filtered))
```

|  latitude|   longitude| name                     | city         |
|---------:|-----------:|:-------------------------|:-------------|
|  41.88415|   -87.63241| Gene Chandler            | Chicago, IL  |
|  40.71455|   -74.00712| Paul Horn                | New York, NY |
|  42.33168|   -83.04792| Dorothy Ashby            | Detroit, MI  |
|  40.99471|   -77.60454| Barleyjuice              | Pennsylvania |
|  34.20034|  -119.18044| Madlib                   | Oxnard, CA   |
|  50.73230|     7.10169| Seeed feat. Elephant Man | Bonn         |

``` r
dim(singerloc_filtered)
```

    ## [1] 4129    4

Geocoding API
-------------

Because `register_google` is not in the CRAN version, we need to install the current version from github, using `remotes::install_github("dkahle/ggmap")` or `devtools::install_github("dkahle/ggmap")`.

For more information about ggmap, please check [reference 1](https://github.com/dkahle/ggmap) and [reference 2](https://github.com/dkahle/ggmap/issues/191), also to use the Geocoding API, you must get an API key first, click [Geocoding API](https://cloud.google.com/maps-platform/?__utma=102347093.1350953850.1541400936.1541401784.1541401784.1&__utmb=102347093.0.10.1541401784&__utmc=102347093&__utmx=-&__utmz=102347093.1541401784.1.1.utmcsr=(direct)%7Cutmccn=(direct)%7Cutmcmd=(none)&__utmv=-&__utmk=122929547&_ga=2.59821423.376122712.1541400936-1350953850.1541400936#get-started) to get one, [here](https://developers.google.com/maps/documentation/geocoding/get-api-key) is the detail steps to get an API key and how to add restrictions.

``` r
# install ggmap
# install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
suppressPackageStartupMessages(library(ggmap))
register_google(key = 'AIzaSyDvkwT7TOx1kJr8aOl2p4LcgqmgXDHHB8A')
```

The function revgeocode from the ggmap library allows us to retrieve some information for a pair (vector) of longitude, latitude (warning: notice the order in which you need to pass lat and long).

Tasks
-----

### TASK 1

*Use `purrr` to map latitude and longitude into human readable information on the band's origin places. Notice that revgeocode(... , output = "more") outputs a dataframe, while revgeocode(... , output = "address") returns a string.*

Here we need to map over multiple inputs(latitude, longitude) simultaneously, so we use [`map2()`](https://www.rdocumentation.org/packages/purrr/versions/0.2.5/topics/map2) from `purrr` package. [revgeocode](https://www.rdocumentation.org/packages/ggmap/versions/2.6.1/topics/revgeocode) reverse geocodes a longitude/latitude location using Google Maps.

Because the filtered dataframe still has 4129, it will cost lots of time to query the google cloud plateform, we only use the first 20 rows in this example.

``` r
singerloc_filtered20 <- singerloc_filtered[1:20,]
# Use ggmap get the locations
ggmap_locations <- map2_chr(singerloc_filtered20$longitude,
                            singerloc_filtered20$latitude, 
                            ~ revgeocode(as.numeric(c(.x, .y))))
```

Now we compare the singer location obtained by ggmap and the original singer location in singer\_locations dataframe. Here we use the `cbind` function to combine data frames side-by-side.

``` r
# combine data frames side by side
singer_city <- singerloc_filtered20$city
cityggmap_compare <-cbind(singer_city, ggmap_locations)
# display table in plot
tt <- ttheme_default(
  # Use the smaller text size
  # Alternate the row fill colours
  core = list(fg_params=list(cex = 0.6),
              bg_params=list(fill=c("lightyellow","lightblue"))), 
  colhead = list(fg_params=list(cex = 0.6)), 
  rowhead = list(fg_params=list(cex = 0.6)), rows=NULL)
# show the first 15 columns of the comparison
grid.arrange(tableGrob(head(cityggmap_compare,15),
                       rows = NULL,theme = tt),
             nrow = 1, top = "original city vs. singer location obtained by ggmap")
```

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-18-1.png)

### TASK 2

*Check wether the place in city corresponds to the information you retrieved.* To seperate the word in singer\_city column, we use `boundary()`, which matches boundaries between characters, lines, sentences or words. It’s most useful with `str_split()`. Use `setequal()` to check the correctness of the information.

``` r
cityggmap_compare <- data.frame(cityggmap_compare)
# tranfer to lowercase and split by word
singercity_lower <- cityggmap_compare$singer_city %>%
  str_to_lower() %>% 
  str_split(pattern = boundary("word"))
# tranfer to lowercase and split by word
ggmaploc_lower <- cityggmap_compare$ggmap_locations %>%
  str_to_lower() %>% 
  str_split(pattern = boundary("word"))

# check the correctness
setequal(singercity_lower, ggmaploc_lower)
```

    ## [1] FALSE

As we can see, not all the place in city corresponds to the information retrieved before. Now we use `intersect` to make a further look. Here we define the singer\_city in lower case at least match 1 word in ggamp\_locations in lower case. After that we combine the correctness with the previous ggmap\_compare data frame to vertify the correctness.

``` r
correctness <- map2(singercity_lower, ggmaploc_lower, 
                    ~intersect(.x, .y)) %>% 
  map(function(l) {
    return(length(l) >= 1)
  })
combine_correctness <-cbind(singer_city, ggmap_locations, correctness)
knitr::kable(combine_correctness)
```

| singer\_city                 | ggmap\_locations                                         | correctness |
|:-----------------------------|:---------------------------------------------------------|:------------|
| Chicago, IL                  | 134 N LaSalle St suite 1720, Chicago, IL 60602, USA      | TRUE        |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE        |
| Detroit, MI                  | 1001 Woodward Ave, Detroit, MI 48226, USA                | TRUE        |
| Pennsylvania                 | Z. H. Confair Memorial Hwy, Howard, PA 16841, USA        | FALSE       |
| Oxnard, CA                   | 300 W 3rd St, Oxnard, CA 93030, USA                      | TRUE        |
| Bonn                         | Regina-Pacis-Weg 1, 53113 Bonn, Germany                  | TRUE        |
| Hawaii                       | Unnamed Road, Hawaii, USA                                | TRUE        |
| Los Angeles, CA              | 1420 S Oakhurst Dr, Los Angeles, CA 90035, USA           | TRUE        |
| Staten Island, NY            | 215 Arthur Kill Rd, Staten Island, NY 10306, USA         | TRUE        |
| Portland, OR                 | 1500 SW 1st Ave, Portland, OR 97201, USA                 | TRUE        |
| UK - England - London        | 39 Whitehall, Westminster, London SW1A 2BY, UK           | TRUE        |
| Poggio Bustone, Rieti, Italy | Localita' Pescatore, Poggio Bustone, RI 02018, Italy     | TRUE        |
| Pittsburgh, PA               | 410 Grant St, Pittsburgh, PA 15219, USA                  | TRUE        |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE        |
| New York, NY                 | 1 Dr Carlton B Goodlett Pl, San Francisco, CA 94102, USA | FALSE       |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE        |
| Los Angeles, CA              | 1420 S Oakhurst Dr, Los Angeles, CA 90035, USA           | TRUE        |
| California                   | Shaver Lake, CA 93634, USA                               | FALSE       |
| Panama                       | Calle Aviacion, Río Hato, Panama                         | TRUE        |
| KENT, WASHINGTON             | 220 4th Ave S, Kent, WA 98032, USA                       | TRUE        |

However, it can be noticed that some of the FALSE answer are uncorrect, since we ignore the abbreviation of some states also works. Here is the updated version.

``` r
patterns <- c("new york" = "ny", "pennsylvania" = "pa", "california" = "ca")
resingercity_lower <- singercity_lower %>%
   map(str_replace_all, patterns)
reggmaploc_lower <-ggmaploc_lower %>%
   map(str_replace_all, patterns)
recheck <- map2(resingercity_lower, reggmaploc_lower, 
                    ~intersect(.x, .y)) %>% 
  map(function(l) {
    return(length(l) >= 1)
  })
recombine_correctness <-cbind(singer_city, ggmap_locations, recheck)
knitr::kable(recombine_correctness)
```

| singer\_city                 | ggmap\_locations                                         | recheck |
|:-----------------------------|:---------------------------------------------------------|:--------|
| Chicago, IL                  | 134 N LaSalle St suite 1720, Chicago, IL 60602, USA      | TRUE    |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE    |
| Detroit, MI                  | 1001 Woodward Ave, Detroit, MI 48226, USA                | TRUE    |
| Pennsylvania                 | Z. H. Confair Memorial Hwy, Howard, PA 16841, USA        | TRUE    |
| Oxnard, CA                   | 300 W 3rd St, Oxnard, CA 93030, USA                      | TRUE    |
| Bonn                         | Regina-Pacis-Weg 1, 53113 Bonn, Germany                  | TRUE    |
| Hawaii                       | Unnamed Road, Hawaii, USA                                | TRUE    |
| Los Angeles, CA              | 1420 S Oakhurst Dr, Los Angeles, CA 90035, USA           | TRUE    |
| Staten Island, NY            | 215 Arthur Kill Rd, Staten Island, NY 10306, USA         | TRUE    |
| Portland, OR                 | 1500 SW 1st Ave, Portland, OR 97201, USA                 | TRUE    |
| UK - England - London        | 39 Whitehall, Westminster, London SW1A 2BY, UK           | TRUE    |
| Poggio Bustone, Rieti, Italy | Localita' Pescatore, Poggio Bustone, RI 02018, Italy     | TRUE    |
| Pittsburgh, PA               | 410 Grant St, Pittsburgh, PA 15219, USA                  | TRUE    |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE    |
| New York, NY                 | 1 Dr Carlton B Goodlett Pl, San Francisco, CA 94102, USA | FALSE   |
| New York, NY                 | 80 Chambers St, New York, NY 10007, USA                  | TRUE    |
| Los Angeles, CA              | 1420 S Oakhurst Dr, Los Angeles, CA 90035, USA           | TRUE    |
| California                   | Shaver Lake, CA 93634, USA                               | TRUE    |
| Panama                       | Calle Aviacion, Río Hato, Panama                         | TRUE    |
| KENT, WASHINGTON             | 220 4th Ave S, Kent, WA 98032, USA                       | TRUE    |

Now there is only one FALSE left, and it's reasonable.

### TASK 3 : Data visualization

*Give a look to the library leaflet and plot some information about the bands.*

To make the plot more concise, we only visualize the first 20 rows of the filtered singer\_locations(no NA) data frame, the map shows the locations of each singer.

    suppressPackageStartupMessages(library(leaflet))

    singerloc_filtered20 %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(city), 
                 label = ~as.character(city)) %>%
      addProviderTiles("Esri.WorldImagery")

Because the interactice map works in Rmd and html version, to take a look of the effect, gif version is applied here, to see more about this map, please run it in R.

![](https://media.giphy.com/media/2eKb8x6R7bg4eUkcd1/giphy.gif)
