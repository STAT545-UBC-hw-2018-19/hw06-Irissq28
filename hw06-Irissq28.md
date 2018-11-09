Factor and figure management
================
Irissq28
11th October, 2018

-   [Load packages](#load-packages)
-   [Part 2: Writing functions](#part-2-writing-functions)
-   [Part 4: Work with the singer data](#part-4-work-with-the-singer-data)
    -   [Introduction to Singer](#introduction-to-singer)
        -   [Familiar with singer\_locations](#familiar-with-singer_locations)
        -   [Filtering data](#filtering-data)
    -   [Geocoding API](#geocoding-api)
        -   [TASK 1](#task-1)
        -   [TASK 2](#task-2)
        -   [TASK 3 : Data visualization](#task-3-data-visualization)

Load packages
=============

``` r
## load packages
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gridBase))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(repurrrsive))
```

Part 2: Writing functions
=========================

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

The singer\_locations dataframe in the singer package contains geographical information stored in two different formats: 1. as a (dirty!) variable named city; 2. as a latitude / longitude pair (stored in latitude, longitude respectively).

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

![](hw06-Irissq28_files/figure-markdown_github/unnamed-chunk-7-1.png)

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

However, it can be noticed that some of the FALSE answer are uncorrect, since we ignore the abbreviation of each some states. Here is the updated version.

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

To make the plot more concise, we only visualize the first 40 rows of the filtered singer\_locations(no NA) data frame, the map shows the locations of each singer.

``` r
suppressPackageStartupMessages(library(leaflet))

singerloc_filtered20 %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(city), 
             label = ~as.character(city)) %>%
  addProviderTiles("Esri.WorldImagery")
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-673cbc8fbbcd9fe01578">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[41.88415,40.71455,42.33168,40.99471,34.20034,50.7323,19.59009,34.05349,40.5725,45.51179,51.50632,42.50172,40.43831,40.71455,37.77916,40.71455,34.05349,37.27188,8.4177,47.38028],[-87.63241,-74.00712,-83.04792,-77.60454,-119.18044,7.10169,-155.43414,-118.24532,-74.154,-122.67563,-0.12714,12.88512,-79.99745,-74.00712,-122.42005,-74.00712,-118.24532,-119.27023,-80.11278,-122.23742],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Chicago, IL","New York, NY","Detroit, MI","Pennsylvania","Oxnard, CA","Bonn","Hawaii","Los Angeles, CA","Staten Island, NY","Portland, OR","UK - England - London","Poggio Bustone, Rieti, Italy","Pittsburgh, PA","New York, NY","New York, NY","New York, NY","Los Angeles, CA","California","Panama","KENT, WASHINGTON"],null,null,null,["Chicago, IL","New York, NY","Detroit, MI","Pennsylvania","Oxnard, CA","Bonn","Hawaii","Los Angeles, CA","Staten Island, NY","Portland, OR","UK - England - London","Poggio Bustone, Rieti, Italy","Pittsburgh, PA","New York, NY","New York, NY","New York, NY","Los Angeles, CA","California","Panama","KENT, WASHINGTON"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]}],"limits":{"lat":[8.4177,51.50632],"lng":[-155.43414,12.88512]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
