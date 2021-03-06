Assignment 1
================
Carey Hedges (751546)
11 July 2016

R Markdown
----------

"I love Octocat. She's the coolest cat in town"

![Octocat](C:\Users\Carey%20Hedges\Desktop\Mini%20Assignments%20-%20Stats\octocat.png)

Assignment 2
------------

``` r
data (anscombe)
dim (anscombe)
```

    ## [1] 11  8

``` r
head (anscombe)
```

    ##   x1 x2 x3 x4   y1   y2    y3   y4
    ## 1 10 10 10  8 8.04 9.14  7.46 6.58
    ## 2  8  8  8  8 6.95 8.14  6.77 5.76
    ## 3 13 13 13  8 7.58 8.74 12.74 7.71
    ## 4  9  9  9  8 8.81 8.77  7.11 8.84
    ## 5 11 11 11  8 8.33 9.26  7.81 8.47
    ## 6 14 14 14  8 9.96 8.10  8.84 7.04

``` r
tail (anscombe)
```

    ##    x1 x2 x3 x4    y1   y2   y3    y4
    ## 6  14 14 14  8  9.96 8.10 8.84  7.04
    ## 7   6  6  6  8  7.24 6.13 6.08  5.25
    ## 8   4  4  4 19  4.26 3.10 5.39 12.50
    ## 9  12 12 12  8 10.84 9.13 8.15  5.56
    ## 10  7  7  7  8  4.82 7.26 6.42  7.91
    ## 11  5  5  5  8  5.68 4.74 5.73  6.89

``` r
summary (anscombe)
```

    ##        x1             x2             x3             x4    
    ##  Min.   : 4.0   Min.   : 4.0   Min.   : 4.0   Min.   : 8  
    ##  1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 8  
    ##  Median : 9.0   Median : 9.0   Median : 9.0   Median : 8  
    ##  Mean   : 9.0   Mean   : 9.0   Mean   : 9.0   Mean   : 9  
    ##  3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.: 8  
    ##  Max.   :14.0   Max.   :14.0   Max.   :14.0   Max.   :19  
    ##        y1               y2              y3              y4        
    ##  Min.   : 4.260   Min.   :3.100   Min.   : 5.39   Min.   : 5.250  
    ##  1st Qu.: 6.315   1st Qu.:6.695   1st Qu.: 6.25   1st Qu.: 6.170  
    ##  Median : 7.580   Median :8.140   Median : 7.11   Median : 7.040  
    ##  Mean   : 7.501   Mean   :7.501   Mean   : 7.50   Mean   : 7.501  
    ##  3rd Qu.: 8.570   3rd Qu.:8.950   3rd Qu.: 7.98   3rd Qu.: 8.190  
    ##  Max.   :10.840   Max.   :9.260   Max.   :12.74   Max.   :12.500

Assignment 3
------------

![](Mini_Assignment_1_files/figure-markdown_github/xy_plot-1.png)

Assignment 4
------------

### Code Chunk 4

``` r
df <- read.csv('analgesic.csv')
```

### Code Chunk 5

``` r
dim (df)
```

    ## [1] 40  5

``` r
colnames (df)
```

    ## [1] "ID"            "Group"         "Measurement_1" "Measurement_2"
    ## [5] "Measurement_3"

``` r
head (df)
```

    ##   ID     Group Measurement_1 Measurement_2 Measurement_3
    ## 1  1 Analgesic            26            26            21
    ## 2  2 Analgesic            29            26            23
    ## 3  3 Analgesic            24            28            22
    ## 4  4 Analgesic            25            22            24
    ## 5  5 Analgesic            24            28            23
    ## 6  6 Analgesic            22            23            26

``` r
tail (df)
```

    ##    ID   Group Measurement_1 Measurement_2 Measurement_3
    ## 35 35 Placebo            17            21            15
    ## 36 36 Placebo            19            17            15
    ## 37 37 Placebo            14            19            13
    ## 38 38 Placebo            17            19            13
    ## 39 39 Placebo            11            20            18
    ## 40 40 Placebo            15            18            12

``` r
summary (df)
```

    ##        ID              Group    Measurement_1   Measurement_2 
    ##  Min.   : 1.00   Analgesic:20   Min.   :10.00   Min.   : 8.0  
    ##  1st Qu.:10.75   Placebo  :20   1st Qu.:17.00   1st Qu.:17.0  
    ##  Median :20.50                  Median :20.00   Median :20.0  
    ##  Mean   :20.50                  Mean   :20.12   Mean   :20.7  
    ##  3rd Qu.:30.25                  3rd Qu.:24.00   3rd Qu.:25.0  
    ##  Max.   :40.00                  Max.   :30.00   Max.   :32.0  
    ##  Measurement_3  
    ##  Min.   :12.00  
    ##  1st Qu.:16.00  
    ##  Median :20.50  
    ##  Mean   :20.52  
    ##  3rd Qu.:24.25  
    ##  Max.   :30.00

### Code Chunk 6

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 3.3.1

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df2 <- gather(df,"Measurement", "Value", Measurement_1:Measurement_3) #Change wide to long
df2
```

    ##     ID     Group   Measurement Value
    ## 1    1 Analgesic Measurement_1    26
    ## 2    2 Analgesic Measurement_1    29
    ## 3    3 Analgesic Measurement_1    24
    ## 4    4 Analgesic Measurement_1    25
    ## 5    5 Analgesic Measurement_1    24
    ## 6    6 Analgesic Measurement_1    22
    ## 7    7 Analgesic Measurement_1    25
    ## 8    8 Analgesic Measurement_1    28
    ## 9    9 Analgesic Measurement_1    22
    ## 10  10 Analgesic Measurement_1    18
    ## 11  11 Analgesic Measurement_1    25
    ## 12  12 Analgesic Measurement_1    26
    ## 13  13 Analgesic Measurement_1    26
    ## 14  14 Analgesic Measurement_1    19
    ## 15  15 Analgesic Measurement_1    24
    ## 16  16 Analgesic Measurement_1    23
    ## 17  17 Analgesic Measurement_1    24
    ## 18  18 Analgesic Measurement_1    24
    ## 19  19 Analgesic Measurement_1    23
    ## 20  20 Analgesic Measurement_1    30
    ## 21  21   Placebo Measurement_1    19
    ## 22  22   Placebo Measurement_1    10
    ## 23  23   Placebo Measurement_1    12
    ## 24  24   Placebo Measurement_1    17
    ## 25  25   Placebo Measurement_1    18
    ## 26  26   Placebo Measurement_1    12
    ## 27  27   Placebo Measurement_1    14
    ## 28  28   Placebo Measurement_1    20
    ## 29  29   Placebo Measurement_1    16
    ## 30  30   Placebo Measurement_1    17
    ## 31  31   Placebo Measurement_1    18
    ## 32  32   Placebo Measurement_1    20
    ## 33  33   Placebo Measurement_1    12
    ## 34  34   Placebo Measurement_1    20
    ## 35  35   Placebo Measurement_1    17
    ## 36  36   Placebo Measurement_1    19
    ## 37  37   Placebo Measurement_1    14
    ## 38  38   Placebo Measurement_1    17
    ## 39  39   Placebo Measurement_1    11
    ## 40  40   Placebo Measurement_1    15
    ## 41   1 Analgesic Measurement_2    26
    ## 42   2 Analgesic Measurement_2    26
    ## 43   3 Analgesic Measurement_2    28
    ## 44   4 Analgesic Measurement_2    22
    ## 45   5 Analgesic Measurement_2    28
    ## 46   6 Analgesic Measurement_2    23
    ## 47   7 Analgesic Measurement_2    25
    ## 48   8 Analgesic Measurement_2    21
    ## 49   9 Analgesic Measurement_2    26
    ## 50  10 Analgesic Measurement_2    25
    ## 51  11 Analgesic Measurement_2    29
    ## 52  12 Analgesic Measurement_2    25
    ## 53  13 Analgesic Measurement_2    25
    ## 54  14 Analgesic Measurement_2    30
    ## 55  15 Analgesic Measurement_2    20
    ## 56  16 Analgesic Measurement_2    24
    ## 57  17 Analgesic Measurement_2    32
    ## 58  18 Analgesic Measurement_2    17
    ## 59  19 Analgesic Measurement_2    25
    ## 60  20 Analgesic Measurement_2    18
    ## 61  21   Placebo Measurement_2    12
    ## 62  22   Placebo Measurement_2    16
    ## 63  23   Placebo Measurement_2    11
    ## 64  24   Placebo Measurement_2    17
    ## 65  25   Placebo Measurement_2    18
    ## 66  26   Placebo Measurement_2    16
    ## 67  27   Placebo Measurement_2    17
    ## 68  28   Placebo Measurement_2    19
    ## 69  29   Placebo Measurement_2    19
    ## 70  30   Placebo Measurement_2    15
    ## 71  31   Placebo Measurement_2    21
    ## 72  32   Placebo Measurement_2    13
    ## 73  33   Placebo Measurement_2     8
    ## 74  34   Placebo Measurement_2    17
    ## 75  35   Placebo Measurement_2    21
    ## 76  36   Placebo Measurement_2    17
    ## 77  37   Placebo Measurement_2    19
    ## 78  38   Placebo Measurement_2    19
    ## 79  39   Placebo Measurement_2    20
    ## 80  40   Placebo Measurement_2    18
    ## 81   1 Analgesic Measurement_3    21
    ## 82   2 Analgesic Measurement_3    23
    ## 83   3 Analgesic Measurement_3    22
    ## 84   4 Analgesic Measurement_3    24
    ## 85   5 Analgesic Measurement_3    23
    ## 86   6 Analgesic Measurement_3    26
    ## 87   7 Analgesic Measurement_3    30
    ## 88   8 Analgesic Measurement_3    21
    ## 89   9 Analgesic Measurement_3    20
    ## 90  10 Analgesic Measurement_3    29
    ## 91  11 Analgesic Measurement_3    28
    ## 92  12 Analgesic Measurement_3    23
    ## 93  13 Analgesic Measurement_3    26
    ## 94  14 Analgesic Measurement_3    27
    ## 95  15 Analgesic Measurement_3    24
    ## 96  16 Analgesic Measurement_3    27
    ## 97  17 Analgesic Measurement_3    28
    ## 98  18 Analgesic Measurement_3    25
    ## 99  19 Analgesic Measurement_3    23
    ## 100 20 Analgesic Measurement_3    25
    ## 101 21   Placebo Measurement_3    18
    ## 102 22   Placebo Measurement_3    18
    ## 103 23   Placebo Measurement_3    20
    ## 104 24   Placebo Measurement_3    18
    ## 105 25   Placebo Measurement_3    20
    ## 106 26   Placebo Measurement_3    16
    ## 107 27   Placebo Measurement_3    17
    ## 108 28   Placebo Measurement_3    18
    ## 109 29   Placebo Measurement_3    15
    ## 110 30   Placebo Measurement_3    13
    ## 111 31   Placebo Measurement_3    14
    ## 112 32   Placebo Measurement_3    16
    ## 113 33   Placebo Measurement_3    21
    ## 114 34   Placebo Measurement_3    16
    ## 115 35   Placebo Measurement_3    15
    ## 116 36   Placebo Measurement_3    15
    ## 117 37   Placebo Measurement_3    13
    ## 118 38   Placebo Measurement_3    13
    ## 119 39   Placebo Measurement_3    18
    ## 120 40   Placebo Measurement_3    12

``` r
df3 <- group_by (df2, Group) #Group by Placebo and Analgesic
df3
```

    ## Source: local data frame [120 x 4]
    ## Groups: Group [2]
    ## 
    ##       ID     Group   Measurement Value
    ##    (int)    (fctr)         (chr) (int)
    ## 1      1 Analgesic Measurement_1    26
    ## 2      2 Analgesic Measurement_1    29
    ## 3      3 Analgesic Measurement_1    24
    ## 4      4 Analgesic Measurement_1    25
    ## 5      5 Analgesic Measurement_1    24
    ## 6      6 Analgesic Measurement_1    22
    ## 7      7 Analgesic Measurement_1    25
    ## 8      8 Analgesic Measurement_1    28
    ## 9      9 Analgesic Measurement_1    22
    ## 10    10 Analgesic Measurement_1    18
    ## ..   ...       ...           ...   ...

``` r
df4 <- group_by (df3, ID) #Read by ID
df4
```

    ## Source: local data frame [120 x 4]
    ## Groups: ID [40]
    ## 
    ##       ID     Group   Measurement Value
    ##    (int)    (fctr)         (chr) (int)
    ## 1      1 Analgesic Measurement_1    26
    ## 2      2 Analgesic Measurement_1    29
    ## 3      3 Analgesic Measurement_1    24
    ## 4      4 Analgesic Measurement_1    25
    ## 5      5 Analgesic Measurement_1    24
    ## 6      6 Analgesic Measurement_1    22
    ## 7      7 Analgesic Measurement_1    25
    ## 8      8 Analgesic Measurement_1    28
    ## 9      9 Analgesic Measurement_1    22
    ## 10    10 Analgesic Measurement_1    18
    ## ..   ...       ...           ...   ...

``` r
summarise (df4, mean(Value)) #Mean of all replicates
```

    ## Source: local data frame [40 x 2]
    ## 
    ##       ID mean(Value)
    ##    (int)       (dbl)
    ## 1      1    24.33333
    ## 2      2    26.00000
    ## 3      3    24.66667
    ## 4      4    23.66667
    ## 5      5    25.00000
    ## 6      6    23.66667
    ## 7      7    26.66667
    ## 8      8    23.33333
    ## 9      9    22.66667
    ## 10    10    24.00000
    ## ..   ...         ...
