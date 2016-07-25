Untitled
================
Carey Hedges

Assignment 4
============

Question 1
----------

Ho: Chicken feed has no effect on the weight of chickens H1: Chicken feed has an effect on the weight of chickens

In order to evaluate this claim, an ANOVA will need to be used with a posthoc pairwise t-test to evaluate which chicken feed are better for plumper chickens.

This test assumes that the data are parametric and that there are greater than 3 groups.

    ## Warning: package 'dplyr' was built under R version 3.3.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Warning: package 'tidyr' was built under R version 3.3.1

    ##   weight      feed
    ## 1    179 horsebean
    ## 2    160 horsebean
    ## 3    136 horsebean
    ## 4    227 horsebean
    ## 5    217 horsebean
    ## 6    168 horsebean

    ## [1] "weight" "feed"

    ## [1] 71  2

![](Assignment_4_-_Retry_files/figure-markdown_github/chicken%20weights-1.png)

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## feed         5 231129   46226   15.37 5.94e-10 ***
    ## Residuals   65 195556    3009                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  chickwts$weight and chickwts$feed 
    ## 
    ##           casein  horsebean linseed meatmeal soybean
    ## horsebean 2.9e-08 -         -       -        -      
    ## linseed   0.00016 0.09435   -       -        -      
    ## meatmeal  0.18227 9.0e-05   0.09435 -        -      
    ## soybean   0.00532 0.00298   0.51766 0.51766  -      
    ## sunflower 0.81249 1.2e-08   8.1e-05 0.13218  0.00298
    ## 
    ## P value adjustment method: holm

The anova suggests that the feed affects the weight of the chickens. p&lt;0.001, (df=5, F = 5.94 e-10).

The posthoc test indicates that there is a significant difference between casein and horsebean and linseed feed, horsebean and linseed are different to meatmeal, casein and horsebean are different to soybean, horsebean and linseed and soybean are different to sunflower feed.

Question 2
----------

Ho: There is no difference between nurse and machine evaluation of blood-pressure H1: There is a difference in the measurement of blood-pressure from nurse to machine

An unpaired t.test is needed to understand whether there is a difference between machine evaluation of blood-pressure and the nurse's.

This test assumes that the data is parametric and there are greater than 30 measurments.

``` r
knitr::opts_chunk$set(
fig.path = './figures2',
fig.align = 'centre')

library (dplyr)
library (tidyr)
library (knitr)

bp <- read.csv ('blood-pressure.csv')

head (bp)
```

    ##   Method Subject SBP.replicate.1 SBP.replicate.2 SBP.replicate.3
    ## 1  Nurse       1             100             106             107
    ## 2  Nurse       2             108             110             108
    ## 3  Nurse       3              76              84              82
    ## 4  Nurse       4             108             104             104
    ## 5  Nurse       5             124             112             112
    ## 6  Nurse       6             122             140             124

``` r
colnames (bp)
```

    ## [1] "Method"          "Subject"         "SBP.replicate.1" "SBP.replicate.2"
    ## [5] "SBP.replicate.3"

``` r
bp2 <- gather (bp, 'Replicates', 'Value', SBP.replicate.1:SBP.replicate.3) #Gathering data to tidy for a mean
bp2
```

    ##     Method Subject      Replicates Value
    ## 1    Nurse       1 SBP.replicate.1   100
    ## 2    Nurse       2 SBP.replicate.1   108
    ## 3    Nurse       3 SBP.replicate.1    76
    ## 4    Nurse       4 SBP.replicate.1   108
    ## 5    Nurse       5 SBP.replicate.1   124
    ## 6    Nurse       6 SBP.replicate.1   122
    ## 7    Nurse       7 SBP.replicate.1   116
    ## 8    Nurse       8 SBP.replicate.1   114
    ## 9    Nurse       9 SBP.replicate.1   100
    ## 10   Nurse      10 SBP.replicate.1   108
    ## 11   Nurse      11 SBP.replicate.1   100
    ## 12   Nurse      12 SBP.replicate.1   108
    ## 13   Nurse      13 SBP.replicate.1   112
    ## 14   Nurse      14 SBP.replicate.1   104
    ## 15   Nurse      15 SBP.replicate.1   106
    ## 16   Nurse      16 SBP.replicate.1   122
    ## 17   Nurse      17 SBP.replicate.1   100
    ## 18   Nurse      18 SBP.replicate.1   118
    ## 19   Nurse      19 SBP.replicate.1   140
    ## 20   Nurse      20 SBP.replicate.1   150
    ## 21   Nurse      21 SBP.replicate.1   166
    ## 22   Nurse      22 SBP.replicate.1   148
    ## 23   Nurse      23 SBP.replicate.1   174
    ## 24   Nurse      24 SBP.replicate.1   174
    ## 25   Nurse      25 SBP.replicate.1   140
    ## 26   Nurse      26 SBP.replicate.1   128
    ## 27   Nurse      27 SBP.replicate.1   146
    ## 28   Nurse      28 SBP.replicate.1   146
    ## 29   Nurse      29 SBP.replicate.1   220
    ## 30   Nurse      30 SBP.replicate.1   208
    ## 31   Nurse      31 SBP.replicate.1    94
    ## 32   Nurse      32 SBP.replicate.1   114
    ## 33   Nurse      33 SBP.replicate.1   126
    ## 34   Nurse      34 SBP.replicate.1   124
    ## 35   Nurse      35 SBP.replicate.1   110
    ## 36   Nurse      36 SBP.replicate.1    90
    ## 37   Nurse      37 SBP.replicate.1   106
    ## 38   Nurse      38 SBP.replicate.1   218
    ## 39   Nurse      39 SBP.replicate.1   130
    ## 40   Nurse      40 SBP.replicate.1   136
    ## 41   Nurse      41 SBP.replicate.1   100
    ## 42   Nurse      42 SBP.replicate.1   100
    ## 43   Nurse      43 SBP.replicate.1   124
    ## 44   Nurse      44 SBP.replicate.1   164
    ## 45   Nurse      45 SBP.replicate.1   100
    ## 46   Nurse      46 SBP.replicate.1   136
    ## 47   Nurse      47 SBP.replicate.1   114
    ## 48   Nurse      48 SBP.replicate.1   148
    ## 49   Nurse      49 SBP.replicate.1   160
    ## 50   Nurse      50 SBP.replicate.1    84
    ## 51   Nurse      51 SBP.replicate.1   156
    ## 52   Nurse      52 SBP.replicate.1   110
    ## 53   Nurse      53 SBP.replicate.1   100
    ## 54   Nurse      54 SBP.replicate.1   100
    ## 55   Nurse      55 SBP.replicate.1    86
    ## 56   Nurse      56 SBP.replicate.1   106
    ## 57   Nurse      57 SBP.replicate.1   108
    ## 58   Nurse      58 SBP.replicate.1   168
    ## 59   Nurse      59 SBP.replicate.1   166
    ## 60   Nurse      60 SBP.replicate.1   146
    ## 61   Nurse      61 SBP.replicate.1   204
    ## 62   Nurse      62 SBP.replicate.1    96
    ## 63   Nurse      63 SBP.replicate.1   134
    ## 64   Nurse      64 SBP.replicate.1   138
    ## 65   Nurse      65 SBP.replicate.1   134
    ## 66   Nurse      66 SBP.replicate.1   156
    ## 67   Nurse      67 SBP.replicate.1   124
    ## 68   Nurse      68 SBP.replicate.1   114
    ## 69   Nurse      69 SBP.replicate.1   112
    ## 70   Nurse      70 SBP.replicate.1   112
    ## 71   Nurse      71 SBP.replicate.1   202
    ## 72   Nurse      72 SBP.replicate.1   132
    ## 73   Nurse      73 SBP.replicate.1   158
    ## 74   Nurse      74 SBP.replicate.1    88
    ## 75   Nurse      75 SBP.replicate.1   170
    ## 76   Nurse      76 SBP.replicate.1   182
    ## 77   Nurse      77 SBP.replicate.1   112
    ## 78   Nurse      78 SBP.replicate.1   120
    ## 79   Nurse      79 SBP.replicate.1   110
    ## 80   Nurse      80 SBP.replicate.1   112
    ## 81   Nurse      81 SBP.replicate.1   154
    ## 82   Nurse      82 SBP.replicate.1   116
    ## 83   Nurse      83 SBP.replicate.1   108
    ## 84   Nurse      84 SBP.replicate.1   106
    ## 85   Nurse      85 SBP.replicate.1   122
    ## 86  Device       1 SBP.replicate.1   122
    ## 87  Device       2 SBP.replicate.1   121
    ## 88  Device       3 SBP.replicate.1    95
    ## 89  Device       4 SBP.replicate.1   127
    ## 90  Device       5 SBP.replicate.1   140
    ## 91  Device       6 SBP.replicate.1   139
    ## 92  Device       7 SBP.replicate.1   122
    ## 93  Device       8 SBP.replicate.1   130
    ## 94  Device       9 SBP.replicate.1   119
    ## 95  Device      10 SBP.replicate.1   126
    ## 96  Device      11 SBP.replicate.1   107
    ## 97  Device      12 SBP.replicate.1   123
    ## 98  Device      13 SBP.replicate.1   131
    ## 99  Device      14 SBP.replicate.1   123
    ## 100 Device      15 SBP.replicate.1   127
    ## 101 Device      16 SBP.replicate.1   142
    ## 102 Device      17 SBP.replicate.1   104
    ## 103 Device      18 SBP.replicate.1   117
    ## 104 Device      19 SBP.replicate.1   139
    ## 105 Device      20 SBP.replicate.1   143
    ## 106 Device      21 SBP.replicate.1   181
    ## 107 Device      22 SBP.replicate.1   149
    ## 108 Device      23 SBP.replicate.1   173
    ## 109 Device      24 SBP.replicate.1   160
    ## 110 Device      25 SBP.replicate.1   158
    ## 111 Device      26 SBP.replicate.1   139
    ## 112 Device      27 SBP.replicate.1   153
    ## 113 Device      28 SBP.replicate.1   138
    ## 114 Device      29 SBP.replicate.1   228
    ## 115 Device      30 SBP.replicate.1   190
    ## 116 Device      31 SBP.replicate.1   103
    ## 117 Device      32 SBP.replicate.1   131
    ## 118 Device      33 SBP.replicate.1   131
    ## 119 Device      34 SBP.replicate.1   126
    ## 120 Device      35 SBP.replicate.1   121
    ## 121 Device      36 SBP.replicate.1    97
    ## 122 Device      37 SBP.replicate.1   116
    ## 123 Device      38 SBP.replicate.1   215
    ## 124 Device      39 SBP.replicate.1   141
    ## 125 Device      40 SBP.replicate.1   153
    ## 126 Device      41 SBP.replicate.1   113
    ## 127 Device      42 SBP.replicate.1   109
    ## 128 Device      43 SBP.replicate.1   145
    ## 129 Device      44 SBP.replicate.1   192
    ## 130 Device      45 SBP.replicate.1   112
    ## 131 Device      46 SBP.replicate.1   152
    ## 132 Device      47 SBP.replicate.1   141
    ## 133 Device      48 SBP.replicate.1   206
    ## 134 Device      49 SBP.replicate.1   151
    ## 135 Device      50 SBP.replicate.1   112
    ## 136 Device      51 SBP.replicate.1   162
    ## 137 Device      52 SBP.replicate.1   117
    ## 138 Device      53 SBP.replicate.1   119
    ## 139 Device      54 SBP.replicate.1   136
    ## 140 Device      55 SBP.replicate.1   112
    ## 141 Device      56 SBP.replicate.1   120
    ## 142 Device      57 SBP.replicate.1   117
    ## 143 Device      58 SBP.replicate.1   194
    ## 144 Device      59 SBP.replicate.1   167
    ## 145 Device      60 SBP.replicate.1   173
    ## 146 Device      61 SBP.replicate.1   228
    ## 147 Device      62 SBP.replicate.1    77
    ## 148 Device      63 SBP.replicate.1   154
    ## 149 Device      64 SBP.replicate.1   154
    ## 150 Device      65 SBP.replicate.1   145
    ## 151 Device      66 SBP.replicate.1   200
    ## 152 Device      67 SBP.replicate.1   188
    ## 153 Device      68 SBP.replicate.1   149
    ## 154 Device      69 SBP.replicate.1   136
    ## 155 Device      70 SBP.replicate.1   128
    ## 156 Device      71 SBP.replicate.1   204
    ## 157 Device      72 SBP.replicate.1   184
    ## 158 Device      73 SBP.replicate.1   163
    ## 159 Device      74 SBP.replicate.1    93
    ## 160 Device      75 SBP.replicate.1   178
    ## 161 Device      76 SBP.replicate.1   202
    ## 162 Device      77 SBP.replicate.1   162
    ## 163 Device      78 SBP.replicate.1   227
    ## 164 Device      79 SBP.replicate.1   133
    ## 165 Device      80 SBP.replicate.1   202
    ## 166 Device      81 SBP.replicate.1   158
    ## 167 Device      82 SBP.replicate.1   124
    ## 168 Device      83 SBP.replicate.1   114
    ## 169 Device      84 SBP.replicate.1   137
    ## 170 Device      85 SBP.replicate.1   121
    ## 171  Nurse       1 SBP.replicate.2   106
    ## 172  Nurse       2 SBP.replicate.2   110
    ## 173  Nurse       3 SBP.replicate.2    84
    ## 174  Nurse       4 SBP.replicate.2   104
    ## 175  Nurse       5 SBP.replicate.2   112
    ## 176  Nurse       6 SBP.replicate.2   140
    ## 177  Nurse       7 SBP.replicate.2   108
    ## 178  Nurse       8 SBP.replicate.2   110
    ## 179  Nurse       9 SBP.replicate.2   108
    ## 180  Nurse      10 SBP.replicate.2    92
    ## 181  Nurse      11 SBP.replicate.2   106
    ## 182  Nurse      12 SBP.replicate.2   112
    ## 183  Nurse      13 SBP.replicate.2   112
    ## 184  Nurse      14 SBP.replicate.2   108
    ## 185  Nurse      15 SBP.replicate.2   108
    ## 186  Nurse      16 SBP.replicate.2   122
    ## 187  Nurse      17 SBP.replicate.2   102
    ## 188  Nurse      18 SBP.replicate.2   118
    ## 189  Nurse      19 SBP.replicate.2   134
    ## 190  Nurse      20 SBP.replicate.2   148
    ## 191  Nurse      21 SBP.replicate.2   154
    ## 192  Nurse      22 SBP.replicate.2   156
    ## 193  Nurse      23 SBP.replicate.2   172
    ## 194  Nurse      24 SBP.replicate.2   166
    ## 195  Nurse      25 SBP.replicate.2   144
    ## 196  Nurse      26 SBP.replicate.2   134
    ## 197  Nurse      27 SBP.replicate.2   138
    ## 198  Nurse      28 SBP.replicate.2   152
    ## 199  Nurse      29 SBP.replicate.2   218
    ## 200  Nurse      30 SBP.replicate.2   200
    ## 201  Nurse      31 SBP.replicate.2    84
    ## 202  Nurse      32 SBP.replicate.2   124
    ## 203  Nurse      33 SBP.replicate.2   120
    ## 204  Nurse      34 SBP.replicate.2   124
    ## 205  Nurse      35 SBP.replicate.2   120
    ## 206  Nurse      36 SBP.replicate.2    90
    ## 207  Nurse      37 SBP.replicate.2   106
    ## 208  Nurse      38 SBP.replicate.2   202
    ## 209  Nurse      39 SBP.replicate.2   128
    ## 210  Nurse      40 SBP.replicate.2   136
    ## 211  Nurse      41 SBP.replicate.2    96
    ## 212  Nurse      42 SBP.replicate.2    98
    ## 213  Nurse      43 SBP.replicate.2   116
    ## 214  Nurse      44 SBP.replicate.2   168
    ## 215  Nurse      45 SBP.replicate.2   102
    ## 216  Nurse      46 SBP.replicate.2   126
    ## 217  Nurse      47 SBP.replicate.2   108
    ## 218  Nurse      48 SBP.replicate.2   120
    ## 219  Nurse      49 SBP.replicate.2   150
    ## 220  Nurse      50 SBP.replicate.2    92
    ## 221  Nurse      51 SBP.replicate.2   162
    ## 222  Nurse      52 SBP.replicate.2    98
    ## 223  Nurse      53 SBP.replicate.2   106
    ## 224  Nurse      54 SBP.replicate.2   102
    ## 225  Nurse      55 SBP.replicate.2    74
    ## 226  Nurse      56 SBP.replicate.2   100
    ## 227  Nurse      57 SBP.replicate.2   110
    ## 228  Nurse      58 SBP.replicate.2   188
    ## 229  Nurse      59 SBP.replicate.2   150
    ## 230  Nurse      60 SBP.replicate.2   142
    ## 231  Nurse      61 SBP.replicate.2   198
    ## 232  Nurse      62 SBP.replicate.2    94
    ## 233  Nurse      63 SBP.replicate.2   126
    ## 234  Nurse      64 SBP.replicate.2   144
    ## 235  Nurse      65 SBP.replicate.2   136
    ## 236  Nurse      66 SBP.replicate.2   160
    ## 237  Nurse      67 SBP.replicate.2   138
    ## 238  Nurse      68 SBP.replicate.2   110
    ## 239  Nurse      69 SBP.replicate.2   116
    ## 240  Nurse      70 SBP.replicate.2   116
    ## 241  Nurse      71 SBP.replicate.2   220
    ## 242  Nurse      72 SBP.replicate.2   136
    ## 243  Nurse      73 SBP.replicate.2   162
    ## 244  Nurse      74 SBP.replicate.2    76
    ## 245  Nurse      75 SBP.replicate.2   174
    ## 246  Nurse      76 SBP.replicate.2   176
    ## 247  Nurse      77 SBP.replicate.2   114
    ## 248  Nurse      78 SBP.replicate.2   118
    ## 249  Nurse      79 SBP.replicate.2   108
    ## 250  Nurse      80 SBP.replicate.2   112
    ## 251  Nurse      81 SBP.replicate.2   134
    ## 252  Nurse      82 SBP.replicate.2   112
    ## 253  Nurse      83 SBP.replicate.2   110
    ## 254  Nurse      84 SBP.replicate.2    98
    ## 255  Nurse      85 SBP.replicate.2   112
    ## 256 Device       1 SBP.replicate.2   128
    ## 257 Device       2 SBP.replicate.2   127
    ## 258 Device       3 SBP.replicate.2    94
    ## 259 Device       4 SBP.replicate.2   127
    ## 260 Device       5 SBP.replicate.2   131
    ## 261 Device       6 SBP.replicate.2   142
    ## 262 Device       7 SBP.replicate.2   112
    ## 263 Device       8 SBP.replicate.2   129
    ## 264 Device       9 SBP.replicate.2   122
    ## 265 Device      10 SBP.replicate.2   113
    ## 266 Device      11 SBP.replicate.2   113
    ## 267 Device      12 SBP.replicate.2   125
    ## 268 Device      13 SBP.replicate.2   129
    ## 269 Device      14 SBP.replicate.2   126
    ## 270 Device      15 SBP.replicate.2   119
    ## 271 Device      16 SBP.replicate.2   133
    ## 272 Device      17 SBP.replicate.2   116
    ## 273 Device      18 SBP.replicate.2   113
    ## 274 Device      19 SBP.replicate.2   127
    ## 275 Device      20 SBP.replicate.2   155
    ## 276 Device      21 SBP.replicate.2   170
    ## 277 Device      22 SBP.replicate.2   156
    ## 278 Device      23 SBP.replicate.2   170
    ## 279 Device      24 SBP.replicate.2   155
    ## 280 Device      25 SBP.replicate.2   152
    ## 281 Device      26 SBP.replicate.2   144
    ## 282 Device      27 SBP.replicate.2   150
    ## 283 Device      28 SBP.replicate.2   144
    ## 284 Device      29 SBP.replicate.2   228
    ## 285 Device      30 SBP.replicate.2   183
    ## 286 Device      31 SBP.replicate.2    99
    ## 287 Device      32 SBP.replicate.2   131
    ## 288 Device      33 SBP.replicate.2   123
    ## 289 Device      34 SBP.replicate.2   129
    ## 290 Device      35 SBP.replicate.2   114
    ## 291 Device      36 SBP.replicate.2    94
    ## 292 Device      37 SBP.replicate.2   121
    ## 293 Device      38 SBP.replicate.2   201
    ## 294 Device      39 SBP.replicate.2   133
    ## 295 Device      40 SBP.replicate.2   143
    ## 296 Device      41 SBP.replicate.2   107
    ## 297 Device      42 SBP.replicate.2   105
    ## 298 Device      43 SBP.replicate.2   102
    ## 299 Device      44 SBP.replicate.2   178
    ## 300 Device      45 SBP.replicate.2   116
    ## 301 Device      46 SBP.replicate.2   144
    ## 302 Device      47 SBP.replicate.2   141
    ## 303 Device      48 SBP.replicate.2   188
    ## 304 Device      49 SBP.replicate.2   147
    ## 305 Device      50 SBP.replicate.2   125
    ## 306 Device      51 SBP.replicate.2   165
    ## 307 Device      52 SBP.replicate.2   118
    ## 308 Device      53 SBP.replicate.2   131
    ## 309 Device      54 SBP.replicate.2   116
    ## 310 Device      55 SBP.replicate.2   115
    ## 311 Device      56 SBP.replicate.2   118
    ## 312 Device      57 SBP.replicate.2   118
    ## 313 Device      58 SBP.replicate.2   191
    ## 314 Device      59 SBP.replicate.2   160
    ## 315 Device      60 SBP.replicate.2   161
    ## 316 Device      61 SBP.replicate.2   218
    ## 317 Device      62 SBP.replicate.2    89
    ## 318 Device      63 SBP.replicate.2   156
    ## 319 Device      64 SBP.replicate.2   155
    ## 320 Device      65 SBP.replicate.2   154
    ## 321 Device      66 SBP.replicate.2   180
    ## 322 Device      67 SBP.replicate.2   147
    ## 323 Device      68 SBP.replicate.2   217
    ## 324 Device      69 SBP.replicate.2   132
    ## 325 Device      70 SBP.replicate.2   125
    ## 326 Device      71 SBP.replicate.2   222
    ## 327 Device      72 SBP.replicate.2   187
    ## 328 Device      73 SBP.replicate.2   160
    ## 329 Device      74 SBP.replicate.2    88
    ## 330 Device      75 SBP.replicate.2   181
    ## 331 Device      76 SBP.replicate.2   199
    ## 332 Device      77 SBP.replicate.2   166
    ## 333 Device      78 SBP.replicate.2   227
    ## 334 Device      79 SBP.replicate.2   127
    ## 335 Device      80 SBP.replicate.2   190
    ## 336 Device      81 SBP.replicate.2   121
    ## 337 Device      82 SBP.replicate.2   149
    ## 338 Device      83 SBP.replicate.2   118
    ## 339 Device      84 SBP.replicate.2   135
    ## 340 Device      85 SBP.replicate.2   123
    ## 341  Nurse       1 SBP.replicate.3   107
    ## 342  Nurse       2 SBP.replicate.3   108
    ## 343  Nurse       3 SBP.replicate.3    82
    ## 344  Nurse       4 SBP.replicate.3   104
    ## 345  Nurse       5 SBP.replicate.3   112
    ## 346  Nurse       6 SBP.replicate.3   124
    ## 347  Nurse       7 SBP.replicate.3   102
    ## 348  Nurse       8 SBP.replicate.3   112
    ## 349  Nurse       9 SBP.replicate.3   112
    ## 350  Nurse      10 SBP.replicate.3   100
    ## 351  Nurse      11 SBP.replicate.3   104
    ## 352  Nurse      12 SBP.replicate.3   122
    ## 353  Nurse      13 SBP.replicate.3   110
    ## 354  Nurse      14 SBP.replicate.3   104
    ## 355  Nurse      15 SBP.replicate.3   102
    ## 356  Nurse      16 SBP.replicate.3   114
    ## 357  Nurse      17 SBP.replicate.3   102
    ## 358  Nurse      18 SBP.replicate.3   120
    ## 359  Nurse      19 SBP.replicate.3   138
    ## 360  Nurse      20 SBP.replicate.3   144
    ## 361  Nurse      21 SBP.replicate.3   154
    ## 362  Nurse      22 SBP.replicate.3   134
    ## 363  Nurse      23 SBP.replicate.3   166
    ## 364  Nurse      24 SBP.replicate.3   150
    ## 365  Nurse      25 SBP.replicate.3   144
    ## 366  Nurse      26 SBP.replicate.3   130
    ## 367  Nurse      27 SBP.replicate.3   140
    ## 368  Nurse      28 SBP.replicate.3   148
    ## 369  Nurse      29 SBP.replicate.3   220
    ## 370  Nurse      30 SBP.replicate.3   192
    ## 371  Nurse      31 SBP.replicate.3    86
    ## 372  Nurse      32 SBP.replicate.3   116
    ## 373  Nurse      33 SBP.replicate.3   122
    ## 374  Nurse      34 SBP.replicate.3   132
    ## 375  Nurse      35 SBP.replicate.3   128
    ## 376  Nurse      36 SBP.replicate.3    94
    ## 377  Nurse      37 SBP.replicate.3   110
    ## 378  Nurse      38 SBP.replicate.3   208
    ## 379  Nurse      39 SBP.replicate.3   130
    ## 380  Nurse      40 SBP.replicate.3   130
    ## 381  Nurse      41 SBP.replicate.3    88
    ## 382  Nurse      42 SBP.replicate.3    88
    ## 383  Nurse      43 SBP.replicate.3   122
    ## 384  Nurse      44 SBP.replicate.3   154
    ## 385  Nurse      45 SBP.replicate.3   100
    ## 386  Nurse      46 SBP.replicate.3   122
    ## 387  Nurse      47 SBP.replicate.3   122
    ## 388  Nurse      48 SBP.replicate.3   132
    ## 389  Nurse      49 SBP.replicate.3   148
    ## 390  Nurse      50 SBP.replicate.3    98
    ## 391  Nurse      51 SBP.replicate.3   152
    ## 392  Nurse      52 SBP.replicate.3    98
    ## 393  Nurse      53 SBP.replicate.3   106
    ## 394  Nurse      54 SBP.replicate.3    94
    ## 395  Nurse      55 SBP.replicate.3    76
    ## 396  Nurse      56 SBP.replicate.3   110
    ## 397  Nurse      57 SBP.replicate.3   106
    ## 398  Nurse      58 SBP.replicate.3   178
    ## 399  Nurse      59 SBP.replicate.3   154
    ## 400  Nurse      60 SBP.replicate.3   132
    ## 401  Nurse      61 SBP.replicate.3   188
    ## 402  Nurse      62 SBP.replicate.3    86
    ## 403  Nurse      63 SBP.replicate.3   124
    ## 404  Nurse      64 SBP.replicate.3   140
    ## 405  Nurse      65 SBP.replicate.3   142
    ## 406  Nurse      66 SBP.replicate.3   154
    ## 407  Nurse      67 SBP.replicate.3   138
    ## 408  Nurse      68 SBP.replicate.3   114
    ## 409  Nurse      69 SBP.replicate.3   122
    ## 410  Nurse      70 SBP.replicate.3   134
    ## 411  Nurse      71 SBP.replicate.3   228
    ## 412  Nurse      72 SBP.replicate.3   134
    ## 413  Nurse      73 SBP.replicate.3   152
    ## 414  Nurse      74 SBP.replicate.3    88
    ## 415  Nurse      75 SBP.replicate.3   176
    ## 416  Nurse      76 SBP.replicate.3   180
    ## 417  Nurse      77 SBP.replicate.3   124
    ## 418  Nurse      78 SBP.replicate.3   120
    ## 419  Nurse      79 SBP.replicate.3   106
    ## 420  Nurse      80 SBP.replicate.3   106
    ## 421  Nurse      81 SBP.replicate.3   130
    ## 422  Nurse      82 SBP.replicate.3    94
    ## 423  Nurse      83 SBP.replicate.3   114
    ## 424  Nurse      84 SBP.replicate.3   100
    ## 425  Nurse      85 SBP.replicate.3   112
    ## 426 Device       1 SBP.replicate.3   124
    ## 427 Device       2 SBP.replicate.3   128
    ## 428 Device       3 SBP.replicate.3    98
    ## 429 Device       4 SBP.replicate.3   135
    ## 430 Device       5 SBP.replicate.3   124
    ## 431 Device       6 SBP.replicate.3   136
    ## 432 Device       7 SBP.replicate.3   112
    ## 433 Device       8 SBP.replicate.3   135
    ## 434 Device       9 SBP.replicate.3   122
    ## 435 Device      10 SBP.replicate.3   111
    ## 436 Device      11 SBP.replicate.3   111
    ## 437 Device      12 SBP.replicate.3   125
    ## 438 Device      13 SBP.replicate.3   122
    ## 439 Device      14 SBP.replicate.3   114
    ## 440 Device      15 SBP.replicate.3   126
    ## 441 Device      16 SBP.replicate.3   137
    ## 442 Device      17 SBP.replicate.3   115
    ## 443 Device      18 SBP.replicate.3   112
    ## 444 Device      19 SBP.replicate.3   113
    ## 445 Device      20 SBP.replicate.3   133
    ## 446 Device      21 SBP.replicate.3   166
    ## 447 Device      22 SBP.replicate.3   140
    ## 448 Device      23 SBP.replicate.3   154
    ## 449 Device      24 SBP.replicate.3   170
    ## 450 Device      25 SBP.replicate.3   154
    ## 451 Device      26 SBP.replicate.3   141
    ## 452 Device      27 SBP.replicate.3   154
    ## 453 Device      28 SBP.replicate.3   131
    ## 454 Device      29 SBP.replicate.3   226
    ## 455 Device      30 SBP.replicate.3   184
    ## 456 Device      31 SBP.replicate.3   106
    ## 457 Device      32 SBP.replicate.3   124
    ## 458 Device      33 SBP.replicate.3   124
    ## 459 Device      34 SBP.replicate.3   125
    ## 460 Device      35 SBP.replicate.3   125
    ## 461 Device      36 SBP.replicate.3    96
    ## 462 Device      37 SBP.replicate.3   127
    ## 463 Device      38 SBP.replicate.3   207
    ## 464 Device      39 SBP.replicate.3   146
    ## 465 Device      40 SBP.replicate.3   138
    ## 466 Device      41 SBP.replicate.3   102
    ## 467 Device      42 SBP.replicate.3    97
    ## 468 Device      43 SBP.replicate.3   137
    ## 469 Device      44 SBP.replicate.3   171
    ## 470 Device      45 SBP.replicate.3   116
    ## 471 Device      46 SBP.replicate.3   147
    ## 472 Device      47 SBP.replicate.3   137
    ## 473 Device      48 SBP.replicate.3   166
    ## 474 Device      49 SBP.replicate.3   136
    ## 475 Device      50 SBP.replicate.3   124
    ## 476 Device      51 SBP.replicate.3   189
    ## 477 Device      52 SBP.replicate.3   109
    ## 478 Device      53 SBP.replicate.3   124
    ## 479 Device      54 SBP.replicate.3   113
    ## 480 Device      55 SBP.replicate.3   104
    ## 481 Device      56 SBP.replicate.3   132
    ## 482 Device      57 SBP.replicate.3   115
    ## 483 Device      58 SBP.replicate.3   196
    ## 484 Device      59 SBP.replicate.3   161
    ## 485 Device      60 SBP.replicate.3   154
    ## 486 Device      61 SBP.replicate.3   189
    ## 487 Device      62 SBP.replicate.3   101
    ## 488 Device      63 SBP.replicate.3   141
    ## 489 Device      64 SBP.replicate.3   148
    ## 490 Device      65 SBP.replicate.3   166
    ## 491 Device      66 SBP.replicate.3   179
    ## 492 Device      67 SBP.replicate.3   139
    ## 493 Device      68 SBP.replicate.3   192
    ## 494 Device      69 SBP.replicate.3   133
    ## 495 Device      70 SBP.replicate.3   142
    ## 496 Device      71 SBP.replicate.3   224
    ## 497 Device      72 SBP.replicate.3   192
    ## 498 Device      73 SBP.replicate.3   152
    ## 499 Device      74 SBP.replicate.3    88
    ## 500 Device      75 SBP.replicate.3   181
    ## 501 Device      76 SBP.replicate.3   195
    ## 502 Device      77 SBP.replicate.3   148
    ## 503 Device      78 SBP.replicate.3   219
    ## 504 Device      79 SBP.replicate.3   126
    ## 505 Device      80 SBP.replicate.3   213
    ## 506 Device      81 SBP.replicate.3   134
    ## 507 Device      82 SBP.replicate.3   137
    ## 508 Device      83 SBP.replicate.3   126
    ## 509 Device      84 SBP.replicate.3   134
    ## 510 Device      85 SBP.replicate.3   128

``` r
bp3 <- group_by (bp2, Method) # grouping methods 
bp3
```

    ## Source: local data frame [510 x 4]
    ## Groups: Method [2]
    ## 
    ##    Method Subject      Replicates Value
    ##    (fctr)   (int)           (chr) (int)
    ## 1   Nurse       1 SBP.replicate.1   100
    ## 2   Nurse       2 SBP.replicate.1   108
    ## 3   Nurse       3 SBP.replicate.1    76
    ## 4   Nurse       4 SBP.replicate.1   108
    ## 5   Nurse       5 SBP.replicate.1   124
    ## 6   Nurse       6 SBP.replicate.1   122
    ## 7   Nurse       7 SBP.replicate.1   116
    ## 8   Nurse       8 SBP.replicate.1   114
    ## 9   Nurse       9 SBP.replicate.1   100
    ## 10  Nurse      10 SBP.replicate.1   108
    ## ..    ...     ...             ...   ...

``` r
bp4 <- group_by (bp3, Subject) #Telling R that subject number one is always the same subject
bp4
```

    ## Source: local data frame [510 x 4]
    ## Groups: Subject [85]
    ## 
    ##    Method Subject      Replicates Value
    ##    (fctr)   (int)           (chr) (int)
    ## 1   Nurse       1 SBP.replicate.1   100
    ## 2   Nurse       2 SBP.replicate.1   108
    ## 3   Nurse       3 SBP.replicate.1    76
    ## 4   Nurse       4 SBP.replicate.1   108
    ## 5   Nurse       5 SBP.replicate.1   124
    ## 6   Nurse       6 SBP.replicate.1   122
    ## 7   Nurse       7 SBP.replicate.1   116
    ## 8   Nurse       8 SBP.replicate.1   114
    ## 9   Nurse       9 SBP.replicate.1   100
    ## 10  Nurse      10 SBP.replicate.1   108
    ## ..    ...     ...             ...   ...

``` r
plot(x = bp4$Method, y = bp4$Value, col = 'pink')
```

<img src="./figures2blood pressure-1.png"  />

``` r
bp5 <- summarise (bp4, mean (Value))
bp5
```

    ## Source: local data frame [85 x 2]
    ## 
    ##    Subject mean(Value)
    ##      <int>       <dbl>
    ## 1        1   114.50000
    ## 2        2   117.00000
    ## 3        3    88.16667
    ## 4        4   117.50000
    ## 5        5   123.83333
    ## 6        6   133.83333
    ## 7        7   112.00000
    ## 8        8   121.66667
    ## 9        9   113.83333
    ## 10      10   108.33333
    ## ..     ...         ...

``` r
bp6 <- spread (bp4, 'Method', Value)
bp6
```

    ## Source: local data frame [255 x 4]
    ## Groups: Subject [85]
    ## 
    ##    Subject      Replicates Device Nurse
    ##      (int)           (chr)  (int) (int)
    ## 1        1 SBP.replicate.1    122   100
    ## 2        1 SBP.replicate.2    128   106
    ## 3        1 SBP.replicate.3    124   107
    ## 4        2 SBP.replicate.1    121   108
    ## 5        2 SBP.replicate.2    127   110
    ## 6        2 SBP.replicate.3    128   108
    ## 7        3 SBP.replicate.1     95    76
    ## 8        3 SBP.replicate.2     94    84
    ## 9        3 SBP.replicate.3     98    82
    ## 10       4 SBP.replicate.1    127   108
    ## ..     ...             ...    ...   ...

``` r
bp7 <- group_by (bp6, Subject) 
bp7
```

    ## Source: local data frame [255 x 4]
    ## Groups: Subject [85]
    ## 
    ##    Subject      Replicates Device Nurse
    ##      (int)           (chr)  (int) (int)
    ## 1        1 SBP.replicate.1    122   100
    ## 2        1 SBP.replicate.2    128   106
    ## 3        1 SBP.replicate.3    124   107
    ## 4        2 SBP.replicate.1    121   108
    ## 5        2 SBP.replicate.2    127   110
    ## 6        2 SBP.replicate.3    128   108
    ## 7        3 SBP.replicate.1     95    76
    ## 8        3 SBP.replicate.2     94    84
    ## 9        3 SBP.replicate.3     98    82
    ## 10       4 SBP.replicate.1    127   108
    ## ..     ...             ...    ...   ...

``` r
bp8 <- summarise (bp7, mean(Nurse), mean (Device))
bp8
```

    ## Source: local data frame [85 x 3]
    ## 
    ##    Subject mean(Nurse) mean(Device)
    ##      <int>       <dbl>        <dbl>
    ## 1        1   104.33333    124.66667
    ## 2        2   108.66667    125.33333
    ## 3        3    80.66667     95.66667
    ## 4        4   105.33333    129.66667
    ## 5        5   116.00000    131.66667
    ## 6        6   128.66667    139.00000
    ## 7        7   108.66667    115.33333
    ## 8        8   112.00000    131.33333
    ## 9        9   106.66667    121.00000
    ## 10      10   100.00000    116.66667
    ## ..     ...         ...          ...

``` r
plot(bp8$'mean(Nurse)', bp8$'mean(Device)', main = 'Correlation between Device and Nurse Measurement of Blood-Pressure', xlab = 'Nurse', ylab = 'Device', col = 'turquoise', pch = 5, abline(lm(bp8$'mean(Nurse)'~bp8$'mean(Device)')))
```

<img src="./figures2blood pressure-2.png"  />

Question 3
----------
