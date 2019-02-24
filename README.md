---
title: "Reading from Petrel"
output:
  html_document: 
    keep_md: yes
---



# Objective
Convert the sparse matrix representation of coordinates in a `GRDECL` file to a matrix of `n` rows and 10 columns in R.


# Transforming a `GRDECL` file to a matrix

## Read file


```r
library(dplyr)

# read the Petrel/Eclipse file. Assign it to tbl object
tbl <- read.csv("wellRegion.GRDECL", skip = 1, header = FALSE, 
         stringsAsFactors = FALSE, strip.white = TRUE)
head(tbl)
#>                                                                                                                                  V1
#> 1  25*0 5*47 83*0 34 34 13*47 69*0 12*34 16*47 65*0 13*34 17*47 63*0 14*34 17*47 63*0 14*34 17*47 63*0 15*34 16*47 63*0 34 34 34 34
#> 2  10*34 17*47 63*0 15*34 16*47 63*0 15*34 15*47 64*0 15*34 14*47 6*37 59*0 15*34 22 22 22 22 6*47 5*23 6*37 59*0 13*34 22 22 22 22
#> 3  5*22 7*23 10*37 55*0 11*34 31 31 9*22 6*23 11*37 55*0 7*34 6*31 8*22 7*23 13*37 54*0 34 34 34 34 8*31 8*22 7*23 13*37 55*0 31 31
#> 4 10*31 7*22 23 23 23 26 23 23 23 13*37 55*0 12*31 6*22 23 23 23 23 26 26 23 23 13*37 55*0 12*31 6*22 23 23 23 23 26 26 26 26 37 37
#> 5  11*37 55*0 12*31 5*22 5*23 26 26 26 13*37 21 21 54*0 13*31 22 22 22 22 8*23 12*37 21 21 21 21 53*0 13*31 22 22 22 27 27 23 23 23
#> 6  23 23 23 23 10*37 6*21 54*0 11*31 45 45 24 24 27 27 27 24 24 24 24 23 23 9*37 8*21 53*0 9*31 5*45 24 27 27 27 6*24 7*41 21 21 21
tail(tbl)
#>                                                                                                                                      V1
#> 1412  13 13 7*6 10*53 14*55 30*0 42 42 42 42 14*43 10*15 13 13 13 10*6 9*53 13*55 31*0 42 42 16*43 5*15 5*28 14*6 8*53 12*55 34*0 43 43
#> 1413   14*43 15 15 8*28 14*6 8*53 11*55 34*0 18*43 9*28 15*6 7*53 10*55 35*0 18*43 8*28 17*6 5*53 12*46 34*0 18*43 28 28 28 28 50 50 50
#> 1414 50 50 50 15*6 0 0 0 0 14*46 34*0 17*43 13*50 12*6 0 0 0 16*46 33*0 17*43 14*50 10*6 0 0 0 0 16*46 33*0 17*43 15*50 9*6 0 0 0 46 46
#> 1415  16*46 33*0 16*43 17*50 6*6 0 0 0 0 18*46 33*0 15*43 20*50 6 6 6 0 0 0 0 20*46 33*0 14*43 0 20*50 6*0 20*46 34*0 11*43 0 0 0 50 50
#> 1416 17*50 7*0 20*46 36*0 8*43 5*0 18*50 7*0 20*46 50*0 16*50 8*0 20*46 51*0 14*50 9*0 20*46 52*0 12*50 11*0 19*46 54*0 8*50 14*0 46 46
#> 1417                                                                 15*46 78*0 16*46 78*0 15*46 80*0 13*46 83*0 9*46 87*0 5*46 667*0 /
```


```r
# dimensions of the dataframe
dim(tbl)
#> [1] 1417    1
```



```r
# convert string to character vectors.
alist_vectors <- lapply(tbl$V1, function(x) unlist(strsplit(x, " ")))
alist_vectors[[1]]  # show a sample
#>  [1] "25*0"  "5*47"  "83*0"  "34"    "34"    "13*47" "69*0"  "12*34"
#>  [9] "16*47" "65*0"  "13*34" "17*47" "63*0"  "14*34" "17*47" "63*0" 
#> [17] "14*34" "17*47" "63*0"  "15*34" "16*47" "63*0"  "34"    "34"   
#> [25] "34"    "34"
```


```r
# number of rows in the list
length(alist_vectors)
#> [1] 1417
```



```r
# show the number of elements in each row
len_str <- unlist(lapply(tbl$V1, function(x) 
    length(unlist(strsplit(trimws(x), " ")))))

head(len_str)
#> [1] 26 27 28 35 33 35
tail(len_str)
#> [1] 30 28 35 34 26 13
length(len_str)   # total number of rows in the file
#> [1] 1417
```

## Extract elements from file


```r
# extract row and values for all the strings in the file
# including numbers without the asterisk
all_df <- data.frame()
invisible(lapply(seq_along(alist_vectors), function(x) {
    r <- data.frame(row=x, value=alist_vectors[[x]], stringsAsFactors = FALSE)
    all_df <<- rbind(all_df, r)
}))
tibble::as_tibble(all_df) 
#> # A tibble: 44,610 x 2
#>      row value
#>    <int> <chr>
#>  1     1 25*0 
#>  2     1 5*47 
#>  3     1 83*0 
#>  4     1 34   
#>  5     1 34   
#>  6     1 13*47
#>  7     1 69*0 
#>  8     1 12*34
#>  9     1 16*47
#> 10     1 65*0 
#> # ... with 44,600 more rows
```


```r
# dimensions of the dataframe
dim(all_df)
#> [1] 44610     2
```


## Remove non-numeric elements


```r
# example
# we provide a sample of what will results if we apply grep/grepl
# the asterisk in between numbers is accepted as valied with regex "\\d"
grep("\\d", c("10*5", "34", "7*7", "/", "3*5", "*", "\\", "-",  "\\."), value = TRUE)
#> [1] "10*5" "34"   "7*7"  "3*5"
```


```r
# we proceed to remove extraneous characters from the value variable
all_df %>% 
  filter(grepl("\\d", value)) %>% 
  as_tibble() %>% 
  print() -> all_df
#> # A tibble: 44,609 x 2
#>      row value
#>    <int> <chr>
#>  1     1 25*0 
#>  2     1 5*47 
#>  3     1 83*0 
#>  4     1 34   
#>  5     1 34   
#>  6     1 13*47
#>  7     1 69*0 
#>  8     1 12*34
#>  9     1 16*47
#> 10     1 65*0 
#> # ... with 44,599 more rows
```


```r
# dimensions of the dataframe
dim(all_df)
#> [1] 44609     2
```

Notice that a row has been removed from the GRDECL file. If you go to the raw data file you will see that the end character in the files is "\\". That characters has been removed.

## Parse the string elements and convert 


```r
all_df1 <- 
all_df %>% 
  mutate(A = as.integer(gsub("\\*.*", "", value))) %>%  # extract the part on the left
  mutate(B = as.integer(gsub("(^[0-9]{1,2}|.*\\*)", "", value))) %>%  # part on the right
  rowwise() %>%                                        # operate on the row
  mutate(C = ifelse(!is.na(B), list(rep(B, A)), A)) %>%   # repeat when A and B are different
  as_tibble() %>% 
  print()
#> # A tibble: 44,609 x 5
#>      row value     A     B C         
#>    <int> <chr> <int> <int> <list>    
#>  1     1 25*0     25     0 <int [25]>
#>  2     1 5*47      5    47 <int [5]> 
#>  3     1 83*0     83     0 <int [83]>
#>  4     1 34       34    NA <int [1]> 
#>  5     1 34       34    NA <int [1]> 
#>  6     1 13*47    13    47 <int [13]>
#>  7     1 69*0     69     0 <int [69]>
#>  8     1 12*34    12    34 <int [12]>
#>  9     1 16*47    16    47 <int [16]>
#> 10     1 65*0     65     0 <int [65]>
#> # ... with 44,599 more rows
```


## Testing the correct parsing

Verifying from the file for the first 9 rows:

    1 25*0			
    2 5*47			
    3 83*0			
    4 34			
    5 34			
    6 13*47			
    7 69*0			
    8 12*34			
    9 16*47			

Corresponds to the follwoing matrix:


```r
matrix(c(
  all_df1$C[[1]], # 1 25*0			
  all_df1$C[[2]], # 2 5*47
  all_df1$C[[3]], # 3 83*0	
  all_df1$C[[4]], # 4 34
  all_df1$C[[5]], # 5 34
  all_df1$C[[6]], # 6 13*47	
  all_df1$C[[7]], # 7 69*0
  all_df1$C[[8]], # 8 12*34	
  all_df1$C[[9]]  # 9 16*47	
  ), ncol=10, byrow = TRUE)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    0    0    0    0    0    0    0    0    0     0
#>  [2,]    0    0    0    0    0    0    0    0    0     0
#>  [3,]    0    0    0    0    0   47   47   47   47    47
#>  [4,]    0    0    0    0    0    0    0    0    0     0
#>  [5,]    0    0    0    0    0    0    0    0    0     0
#>  [6,]    0    0    0    0    0    0    0    0    0     0
#>  [7,]    0    0    0    0    0    0    0    0    0     0
#>  [8,]    0    0    0    0    0    0    0    0    0     0
#>  [9,]    0    0    0    0    0    0    0    0    0     0
#> [10,]    0    0    0    0    0    0    0    0    0     0
#> [11,]    0    0    0    0    0    0    0    0    0     0
#> [12,]    0    0    0   34   34   47   47   47   47    47
#> [13,]   47   47   47   47   47   47   47   47    0     0
#> [14,]    0    0    0    0    0    0    0    0    0     0
#> [15,]    0    0    0    0    0    0    0    0    0     0
#> [16,]    0    0    0    0    0    0    0    0    0     0
#> [17,]    0    0    0    0    0    0    0    0    0     0
#> [18,]    0    0    0    0    0    0    0    0    0     0
#> [19,]    0    0    0    0    0    0    0    0    0     0
#> [20,]    0    0    0    0    0    0    0   34   34    34
#> [21,]   34   34   34   34   34   34   34   34   34    47
#> [22,]   47   47   47   47   47   47   47   47   47    47
#> [23,]   47   47   47   47   47    0    0    0    0     0
```

## Convert dataframe variable to matrix

```r
sa <- sapply(all_df1$C, c)                   # dataframe var to list of vectors
m <- matrix(unlist(sa), ncol=10, byrow=TRUE) # list of vectors to matrix. 10 cols
dim(m)                                       # dimensions of the matrix
#> [1] 45675    10
```

### Extract the first row from the matrix


```r
m[1, ]
#>  [1] 0 0 0 0 0 0 0 0 0 0
```

### Extract the third row from the matrix


```r
m[3, ]
#>  [1]  0  0  0  0  0 47 47 47 47 47
```

### Extract the row 21 from the matrix


```r
m[21, ]
#>  [1] 34 34 34 34 34 34 34 34 34 47
```

### Extract the last row from the matrix


```r
m[nrow(m),]
#>  [1] 0 0 0 0 0 0 0 0 0 0
```

# Convert to a sparse matrix
Finally we convert the dense matrix to a sparse matrix.


```r
library(Matrix)

ms <- Matrix(m, sparse = TRUE)
ms[1:50,]
#> 50 x 10 sparse Matrix of class "dgCMatrix"
#>                                    
#>  [1,]  .  .  .  .  .  .  .  .  .  .
#>  [2,]  .  .  .  .  .  .  .  .  .  .
#>  [3,]  .  .  .  .  . 47 47 47 47 47
#>  [4,]  .  .  .  .  .  .  .  .  .  .
#>  [5,]  .  .  .  .  .  .  .  .  .  .
#>  [6,]  .  .  .  .  .  .  .  .  .  .
#>  [7,]  .  .  .  .  .  .  .  .  .  .
#>  [8,]  .  .  .  .  .  .  .  .  .  .
#>  [9,]  .  .  .  .  .  .  .  .  .  .
#> [10,]  .  .  .  .  .  .  .  .  .  .
#> [11,]  .  .  .  .  .  .  .  .  .  .
#> [12,]  .  .  . 34 34 47 47 47 47 47
#> [13,] 47 47 47 47 47 47 47 47  .  .
#> [14,]  .  .  .  .  .  .  .  .  .  .
#> [15,]  .  .  .  .  .  .  .  .  .  .
#> [16,]  .  .  .  .  .  .  .  .  .  .
#> [17,]  .  .  .  .  .  .  .  .  .  .
#> [18,]  .  .  .  .  .  .  .  .  .  .
#> [19,]  .  .  .  .  .  .  .  .  .  .
#> [20,]  .  .  .  .  .  .  . 34 34 34
#> [21,] 34 34 34 34 34 34 34 34 34 47
#> [22,] 47 47 47 47 47 47 47 47 47 47
#> [23,] 47 47 47 47 47  .  .  .  .  .
#> [24,]  .  .  .  .  .  .  .  .  .  .
#> [25,]  .  .  .  .  .  .  .  .  .  .
#> [26,]  .  .  .  .  .  .  .  .  .  .
#> [27,]  .  .  .  .  .  .  .  .  .  .
#> [28,]  .  .  .  .  .  .  .  .  .  .
#> [29,]  .  .  .  .  .  .  .  .  .  .
#> [30,] 34 34 34 34 34 34 34 34 34 34
#> [31,] 34 34 34 47 47 47 47 47 47 47
#> [32,] 47 47 47 47 47 47 47 47 47 47
#> [33,]  .  .  .  .  .  .  .  .  .  .
#> [34,]  .  .  .  .  .  .  .  .  .  .
#> [35,]  .  .  .  .  .  .  .  .  .  .
#> [36,]  .  .  .  .  .  .  .  .  .  .
#> [37,]  .  .  .  .  .  .  .  .  .  .
#> [38,]  .  .  .  .  .  .  .  .  .  .
#> [39,]  .  .  . 34 34 34 34 34 34 34
#> [40,] 34 34 34 34 34 34 34 47 47 47
#> [41,] 47 47 47 47 47 47 47 47 47 47
#> [42,] 47 47 47 47  .  .  .  .  .  .
#> [43,]  .  .  .  .  .  .  .  .  .  .
#> [44,]  .  .  .  .  .  .  .  .  .  .
#> [45,]  .  .  .  .  .  .  .  .  .  .
#> [46,]  .  .  .  .  .  .  .  .  .  .
#> [47,]  .  .  .  .  .  .  .  .  .  .
#> [48,]  .  .  .  .  .  .  . 34 34 34
#> [49,] 34 34 34 34 34 34 34 34 34 34
#> [50,] 34 47 47 47 47 47 47 47 47 47
```


# References
* http://petrofaq.org/wiki/Eclipse_Input_Data
* http://www.johnmyleswhite.com/notebook/2011/10/31/using-sparse-matrices-in-r/



