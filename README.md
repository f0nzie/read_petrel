---
title: "Reading from Petrel"
output:
  html_document: 
    keep_md: yes
---




## Read file


```r
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

## Examples


```r
# get only elements with asterisk in row 1
grep("\\*[0-9]{1,2}", alist_vectors[[1]], value = TRUE, perl=TRUE)
#>  [1] "25*0"  "5*47"  "83*0"  "13*47" "69*0"  "12*34" "16*47" "65*0" 
#>  [9] "13*34" "17*47" "63*0"  "14*34" "17*47" "63*0"  "14*34" "17*47"
#> [17] "63*0"  "15*34" "16*47" "63*0"
```


```r
# get elements with asterisk in row 1417
grep("\\*[0-9]{1,2}", alist_vectors[[1417]], value = TRUE, perl=TRUE)
#>  [1] "15*46" "78*0"  "16*46" "78*0"  "15*46" "80*0"  "13*46" "83*0" 
#>  [9] "9*46"  "87*0"  "5*46"  "667*0"
```

## Extract only numbers that have asterisk in the middle


```r
# extract with regex
wanted <- lapply(alist_vectors, function(x) grep("\\*[0-9]{1,2}", x, 
                                                 value = TRUE, perl=TRUE))
wanted[[1]]   # show a sample
#>  [1] "25*0"  "5*47"  "83*0"  "13*47" "69*0"  "12*34" "16*47" "65*0" 
#>  [9] "13*34" "17*47" "63*0"  "14*34" "17*47" "63*0"  "14*34" "17*47"
#> [17] "63*0"  "15*34" "16*47" "63*0"
```

### Create a dataframe with row and value


```r
# extract row and values. Method #1
df <- data.frame()
invisible(lapply(seq_along(wanted), function(x) {
    r <- data.frame(row=x, value=wanted[[x]], stringsAsFactors = FALSE)
    df <<- rbind(df, r)
}))
tibble::as.tibble(df) 
#> # A tibble: 24,147 x 2
#>      row value
#>    <int> <chr>
#>  1     1 25*0 
#>  2     1 5*47 
#>  3     1 83*0 
#>  4     1 13*47
#>  5     1 69*0 
#>  6     1 12*34
#>  7     1 16*47
#>  8     1 65*0 
#>  9     1 13*34
#> 10     1 17*47
#> # ... with 24,137 more rows
```



```r
# extract row and values. Method #2
df <- data.frame()
for (i in 1:length(wanted)) {
    x <- data.frame(row=i, value=wanted[[i]], stringsAsFactors = FALSE)
    df <- rbind(df, x)
}
tibble::as.tibble(df) 
#> # A tibble: 24,147 x 2
#>      row value
#>    <int> <chr>
#>  1     1 25*0 
#>  2     1 5*47 
#>  3     1 83*0 
#>  4     1 13*47
#>  5     1 69*0 
#>  6     1 12*34
#>  7     1 16*47
#>  8     1 65*0 
#>  9     1 13*34
#> 10     1 17*47
#> # ... with 24,137 more rows
```


```r
# get specific row from the original file
library(dplyr)

df %>% 
    filter(row==1)
#>    row value
#> 1    1  25*0
#> 2    1  5*47
#> 3    1  83*0
#> 4    1 13*47
#> 5    1  69*0
#> 6    1 12*34
#> 7    1 16*47
#> 8    1  65*0
#> 9    1 13*34
#> 10   1 17*47
#> 11   1  63*0
#> 12   1 14*34
#> 13   1 17*47
#> 14   1  63*0
#> 15   1 14*34
#> 16   1 17*47
#> 17   1  63*0
#> 18   1 15*34
#> 19   1 16*47
#> 20   1  63*0
```


```r
# another way to extract row 1
df[df$row==1,]
#>    row value
#> 1    1  25*0
#> 2    1  5*47
#> 3    1  83*0
#> 4    1 13*47
#> 5    1  69*0
#> 6    1 12*34
#> 7    1 16*47
#> 8    1  65*0
#> 9    1 13*34
#> 10   1 17*47
#> 11   1  63*0
#> 12   1 14*34
#> 13   1 17*47
#> 14   1  63*0
#> 15   1 14*34
#> 16   1 17*47
#> 17   1  63*0
#> 18   1 15*34
#> 19   1 16*47
#> 20   1  63*0
```


