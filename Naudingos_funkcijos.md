Naudingos funkcijos
======================

## Funkcija suvartyti keleta stulpeliu is karto i factor
***columns_to_convert*** turi buti kabutese.
``` r
convertToFactors <- function(df, columns_to_convert) {
  for (col_name in columns_to_convert) {
    if (col_name %in% names(df)) {
      df[[col_name]] <- as.factor(df[[col_name]])
    } else {
      cat(paste("Column", col_name, "not found in the data frame.\n"))
    }
  }
  return(df)
}
```
## Funkcija paimti is HSD outputo statistika ir grupes
***factor_name*** turi buti kabutese<br>
pvz *grass_stat <- hsd_to_table(gras_hsd, "year_distr")*

``` r
hsd_to_table <- function(hsd_output,factor_name) {
  df <- hsd_output$means
  df <- cbind(col1 = rownames(df), df)
  row.names(df) <- NULL
  colnames(df)[1] <- factor_name
  df <- df %>% arrange(desc(.[[2]]))
  df$group <- hsd_output$groups$groups
  return(df)
}
```
## Jaccard index for binary data
Taikoma kai duomenys yra 0 1
``` r
jaccard <- function(M, user1, user2) {
  sums = rowSums(M[,c(user1, user2)])
  
  similarity = length(sums[sums==2])
  total = length(sums[sums==1]) + similarity
  
  similarity/total
}
```
## Kaip gauti Jaccard indeksu lentele
**df** yra duomenu failas

``` r
num_cols <- ncol(df)
result_matrix <- matrix(0, nrow = num_cols, ncol = num_cols)

# Loop through all pairs of columns
for (i in 1:num_cols) {
  for (j in 1:num_cols) {
    if (i != j) {
      similarity <- jaccard(df, i, j)
      result_matrix[i, j] <- similarity
    }
  }
}

# Create a dataframe from the result matrix
result_df <- as.data.frame(result_matrix)

# Rename the rows and columns with the column names from df
rownames(result_df) <- colnames(df)
colnames(result_df) <- colnames(df)
```
