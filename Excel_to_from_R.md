excel import export to R
================

### Packages

Turi buti instaliuoti packages **“readxl”** ir **“writexl”**.
Instrukcijos importavimui paimtos [is
cia](https://datatofish.com/import-excel-r/), eksportavimui i atskirus
to paties failo lapus [is
cia](https://www.statology.org/r-export-to-excel-multiple-sheets/).  
install.packages(“readxl”)  
install.packages(“writexl”)

### Laibrariai

``` r
library(readxl)
library(openxlsx)
```

### Importavimas

Svarbu nurodant path naudoti **dviguba slasha**, jei viengubas meta
error

``` r
sheet1 <- read_excel("C:\\Users\\Grazina S\\OneDrive - LAMMC\\R programa\\import_to_R.xlsx", sheet = "Sheet1")
sheet2 <- read_excel("C:\\Users\\Grazina S\\OneDrive - LAMMC\\R programa\\import_to_R.xlsx", sheet = "Sheet2")
```

### Eksportavimas

Nurodyti path nereikia, failas atsiras working direktorijoje.  
Pirma padaromas list kur nurodyta, kaip vadinsis lapai, i kuriuos eis
datafreimai. Paskui excel jie bus tokiu eiliskumu kaip tame vardu liste

``` r
sheet_names <- list('lapas1' = sheet1, 'lapas2' = sheet2)
openxlsx::write.xlsx(sheet_names, file = 'test_export.xlsx')
```
