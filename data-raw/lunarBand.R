library(dplyr)
library(usethis)

# set column names for the data features and label column ( DN )
col_names <- c("DN", "Phase", "Emission", "Incidence", "LEmission", "LIncidence", "Lat", "Long", "SunAz", "CraftAz")

lunarData <- readr::read_delim( "data-raw/band1unit7.dat", delim = " ", col_names = col_names, trim_ws = TRUE )
lunarData2 <- readr::read_delim( "data-raw/band2unit7.dat", delim = " ", col_names = col_names, trim_ws = TRUE )

lunarData <- as.data.frame( lunarData )
lunarData2 <- as.data.frame( lunarData2 )

lunarData <- lunarData[ lunarData$Incidence <= 70,  ]
lunarData2 <- lunarData2[ lunarData2$Incidence <= 70, ]

use_data( lunarData, overwrite = TRUE, compress = "xz" )
use_data( lunarData2, overwrite = TRUE, compress = "xz")
