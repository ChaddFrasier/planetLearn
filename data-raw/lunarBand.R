library(dplyr)
library(usethis)

# set column names for the data features and label column ( DN )
col_names <- c("DN", "Phase", "Emission", "Incidence", "LEmission", "LIncidence", "Lat", "Long", "SunAz", "CraftAz")

lunarData <- readr::read_delim( "data/data-raw/band1unit7.dat", delim = " ", col_names = col_names, trim_ws = TRUE )

lunarData <- as.data.frame( lunarData )
lunarData <- lunarData[ lunarData$Incidence <= 70 ]

use_data( lunarData, overwrite = TRUE, compress = "xz" )
