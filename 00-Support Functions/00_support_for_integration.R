library(tidyverse)

read_FAO_raw <- function(FAOCatch_file){
  df <- read.csv(FAOCatch_file) %>%
      select(-starts_with('S')) %>%
      rename_all(~str_remove(., '\\.')) %>%
      rename(Country = `Country.Name.`,
             English_name = `ASFISspecies..Name.`,
             FAO_area = `FAOmajor.fishing.area..Name.`,
             Production_source = `Detailedproduction.source..Name.`,
             Unit = `Unit.Name.`) %>%
      rename_all(~str_remove(., '\\.'))
  return(df)
}
