library(pacman)
p_load(tidyverse)
# 01 Define raw data path 
# Aquaculture production by quantity
aquaProd_file <- './03-Input/aquaculture.production.csv'

# define range 
AquaRef <- './03-Input/aquaculture.species.list'

# taxa info
taxaList_path <- './03-Input/aquaculture.taxa.csv'

# 02 define research range 
# filter taxa of farming by Hadley（2018）

reach_range <- readLines(AquaRef)

# 03 FAO aquaculture production 
Aquaculture_prod <- read_csv(aquaProd_file)  |> 
    select(-Environment) |> 
    pivot_longer(-c(Country, Name_en),
                 names_to = 'Year',
                 values_to = 'Production') |> 
    filter(Year >= 2012)  |>   # Using only data of 10 years before(2009 - 2018)
    group_by(Country, Name_en, Year)  |> 
    summarise(Production = sum(Production, na.rm = T))  |> 
    ungroup()  |>  
    mutate(Identifier = paste0(Country, '-', Name_en))

# 04 aquaculture taxa data

taxaList <- read_csv(taxaList_path)  |>  
    mutate(Identifier = paste0(Country, '-', Name_en))  |> 
    select(Identifier, Taxa)

# 05 average production of last decade
Ch_Territory <- c('China, Hong Kong SAR', "Taiwan Province of China")

Aquaculture_decade <- Aquaculture_prod |>  
    filter(Identifier %in% reach_range) |> 
    left_join(taxaList, by = 'Identifier')  |> 
    filter(!is.na(Taxa)) |> 
    mutate(Country = if_else(Country %in% Ch_Territory,
                             'China', Country)) |> 
    group_by(Country, Taxa) |> 
    summarise(decade_avg = mean(Production)) |> 
    ungroup() 

# production by country in 2022
Aquaculture_2022 <- Aquaculture_prod  |>  
    filter(Identifier %in% reach_range)  |> 
    left_join(taxaList, by = 'Identifier')  |> 
    filter(!is.na(Taxa)) |> 
    mutate(Country = if_else(Country %in% Ch_Territory,
                             'China', Country)) |> 
    filter(Year == 2022) |> 
    group_by(Country, Taxa) |> 
    summarise(Year_2022 = sum(Production)) |> 
    ungroup()

# 07 Join权重和2018各国产量数据 ----------------------------------------------------

country_taxa_weight <- Aquaculture_decade  |> 
    full_join(Aquaculture_2022, by = c('Country', 'Taxa')) |> 
    filter(Year_2022 != 0)  |> 
    group_by(Taxa) |> 
    mutate(taxa_weight = decade_avg / sum(decade_avg)) |> 
    select(-decade_avg) 

write_rds(country_taxa_weight, 
          './Results/Country.Taxa.Weights.rds')


    