library(pacman)
p_load(multidplyr, stringr)

# define basic file path
cmsyResults_file <- './Results/FF.cmsy.stock.status.rds' # catch msy model data path
aquaModel_file <- './Results/FishmealUsage.rds' # FF demand data path
ffdr_file <- './Results/FFDRs.rds'
forageFishDemand_file <- './Results/FishmealUsage.rds'

aquaProd_file <- './03-Input/aquaculture.production.csv'
aquaValue_file <- './03-Input/aquaculture.value.csv'
taxaInfo_file <- './03-Input/aquaculture.taxa.csv'
forageFishCatch_path <- './03-Input/ff.landing.csv'

forageFishCatch <- read_csv(forageFishCatch_path)
taxaInfo <- read_csv(taxaInfo_file)
Aquaculture_prod <- read_csv(aquaProd_file)
Aquaculture_value <- read_csv(aquaValue_file)
ffdr <- read_rds(ffdr_file)
ffDemand <- read_csv(forageFishDemand_file)

# 01.  Taxa weights -------------------------------------------------------
# production data wide to long
Aquaculture_prod <- Aquaculture_prod  %>% 
    pivot_longer(-c(Country, Name_en, 
                    Environment),
                 names_to = 'Year',
                 values_to = 'Production') %>% 
    filter(Year >= 2017) %>%  # 5-year average
    group_by(Country, Name_en, 
             Year, Environment) %>%
    summarise(Production = sum(Production, na.rm = T)) %>%
    ungroup()

# value data wide to long
Aquaculture_value <- Aquaculture_value %>% 
    pivot_longer(-c(Country, Name_en, 
                    Environment),
                 names_to = 'Year',
                 values_to = 'Value') %>% 
    filter(Year >= 2017) %>% 
    group_by(Country, Name_en, Year, Environment) %>%
    summarise(Value = sum(Value, na.rm = T)) %>%
    ungroup() 

# combine aquaculture data
AquaUnitPrice <- Aquaculture_prod %>%
    left_join(Aquaculture_value, 
              by = c('Country', 'Name_en',
                     'Year', 'Environment')) %>% 
    mutate(UnitPrice = Value/Production) %>% 
    select(-Value, -Production) %>% 
    pivot_wider(id_cols = c(Country, Name_en, Environment), 
                names_from = Year, 
                values_from = UnitPrice) 

write_csv(AquaUnitPrice, './Results/Aquacuture.Unit.Price.csv')

# ffdr preprocess
ffdr <- ffdr %>% 
    rename(FFDR = meanFFDR) 

# Forage fish demand data
forageFishDemand <- read_rds(forageFishDemand_file)
forageFishDemand <- forageFishDemand %>% 
    mutate(Year = as.numeric(Year)) %>%
    select(-starts_with('...')) %>%
    filter(Year >= 2017) %>% 
    group_by(Taxa) %>% 
    summarise(ffDemand = mean(meanMO)*5.2) %>% 
    ungroup() %>% 
    mutate(demandRatio = ffDemand/sum(ffDemand))

# calculate taxa weights
AquacultureWeights <- taxaInfo %>%
    left_join(AquaUnitPrice, 
              by = c('Country', 'Name_en', 'Environment')) %>% 
    drop_na() %>% 
    # some species cannot be found in database #drop# them all, 
    # this is caused by no production in year 2014- 2018
    pivot_longer(-c(Country, `Name_en`, ISSCAAP_Group,
                    Taxa, Environment, `Scientific_name`),
                 names_to = 'Year', 
                 values_to = 'UnitPrice') %>%
    group_by(Taxa) %>%
    summarise(UnitPrice_avg = mean(UnitPrice, na.rm = T)) %>% 
    ungroup() %>%
    mutate(Taxa = str_to_title(Taxa)) %>%
    mutate(Taxa = if_else(Taxa == 'Marine Crustacean',
                          'Marine Crustaceans',
                          Taxa)) %>% 
    full_join(ffdr, by = c('Taxa')) %>%
    full_join(forageFishDemand, by = c('Taxa')) %>% 
    select(-ffDemand) %>% 
    mutate(weight = FFDR * demandRatio/UnitPrice_avg)

lossAlloc <- AquacultureWeights$weight
names(lossAlloc) <- AquacultureWeights$Taxa

# 02.  Country weights ----------------------------------------------------
# Here define 3 types of country weights at once for convenience purpose
# However only one or multiple plan would be actual used 

AquacultureOverview <- Aquaculture_prod %>%
    left_join(Aquaculture_value, 
              by = c('Country', 'Name_en',
                     'Year', 'Environment')) 

CountryInfo <- AquacultureOverview %>% 
    group_by(Country, Year) %>% 
    summarise(totalProd = sum(Production, 
                              na.rm = T),
              totalValue = sum(Value, 
                               na.rm = T)) %>% 
    ungroup() %>% 
    filter(totalProd != 0,
           totalValue != 0,
           Year >= 2017) %>%
    mutate(UnitPrice = totalValue/totalProd) %>% 
    group_by(Country) %>% 
    summarise(avgProd = mean(totalProd,
                             na.rm = T),
              avgValue = mean(totalValue, 
                              na.rm = T),
              avgUnitPrice = mean(UnitPrice, 
                                  na.rm = T)) %>% 
    ungroup() %>% 
    mutate(prod_w = avgProd/sum(avgProd),
           value_w = avgValue/sum(avgValue),
           uprice_w = avgUnitPrice/sum(avgUnitPrice,
                                       na.rm = T)) %>%  
    select(-avgProd, -avgValue, -avgUnitPrice)

prodWeight <- CountryInfo$prod_w
names(prodWeight) <- CountryInfo$Country

valueWeight <- CountryInfo$value_w
names(valueWeight) <- CountryInfo$Country

upriceWeight <- CountryInfo$uprice_w
names(upriceWeight) <- CountryInfo$Country


# 03. Forage fish average catch for last 5 years ---------------------------
forageFishCatch_5yr_avg <- forageFishCatch %>% 
    filter(Year >= 2017) %>% 
    group_by(Year) %>% 
    summarise(totalCatch = sum(catch)) %>% 
    ungroup() %>% 
    pull(totalCatch) %>% 
    mean()

# 04. Aquaculture production by taxa group --------------------------------
AquaProd_taxa <- taxaInfo %>% 
    left_join(Aquaculture_prod, 
              by = c('Country', 'Name_en', 'Environment')) %>% 
    filter(Year == 2022) %>% 
    group_by(Taxa) %>% 
    summarise(Production = sum(Production, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(Taxa = str_to_title(Taxa)) %>%
    mutate(Taxa = if_else(Taxa == 'Marine Crustacean',
                          'Marine Crustaceans',
                          Taxa))

# 06. Combine all data into single list -----------------------------------
# 1. Taxa weights
# 2. Country weights
# 3. BoF information
# 4. 2014- 2018 annual average forage fish catch
# 5. Marine culture productions in 2018 by taxa
# 6. Forage fish landing data
# 7. Forage fish demand data

# Forage fish demand data
ffDemand <- forageFishDemand$ffDemand
names(ffDemand) <- forageFishDemand$Taxa

modelInput <- list(
    taxaWeights = lossAlloc,
    foragefish_5yr_landing = forageFishCatch_5yr_avg,
    taxaCulture = AquaProd_taxa,
    foragefish_landing_raw = forageFishCatch,
    foragefish_demand = ffDemand
)

write_rds(modelInput,
          './Results/Senario.model.input.rds', 
          compress = 'gz')
