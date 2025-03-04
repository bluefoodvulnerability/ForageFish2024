# load functions

source('./00-Support Functions/02_modelSupport.R')

# define path
modelInput_path <- './Results/Senario.model.input.rds'

# load data

modelData <- read_rds(modelInput_path)

# separate data for use

initWeights <- modelData$taxaWeights # using taxa weights to allocate forage fish loss
FFavgCatch <- modelData$foragefish_5yr_landing
AquaProdRealDF <- modelData$taxaCulture
CountryWeight <- modelData$countryWeight
FFData <- modelData$foragefish_landing_raw
ffDemand <- modelData$foragefish_demand

# get real 2018 production of aqua Taxa

taxonList <- AquaProdRealDF$Taxa
AquaProdReal <- AquaProdRealDF$Production
names(AquaProdReal) <- AquaProdRealDF$Taxa
AquaProdReal <- rep(AquaProdReal, each = 500)

# 01 Scenario model I ----------------------------------------------------

# reduce landings from over-fished and collapsed stocks
# This require the judgment results of forage fishes' stock
# about 11.5% landing loss for 2018

stockStatus_path <- './Results/FF.cmsy.stock.status.rds'
stockStatus <- read_rds(stockStatus_path)
speciesEnv <- read_csv('Data/FF.species.info.env.iucn.csv')

# extract stocks of status Over-exploited and Collapsed

reduceStocksSI <- stockStatus %>%
    filter(status %in% c('Over-exploited', 'Collapsed')) %>%
    pull(stock) %>%
    unique()

# reduce over-exploited/collapsed stocks

reduceQuantitySI <- FFData %>%
    filter(Year == 2022,
           (stock %in% reduceStocksSI)) %>%
    pull(catch) %>%
    sum()

FF_2022 <- FFData %>%
     filter(Year == 2022) %>%
     pull(catch) %>%
     sum()

# 02 Scenario model II -----------------------------------------------------

# add fao area code first

# reference table load

faoAreaInfo_path <- './Data/FAO.area.code.csv'
faoAreaInfo <- read_csv(faoAreaInfo_path)

# define reduce quantity in scenario II
# Area 21 48 and 87 are selected because collapsed stock still contributes in 2018 landing
# so they are identified as lacking of management

reduceQuantitySII <- (FFData %>%
    filter(Year == 2022) %>%
    select(stock, Species, FAO_area, catch) %>%
    left_join(faoAreaInfo, by = 'FAO_area') %>%
    filter(Area_code %in% c(21, 48, 87)) %>%
    pull(catch) %>%
    sum()) * 0.5


# 03 Scenario Model III ---------------------------------------------------

# the landing of poorly protected species loss
# by IUCN code, NT drop 5%, VU drop 10% and N.E. drop 30%

# calculate landing gap

landingSIII <- FFData %>%
    filter(Year == 2022) %>%
    left_join(speciesEnv, by = c('Species' = 'Species')) %>%
    mutate(catch_proj = case_when(IUCN_Code == 'NT'~catch*.5,
                                  IUCN_Code == 'VU'~catch*0,
                                  IUCN_Code == 'N.E.'~catch*.8,
                                  IUCN_Code == 'DD'~catch*.5,
                                  !(IUCN_Code %in% c('NT', 'VU', 'N.E.', 'DD')) ~ catch))

reduceQuantitySIII <- sum(landingSIII$catch) - sum(landingSIII$catch_proj)

# 04 Scenario Model IV ----------------------------------------------------

# climate incurred tropic and polar landing loss
# assuming same effect on tropic and polar area (40%)

# add environment temperature data
# calculate landing gap

landingSIV <- FFData %>%
    filter(Year == 2022) %>%
    left_join(speciesEnv, by = c('Species' = 'Species')) %>%
    mutate(catch_proj = case_when(EnvTemp == 'tropical'~catch*.6,
                                  EnvTemp == 'polar'~catch*.6,
                                  !(EnvTemp %in% c('tropical', 'polar')) ~ catch))

reduceQuantitySIV <- sum(landingSIV$catch) - sum(landingSIV$catch_proj)

# 05 Scenario Model V -----------------------------------------------------

# all happened in scenario II III IV

landingSV <- FFData %>%
    filter(Year == 2022) %>%
    select(stock, Species, FAO_area, catch) %>%
    left_join(faoAreaInfo, by = 'FAO_area') %>%
    left_join(speciesEnv, by = c('Species' = 'Species')) %>%
    mutate(decrease_projS3 = case_when(IUCN_Code == 'NT'~catch*.5,
                                       IUCN_Code == 'VU'~catch*1,
                                       IUCN_Code == 'N.E.'~catch*.2,
                                       IUCN_Code == 'DD'~catch*.5,
                                       !(IUCN_Code %in% c('NT', 'VU', 'N.E.', 'DD')) ~ 0)) |> 
    mutate(decrease_projS4 = case_when(EnvTemp == 'tropical'~catch*.4,
                                       EnvTemp == 'polar'~catch*.4,
                                       !(EnvTemp %in% c('tropical', 'polar')) ~ 0)) %>%
    mutate(totalDecrease_s1 = decrease_projS3 + decrease_projS4,
           totalDecrease = if_else(totalDecrease_s1 <= catch, 
                                   totalDecrease_s1,
                                   catch))

reduceQuantitySV <- sum(landingSV$totalDecrease)


# 06 running models -------------------------------------------------------

resultsS1 <- scenarioModel_simu(.ffLoss = reduceQuantitySI,
                           .Scenario = 'I',
                           .weights = initWeights,
                           .realAqua = AquaProdReal,
                           .ffdemand = ffDemand)
resultsS2 <- scenarioModel_simu(.ffLoss = reduceQuantitySII,
                           .Scenario = 'II',
                           .weights = initWeights,
                           .realAqua = AquaProdReal,
                           .ffdemand = ffDemand)
resultsS3 <- scenarioModel_simu(.ffLoss = reduceQuantitySIII,
                           .Scenario = 'III',
                           .weights = initWeights,
                           .realAqua = AquaProdReal,
                           .ffdemand = ffDemand)
resultsS4 <- scenarioModel_simu(.ffLoss = reduceQuantitySIV,
                           .Scenario = 'IV',
                           .weights = initWeights,
                           .realAqua = AquaProdReal,
                           .ffdemand = ffDemand)
resultsS5 <- scenarioModel_simu(.ffLoss = reduceQuantitySV,
                           .Scenario = 'V',
                           .weights = initWeights,
                           .realAqua = AquaProdReal,
                           .ffdemand = ffDemand)

# Gather results
scenarioModelRes <- bind_rows(list(resultsS1, resultsS2,
                                   resultsS3,
                                   resultsS4, resultsS5)) 

baseline <- sum(landingSIII$catch) 
print(paste0("Decrease in scenario I: ", round(reduceQuantitySI*1e-6, 2), 
             'mmt (', round(reduceQuantitySI / baseline, 4) *100, '%)'))
print(paste0("Decrease in scenario II: ", round(reduceQuantitySII*1e-6, 2), 
             "mmt (", round(reduceQuantitySII / baseline, 4) *100, '%)'))
print(paste0("Decrease in scenario III: ", round(reduceQuantitySIII*1e-6, 2),
             "mmt (", round(reduceQuantitySIII / baseline, 4) *100, '%)'))
print(paste0("Decrease in scenario IV: ", round(reduceQuantitySIV*1e-6, 2),
             "mmt (", round(reduceQuantitySIV / baseline, 4) *100, '%)'))
print(paste0("Decrease in scenario V: ", round(reduceQuantitySV*1e-6, 2),
             "mmt (", round(reduceQuantitySV / baseline, 4) *100, '%)'))

write_rds(scenarioModelRes,
       './Results/Senario.model.results.rds',
       compress = 'gz')
