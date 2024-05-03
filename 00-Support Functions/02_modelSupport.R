# r script for basic scenario model functions
# Copyright danilliu(liuyue9312@sjtu.edu.cn)

# 00 load packages --------------------------------------------------------

library(pacman)
p_load(tidyverse, stringr, readxl, purrr, rfishbase,
       triangle, rlist, ggthemes, sf, scatterpie, patchwork)

taxaLevel <- c(
    'Carps',
    'Catfishes',
    'Eels',
    'Milkfishes',
    'Salmonids',
    'Shrimps',
    'Tilapias',
    'Diadromous Fishes',
    'Fw Crustaceans',
    'Fw Fishes',
    'Marine Crustaceans',
    "Marine Fishes"
)

# 01 aquaculture production simulate function -----------------------------

# function for generate auqaculture production by taxa using Forage fish quantity
# theoretically
# parameters used: input forage fish quantity; taxa's biomass on feed; taxa
# 'diet'; and feed conventional ratio by taxa's feed

prodFun <- function(FFinput, Bof, diet, fcr){

    aquaProd <- (0.21*FFinput)/ (0.67 *Bof * fcr * diet)

    aquaProd

}


# 02 aquaculture taxa's feed parameter generator --------------------------

# generating feed parameter,
# parameter simulating using TACON A G J, HASAN M R, METIAN M. Demand and Supply
# of Feed Ingredients for Farmed Fish and Crustaceans[M]. Rome: Food and
# Agriculture Organization of the United Nations, 2011
# http://www.fao.org/3/a-ba0002e.pdf
# parameter generated are fcr, diet, bof and taxa

paraSimulator <- function(taxa) {

    fcr <- case_when(
        taxa == "Fw Crustaceans" ~ round(rtriangle(n=1,
                                                   a = 1.1,
                                                   b = 1.8,
                                                   c= 1.7), 2),
        taxa == "Shrimps" ~ round(rtriangle(n = 1, 
                                            a = 1.1, 
                                            b = 1.6,
                                            c = 1.4), 2),
        taxa == "Carps" ~ round(rtriangle(n = 1,
                                          a = 1.1, 
                                          b = 1.7,
                                          c = 1.5), 2),
        taxa == "Fw Fishes" ~ round(rtriangle(n = 1,
                                              a = 1.1, 
                                              b = 1.7,
                                              c = 1.4), 2),
        taxa == "Marine Fishes" ~ round(rtriangle(n = 1,
                                                  a = 1.1,
                                                  b = 1.7,
                                                  c = 1.5), 2),
        taxa == "Tilapias" ~ round(rtriangle(n = 1, 
                                             a = 1.1,
                                             b = 1.7,
                                             c = 1.6), 2),
        taxa == "Diadromous Fishes" ~ round(rtriangle(n = 1,
                                                      a = 1.1,
                                                      b = 1.7,
                                                      c = 1.5), 2),
        taxa == "Salmonids" ~ round(rtriangle(n = 1, 
                                            a = 1.1,
                                            b = 1.3,
                                            c = 1.3), 2),
        taxa == "Eels" ~ round(rtriangle(n = 1,
                                         a = 1.1, 
                                         b = 1.6,
                                         c = 1.5), 2),
        taxa == "Catfishes" ~ round(rtriangle(n = 1,
                                              a = 1, 
                                              b = 1.6,
                                              c = 1.3), 2),
        taxa == "Marine Crustaceans" ~ round(rtriangle(n = 1,
                                                      a = 1.1,
                                                      b = 1.6,
                                                      c = 1.6), 2),
        taxa == "Milkfishes" ~ round(rtriangle(n = 1,
                                          a = 1.1,
                                          b = 1.7,
                                          c = 1.5), 2)
    )

    diet <- case_when(
        taxa == "Fw Crustaceans" ~ round(rtriangle(n = 1, 
                                                   a = 0.04,
                                                   b = 0.09,
                                                   c = 0.06), 2),
        taxa == "Shrimps" ~ round(rtriangle(n = 1, 
                                            a = 0.06,
                                            b = 0.12,
                                            c = 0.09), 2),
        taxa == "Carps" ~ round(rtriangle(n = 1,
                                          a = 0.005,
                                          b = 0.02,
                                          c = 0.007), 4),
        taxa == "Fw Fishes" ~ round(rtriangle(n = 1,
                                              a = 0.06,
                                              b = 0.11,
                                              c = 0.07), 2),
        taxa == "Marine Fishes" ~ round(rtriangle(n = 1,
                                                  a = 0.08,
                                                  b = 0.15,
                                                  c = 0.12), 2),
        taxa == "Tilapias" ~ round(rtriangle(n = 1, 
                                             a = 0.02,
                                             b = 0.04,
                                             c = 0.03), 4),
        taxa == "Diadromous Fishes" ~ round(rtriangle(n = 1,
                                                      a = 0.08,
                                                      b = 0.12,
                                                      c = 0.10), 2),
        taxa == "Salmonids" ~ round(rtriangle(n = 1, 
                                            a = 0.10,
                                            b = 0.20,
                                            c = 0.17), 2),
        taxa == "Eels" ~ round(rtriangle(n = 1,
                                         a = 0.35,
                                         b = 0.45,
                                         c = 0.4), 2),
        taxa == "Catfishes" ~ round(rtriangle(n = 1,
                                              a = 0.02,
                                              b = 0.06,
                                              c = 0.04), 2),
        taxa == "Marine Crustaceans" ~ round(rtriangle(n = 1,
                                                      a = 0.08,
                                                      b = 0.15,
                                                      c = 0.1), 2),
        taxa == "Milkfishes" ~ round(rtriangle(n = 1, 
                                             a = 0.06,
                                             b = 0.11,
                                             c = 0.07), 2),
    )
    
    Bof <- case_when(
        taxa == "Fw Crustaceans" ~ 0.6,
        taxa == "Shrimps" ~ 0.86,
        taxa == "Carps" ~ 0.57,
        taxa == "Fw Fishes" ~ 0.6,
        taxa == "Marine Fishes" ~ 0.8,
        taxa == "Tilapias" ~ 0.92,
        taxa == "Diadromous Fishes" ~ 0.43,
        taxa == "Salmonids" ~ 1,
        taxa == "Eels" ~ 1,
        taxa == "Catfishes" ~ 0.81,
        taxa == "Shads" ~ 0.6,
        taxa == "Marine Crustaceans" ~ 0.87,
        taxa == "Milkfishes" ~ .55,
    )

    return(
        list(fcr = fcr, diet = diet,
             bof = Bof,
             taxa = taxa)
    )
}

# 03 recover diet calculator ----------------------------------------------

# for depletion situation, calculate how diet change can offset the depletion
# parameter used:
# aquaConst - the fixed target aquaculture production quantity;
# ffNeed - the needed forage fish quantity for target production;
# ffGap - the lost quantity of forage fish landing;
# Bof - biomass on feed value of taxa;
# fcr - feed conventional ratio value of taxa;

recoveryDiet <- function(aquaConst, ffNeed, ffGap, Bof, fcr) {

    recoverDiet <- (ffNeed * 0.67 - ffGap)/(aquaConst * Bof * fcr * 0.67 * 5.2)
    
    ifelse(recoverDiet <= 0,
           return(0),
           return(recoverDiet))

}

# 04 scenario model calculator --------------------------------------------

scenarioModel_simu <- function(
        .Bof = Bof,
        .ffavgCatch = FFavgCatch,
        .realAqua = AquaProdReal,
        .ffdemand = ffDemand,
        .ffLoss,
        .weights,
        .Scenario
) {
    taxonList <- names(ffDemand)
    ffred <- lossDist(initWeight = .weights,
                      totalLoss = .ffLoss,
                      ffDemand = .ffdemand)
    ffred <- ffred * 0.6
    
    set.seed(931223)
    pb <- txtProgressBar(min = 0,
                         max = length(taxonList) * 500,
                         style = 3) # pb bar
    k <- 0 # counter for pb bar
    
    taxonAllDF <- tibble()
    for (Taxa in taxonList) {
        
        taxaSimu <- tibble()
        for (i in 1:500) {
            
            paraOnce <- paraSimulator(taxa = Taxa)
            
            prodLoss <- prodFun(FFinput = ffred[Taxa],
                                Bof = paraOnce$bof,
                                diet = paraOnce$diet,
                                fcr = paraOnce$fcr)
            
            prodEst <- AquaProdReal[Taxa] - prodLoss
            prodEst <- ifelse(prodEst < 0, 0, prodEst)
            
            diet.Recover <- recoveryDiet(aquaConst = AquaProdReal[Taxa],
                                         ffNeed = ffDemand[Taxa],
                                         ffGap = ffred[Taxa],
                                         Bof = paraOnce$bof,
                                         fcr = paraOnce$fcr)
            taxaSimu <- taxaSimu %>%
                bind_rows(
                    tibble(
                        taxa = Taxa,
                        EstProd = prodEst,
                        dietOrigin = paraOnce$diet,
                        diet_Rec = diet.Recover
                    )
                ) %>%
                mutate(
                    dietImprov = (dietOrigin - diet_Rec)/dietOrigin
                )
            
            k<-k+1
            setTxtProgressBar(pb, k)
            
        }
        
        taxonAllDF <- taxonAllDF %>%
            bind_rows(taxaSimu)
        
    }
    
    depletionEst <- taxonAllDF %>%
        mutate(Senario = .Scenario,
               avgReal = .realAqua,
               aquaLoss.r = (avgReal - EstProd)/avgReal)
    
    return(depletionEst)
    
}


# 05 plot cultivate production decrease ratio by taxon ---------------------

# plot cultivate production decrease ratio by taxon
# show in grid style
# input: gathered scenario model results,
# **the results should be gathered first**

plotTaxaDecrease <- function(scenarioModelRes,
                             save_path = NA,
                             .device = 'jpeg',
                             barColor = c('#45c89a', '#ffb366',
                                          '#ffd859', '#ec6762',
                                          '#c74d4c'),
                             .taxaLevel = taxaLevel){
    scenarioModelRes <- scenarioModelRes %>%
        mutate(taxa = str_to_title(taxa)) %>%
        mutate(taxa = factor(
            taxa,
            levels = .taxaLevel
        ),
        Senario = factor(Senario, 
                         levels = rev(c('I', 'II', 'III', 'IV', 'V'))))
    
    p <- scenarioModelRes %>% 
        ggplot(aes(x = aquaLoss.r*100, y = Senario)) +
        stat_gradientinterval(aes(fill = Senario, slab_alpha=stat(-pmax(abs(1-2*cdf), .95))), 
                              fill_type = 'segments', position = 'dodge') +
        facet_wrap(~taxa) +
        scale_fill_manual(values = barColor) +
        scale_x_continuous(limits = c(0, 105),
                           breaks = seq(0, 100, 20),
                           expand = expansion(mult = c(0, 0)),
                           position = 'bottom') +
        theme_wsj(base_family = 'Helvetica',
                  title_family = 'Helvetica') +
        theme(panel.background = element_rect(fill = 'white'),
              plot.background = element_rect(fill = 'white'),
              strip.background = element_rect(fill = 'white'),
              legend.background = element_rect(fill = 'white'),
              legend.text = element_text(size = 7),
              legend.title = element_text(size=7),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(hjust = 0, size = 7), 
              panel.spacing.x = unit(1.2, 'lines'))
    
    if (!is.na(save_path)) {
        
        ggsave(save_path,
               plot = p,
               dpi = 600, device = .device,
               width = 179,
               height = 150,
               units = 'mm')
        
    }
    
    return(p)
    
}


# 06 plot diet increase ----------------------------------------------------

# plot recovery diet for each corresponding scenarios

plotdietIncrease <- function(scenarioModelRes,
                             save_path = NA,
                             .device = 'jpeg',
                             .fill = rev(c('#B389ED',
                                           '#364F6B',
                                           '#3FC1C9',
                                           '#A9D158',
                                           '#FFD947',
                                           '#FC5185')),
                             .taxaLevel = taxaLevel,
                             originDiet.path){
    
    dietS0 <- read_xlsx(originDiet.path) %>%
        select(Taxa, minDiet, maxDiet, mxLik) %>%
        mutate(Scenario = 'Baseline',
               source = 'Origin',
               mxLik = 100*(minDiet + (maxDiet - minDiet)/2))
    
    dietChange.d <- scenarioModelRes %>%
        select(taxa, diet_Rec, Senario) %>%
        rename(Taxa = taxa,
               Scenario = Senario) %>%
        bind_rows(dietS0)
    
    dietChange.d <- dietChange.d %>% 
        mutate(Taxa = str_to_title(Taxa)) %>% 
        mutate(Taxa = factor(Taxa, 
                             levels = taxaLevel),
               Scenario = factor(Scenario,
                                 levels = rev(c('Baseline', 'I', 'II', 
                                                'III', 'IV', 'V'))))
    
    p <- ggplot(dietChange.d,
                aes(y = Scenario),
                color = factor(source,
                               levels = c('Origin',
                                          'Recoverable',
                                          'Unrecoverable'))) +
        geom_errorbar(aes(xmin = minDiet*100,
                          xmax = maxDiet*100),
                      width = .3,
                      lwd = .7) +
        geom_point(aes(x = mxLik, 
                       fill = Scenario),
                   shape = 22, color = 'black', size = 2) +
        stat_gradientinterval(aes(x = diet_Rec*100, 
                                  fill = Scenario, 
                                  slab_alpha=stat(-pmax(abs(1-2*cdf), .95))), 
                              fill_type = 'segments', position = 'dodge') +
        facet_wrap(~Taxa, scales = 'free_x',strip.position = 'top') +
        scale_x_continuous(labels = scales::label_number_auto(),
                           position = 'bottom') +
        geom_vline(xintercept = 0, color = 'grey20', linetype = 'dashed') +
        scale_fill_manual(values = .fill) +
        theme_wsj(title_family = 'Helvetica') +
        theme(panel.background = element_rect(fill = 'white'),
              plot.background = element_rect(fill = 'white'),
              strip.background = element_rect(fill = 'white'),
              legend.background = element_rect(fill = 'white'),
              legend.text = element_text(size = 7),
              legend.title = element_text(size=7),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(hjust = 0, size = 7), 
              panel.spacing.x = unit(1.2, 'lines'))
    
    if (!is.na(save_path)) {
        
        ggsave(save_path, plot = p,
               dpi = 600, device = .device,
               width = 190,
               height = 140,
               units = 'mm')
        
    }
    
    return(p)
    
}

# 07 mapping scenario impact on country -----------------------------------

# map the scenarios on map

mapImpact <- function(scenarioModelRes,
                      countryWeights,
                      folderPath = './',
                      outDevice = 'jpeg',
                      .colors = rev(rainbow(5))){

    totalLossDF <- scenarioModelRes %>%
        group_by(Senario) %>%
        summarise(totalLoss = sum(prodLoss))

    totalLossLs <- totalLossDF$totalLoss
    names(totalLossLs) <- totalLossDF$Senario

    # modified world map

    worldMapCSV <- './Data/worldMapbase_fix.csv'
    worldBasic <- read_csv(worldMapCSV) 

    allLost <- c()
    for (scenario in unique(scenarioModelRes$Senario)) {

        sLost <- totalLossLs[scenario] * CountryWeight
        allLost <- c(allLost, sLost)

    }

    lmax <- max(allLost*1e-6)
    valueLimit <- lmax + (1 - lmax%%1)

    for (scenario in unique(scenarioModelRes$Senario)) {

        temporalTibble <- tibble(country = names(CountryWeight),
                                 lost = totalLossLs[scenario] * CountryWeight)

        scenarioMapData <- worldBasic %>%
            left_join(temporalTibble, by = c('region' = 'country')) %>%
            filter(region != 'Antarctica')

        scenarioMapBase <- ggplot(scenarioMapData) +
            geom_map(map=worldBasic,
                     aes(x=long, y=lat, map_id=region, fill=lost*1e-6),
                     color="white", size=0.05, alpha=0.8,color = 'grey70')

        print(paste0('Saving ', 'scenario ', scenario, ' ...'))
        p <- scenarioMapBase +
            geom_text(aes(x = -170, y = - 70),
                      label = paste0('Scenario', ' ', scenario)) +
            scale_fill_gradientn(colors = .colors,
                                 na.value = 'grey90',
                                 name = 'Production \n decrease (mmt)',
                                 guide = 'colorbar',
                                 limits = c(0, valueLimit),
                                 breaks = seq(0, valueLimit, length.out = 5),
                                 labels = seq(0, valueLimit, length.out = 5)) +
            guides(fill = guide_colorbar(title.position = 'top'), label = F) +
            theme_map() +
            
            theme(legend.direction = 'vertical',
                  legend.position = c(0, 0),
                  legend.key.width = unit(0.3, 'cm'),
                  legend.key.height = unit(.7, 'cm'),
                  text = element_text(family = 'Arial Black', size = 10))

        savePath <- paste0(folderPath,
                           'Scenario',
                           '_',
                           scenario,
                           '.jpeg')

        ggsave(savePath, plot = p,
               dpi = 600, device = outDevice,
               width = 190,
               height = 100,
               units = 'mm')
        
        print('...done!')
    }
    
}

# 08 mapping scenario impact by percentage on country ---------------------

mapImpact_perc <- function(scenarioModelRes,
                      countryWeights,
                      folderPath = './',
                      outDevice = 'jpeg',
                      .colors = rev(rainbow(5))){
    
    totalLossDF <- scenarioModelRes %>%
        group_by(Senario) %>%
        summarise(totalLoss = sum(prodLoss))
    
    totalLossLs <- totalLossDF$totalLoss
    names(totalLossLs) <- totalLossDF$Senario
    
    # modified world map
    
    worldMapCSV <- './Data/worldMapbase_fix.csv'
    aquaProd2018_path <- 'Data/RData/aquaProd2018.rds'
    
    worldBasic <- read_csv(worldMapCSV) 
    aquaProd2018 <- read_rds(aquaProd2018_path)
    
    allLost <- c()
    for (scenario in unique(scenarioModelRes$Senario)) {
        
        sLost <- totalLossLs[scenario] * CountryWeight
        allLost <- c(allLost, sLost)
        
    }
    
    lmax <- max(allLost*1e-6)
    valueLimit <- lmax + (1 - lmax%%1)
    
    for (scenario in unique(scenarioModelRes$Senario)) {
        
        temporalTibble <- tibble(country = names(CountryWeight),
                                 lost = totalLossLs[scenario] * CountryWeight) %>% 
            left_join(aquaProd2018, by = c('country' = 'Country')) %>% 
            mutate(Percentage = lost / Production_2018) %>% 
            filter(Percentage != Inf) %>% 
            mutate(Percentage = if_else(Percentage >= 1, 1, Percentage))
        
        scenarioMapData <- worldBasic %>%
            left_join(temporalTibble, by = c('region' = 'country')) %>%
            filter(region != 'Antarctica')
        
        scenarioMapBase <- ggplot(scenarioMapData) +
            geom_map(map=worldBasic,
                     aes(x=long, y=lat, map_id=region, fill=Percentage),
                     color="white", size=0.05, alpha=0.8,color = 'grey70')
        
        print(paste0('Saving ', 'scenario ', scenario, ' ...'))
        p <- scenarioMapBase +
            scale_fill_gradientn(colors = .colors,
                                 na.value = 'grey90',
                                 name = '% production\ndecrease',
                                 guide = 'colorbar',
                                 limits = c(0, 1),
                                 breaks = seq(0, 1, .25),
                                 labels = seq(0, 100, 25)) +
            guides(fill = guide_colorbar(title.position = 'top'), label = F) +
            theme_map() +
            theme(legend.direction = 'vertical',
                  legend.position = c(0, 0),
                  legend.key.width = unit(0.3, 'cm'),
                  legend.key.height = unit(.7, 'cm'),
                  text = element_text(family = 'Arial Black', size = 10))
        
        savePath <- paste0(folderPath,
                           'Scenario_percentage',
                           '_',
                           scenario,
                           '.jpeg')

        ggsave(savePath, plot = p,
               dpi = 600, device = outDevice,
               width = 190,
               height = 90,
               units = 'mm')
        print('...done!')
    }
    
}

# 09 loss distributor -------------------------------------------------------
# A function for auto update taxa weights
weightUpdate <- function(weight, taxa_list){
    
    weightN <- weight[names(weight) %in% taxa_list]
    alloc <- weightN/sum(weightN)
    
    return(alloc)
}

# Automatically distribute losses to taxa and reset overflow
lossDist <- function(initWeight, totalLoss, ffDemand){
    
    lossRemain <- totalLoss
    
    weight <- initWeight
    taxa_list <- names(initWeight)
    L <- list()
    substract <- list(ffDemand)
    while (lossRemain != 0) {
        
        lossAllocN <- weightUpdate(weight, taxa_list)
        scheme <- lossRemain*lossAllocN*.7
        
        substract[2] <- list(-scheme)
        x <- tapply(unlist(substract), 
                    names(unlist(substract)), 
                    sum)
        
        taxa_list <- names(x[x > 0])
        lossRemain <- abs(sum(x[x < 0]))
        
        L <- list.append(L, scheme)
        
    }
    
    y <- tapply(unlist(L), names(unlist(L)), sum)
    
    allLostIndex <- y > ffDemand
    y[allLostIndex] <- ffDemand[allLostIndex]
    
    return(y)
}


