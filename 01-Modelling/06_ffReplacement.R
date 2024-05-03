source('./Scripts/00-Support Functions/02_modelSupport.R')
scenarioModelRes <- read_rds('./Results/Senario.model.results.rds')
taxonLs <- read_rds('Results/Senario.model.input.rds')$taxaCulture$Taxa

scenarioModelRes <- scenarioModelRes |> 
    group_by(taxa, Senario) |> 
    summarise(
        avgEstProd = mean(EstProd),
        q05Prod = quantile(EstProd, probs = .025, na.rm = T),
        q95Prod = quantile(EstProd, probs = .975, na.rm = T),
        recDiet = mean(diet_Rec),
        q05Diet_rec = quantile(diet_Rec, probs = .025, na.rm = T),
        q95Diet_rec = quantile(diet_Rec, probs = .975, na.rm = T),
        recImprov = mean(dietImprov),
        q05recImprov = quantile(dietImprov, probs = .025, na.rm = T),
        q95recImprov = quantile(dietImprov, probs = .975, na.rm = T),
        avgReal = mean(avgReal),
        aquaLoss.r = mean(aquaLoss.r),
        aquaLoss.r.05 = quantile(aquaLoss.r, probs = .025, na.rm = T),
        aquaLoss.r.95 = quantile(aquaLoss.r, probs = .0975, na.rm = T)
    ) 

pb <- txtProgressBar(min = 0, 
                     max = length(taxonLs) * 5000, style = 3) # pb bar
k <- 0 # counter for pb bar
replaceRes <- tibble()
for (Taxa in taxonLs) {
    
    taxaLoss <- scenarioModelRes %>%
        mutate(prodLoss = avgReal*aquaLoss.r) |> 
        filter(taxa == Taxa) %>% 
        select(taxa, prodLoss, Senario) %>%
        rename(Scenario = Senario)
    
    print(paste0(Taxa, ' start!'))
    for (i in 1:5000) {
        # simulating diet & fcr
        para <- paraSimulator(Taxa)
        
        FMReplace <- taxaLoss %>%
                   mutate(fMeal = prodLoss*para$fcr*para$diet*para$bof,
                          run = i)
        
        replaceRes <- replaceRes %>%
            bind_rows(FMReplace)
        
        # operating pb bar
        k<-k+1
        setTxtProgressBar(pb, k)
    }
}


write_rds(replaceRes, 
          './Results/FF.replacement.res.rds',
          compress = 'gz')
