# 00.Package Import -------------------------------------------------------
library(pacman)
source('./Scripts/00-Support Functions/02_modelSupport.R')
p_load(tidyverse, multidplyr)

# 01.Required File Readin & Preprocess-------------------------------------
taxaList_path <- './Data/FarmSpeciesList.csv'

taxaList <- read_csv(taxaList_path)

# 02. FFDR Estimation ---------------------------------------------------------

# Taxas in consideration
taxaIC <- unique(taxaList$Taxa)

pb <- txtProgressBar(min = 0,
                     max = length(taxaIC) * 500, style = 3) # pb bar
k <- 0 # counter for pb bar
allFFDR <- tibble()
for (taxa in taxaIC) {

    set.seed(20240417)
    speFFDR <- c()
    for (i in 1:500) {
        # simulating diet & fcr
        allPara <- paraSimulator(taxa = taxa)
        
        fcr <- allPara$fcr
        diet <- allPara$diet
        bof <- allPara$bof
        
        singleFFDR <- (fcr * diet * 100) / 24
        speFFDR <- c(speFFDR, singleFFDR)
        # operating pb bar
        k<-k+1
        setTxtProgressBar(pb, k)
    }
    
    allFFDR <- bind_rows(allFFDR, tibble(taxa = taxa, FFDR = speFFDR))

}

levelFFDR <- allFFDR %>% 
    group_by(taxa) %>% 
    summarise(p = mean(FFDR)) %>% 
    arrange(p) %>% 
    pull(taxa) %>% 
    str_to_title()

allFFDR <- allFFDR %>%
    group_by(taxa) %>%
    summarise(meanFFDR = mean(FFDR),
              upper = quantile(FFDR, .95),
              lower = quantile(FFDR, .05)) %>% 
    ungroup() %>% 
    mutate(taxa = str_to_title(taxa)) %>% 
    rename(Taxa = taxa) %>% 
    mutate(Taxa = factor(Taxa, levels = levelFFDR))

# save results in rds
write_rds(allFFDR, './Results/FFDRs.rds', 
          compress = 'gz')
