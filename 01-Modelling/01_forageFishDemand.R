# 00.Package Import -------------------------------------------------------
library(pacman)
p_load(vroom, multidplyr, tidyverse)

source('./Scripts/00-Support Functions/02_modelSupport.R')

# 01.Required File Readin & Preprocess-------------------------------------
taxaList_path <- './Data/FarmSpeciesList.csv'
FAO_aqua_path <- './Data/AquacultureProductionClean.csv'

taxaList <- read_csv(taxaList_path)
aquaProdduction_fao <- read_csv(FAO_aqua_path)

aquaProduction <- taxaList %>%
    filter(Name_en != 0) %>%
    left_join(aquaProdduction_fao,
              by = c('Country', 'Name_en', 'Environment')) %>%
    drop_na()  # some species cannot be found in database #drop# them all

# 02 Calculating Fish Meal & Oil Usage ------------------------------------

# Taxas in consideration

taxaIC <- unique(aquaProduction$Taxa)

aquaProduction <- aquaProduction %>%
    pivot_longer(-c(Country, `Name_en`, ISSCAAP_Group,
                    Taxa, Environment, Scientific_name),
                 names_to = 'Year') # wide to long

# initialize empty tibble
allMO <- tibble() 

# simulation begin
pb <- txtProgressBar(min = 0,
                     max = length(taxaIC) * 500, style = 3) # pb bar
k <- 0 # counter for pb bar
for (taxa in taxaIC) {
    taxaProd <- aquaProduction %>%
        filter(Taxa == taxa)
    print(paste0(taxa, ' start!'))

    set.seed(20201205)
    MOs <- tibble(.rows = dim(taxaProd)[1])
    for (i in 1:500) {
        allPara <- paraSimulator(taxa = taxa)
        # simulating diet & fcr
        fcr <- allPara$fcr

        diet <- allPara$diet
        
        bof <- allPara$bof

        MO <- (taxaProd %>%
            mutate(MO = value*fcr*diet*bof))$MO

        MOs <- bind_cols(MOs, MO,
                         .name_repair = ~ vctrs::vec_as_names(..., 
                                                              repair = "unique", 
                                                              quiet = TRUE))

        # operating pb bar
        k<-k+1
        setTxtProgressBar(pb, k)
    }
    MOData <- taxaProd %>%
        dplyr::select(-value) %>%
        bind_cols(MOs,
                  .name_repair = ~ vctrs::vec_as_names(..., 
                                                       repair = "unique",
                                                       quiet = TRUE))

    allMO <- allMO %>%
        bind_rows(MOData)
}

# Check
allMO %>% 
    select(starts_with('...')) %>% 
    is.na() %>% 
    sum()

# 04 Clean up calculated data ---------------------------------------------

# use multithreading computation to speed up
# on server
coreCluster <- new_cluster(20) # make core cluster for faster computation

# copy function to the clusters
cluster_copy(coreCluster,
             c('c_across', 'starts_with',
               'sd', 'sqrt', 'across', 'sum', 'quantile'))

# gather MO data
system.time(
    anualTotal <- allMO %>%
        group_by(Taxa, Year) %>%
        partition(coreCluster) %>%
        summarise(across(starts_with('...'),
                                    sum, .names = "{.col}.{.fn}")) %>%
        collect()
)

system.time({
    anualStat <- anualTotal %>%
        rowwise() %>%
        partition(coreCluster) %>%
        mutate(meanMO = mean(c_across(starts_with('...'))),
               sdMO = sd(c_across(starts_with('...'))),
               seMO = sd(c_across(starts_with('...')))/sqrt(500),
               q05MO = quantile(c_across(starts_with('...')),
                                probs = .05),
               q95MO = quantile(c_across(starts_with('...')),
                                probs = .95)) %>%
        collect()
    })

# data save for convinience
write_rds(anualStat,
          './Results/FishmealUsage.rds',
          compress = 'gz')
