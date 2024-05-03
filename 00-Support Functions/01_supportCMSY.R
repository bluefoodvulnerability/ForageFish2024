# 00. loading packages
pacman::p_load(datalimited2, rfishbase, readxl, purrr,
               furrr, tictoc, tidyverse, multidplyr,
               stringr, vroom, progressr)

# 01. packed cmsy looping function ---------------------------------------------
nestedCMSY <- function(df, rlow, rhi){
    yr <- df$Year
    ct <- df$catch

    if (sum(ct) > 100) {         # drop small stocks

        messager <- try(stockAssess <- cmsy2(yr, ct, 
                                             r.low = rlow, r.hi = rhi), 
                        silent = T)

        if ('try-error' %in% class(messager)) {

            return('NA')

        } else{

            return(stockAssess)

        }
    }
}

# 02. Get parameter for each run; ---------------------------------------------

extr_RefTsFunc <- function(mod){
    mod['ref_ts']
}

extr_RefPtsFunc <- function(mod){
    mod['ref_pts']
}
