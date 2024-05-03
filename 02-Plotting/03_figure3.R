library(pacman)
p_load(sf, ggdist, scales, tidyverse, 
       paletteer, ggthemes, measurements)

scale_filler <- function(GUIDE = F, NAME = NA){
    if (GUIDE == F) {
        scale_fill_paletteer_c(palette = 'harrypotter::ravenclaw',
                               na.value = 'white', guide = 'none', 
                               # direction = -1
                               )
    }else {
        scale_fill_paletteer_c(na.value = 'white',
                               label = scales::percent,
                               name = NAME,
                               palette = 'harrypotter::ravenclaw',
                               # direction = -1
                               )
    }
}

# 01 Basic Processing 
source('./Scripts/00-Support Functions/02_modelSupport.R')
scenarioModelRes_path <- './Results/Senario.model.results.rds'
# weights based on country production and taxa productivity
country_taxa_weight_path <- './Results/Country.Taxa.Weights.rds'

scenarioModelRes <- read_rds(scenarioModelRes_path)
country_taxa_weight <- read_rds(country_taxa_weight_path)

senarioLoss <- scenarioModelRes %>%
    mutate(prodLoss = avgReal - EstProd) %>%
    select(-dietOrigin,
            -diet_Rec,
           -dietImprov,
           -avgReal,
           -EstProd,
           -aquaLoss.r) %>% 
    rename(Taxa = taxa)


senario_loss <- senarioLoss %>% 
    full_join(country_taxa_weight, by = 'Taxa') %>% 
    mutate(country_taxa_loss = taxa_weight * prodLoss,
           country_taxa_loss = if_else(country_taxa_loss >= Year_2022,
                                       country_taxa_loss, 
                                       country_taxa_loss),
           country_taxa_loss_r = country_taxa_loss/Year_2022) 


# country level

loss_map <- senario_loss %>%
    group_by(Senario, Country, Taxa) |> 
    group_split() |> 
    map(~mutate(.x, simu = 1:500)) |> 
    bind_rows() |> 
    group_by(Senario, Country, simu) %>%
    summarise(pod_loss = sum(country_taxa_loss),
              prod_2022 = sum(Year_2022)) |> 
    ungroup() %>%
    group_by(Senario, Country) %>%
    summarise(pod_loss = mean(pod_loss),
              prod_2022 = mean(prod_2022)) |> 
    mutate(loss_r = pod_loss / prod_2022,
           loss_r = if_else(loss_r > 1, 1, loss_r)) 

# Load world shape
world_shp <- './Data/WorldMap/world_shp.shp'
world_map <- read_sf(world_shp)

# 04 Map theme ---------------------------------------------------------------
theme_senario <- theme_map(base_size =  9) +
    theme(plot.title = element_text(hjust = .5))

# split scenario model results

scenarioI_data <- loss_map %>%
    filter(Senario == 'I')

scenarioII_data <- loss_map %>%
    filter(Senario == 'II')

scenarioIII_data <- loss_map %>%
    filter(Senario == 'III')

scenarioIV_data <- loss_map %>%
    filter(Senario == 'IV')

scenarioV_data <- loss_map %>%
    filter(Senario == 'V')


# map scenario I
scenario_map_I_data <- world_map %>%
    left_join(scenarioI_data, 
              by = c('name' = 'Country'))

smap_1 <- ggplot() +
    geom_sf(data = scenario_map_I_data,
            aes(fill = loss_r),
            color = 'black',
            size = .1) +
    scale_filler() +
    theme_senario +
    ggtitle('\nI: Fishing ban')

# map scenario II
scenario_map_II_data <- world_map %>%
    left_join(scenarioII_data, 
              by = c('name' = 'Country'))

smap_2 <- ggplot() +
    geom_sf(data = scenario_map_II_data,
            aes(fill = loss_r),
            color = 'black',
            size = .1) +
    scale_filler() +
    theme_senario +
    ggtitle('\nII: Quota cut')


# map scenario III
scenario_map_III_data <- world_map %>%
    left_join(scenarioIII_data, 
              by = c('name' = 'Country'))

smap_3 <- ggplot() +
    geom_sf(data = scenario_map_III_data,
            aes(fill = loss_r),
            color = 'black',
            size = .1) +
    scale_filler() +
    theme_senario +
    ggtitle('III: IUCN alert')


# map scenario IV
scenario_map_IV_data <- world_map %>%
    left_join(scenarioIV_data, 
              by = c('name' = 'Country'))

smap_4 <- ggplot() +
    geom_sf(data = scenario_map_IV_data,
            aes(fill = loss_r),
            color = 'black',
            size = .1) +
    scale_filler() + 
    theme_senario +
    ggtitle('IV: Climate impact')


# map scenario V
scenario_map_V_data <- world_map %>%
    left_join(scenarioV_data, 
              by = c('name' = 'Country'))

theme_senario <- theme_map(base_size = 9) 

smap_5 <- ggplot() +
    geom_sf(data = scenario_map_V_data,
            aes(fill = loss_r),
            color = 'black',
            size = .1) +
    scale_filler(GUIDE = T, NAME = 'Production loss') +
    theme_senario +
    ggtitle('V: Compound effect')

# 
summary_data <- scenarioModelRes %>%
    group_by(taxa, Senario) %>%
    mutate(simu_id = 1:500)  %>%
    ungroup() %>%
    mutate(prodLoss = avgReal - EstProd) %>%
    group_by(Senario, simu_id) %>%
    summarise(total_loss = sum(prodLoss)) %>%
    ungroup() %>%
    mutate(Year2018 = 56797208,
           loss_r = total_loss / Year2018,
           Senario = factor(Senario, 
                            levels = rev(c('V', 'IV', 'III', 'II', 'I')))) 

cmap_6 <- ggplot(summary_data,
                 aes(y = -loss_r * 100,
                     x = Senario,
                     fill = Senario)) +
    geom_vline(xintercept = 0, lty = 'dotted') +
    # stat_pointinterval(point_size = .7) + 
    geom_violin() +
    stat_summary(fun = "mean",
                 geom = "point",
                 color = "black") + 
    scale_y_continuous(breaks = seq(0, -40, -5),
                       limits = c(-40, 0),
                       labels = seq(0, 40, 5)) +
    scale_x_discrete(position = 'top') +
    scale_fill_manual(values = c('#364F6B',
                                 '#3FC1C9',
                                 '#A9D158',
                                 '#FFD947',
                                 '#FC5185')) +
    guides(fill = 'none') +
    labs(y = 'Production loss by percentage (%)',
         x = 'Scenario') +
    theme_wsj(base_size = 7,
              base_family = 'Helvetica',
              color = 'white') +
    theme(axis.title = element_text(size = 7,
                                    family = 'Helvetica',
                                    face = 'bold'))

combined_map <- ((smap_1 | smap_2 )/
    (smap_3 | smap_4) /
    (smap_5 | cmap_6)) +
    plot_layout(guides = 'collect') & 
    theme(legend.position = 'top', 
          legend.key.height = unit(2, 'mm'),
          legend.text = element_text(size = 5))


## C

# taxa decrease
p <- plotTaxaDecrease(scenarioModelRes,
                      barColor = rev(c('#364F6B',
                                       '#3FC1C9',
                                       '#A9D158',
                                       '#FFD947',
                                       '#FC5185')))

diet_p <- p + 
    labs(x = 'Production loss by percentage (%)',
         y = 'Scenario',
         fill = 'Scenario') +
    geom_vline(xintercept = 0, lty = 'dashed') +
    theme(strip.background = element_rect(fill = 'grey90'),
          strip.text = element_text(hjust = .5, size = 7,
                                    face = 'bold'),
          axis.title = element_text(size = 7,
                                    family = 'Helvetica',
                                    face = 'bold')) +
    guides(fill = guide_none())

ggsave(plot = combined_map,
       device = 'pdf', filename = 'Visuals/fig3A.pdf',
       width = 174, height = 150, units = 'mm')
ggsave(plot = diet_p,
       device = 'pdf', filename = 'Visuals/fig3B.pdf',
       width = 174, height = 84, units = 'mm')    
