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

# 02 国家数据整理 ---------------------------------------------------------------

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
    mutate(pod_loss = if_else(pod_loss > prod_2022, 
                              prod_2022, pod_loss))


# Load world shape

world_shp <- './Data/WorldMap/world_shp.shp'
world_map <- read_sf(world_shp)

# 04 Map theme

theme_senario <- theme_map(base_size =  9) +
    theme(plot.title = element_text(hjust = .5))

theme_senario <- theme_map(base_size  = 7) +
    theme(plot.title = element_text(hjust = .5,
                                    size = 7,
                                    face = 'bold'))

# change unit to mmt
loss_map$pod_loss <- loss_map$pod_loss * 1e-6 

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


# 06 Define the limitation of production loss 

map_limit <- round(range(loss_map$pod_loss), 0)

# 07 map scenario I

scenario_map_I_data <- world_map %>%
    left_join(scenarioI_data, 
              by = c('name' = 'Country'))

smap_1 <- ggplot() +
    geom_sf(data = scenario_map_I_data,
            aes(fill = pod_loss),
            color = 'black',
            size = .1) +
    scale_filler() +
    theme_senario +
    ggtitle('\nI: Fishing ban')


# 07 场景2地图 ---------------------------------------------------------------

scenario_map_II_data <- world_map %>%
    left_join(scenarioII_data, 
              by = c('name' = 'Country'))

smap_2 <- ggplot() +
    geom_sf(data = scenario_map_II_data,
            aes(fill = pod_loss),
            color = 'black',
            size = .1) +
    scale_filler() +
    theme_senario +
    ggtitle('\nII: Quota cut')

# 08 map scenario III 
scenario_map_III_data <- world_map %>%
    left_join(scenarioIII_data, 
              by = c('name' = 'Country'))

smap_3 <- ggplot() +
    geom_sf(data = scenario_map_III_data,
            aes(fill = pod_loss),
            color = 'black',
            size = .1) +
    scale_filler() + 
    theme_senario +
    ggtitle('III: IUCN alert')


# 09 map scenario IV

scenario_map_IV_data <- world_map %>%
    left_join(scenarioIV_data, 
              by = c('name' = 'Country'))

smap_4 <- ggplot() +
    geom_sf(data = scenario_map_IV_data,
            aes(fill = pod_loss),
            color = 'black',
            size = .1) +
    scale_filler() + 
    theme_senario +
    ggtitle('IV: Climate impact')


# 05 map scenario V

scenario_map_V_data <- world_map %>%
    left_join(scenarioV_data, 
              by = c('name' = 'Country'))


smap_5 <- ggplot() +
    geom_sf(data = scenario_map_V_data,
            aes(fill = pod_loss),
            color = 'black',
            size = .1) +
    scale_filler(GUIDE = T, NAME = 'Production loss (mmt)') +
    theme_senario +
    ggtitle('V: Compound effect')

# 06 

summary_data <- scenarioModelRes %>%
    group_by(taxa, Senario) %>%
    mutate(simu_id = 1:500)  %>%
    ungroup() %>%
    mutate(prodLoss = avgReal - EstProd) %>%
    group_by(Senario, simu_id) %>%
    summarise(total_loss = sum(prodLoss)) %>%
    ungroup() %>%
    mutate(Year2022 = 56797208,
           loss_r = total_loss / Year2022,
           Senario = factor(Senario, 
                            levels = c('V', 'IV', 'III', 'II', 'I'))) 

cmap_6 <- ggplot(summary_data,
                 aes(x = total_loss * 1e-6,
                     y = Senario)) +
    geom_vline(xintercept = 0, lty = 'dotted') +
    stat_pointinterval() + 
    labs(x = 'Production loss (mmt)',
         y = 'Scenario') +
    theme_wsj(base_size = 7,
              base_family = 'Helvetica',
              color = 'white') +
    theme(axis.title = element_text(size = 7,
                                    family = 'Helvetica',
                                    face = 'bold'))


figS2 <- ((smap_1 | smap_2 )/
        (smap_3 | smap_4) /
        (smap_5 | cmap_6)) +
    plot_layout(guides = 'collect') & 
    theme(legend.position = 'top', 
          legend.key.height = unit(2, 'mm'),
          legend.text = element_text(size = 5))

ggsave(plot = figS2, 
       device = 'pdf', filename = 'Visuals/FigS2.pdf',
       width = 174, height = 150, units = 'mm') 

