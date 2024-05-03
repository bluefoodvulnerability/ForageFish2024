# Process script of the article rmarkdown
# used for map the exploite status of forage fish resources
# Danil Liu 2021/2/5

source('./Scripts/00-Support Functions/02_modelSupport.R')

sau_raw_path <- './Data/FF.enduse.SAU.csv'
sau_enduse <- read_csv(sau_raw_path, col_types = cols())
FAOMajor <- read_sf('Data/FAO.Fishing.Area.Map.geojson')
WorldBase <- read_sf('Data/WorldMap/world_shp.shp')
ffLanding <- read_csv('Data/ForageFishLandingClean.csv')
stockStatus <- read_rds('Results/FF.cmsy.stock.status.rds')


fmfo_use <- sau_enduse %>% 
    filter(end_use_type == "Fishmeal and fish oil") %>% 
    group_by(year) %>% 
    summarise(fmfo_use = sum(tonnes)) %>% 
    ungroup() 

fao_landing_path <- './Data/ForageFishLandingClean.csv'
fao_landing <- read_csv(fao_landing_path, col_types = cols())

top_speices <- fao_landing %>%
    group_by(Species) %>%
    summarise(totalCatch = sum(catch)) %>%
    arrange(desc(totalCatch)) %>%
    mutate(rank = row_number()) %>%
    top_n(10, wt = totalCatch) %>%
    pull(Species) %>%
    as.character()

landing_cmponent <- fao_landing %>%
    mutate(gp = ifelse(Species %in% top_speices,
                       'Top 10 species landing',
                       'Other')) %>%
    group_by(Year, gp) %>%
    summarise(annualCatch = sum(catch)) %>% 
    pivot_wider(id_cols = Year, 
                names_from = gp, 
                values_from = annualCatch) %>% 
    mutate(`Total landing` = `Top 10 species landing` + Other) %>%
    left_join(fmfo_use, by = c('Year' = 'year')) %>% 
    rename(`FMFO used` = fmfo_use) %>% 
    select(-Other) %>% 
    pivot_longer(-Year, names_to = 'gp', values_to = 'component_landing') %>%
    mutate(gp = factor(gp, 
                       levels = c('Total landing',
                                  'Top 10 species landing',
                                  'FMFO used')))

cli_info_p <- './Data/Climate.Info.csv'
cli_info <- read_csv(cli_info_p, col_types = cols())
cli_info <- cli_info %>% 
    filter(year <= 2019) %>% 
    mutate(ymin = 0,
           ymax = if_else(state != 'El_Nino',
                          -1, 40),
           year_lag = if_else(year == 2018,
                              2018,
                              year+1), 
           state = if_else(state != 'El_Nino',
                           'Other Year',
                           'Year of El-Nino'),
           state = factor(state, levels = c('Year of El-Nino',
                                            'Other Year')))

fig1A <- ggplot() +
    geom_rect(data = cli_info,
              aes(xmin = year,xmax = year_lag,
                  ymin=ymin, ymax=ymax, fill=state),
              alpha = 1) +
    geom_line(data = landing_cmponent,
              aes(x = Year, y = component_landing * 1e-6,
                  group = gp, color = gp, 
                  linetype = gp),lwd = .5) +
    scale_x_continuous(breaks = seq(1950, 2020, 10),
                       expand = c(0.05, .01)) +
    scale_y_continuous(breaks = seq(0, 40, 10),
                       expand = c(0, 0)) +
    scale_color_manual(values = c('#3fc1c9', '#364f6b', '#fc5186'),
                       labels = c('Total landing',
                                  'Top 10 species',
                                  'Reduction fisheries')) +
    scale_linetype_manual(labels = c('Total landing',
                                  'Top 10 species',
                                  'Reduction fisheries'),
                          values = c('solid', 'dotted', 'dashed')) +
    scale_fill_manual(values = c('#c4dfe6', '#dcedc2')) +
    guides(fill = guide_legend(nrow=2,byrow=TRUE),
           color = guide_legend(nrow=2,byrow=TRUE)) +
    labs(x = "Year", 
         y = "Forage fish landings (mmt)") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = 'top', 
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.box.margin = margin(),
          legend.direction = 'horizontal', legend.byrow = T,
          text = element_text(family = 'Arial', size = 7),
          axis.text = element_text(family = 'Arial', size = 6),
          legend.box = 'horizontal',legend.key.size = unit(3, 'mm'))

fig1B <- fao_landing %>%
    filter(`catch` != 0) %>%
    group_by(Year, Species) %>%
    summarise(catch = sum(catch)) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Exploit = n()) %>%
    ggplot(aes(Year, Exploit)) +
    geom_segment(
        aes(x = Year, xend = Year, y = 100, yend = Exploit),
        color = 'grey10',
        linewidth = .1
    ) +
    geom_point(
        size = .5,
        color = "black",
        fill = "black",
        shape = 21,
        stroke = .1
    ) +
    scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0.05, .01)) +
    scale_y_continuous(limits = c(100, 300),
                       breaks = seq(0, 300, 50)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(family = 'Arial', size = 7),
          axis.text = element_text(family = 'Arial', size = 6)) +
    labs(x = "Year",
         y = "Number of landed species")


catch_all_2022 <- ffLanding %>%
    mutate(model_status = ifelse(stock %in% unique(stockStatus$stock),
                                 'Successful', 'Fail')) %>%
    filter(Year == 2022) %>%
    pull(catch) %>%
    sum()

sucRate <- ffLanding %>%
    mutate(model_status = ifelse(stock %in% unique(stockStatus$stock),
                                 'Successful', 'Fail')) %>%
    filter(Year == 2022) %>%
    group_by(model_status) %>%
    summarise(catch_all = sum(catch),
              perc = catch_all/catch_all_2022)

all.stocks.size <- length(unique(stockStatus$stock))
stockAss <- stockStatus %>%
    group_by(status) %>%
    summarise(n = n(),
              perc = n/all.stocks.size)

pivotAss_n <- ffLanding %>%
    mutate(model_status = ifelse(stock %in% unique(stockStatus$stock),
                                 'Successful', 'Fail')) %>%
    filter(Year == 2022) %>%
    separate(stock, c('spe', 'FAO_area'), '-', remove = F) %>%
    left_join(stockStatus %>% select(stock, status),
              by = 'stock') %>%
    replace_na(list(status = 'UnAssessed')) %>%
    group_by(FAO_area, status) %>%
    summarise(n = n()) %>%
    pivot_wider(id_cols = FAO_area,
                names_from = status,
                values_from = n,
                values_fill = 0) %>%
    ungroup() %>%
    rowwise(FAO_area) %>%
    mutate(total_n = sum(c_across(where(is.numeric))))

pivotAss_quanti <- ffLanding %>%
    mutate(model_status = ifelse(stock %in% unique(stockStatus$stock),
                                 'Successful', 'Fail')) %>%
    filter(Year == 2022) %>%
    separate(stock, c('spe', 'FAO_area'), '-', remove = F) %>%
    left_join(stockStatus %>% select(stock, status),
              by = 'stock') %>%
    replace_na(list(status = 'UnAssessed')) %>%
    group_by(FAO_area, status) %>%
    summarise(allCatch = sum(catch)) %>%
    pivot_wider(id_cols = FAO_area,
                names_from = status,
                values_from = allCatch,
                values_fill = 0) %>%
    ungroup() %>%
    rowwise(FAO_area) %>%
    mutate(allCatch = sum(c_across(where(is.numeric))))

# prepare for the exploit maps
FAOMajor <- FAOMajor %>%
    filter(F_LEVEL == 'MAJOR')

theme_map <- theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position = 'top',
          legend.title = element_blank())

sf_use_s2(FALSE)
FAOMajor_centroid <- st_centroid(FAOMajor, of_largest_polygon = T)
FAOMajor_centroid <- FAOMajor_centroid %>%
    mutate(lat = unlist(map(FAOMajor_centroid$geometry,1)),
           long = unlist(map(FAOMajor_centroid$geometry,2))) %>%
    select(F_AREA, NAME_EN, lat, long)

mapASS <- FAOMajor_centroid %>%
    left_join(pivotAss_quanti, by = c('NAME_EN' = 'FAO_area'))
mapASS_n <- FAOMajor_centroid %>%
    left_join(pivotAss_n, by = c('NAME_EN' = 'FAO_area'))

mapASS_article <- mapASS %>%
    mutate(prop_collapse = Collapsed/allCatch,
           prop_over = `Over-exploited`/allCatch) %>%
    filter(F_AREA != 18) %>%
    arrange(prop_over)
mapASS_n_article <- mapASS_n %>%
    mutate(prop_collapse = Collapsed/total_n) %>%
    filter(F_AREA != 18) %>%
    arrange(prop_collapse)

# mapping
basicAssMap <- ggplot()+
    geom_sf(data = FAOMajor,
            fill = 'grey99', color = 'grey85',
            linetype = 'dashed') +
    geom_sf(data = WorldBase,
            fill = 'grey85', color = 'white', size = .5) +
    theme_map

exploitMap <- basicAssMap +
    geom_scatterpie(data = as_tibble(mapASS) %>% select(-geometry) %>%  filter(allCatch != 0),
                    mapping = aes(x = lat, y = long,
                                  group = NAME_EN, r = allCatch^(1/8)*2),
                    cols = c('UnAssessed',
                             'Developing',
                             'Fully-exploited',
                             'Over-exploited',
                             'Collapsed')) +
    scale_fill_manual(breaks = c('UnAssessed', 'Developing',
                                 'Fully-exploited', 'Over-exploited',
                                 'Collapsed'),
                      labels = c('Unassessed', 'Developing',
                                 'Fully-exploited', 'Over-exploited',
                                 'Collapsed'),
                      values = c('UnAssessed' = '#A69569',
                                 'Developing' = '#A9D158',
                                 'Fully-exploited' = '#3fC1C9',
                                 'Over-exploited' = '#B389ED',
                                 'Collapsed' = '#fc5185')) +
    geom_scatterpie_legend(seq(1, 15),
                           x = 130, y = -65, n=4,
                           labeller = function(x) round((x/2)^8 * 1e-6, 2)
                           ) +
    labs(x = '', y = '') +
    theme(legend.position = 'bottom')

exploitMap_n <- basicAssMap +
    geom_scatterpie(data = as_tibble(mapASS_n) %>% select(-geometry),
                    mapping = aes(x = lat, y = long,
                                  group = NAME_EN,
                                  r = sqrt(total_n)),
                    cols = c('UnAssessed',
                             'Developing',
                             'Fully-exploited',
                             'Over-exploited',
                             'Collapsed')) +
    scale_fill_manual(breaks = c('UnAssessed', 'Developing',
                                 'Fully-exploited', 'Over-exploited',
                                 'Collapsed'),
                      labels = c('Unassessed', 'Developing',
                                 'Fully-exploited', 'Over-exploited',
                                 'Collapsed'),
                      values = c('UnAssessed' = '#A69569',
                                 'Developing' = '#A9D158',
                                 'Fully-exploited' = '#3fC1C9',
                                 'Over-exploited' = '#B389ED',
                                 'Collapsed' = '#fc5185')) +
    geom_scatterpie_legend(seq(3, 10),
                           x = 130, y = -65, n=3,
                           labeller = function(x) round(x^2, 2)) +
    labs(x = '',
         y = '') +
    theme(legend.position = 'none')

FigGrid <- ((fig1A | fig1B) / exploitMap_n / exploitMap) +
    plot_layout(heights = c(.8, 2, 2))
  
ggsave('./Visuals/Fig1.eps',
       plot = FigGrid, dpi = 600,
       height = 200, width = 134,units = 'mm')
