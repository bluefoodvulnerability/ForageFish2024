# Loading libraries
library(pacman)
p_load(tidyverse, extrafont)

# loading model results
ffDemand <- read_rds('./Results/FishmealUsage.rds')

# define level of taxon
source('./Scripts/00-Support Functions/02_modelSupport.R')

# plotting
ffDemand_plot <-
    ffDemand %>%
    mutate(Year = as.numeric(Year)) %>%
    select(-starts_with('...')) %>%
    filter(Year >= 1960) %>%
    mutate(Taxa = factor(Taxa,
                         levels = taxaLevel)) %>%
    ggplot(aes(x = Year, y = meanMO*5.2*1e-6, group = Taxa)) +
    geom_segment(aes(xend = Year,
                     y = q05MO*5.2*1e-6,
                     yend = q95MO*5.2*1e-6),
                color='#716e77',
                size = .1) +
    geom_point(color = '#005792', size = .3) +
    geom_line(color = '#fd5f00', linewidth = .5) +
    facet_wrap(~Taxa, scales = 'free_y') +
    labs(y = 'Forage fish consumption (mmt)') +
    theme_bw() +
    theme(text = element_text(size = 10),
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(1, "lines"),
          plot.margin = margin(l = 5, t = 3, b = 3, r= 5, unit = 'mm'),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)))

ggsave("./Visuals/Fig2.eps", 
       width = 174, height = 135, units = 'mm')
