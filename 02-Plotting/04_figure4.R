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

source('./Scripts/00-Support Functions/02_modelSupport.R')

scenarioModelRes_path <- './Results/Senario.model.results.rds'
FF.Replace.PATH <- 'Results/FF.replacement.res.rds'

scenarioModelRes <- read_rds(scenarioModelRes_path)
replaceRes <- read_rds(FF.Replace.PATH)

p_diet <- plotdietIncrease(scenarioModelRes, 
                           .fill = rev(c('#B389ED',
                                         '#364F6B',
                                         '#3FC1C9',
                                         '#A9D158',
                                         '#FFD947',
                                         '#FC5185')),
                           originDiet.path = 'Data/Aquacultur.taxa.diet.xlsx')


fig4A <- p_diet + 
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = 'Fishmeal inclusion rate (%) required to maintain 2022 production',
       y = 'Scenarios') +
  geom_vline(xintercept = 0, lty = 'dotted') +
  scale_y_discrete(labels = rev(c('Baseline', 'I. Fishing ban',
                                  'II. Quota cut', 'III. IUCN alert',
                                  'IV. Climate impact', 'V. Compound crisis'))) + 
  guides(fill = guide_legend(nrow = 1,
                             reverse = T)) +
  theme(strip.background = element_rect(fill = 'grey90'),
        strip.text = element_text(hjust = .5, size = 7,
                                  face = 'bold'),
        axis.title = element_text(size =11,
                                  family = 'Helvetica',
                                  face = 'bold'),
        legend.position = 'none')

replaceRes <- replaceRes %>%
  mutate(taxa = str_to_title(taxa)) %>%
  mutate(taxa = factor(taxa,
                       levels = taxaLevel))

Fig4B <- replaceRes %>%
  group_by(Scenario, run) %>%
  summarise(totalFM = sum(fMeal*1e-6)) %>%
  ggplot(aes(x = Scenario, y = totalFM, fill = Scenario)) +
  geom_jitter(color = "grey80", size=0.9, shape = 1, show.legend = F) +
  geom_violin(show.legend = F,
              draw_quantiles = c(.05, .5, .95)) +
  #scale_y_continuous(breaks = seq(0, 150, 25)) +
  scale_fill_manual(values = c('#364F6B',
                               '#3FC1C9',
                               '#A9D158',
                               '#FFD947',
                               '#FC5185')) +
  labs(x = 'Scenarios', y = 'Total to be replaced (mmt)') +
  theme_bw(base_size = 8) +
  theme(plot.margin = margin(l = 1, r = 3, unit = 'cm'),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, 
                                                    b = 0, l = 0)),
        axis.title = element_text(size =11,
                                  family = 'Helvetica',
                                  face = 'bold'))

fig4 <- (fig4A / Fig4B) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(2, 1))

ggsave(plot = fig4,
       device = 'pdf', filename = 'Visuals/Fig4.pdf',
       width = 234, height = 234, units = 'mm')
