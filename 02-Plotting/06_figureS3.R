library(pacman)
p_load(sf, ggdist, scales, tidyverse, 
       paletteer, ggthemes, measurements)

source('./Scripts/00-Support Functions/02_modelSupport.R')

scenarioModelRes_path <- './Results/Senario.model.results.rds'
FF.Replace.PATH <- 'Results/FF.replacement.res.rds'

scenarioModelRes <- read_rds(scenarioModelRes_path)
replaceRes <- read_rds(FF.Replace.PATH)

FigS3 <- ggplot(replaceRes, aes(x = Scenario, y = fMeal*1e-6,
                                fill = Scenario)) +
  geom_violin(draw_quantiles = c(.05, .5, .95), show.legend = F) +
  scale_fill_manual(values = c('#364F6B',
                               '#3FC1C9',
                               '#A9D158',
                               '#FFD947',
                               '#FC5185')) +
  facet_wrap(~taxa, ncol = 4,
             scales = 'free') +
  theme_bw(base_size = 10) +
  labs(x = "Scenarios",
       y = 'Fishmeal to be replaced (mmt)') +
  theme(panel.spacing.x = unit(1, "lines"),
        plot.margin = margin(l = 1, r = 3, unit = 'cm'),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggsave(plot = FigS3,
       device = 'pdf', filename = 'Visuals/FigS3.pdf',
       width = 234, height = 100, units = 'mm')
