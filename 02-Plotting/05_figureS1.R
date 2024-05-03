# Loading libraries
library(pacman)
p_load(tidyverse, ggplot2)

# Loading data
finalFFDR <- read_rds('Results/FFDRs.rds')

# Plotting
ffdrPlot <- ggplot(data = finalFFDR,
           aes(y=Taxa, x=meanFFDR, fill = Taxa)) +
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  position = position_dodge2(.9),
                  width = .3,
                  lwd = .5,
                  color = 'grey40') +
    geom_point(color = 'black', shape = 5, size = 1) +
    geom_vline(xintercept = 1.35, linetype = 'dashed') +
    geom_vline(xintercept = 1, linetype = 'dotted', color = 'navy') +
    scale_x_continuous(limits = c(0, 3),
                       breaks = seq(0, 3, 0.2),expand = c(0, 0)) +
    theme_bw(base_size = 7)+
    theme(legend.position = 'none',
          panel.border = element_blank(),
          axis.ticks.y = element_line(),
          axis.ticks.x = element_line(),
          axis.line.x.bottom = element_line(),
          axis.line.y.left = element_line(),
          text = element_text(size = 7)) +
    labs(x = 'Forage Fish Dependncy Ratio (FFDR)',
         y = '')

ggsave(plot = ffdrPlot,
       device = 'pdf', filename = 'Visuals/FigS1.pdf',
       width = 84, height = 50, units = 'mm')

