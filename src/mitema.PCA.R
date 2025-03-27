library(ggplot2)
mitema <- theme(axis.title = element_text(size = 12, color ="darkgrey"),
                axis.line = element_line(color = "grey"),
                axis.text = element_text(size = 8),
                axis.text.x = element_text( ), 
                legend.position = "none",
                legend.text = element_text(size = 8),
                #legend.title = element_blank(),
                #legend.key.width= unit(0.4, 'cm'),
                panel.grid.major.x = element_blank(), #rayitas fond
                panel.grid.major.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white', colour = 'grey'))

mitema.L <- theme(axis.title = element_text(size = 12, color ="darkgrey"),
                axis.line = element_line(color = "grey"),
                axis.text = element_text(size = 8),
                axis.text.x = element_text( ), 
                legend.position = "bottom",
                legend.text = element_text(size = 8),
                panel.grid.major.x = element_blank(), #rayitas fond
                panel.grid.major.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white', colour = 'grey'))
