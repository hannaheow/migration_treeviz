load("migtree.Rdata")

states = data.frame(state.abb, state.name)
migtree = merge(ppu2, states, by.x = "statename", by.y = "state.abb")
#verify that the discrepancy in n rows is due to dc 
#dc = dplyr::anti_join(ppu2, states, by = c("statename" = "state.abb"))


library(tidyverse)
library(ggplot2)
#inputs: 
#cinterest = fips code for the county of interest
#type = aig or mig 
#comparison = withinstate or withinrural or none 


# origids = unique(migtree$origid)
# 
# oneline = function(origidid) { 
#   cinterest = migtree[migtree$origid == origidid,]
#   
#   p = ggplot() + 
#     geom_path(data = cinterest, 
#               aes(x = x_adj_mig, y = y_adj_mig),
#               color = "blue",
#               alpha = 0.8) +
#               #position = position_dodge(width = 0.2)) +
#     annotate("text", x = 0.5, y = 0, label = paste0(unique(cinterest$name), ", ", 
#                                                   unique(cinterest$statename))) + 
#     scale_color_identity() +
#     scale_size_identity() +
#     scale_alpha_identity() + 
#     #ggtitle(unique(cinterest$name)) + 
#     theme_bw()+
#     theme(panel.border = element_blank(), 
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.line = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title = element_blank(),
#           axis.text = element_blank(),
#           legend.position = "none")
#   print(p)
#  
#   }
# 
# library(gridExtra)
# pdf(file = "manyplots.pdf", onefile = TRUE)
# 
# for (i in 1:10) {
#   grid.arrange(oneline(origids[i]))
# }
# dev.off()


stateplots = list()
urbcolors = RColorBrewer::brewer.pal(6,"Set2")
colorpal = c(`6` = urbcolors[6], `5` = urbcolors[5], 
             `4` = urbcolors[4], `3` = urbcolors[3],
             `2` = urbcolors[2], `1` = urbcolors[1])

statenames = state.name
for (i in 1:length(statenames)){
migtreesub = migtree[migtree$state.name == statenames[i],]

p = ggplot() + 
  geom_path(data = migtreesub[order(migtreesub$year),],
            aes(x = x_adj_mig, y = y_adj_mig,  group = origid, color = as.factor(urbcode)),
            alpha = 0.9,  
  position = position_dodge(width = 0.2)) +
  annotate("text", x = 0, y = 0, label = unique(migtreesub$state.name))+
  scale_color_manual(values = colorpal, labels = c(`6` = "Noncore", 
                                                   `5` = "Micropolitan", 
                                                   `4` = "Small Metro", 
                                                   `3` = "Medium Metro", 
                                                   `2` = "Large Fringe Metro", 
                                                   `1` = "Large Central Metro"), 
                     drop = FALSE) + 
  scale_size_identity() +
  scale_alpha_identity() + 
  #ggtitle(unique(cinterest$name)) + 
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        legend.title = element_blank()) 
stateplots[[i]] = p 
}
library(ggpubr)





pdf(file = "manyplots.pdf", onefile = TRUE, width = 9, height = 17)
do.call("ggarrange", c(stateplots, nrow = 10, ncol = 5, common.legend = TRUE, legend = "bottom"))

dev.off()



library(crayon)

allfig = do.call("ggarrange", c(stateplots, nrow = 10, ncol = 5, common.legend = TRUE, legend = "bottom"))
tgrob = paste0("Inspired by ",
               hyperlink(text = "Kieran Healy's US Population Pyramids (1900-2019)",
               url = "https://kieranhealy.org/prints/pop-pyramids/"), "\n",
               ", these migration trees depict the structure and patterns of the US population over time", "\n",
               "in an artful manner. Much like a population pyramid reveals the age structure ", "\n",
               "of a population, a migration tree's depiction of net migration directionality ", "\n",
               "enables a quick understanding of movement trends into and out of specific places." , "\n",
               "In this representation, each US county is symbolized by a line or branch. ", "\n",
               "The branches curve right for years when more people move to a place than leave it,","\n",
               "and they curve left for years when more people leave than arrive. The tree's base ", "\n",
               "signifies the year 2009, while the branch tips correspond to the year 2020.", "\n",
               "Line colors indicate the urbanicity category of each county, as defined by the NCHS.")

# Amidst climate change, the tree metaphor emphasizes the branching of communities in response to environmental shifts. The structure exposes the intricacies of climate-induced migration, illustrating ripple effects and the evolution of places. This visualization enhances the understanding of climate-migration nuances, providing insights into how populations adapt in a rapidly changing world.
# The data utilized for this visualization is sourced from the ",
# hyperlink(text = "IRS's US County-to-County Migration Data",
# url = "https://www.irs.gov/statistics/soi-tax-stats-migration-data"),
# ". An additional tool for exploring county-specific trends can be found here.")
# 

pdf(file = "manyplots.pdf", onefile = TRUE, width = 9, height = 16)
ggpubr::annotate_figure(allfig, bottom = tgrob)
dev.off()
