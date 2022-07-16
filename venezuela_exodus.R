#---------------------------------------------------------------------------#
# Nom : Venezuela_exodus.r                                                  #
# Description : Sankey diagram to visualize Venezuela's migration crisis    #
# Auteur : Pietro Violo                                                     #
#---------------------------------------------------------------------------#


rm(list=ls(all=TRUE))
options(scipen=999)

# Library
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(tidyverse)

data <- data.frame(next_node =  c(1800000,
                                  1300000,
                                  513900,
                                  465200,
                                  448100,
                                  418200,
                                  345000,
                                  170300,
                                  121600,
                                  115300,
                                  402400)) %>% 
  mutate( x = factor("Venezuela"),
          node = 6100000,
          next_x = factor(c("Colombia",
                     "Peru",
                     "Ecuador",
                     "United States",
                     "Chile",
                     "Spain",
                     "Brazil",
                     "Argentina",
                     "Panama",
                     "Dominican Republic",
                     "Other")))

data$next_node/100

# GG sankey way of handling things is irrational, so we have to make a dataframe
# to correspond its required format

# And because it will order them alphabetically, we will have to use
# abc as the variable names while remembering the order of the true 
# countries when we will edit in Photoshop


Destination = c(rep("a",18000),rep("b", 13000),rep("c", 5139),
  rep("d", 4652), rep("e", 4481), rep("f",4182),
  rep("g", 3450), rep("h", 1703), rep("i", 1216),
  rep("j", 1153), rep("k",4024))

data <- data.frame(Origin = "", Destination = Destination,
                   Origin2 = "Venezuela") %>% 
  ggsankey::make_long(Origin, Destination, Origin2)

# Save graph

png("venezuela_exodus.png", res = 300, width = 2000, height = 1200)

ggplot(data, aes(x = x, next_x = next_x, 
                node = node, next_node = next_node, 
                fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 3, node.color = "gray30", width = 0.01, smooth = 6) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(1) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

dev.off()
