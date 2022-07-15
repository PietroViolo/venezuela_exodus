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


df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb) 

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)

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
                     "Other"))) %>% 
 
  library(tidyverse)
library(networkD3)

data <- tibble::tribble(
  ~Name, ~Annual.Amount,  ~Category,
  "Moira's Earnings",         50000L,   "Income",
  "Johnny's Earnings",        300000L,   "Income",
  "Living Expenses",        140000L, "Expenses",
  "Spent Savings",         25238L, "Expenses",
  "Liabilities",         44280L, "Expenses",
  "Planned Savings",         23000L, "Expenses",
  "Taxes",         98482L, "Expenses",
  "Insurance",         13000L, "Expenses"
)

Nodes = tibble(
  Name = c(data$Name, "Budget") %>% unique()
) %>% as.data.frame()


df = data %>% filter(Category=="Income") %>%
  select(-Category) %>%
  rename(Source = Name) %>%
  mutate(Target = "Budget") %>%
  bind_rows(
    data %>%
      filter(Category=="Expenses") %>%
      select(-Category) %>%
      rename(Target = Name) %>%
      mutate(Source = "Budget")
  ) %>% mutate(
    IDSource = match(Source, Nodes$Name)-1,
    IDTarget = match(Target, Nodes$Name)-1
  ) %>% as.data.frame()

sankeyNetwork(Links = df, Nodes = Nodes,
              Source = "IDSource", Target = "IDTarget",
              Value = "Annual.Amount", NodeID = "Name",
              sinksRight=FALSE, fontSize = 16)

