#---------------------------------------------------------------------------#
# Nom : insulin_prices                                    			            #
# Description : Insulin prices in countries depending on income             #
# Auteur : Pietro Violo                                                     #
# Date : Feb 28  2023                                                       #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)

#---------------------------------------------------------------------------#
# Library and data                                                          #
#---------------------------------------------------------------------------#

library(tidyverse)
library(ggThemeAssist)

insulin <- read.csv("./Data/insulin_countries.csv") %>% 
  rename(Country.Code = cca3)

income <- read.csv("./Data/API_NY.ADJ.NNTY.PC.CD_DS2_en_csv_v2_4772062.csv",
                   skip = 3) %>% 
  select(Country.Name,
         Country.Code,
         X2020
         )
df <- left_join(insulin,income) %>% 
  mutate(relative_cost = X2020/avgCost)

# Adjusted net national income per capita (current US$)


#---------------------------------------------------------------------------#
# Bar plot                                                                  #
#---------------------------------------------------------------------------#

top7 <- df %>% arrange(relative_cost) %>% slice(1:7) 
bot7 <- df %>% arrange(desc(relative_cost)) %>% slice(1:7)

df <- rbind(top7, bot7)

# Top 7
df %>% 
  ggplot(aes(x = reorder(Country.Name, -relative_cost), y = relative_cost)) +
  geom_bar(stat = "identity") +
  theme_minimal() + scale_y_continuous(limits = c(0,7000), breaks = seq(0,7000, by = 1000)) + theme(panel.grid.minor = element_line(linetype = "blank"))

