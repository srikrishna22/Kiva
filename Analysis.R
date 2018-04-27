library(tidyverse)
library(broom)
library(treemap)

#load data
loans <- read_csv('kiva_loans.csv')
regions <- read_csv('kiva_mpi_region_locations.csv')
loan_themes <- read_csv('loan_theme_ids.csv')
region_themes <- read_csv('loan_themes_by_region.csv')




# 10 most popular countries for loans

loans %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  head(10) %>%
  
  ggplot(aes(x = reorder(country, count), y = count)) +
  geom_bar(stat = 'identity', fill = 'red', alpha = 0.5) +
  geom_text(aes(label = paste0("(", count, ")", sep = "")),
            hjust = 1) +
  labs(x = 'Countries',
       y = 'Count', 
       title = '10 most popular Countries + Counts') +
  coord_flip()



# 10 most popular sectors for loans

loans %>%
  group_by(sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  head(10) %>%
  
  ggplot(aes(x = reorder(sector, count), y = count)) +
  geom_bar(stat = 'identity', fill = 'green', alpha = 0.5) +
  geom_text(aes(label = paste0("(", count, ")", sep = "")),
            hjust = 1) +
  labs(x = 'Sectors',
       y = 'Count', 
       title = '10 most popular Sectors + Counts') +
  coord_flip()


# 10 most popular activites for paying loans

loans %>%
  group_by(activity) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  
  ggplot(aes(x = reorder(activity, count), y = count)) +
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.5) +
  geom_text(aes(label = paste0("(", count, ")", sep = "")),
            hjust = 1) +
  labs(x = 'Activity',
       y = 'Count', 
       title = '10 most popular Activity + Counts') +
  coord_flip()



loans_funded_by_country <- loans %>%
                              group_by(country) %>%
                              summarise(ammount = sum(funded_amount)) %>%
                              arrange(desc(ammount)) %>%
                              head(20)

#treemap of funded amounts by country

treemap(loans_funded_by_country,
        index = 'country',
        vSize = 'ammount',
        title = 'Funded amount')


