library(tidyverse)
library(broom)

kiva <- read_csv('kiva_loans.csv')


# 10 most popular countries for loans

kiva %>%
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

kiva %>%
  group_by(sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  head(10) %>%
  
  ggplot(aes(x = reorder(sector, count), y = count)) +
  geom_bar(stat = 'identity', fill = 'red', alpha = 0.5) +
  geom_text(aes(label = paste0("(", count, ")", sep = "")),
            hjust = 1) +
  labs(x = 'Sectors',
       y = 'Count', 
       title = '10 most popular Sectors + Counts') +
  coord_flip()

