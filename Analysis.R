library(tidyverse)
library(broom)
library(treemap)
library(scales)
library(leaflet)

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



#boxplots of different sectors, scaled 
loans %>%
  mutate(fill = factor(sector)) %>%
  ggplot(aes(x = sector, y = funded_amount, fill = sector)) +
  scale_y_log10(breaks = trans_breaks(trans = 'log10', inv = function(x) 10^x),
                labels = trans_format(trans = 'log10', format = math_format(10^.x))) +
  geom_boxplot() +
  labs(x = 'Sector type',
       y = 'Funded Amount',
       title = 'Distribution of funded amount') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#genders 

loans %>%
  filter(!is.na(borrower_genders)) %>%
  mutate(gender = if_else(str_detect(borrower_genders, 'female'), 'female', 'male')) %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  
  ggplot(aes(x = gender, y = count)) +
    geom_col(fill = 'green', alpha = 0.5) +
    geom_text(aes(x = gender, y = 1, label = count, hjust = 0, vjust =.5, size = 4)) +
    labs(x = 'Gender',
         y = 'Count',
         title = 'Gender Counts') +
  coord_flip()


#length of loans

loans %>%
  filter(!is.na(term_in_months)) %>%
  group_by(term_in_months) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(term_in_months = reorder(term_in_months, count)) %>%
  head(10) %>%
  
  ggplot(aes(x=term_in_months, y=count)) +
  geom_bar(stat = 'identity', color = 'white', fill = 'blue', alpha = 0.5 ) +
  geom_text(aes(x=term_in_months, y = 1, label = paste0("(",count,")",sep="")), 
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Term in Months', 
       y = 'Count', 
       title = 'Term in Months and Count') +
  coord_flip()
  
#leaflet for all loans in most 

leaflet(region_themes) %>%
  addTiles() %>%
  addCircles(lng= ~lon, lat = ~lat, radius= ~(amount)/10)
setView(lng=0, lat=0, zoom=2)




