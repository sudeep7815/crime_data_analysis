library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)  # it is the package used to manipulate string
read.csv("13_Police_killed_or_injured_on_duty.csv")-> killed

clean_names(killed) ->police_injured
view(police_injured)

police_injured %>%
  rename("state_ut"= "area_name") ->police_injured


colnames(police_injured) %>% str_remove("police_") ->new_names
colnames(police_injured) <- new_names

view(police_injured)
police_injured %>% 
  pull(group_name) %>% unique()
police_injured %>% 
  pull(sub_group_name) %>% unique()

police_injured %>% 
  mutate(police_role = group_name %>% str_remove("Police - ")) %>% 
  select(-sub_group_name, -group_name)->removed_names
removed_names %>% 
  select(-injured_total_policemen, -killed_total_policemen) -> new_police 
new_police %>% 
  pivot_longer(c(3:14),
               names_to = "category",
               values_to = "count") ->  police_injured_cleaned
view(police_injured_cleaned)


police_injured_cleaned %>% 
  mutate(type = ifelse(str_detect(category,"injured"),"Injured", "Killed")) %>% 
  filter(police_role != "Total")-> police_killed_injured

police_killed_injured %>% 
  group_by(type) %>% 
  summarise(total=sum(count, na.rm = T)) %>% 
  ggplot(aes(type,total))+
  geom_col()+
  facet_wrap(~year) %>% 
  labs( title = "Police Killed or Injured",
        subtitle = "2001-2010",
        x= NULL,
        y = "Total count")

police_killed_injured %>% 
  group_by(type,year) %>% 
  summarise(total=sum(count, na.rm = T)) %>% 
  ggplot(aes(year, total, color = type))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 2001:2010)+
  scale_y_continuous(breaks = seq(1000,15000,1000))+
  scale_color_manual( values = c("red", "blue"))+
  theme_minimal()


police_killed_injured %>% 
  group_by(police_role,type) %>% 
  summarise(total=sum(count, na.rm = T)) %>% 
  ggplot(aes(police_role,total))+
  geom_col()+
  facet_wrap(~type, scales="free_y", ncol=1)+
  coord_flip()

