library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)


#1. Read csv and clean column names and print column names (1)
read_csv("10_Property_stolen_and_recovered.csv") %>%
  clean_names() -> data10
  colnames(data10)

# 2. Rename area_name to state_ut
data10 %>%
  rename(state_ut = area_name)

# 3. How many state_ut are there?
n_distinct(data10$state_ut)

#4. How many crime groups are there and what are those?
unique_crime_groups <- unique(data10$group_name)
length(unique_crime_groups)
unique_crime_groups

# 5. What is “Total Property” in group_name and what should we do?
data10 %>%
  filter(group_name != "Total Property") -> data20
print(data)

# 6. What is the total no of property stolen cases and total value- All india

sum(data10$cases_property_stolen, na.rm = TRUE) -> total_stolen_cases
sum(data10$value_of_property_stolen, na.rm = TRUE) ->total_stolen_value

cat("Total Stolen Cases (All India):", total_stolen_cases, "\n")
cat("Total Value of Property Stolen (Rs.):", total_stolen_value, "\n")

# 7. What is the total no of property stolen cases and total value- All india- Year wise.

data10 %>%
  group_by(year) %>%
  summarise(
    total_cases = sum(cases_property_stolen, na.rm = TRUE),
    total_value = sum(value_of_property_stolen, na.rm = TRUE)
  ) -> yearwise_summary

print(yearwise_summary)

# Plot
ggplot(yearwise_summary, aes(x = year)) +
  geom_col(aes(y = total_cases), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = total_value * 10), color = "red", size = 1.2, group = 1) +
  scale_y_continuous(
    name = "Cases (Blue Bars)",
    sec.axis = sec_axis(~./10, name = "Value in Lakhs (Red Line)")
  ) +
  labs(title = "All India - Property Stolen: Cases and Value (Year-wise)",
       x = "Year") +
  theme_minimal()


# Q8: State-wise number of cases plot (5 marks)
data10 %>%
  group_by(state_ut) %>%
  summarise(total_cases = sum(cases_property_stolen, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) -> statewise_cases

ggplot(statewise_cases, aes(x = reorder(state_ut, total_cases), y = total_cases)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total Property Stolen Cases by State/UT",
       x = "State/UT",
       y = "Number of Cases") +
  theme_minimal()


# Q9: Correlation between stolen and recovered values (3 marks)
data10 %>%
  select(value_of_property_stolen,
         value_of_property_recovered) %>%
  cor(use = "complete.obs") -> cor_matrix

print(cor_matrix)

# Plot
corrplot(cor_matrix, method = "circle", type = "upper")

# Q10: Year-wise Stolen vs Recovered cases plot (5 marks)
data10%>%
  group_by(year) %>%
  summarise(
    stolen = sum(cases_property_stolen, na.rm = TRUE),
    recovered = sum(cases_property_recovered, na.rm = TRUE)
  ) -> yearly_cases

ggplot(yearly_cases, aes(x = year)) +
  geom_line(aes(y = stolen, color = "Stolen"), size = 1.2) +
  geom_line(aes(y = recovered, color = "Recovered"), size = 1.2) +
  labs(title = "Stolen vs Recovered Property Cases Year-wise",
       x = "Year",
       y = "Number of Cases",
       color = "Legend") +
  scale_color_manual(values = c("Stolen" = "red", "Recovered" = "blue")) +
  theme_minimal()

