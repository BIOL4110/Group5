# Figure: Proportion of positive AIV over study period

# download libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Clean data
FluData <- filter(Dryad_revised) %>%
  select(c(Region, Species, Sex, AIV, Sampling_Site, Year_sampling, Month_sampling, 
           Temperature, Pond_Density, Population_Density))

# Filter for Blue-winged teals and mallards only
FluDataBM<-FluData %>% filter(Species=="BWTE"|Species=="MALL")

# Create Year-Month column
AIV_frequency <- FluDataBM %>%
  select(Month_sampling, Year_sampling, AIV) %>%
  mutate(Year_Month = paste(Year_sampling, Month_sampling, sep = "-"))

# Filter AIV tests
AIV_1 <- AIV_frequency %>% filter(AIV == 1)
AIV_0 <- AIV_frequency %>% filter(AIV == 0)

# Set Year-Month as a date
AIV_frequency$Year_Month <- ym(AIV_frequency$Year_Month)

# Check Year-Month is a date
str(AIV_frequency)

# Calculate counts for AIV = 0 and AIV = 1
AIV_frequency_proportions <- AIV_frequency %>%
  filter(AIV == 0 | AIV == 1) %>%
  group_by(Year_Month, AIV) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Calculate total counts for each Year_Month
  group_by(Year_Month) %>%
  mutate(total_count = sum(count)) %>%
  # Calculate proportion for each AIV group
  mutate(proportion = count / total_count) %>%
  ungroup()

# View the updated dataset
print(AIV_frequency_proportions)

# Create stacked bar graph
ggplot(AIV_frequency_proportions, aes(x = Year_Month, y = proportion, fill = factor(AIV))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("skyblue", "red"), labels = c("AIV = 0", "AIV = 1")) +
  labs(title = "Proportion of infected birds from 2005 to 2011", 
       x = "Year-Month", 
       y = "Percentage of sampled infected with AIV", 
       fill = "AIV Status") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
