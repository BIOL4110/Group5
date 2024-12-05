# CLEANING/LM/AIC Code

library(dplyr)

# find sum of AIV for each site
sites <- Dryad_revised %>% 
  group_by(Sampling_Site) %>%
  summarize(INFValue = (sum(AIV)))

# find how many times each site is mentioned in original dataset
sites1 <- Dryad_revised %>% 
  count(Sampling_Site)
sites1

# merge tables to get one dataframe with site, number infected, and total number
table<-merge(sites,sites1)
table

# add column to table with infected/total (proportion)
table<-table %>% 
  mutate(proportion=INFValue/n)
table

# add proportion values to original dataset
DATAFRAME <- merge(Dryad_revised, table, by = "Sampling_Site", all.x = TRUE)
print(DATAFRAME)

# take habitat type data and remove replicates to get just one result for each site
habitats <- habitat_data[!duplicated(habitat_data$Sampling_Site), ]

# merge habitat types to main df
DATA_FRAME <- merge(DATAFRAME, habitats, by = "Sampling_Site", all.x = TRUE)
print(DATA_FRAME)

#make sure data frame has only important columns
DATA_FRAME2<-DATA_FRAME %>%
  select(proportion,Sampling_Site,Month_sampling,Temperature,Type_Of_Site,n,Sex)
names(DATA_FRAME2)

# run linear model with proportion AIV infected as response variable, and the rest as predictor variables
lm_model<-lm(proportion~Sex+Temperature+Type_Of_Site+n,data=DATA_FRAME2,na.action = "na.fail")
#proportion = proportion of pop in each site infected with AIV
#sex = sex (M or F)
#Temperature = temp in degrees Celsius
#Type of site = habitat type (pond, lake, river, etc)
#n = total number of mallards/ducks observed in each site

library(MuMIn)

#run dredge function to find AIC values
mod_sel<-dredge(lm_model)
View(mod_sel)
