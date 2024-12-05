library(ggplot2)
##run linear model with proportion AIV infected as response variable, and the rest as predictor variables
model<-lm(proportion~Sex+Month_sampling+Temperature+Type_Of_Site+n,data=DATA_FRAME2,na.action = "na.fail")

##create a scatterplot with regression line for sampling site
PLOT1 <- ggplot(DATA_FRAME2, aes(x = Sampling_Site, y = proportion)) + 
  geom_point() +
  stat_smooth(method = lm) +
  labs(title = "Proportion of Pop. Infected within each Sampling Site", 
       x = "Sampling Sites", 
       y = "Proportion of Population Infected (AIV)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + #to separate the labels into 2 rows
  theme(axis.text.x = element_text(color = "grey20", size = 7, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 6, face = "plain"))+
  coord_flip()
PLOT1

##scatterplot for month_sampling
PLOT2 <- ggplot(DATA_FRAME2, aes(x = Month_sampling, y = proportion)) + 
  geom_point(alpha = 0.4) +
  stat_smooth(method = "lm") +
  labs(title = "Monthly Population Infected with Avian Influenza Virus (AIV)", 
       x = "Months", 
       y = "Proportion of Population Infected (AIV)")
PLOT2
##scatterplot for temperature
PLOT3 <- ggplot(DATA_FRAME2, aes(x = Temperature, y = proportion)) + 
  geom_point(aes(alpha = 0.4)) +
  scale_shape_binned()+
  stat_smooth(method = "lm")+
  labs(title = "Temperatures of Population Infected with Avian Influenza Virus (AIV)", 
       x = "Temperatures (Celsius)", 
       y = "Proportion of Population Infected (AIV)")
PLOT3 +
  guides(fill = "none") #to try to remove the alpha legend

##scatterplot type of site
PLOT4 <- ggplot(DATA_FRAME2, aes(x = Type_Of_Site, y = proportion)) + 
  geom_point() +
  stat_smooth(method = "lm")+
  labs(title = "Habitats of Population Infected with Avian Influenza Virus (AIV)", 
       x = "Habitat of Sampling Site", 
       y = "Proportion of Population Infected (AIV)")
PLOT4
##scatterplot for sex
PLOT5<-ggplot(DATA_FRAME2, aes(x = Sex, y = proportion)) + 
  geom_point(alpha=0.03,  
             position = position_jitter(width = .05, height = 0)) +
  stat_smooth(method = "lm")+
  labs(title = "Sex of Population Infected with Avian Influenza Virus (AIV)", 
       x = "Sex", 
       y = "Proportion of Population Infected (AIV)")
PLOT5

##prints out the summary of the model:
summary(model)

##To check whether the observed data meets the model assumption
par(mfrow=c(2,2))
plot(model)