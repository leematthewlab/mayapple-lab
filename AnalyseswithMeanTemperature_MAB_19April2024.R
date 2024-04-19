#Regression Analyses for temperature and pollination rates

#Written by Melissa Burt

#For Lee Matthew's Poster



#load needed packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(lme4)
library(car)
library(hms)



#load needed data

TemperatureData_all <- merged_data_temp_with_treatment_full
str(TemperatureData_all)

#Create Chamber ID that contains pair number and treatment (ambient and warm)
TemperatureData_all$Chamber_ID_full <- NA
TemperatureData_all$Chamber_ID_full <- paste(TemperatureData_all$Chamber_ID, TemperatureData_all$Treatment, sep = "_")
TemperatureData_all$Chamber_ID_full <- as.factor(TemperatureData_all$Chamber_ID_full)

str(TemperatureData_all)
  
#Find the average temperature across all temperature values for each treatment within 
#each replicate pair.

TemperatureData_Means <- TemperatureData_all %>%
  group_by(Chamber_ID_full, Treatment, Chamber_ID) %>%
  summarise(MeanTemp_C = mean(Temperature))



#linear mixed effects model to determine the effect of treatment on mean temperature
tempmeans_lmer <- lmer(data = TemperatureData_Means,
                       MeanTemp_C ~ Treatment + (1|Chamber_ID))

plot(tempmeans_lmer)

shapiro.test(residuals(tempmeans_lmer)) #W = 0.96477, p-value = 0.6429
hist(residuals(tempmeans_lmer))

summary(tempmeans_lmer)
Anova(tempmeans_lmer)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: MeanTemp_C
#Chisq Df Pr(>Chisq)
#Treatment 0.3989  1     0.5277


#No significant difference between the temperature treatments in ambient vs. passive warming when looking at the averae

#Plot of temperature vs treatment
TreatmentVsTemperature <- ggplot(TemperatureData_Means, aes(x = Treatment, y = MeanTemp_C, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1), color = "black", size = 2) + # Jitter the points
  scale_fill_manual(values = c("#000080", "#4CAF50")) +  # Red for cold, blue for ambient
  labs(x = "Treatment", y = "Mean Temperature (°C)") +
  theme(text = element_text(size = 50)) +
  theme_classic()
png("TreatmentTemperatures_Means.png", units = "in", width = 4, height = 4, res = 400)
TreatmentVsTemperature
dev.off()

#Pollination versus temperature analyses

PollinationData <- MayappleData_VisitorObservations_with_time_2
str(PollinationData)
PollinationData$UnitID <- as.factor(PollinationData$UnitID)

colnames(PollinationData)[colnames(PollinationData) == "Count of Observations"] <- "ObservationCount"



# Split the time strings into hours, minutes, and seconds
time_components <- strsplit(PollinationData$TotalObservationLength, ":")

# Extract hours, minutes, and seconds
hours <- sapply(time_components, function(x) as.numeric(x[1]))
minutes <- sapply(time_components, function(x) as.numeric(x[2]))
seconds <- sapply(time_components, function(x) as.numeric(x[3]))

# Convert to total hours
total_hours <- hours + minutes / 60 + seconds / 3600

PollinationData$TotalHoursofObservation <- NA
PollinationData$TotalHoursofObservation <- total_hours


PollinationData_clean <- PollinationData %>%
  group_by(UnitID) %>%
  summarise(TotalPollinators = sum(ObservationCount),
            AvgObservationTime = mean(TotalHoursofObservation))
  

#Count of visits accounting for differences in total observation time

PollinationData_clean$PollinatorsPerTime <- NA
  
PollinationData_clean$PollinatorsPerTime <- PollinationData_clean$TotalPollinators/PollinationData_clean$AvgObservationTime

#Merge in treatment data
PollinationData_clean <- merge(PollinationData_clean, Treatment_For_Observations_Sheet1, by = "UnitID")

#Split Unit ID into number and letters

PollinationData_clean <- PollinationData_clean %>%
  mutate(Chamber_ID = sapply(strsplit(as.character(UnitID), "(?<=\\d)(?=[A-Za-z])", perl = TRUE), "[[", 1),
         TreatmentLetter = sapply(strsplit(as.character(UnitID), "(?<=\\d)(?=[A-Za-z])", perl = TRUE), "[[", 2))

#Create Chamber ID that contains pair number and treatment (ambient and warm)
PollinationData_clean$Chamber_ID_full <- NA
PollinationData_clean$Chamber_ID_full <- paste(PollinationData_clean$Chamber_ID, PollinationData_clean$Treatment, sep = "_")
PollinationData_clean$Chamber_ID_full <- as.factor(PollinationData_clean$Chamber_ID_full)

#Merge in mean temperature data from above
PollinationData_clean_withtemperature <- merge(PollinationData_clean, TemperatureData_Means, by = "Chamber_ID_Full")
  


#general linear regression
str(PollinationData_clean_withtemperature)
Temperature_lm <- lm(data = PollinationData_clean_withtemperature,
                     PollinatorsPerTime ~ MeanTemp_C)
plot(Temperature_lm)
hist(residuals(Temperature_lm))
shapiro.test(residuals(Temperature_lm))#W = 0.85643, p-value = 0.03458


Temperature_lm_log <- lm(data = PollinationData_clean_withtemperature,
                     log(PollinatorsPerTime + 1) ~ MeanTemp_C)
plot(Temperature_lm_log)
hist(residuals(Temperature_lm_log))
shapiro.test(residuals(Temperature_lm_log))#W = 0.87231, p-value = 0.05617


summary(Temperature_lm_log)
Anova(Temperature_lm_log)


plot(PollinationData_clean_withtemperature$MeanTemp_C, PollinationData_clean_withtemperature$PollinatorsPerTime)
abline(Temperature_lm_log)

str(PollinationData_clean_withtemperature)
ggplot(PollinationData_clean_withtemperature, aes(x = MeanTemp_C, y = PollinatorsPerTime, color = Treatment.y)) +
  geom_point() +
  labs(x = "Temperature (°C)", y = "Mean Pollinator Visits Per Hour", color = "Treatment") +
  theme_minimal()


#Analysis with ambient replicates only
PollinationData_clean_withtemperature_AmbientOnly <- filter(PollinationData_clean_withtemperature, Treatment.y == "Ambient")

str(PollinationData_clean_withtemperature_AmbientOnly)

Temperature_AmbientOnly_lm <- lm(data = PollinationData_clean_withtemperature_AmbientOnly,
                     PollinatorsPerTime ~ MeanTemp_C)
plot(Temperature_AmbientOnly_lm)
hist(residuals(Temperature_AmbientOnly_lm))
shapiro.test(residuals(Temperature_AmbientOnly_lm))#W = 0.92871, p-value = 0.5702


summary(Temperature_AmbientOnly_lm)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.95101    0.14787   13.19 0.000191 ***
#  MeanTemp_C  -0.13888    0.01095  -12.68 0.000223 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.01958 on 4 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9697 
#F-statistic: 160.9 on 1 and 4 DF,  p-value: 0.0002226
#Anova(Temperature_AmbientOnly_lm)

#Anova Table (Type II tests)
#Response: PollinatorsPerTime
#Sum Sq Df F value    Pr(>F)    
#MeanTemp_C 0.061650  1  160.86 0.0002226 ***
#  Residuals  0.001533  4                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


AmbientPollinatorVisitsVsTemp <- ggplot(PollinationData_clean_withtemperature_AmbientOnly, aes(x = MeanTemp_C, y = PollinatorsPerTime)) +
  geom_point(aes(color = "#000080")) +  # Set the color of points
  geom_smooth(method = "lm", se = TRUE, color = "#000080") +  # Add a best-fit line
  scale_color_identity(guide = "none") +  # Remove legend for color
  labs(x = "Temperature (°C)", y = "Mean Pollinator Visits Per Hour") + 
  theme(text = element_text(size = 50)) +
  theme_classic()


png("PollinatorVisitsvsTemp_AmbientOnly.png", units = "in", width = 4, height = 4, res = 400)
AmbientPollinatorVisitsVsTemp
dev.off()
