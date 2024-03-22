#Mayapple Warming Experiment

#People: Lee Matthew and Melissa Burt


#Objective: To find any significant differences in pollinator count, average temperature, 
#between the two treatments, and to plot figures to illustrate these differences. 
#including seed count...



#Load these packages:

library(dplyr)
library(tidyr)
library(ggplot2)

#Load Datasets - give instructions here for what dataset is used in the code. 



#most recent to least recent 
##############################################################################
##############################################################################
##############################################################################
##############################################################################


#2/26/2024 cont

MayappleData_VisitorObservations <- MayappleData_VisitorObservations %>%
  rename('UnitID' = 'Unit ID')
str(merged_data_observations_with_treatment)

MayappleData_VisitorObservations_with_time_2 <- merge(MayappleData_VisitorObservations, MayappleData_ChamberAttributes, by = "UnitID")
View(MayappleData_VisitorObservations_with_time_2)

MayappleData_VisitorObservations_with_time_treatment_5 <- merge(MayappleData_VisitorObservations_with_time_2, Treatment_For_Observations_Sheet1, by = "UnitID")
View(MayappleData_VisitorObservations_with_time_treatment_5)
str(MayappleData_VisitorObservations_with_time_treatment_5)

MayappleData_VisitorObservations_with_time_treatment_5$Treatment = as.factor(MayappleData_VisitorObservations_with_time_treatment_5$Treatment)
str(MayappleData_VisitorObservations_with_time_treatment_5)

MayappleData_VisitorObservations_with_time_treatment_5$UnitID = as.factor(MayappleData_VisitorObservations_with_time_treatment_5$UnitID)
str(MayappleData_VisitorObservations_with_time_treatment_5)

#reduration time data
MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength)
MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  separate(TotalObservationLength, c("hours", "minutes", "seconds"), ":")
str(MayappleData_VisitorObservations_with_time_treatment_5)

MayappleData_VisitorObservations_with_time_treatment_5$hours = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$hours)
MayappleData_VisitorObservations_with_time_treatment_5$minutes = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$minutes)
MayappleData_VisitorObservations_with_time_treatment_5$seconds = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$seconds)
str(MayappleData_VisitorObservations_with_time_treatment_5)

#merged_observations_to_time_by_factor$minutes / 60
#merged_observations_to_time_by_factor$seconds / 60 / 60

MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  mutate(minutes_by_hour_5 = minutes / 60)

str(MayappleData_VisitorObservations_with_time_treatment_5)

MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  mutate(seconds_by_hour_5 = seconds / 3600)

#now i add the minutes and seconds into the hours column

MayappleData_VisitorObservations_with_time_treatment_5$total_time_duration_hours = rowSums(MayappleData_VisitorObservations_with_time_treatment_5[, c("hours", "minutes_by_hour_5", "seconds_by_hour_5")], na.rm = TRUE)
#yippe it worked! now time to divide the count by time

#erm so i tried to make a sum count and messed it up
MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  mutate(count_divided_by_time = sum_count / total_time_duration_hours) #did not do yet

MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  select(-Notes.x)

MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  select(-Notes.y)

sum_observations_1 <- aggregate(MayappleData_VisitorObservations_with_time_treatment_5$`Count of Observations` ~ MayappleData_VisitorObservations_with_time_treatment_5$UnitID, data = MayappleData_VisitorObservations_with_time_treatment_5, sum)

View(sum_observations_1)
#made version 6 for merging
#03/01/2024
#t-test of rate of pollinator count (accounting for video time)
MayappleData_VisitorObservations_with_time_treatment_6 <- merge(MayappleData_VisitorObservations_with_time_treatment_5, sum_observations_1, by.x = "UnitID", by.y = "MayappleData_VisitorObservations_with_time_treatment_5$UnitID")
View(MayappleData_VisitorObservations_with_time_treatment_6)

MayappleData_VisitorObservations_with_time_treatment_6 = MayappleData_VisitorObservations_with_time_treatment_6 %>%
  mutate(count_divided_by_time = MayappleData_VisitorObservations_with_time_treatment_6$`MayappleData_VisitorObservations_with_time_treatment_5$\`Count of Observations\`` / MayappleData_VisitorObservations_with_time_treatment_6$total_time_duration_hours)


ambient_count_divided_by_time_6 = MayappleData_VisitorObservations_with_time_treatment_6$count_divided_by_time[MayappleData_VisitorObservations_with_time_treatment_6$Treatment == "Ambient"]
warm_count_divided_by_time_6 = MayappleData_VisitorObservations_with_time_treatment_6$count_divided_by_time[MayappleData_VisitorObservations_with_time_treatment_6$Treatment == "Warm"]

View(warm_count_divided_by_time_6)

t_test_observation_count_divided_by_time_6 = t.test(ambient_count_divided_by_time_6, warm_count_divided_by_time_6)
print(t_test_observation_count_divided_by_time_6)
#results ata:  ambient_count_divided_by_time_6 and warm_count_divided_by_time_6
#t = 8.3272, df = 111.35, p-value = 2.366e-13

#plots
str(MayappleData_VisitorObservations_with_time_treatment_6)
#rate of pollinator count in treatments
plot6 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = count_divided_by_time, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Rate of Pollinator Count in Ambient and Warm Treatments",
       x = "Treatment",
       y = "Rate of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))
print(plot6)

#pollinator count in treatments, not accounting for video time
plot7 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = `MayappleData_VisitorObservations_with_time_treatment_5$\`Count of Observations\``, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Sum of Pollinator Count in Ambient and Warm",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))
print(plot7)

#seed count data, manually entered, as cannot test because no warm treatment seeds
controlseed <- c(46, 32)
ambientseed <- c(49, 47, 12)
warmseed <- c(0)

seed_data <- data.frame(Treatment = rep(c("Control", "Ambient", "Warm"), 
                                        times = c(length(controlseed), 
                                                  length(ambientseed), 
                                                  length(warmseed))),
                        Count = c(controlseed, ambientseed, warmseed))
print(seed_data)

#seed count plot
plot20 <- ggplot(seed_data, aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Seed Count",
       x = "Treatment",
       y = "Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000", "#FF5733"))

print(plot20)
