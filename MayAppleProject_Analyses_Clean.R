#Mayapple Warming Experiment
#started: 10/30/2023
#People: Lee Matthew (leematthew@vt.edu) and Melissa Burt


#Objective: To find any significant differences in pollinator count, average temperature, 
#between the two treatments, and to plot figures to illustrate these differences. 
#including seed count...



#Load these packages:

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

install.packages("ggrepel")

#Load Datasets - give instructions here for what dataset is used in the code. 

#most recent, relevant "dataset.3.4.2024"

#note: code was put here most recent to least recent 
#-> this is because the two biggest dataset had to be redone becuase of missing data so most relevant code is most recent
#not complete, im missing times on observations dataset
##############################################################################
##############################################################################
##############################################################################
##############################################################################

#10/30/2023 - uploaded main datasets
#11/01/2023 - began merging ibutton IDs/unit IDs to datasets, tried factors unsuccessfully
#11/06/2023 - reupload and remerge data, did some na.omit, some unsuccessful t-tests
#11/08/2023 - observations before the last set was added, did t test
#observations of ambient vs warm, 45 vs 19, 0.007032 p value, this is not the final p value
#but thought i would keep it in though, also did some na.rm = true, did some binary work
#11/13/2023 - made observation treatments through making binary, redid more effectively later
#11/15/2023 - some observation and temperature tests i redid later, merging with treatments
#11/29/2023 - temp t-test p value for incomplete dataset
#pairwise_t_test_temp_warm_vs_ambient <- pairwise.t.test(merged_data_temp_with_treatment$`Temperature (C)`, merged_data_temp_with_treatment$Treatment, data = merged_data_temp_with_treatment)
#print(pairwise_t_test_temp_warm_vs_ambient)
#number give : 0.012 ! yippee?? it says P value adjustment method:holm
#this was for the temperature dataset that was missing data

#12/1/2023 - temperature with treatments
#re-merge temperature_full with treatments, was edited in original data to contain Chamber_ID as well as Ibutton_ID
#also when went back saw that first data set and missing data because of transfer so added that also

str(MayappleData_Temperature_Full)
na.omit(MayappleData_Temperature_Full)
merged_data_temp_with_treatment_full <- merge(MayappleData_ChamberTreatments_Ibutton_ID_good, MayappleData_Temperature_Full, by = "Ibutton_ID")
str(merged_data_temp_with_treatment_full)
View(merged_data_temp_with_treatment_full)
merged_data_temp_with_treatment_full$Treatment <- as.factor(merged_data_temp_with_treatment_full$Treatment)
str(merged_data_temp_with_treatment_full)
#yippee! new full temperature set merged with treatment and made ambient/warm into factors. 
#redo t.test
pairwise_t_test_temp_warm_vs_ambient_full <- pairwise.t.test(merged_data_temp_with_treatment_full$`Temperature (C)`, merged_data_temp_with_treatment_full$Treatment, data = merged_data_temp_with_treatment_full)
print(pairwise_t_test_temp_warm_vs_ambient_full)
#p value 0.056, not significant, heartbreaking, holm p value adjustment

#comparing time of ambient vs warm total video length
str(merged_data_attributes_with_treatment)
merged_data_attributes_with_treatment$Treatment <- as.factor(merged_data_attributes_with_treatment$Treatment)
str(merged_data_attributes_with_treatment)
na.omit(merged_data_attributes_with_treatment)
str(merged_data_attributes_with_treatment)
sum(merged_data_attributes_with_treatment$TotalBloomLength_Days)

#12/04/2023 - this too was redone to edited observation dataset
#divided pollinator observation with time, ibutton ids into factors, connected to attributes
#12/06/2023 - this too was redone to edited observation dataset
#1/29/2024 - skipped because was redone to edited observation dataset

#2/12/2024 - "data breach" found - pretty sure it was observation data?
#ibuttons as factors
MayappleData_Fixed_Temp_2$Ibutton_ID = as.factor(MayappleData_Fixed_Temp_2$Ibutton_ID)
str(MayappleData_Fixed_Temp_2)
#Make chamber id from number into factor with 10 levels
MayappleData_Fixed_Temp_2$Chamber_ID = as.factor(MayappleData_Fixed_Temp_2$Chamber_ID)
str(MayappleData_Fixed_Temp_2)


#2/16/24 - temperature figures
merged_mayappledata_fixed_temp_2_averaged <- merged_mayappledata_fixed_temp_2_filter %>%
  group_by(Date, Treatment) %>%
  summarise(Avg_Temperature = mean(`Temperature (C)`))

#line graph temperature of treatments
plot <- ggplot(merged_mayappledata_fixed_temp_2_averaged, aes(x = Date, y = Avg_Temperature, color = factor(Treatment)) +
  geom_line(size = 1) +
  geom_text(aes(label = sprintf("%.1f", Avg_Temperature)), 
            hjust = 0, vjust = 1, position = position_dodge(0.5), size = 2) + 
  labs(title = "Average Ambient and Warm Temperatures Over Time",
       x = "Date",
       y = "Average Temperature (C)",
       color = "Treatment") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") + 
  scale_color_manual(values = c("#4CAF50", "#008000")) + 
  theme_light() +
  theme(plot.background = element_rect(fill = "#F5F5DC"))
print(plot)


average_temp_weekly <- merged_mayappledata_fixed_temp_2_filter %>%
  group_by(Treatment, week = lubridate::week(Date)) %>%
  summarise(Avg_Temperature = mean(`Temperature (C)`))

#2/16/24 - tests gave weird results, no use
#2/23/2024 - graphs no longer useful
na.omit(merged_observations_to_time_by_factors_hms)
plot4 <- ggplot(merged_observations_to_time_by_factors_hms, aes(x = Treatment, y = sum_count, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Pollinator Count of Ambient and Warm",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))

print(plot4)

#the merging of the factors was incorrect and cut out 4 sets of data so we restart
str(MayappleData_VisitorObservations)

as.factor(MayappleData_VisitorObservations$`UnitID`)
str(MayappleData_VisitorObservations)


summary(merged_data_observations_with_treatment)
MayappleData_VisitorObservations$Pollinator_YorN_binary <- ifelse(MayappleData_VisitorObservations$Pollinator_YorN == "Y", 1, 0)
MayappleData_VisitorObservations$Antagonist_YorN_binary <- ifelse(MayappleData_VisitorObservations$Antagonist_YorN == "Y", 1, 0)

observations_by_binary_treatment <- tapply(MayappleData_VisitorObservations$`Count of Observations`, MayappleData_VisitorObservations$Treatment_binary, sum, na.rm = TRUE)
str(MayappleData_VisitorObservations)
sum_of_observations_by_factor_2 <- MayappleData_VisitorObservations %>%
  filter(!is.na(`Count of Observations`)) %>%
  group_by(`UnitID`) %>%
  summarise(sum_count = sum(`Count of Observations`, na.rm = TRUE))
print(sum_of_observations_by_factor_2)
MayappleData_VisitorObservations$`UnitID` <- as.factor(MayappleData_VisitorObservations$`UnitID`)
str(merged_data_observations_with_treatment)

#below is copied from above
#i need to merge the chamber attributes, which contain the time, to the observations

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
#trying to re-duration the time data
install.packages("hms")
library(hms)

MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength)

library("tidyr")



MayappleData_VisitorObservations_with_time_treatment_5 = MayappleData_VisitorObservations_with_time_treatment_5 %>%
  separate(TotalObservationLength, c("hours", "minutes", "seconds"), ":")
str(MayappleData_VisitorObservations_with_time_treatment_5)


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

#boxplots
str(MayappleData_VisitorObservations_with_time_treatment_6)
#rate of pollinator count in treatments
plot6 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = count_divided_by_time, fill = Treatment)) +
  geom_boxplot() +
  geom_point() + # Add points with jitter to spread out points horizontally
  labs(title = "Rate of Pollinator Count in Ambient and Warm",
       x = "Treatment",
       y = "Rate of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#000080")) +
  theme_classic()

print(plot6)
png("plot6.png", units = "in", width = 4.5, height = 4.5, res = 400)
plot6
dev.off()

#pollinator count in treatments, not accounting for video time
plot7 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = `MayappleData_VisitorObservations_with_time_treatment_5$\`Count of Observations\``, fill = Treatment)) +
  geom_boxplot() +
  geom_point() + # Add points with jitter to spread out points horizontally
  labs(title = "Sum of Pollinator Count in Ambient and Warm",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#000080")) +
  theme_classic()

print(plot7)
png("plot7.png", units = "in",  width = 4.5, height = 4.5, res = 400)
plot7
dev.off()

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
seed_data$Treatment <- factor(seed_data$Treatment, levels = c("Control", "Ambient", "Warm"))

plot20 <- ggplot(seed_data, aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Seed Count",
       x = "Treatment",
       y = "Count") +
  scale_fill_manual(values = c("#FF5733", "#4CAF50", "#000080")) +
  theme_classic()

print(plot20)
png("plot20.png", units = "in",  width = 4.5, height = 4.5, res = 400)
plot20
dev.off()
#fin: 03/17/2024

#04/01/2024
View(merged_data_temp_with_treatment_full)
str(merged_data_temp_with_treatment_full)
#in this date is a character
#time is in 'hms' num which im not sure,
#chamber_ID is also a number
#ibutton_ID is a character
#only treatment is a factor, so make chamber and ibutton into factor, make date into date

merged_data_temp_with_treatment_full$Ibutton_ID = as.factor(merged_data_temp_with_treatment_full$Ibutton_ID)
merged_data_temp_with_treatment_full$Chamber_ID = as.factor(merged_data_temp_with_treatment_full$Chamber_ID)

# Convert the Date column to a Date object
merged_data_temp_with_treatment_full$Date <- as.Date(merged_data_temp_with_treatment_full$Date, format = "%m/%d/%y")
str(merged_data_temp_with_treatment_full)

merged_data_temp_with_treatment_full_1 <- merged_data_temp_with_treatment_full %>%
  rename(Temperature = 'Temperature')

# Convert the date column to a Date object
merged_data_temp_with_treatment_full$Date <- mdy(merged_data_temp_with_treatment_full$Date)

result_8 <- merged_data_temp_with_treatment_full %>%
  group_by(Treatment, Chamber_ID, Date) %>%
  summarize(average_temp_8 = mean(Temperature))

result_8 <- result_8 %>% arrange(Date)

print(result_8)
View(result_8)
#result_8 is successful, will use for figures now in plot 
#figures made for extra, not used in finals but just for visulization

plot9 <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = factor(Treatment))) +
  geom_line() +
  geom_point() +
  labs(title = " Ambient and Warm Temperatures",
       x = "Date",
       y = "Temperature, the average for each date",
       color = "Chamber") +
  theme_minimal()

print(plot9)

plot_all_1 <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = factor(Chamber_ID), linetype = Treatment)) +
  geom_line(size = 0.5) +
  geom_point() +
  labs(title = "Temperature over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Temperature (C)",
       color = "Chamber",
       linetype = "Treatment") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "pink", "brown", "cyan", "gray", "magenta")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") + # Adjust x-axis date format
  theme_minimal()

print(plot_all_1)
str(result_8)

#boxplot of overall temperature by treatment, same that melissa made for poster
boxplot_3 <- ggplot(result_8, aes(x = Treatment, y = average_temp_8)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", color = "red", show.legend = FALSE) + # Add mean point
  stat_summary(fun.y = max, geom = "point", color = "blue", show.legend = FALSE) + # Add max point
  stat_summary(fun.y = min, geom = "point", color = "green", show.legend = FALSE) + # Add min point
  stat_summary(fun.y = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "red", size = 3, show.legend = FALSE) + # Add mean label
  stat_summary(fun.y = max, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "blue", size = 3, show.legend = FALSE) + # Add max label
  stat_summary(fun.y = min, geom = "text", aes(label = round(..y.., 2)), vjust = 1.5, color = "green", size = 3, show.legend = FALSE) + # Add min label
  labs(title = "Boxplot of Temperature by Treatment",
       x = "Treatment",
       y = "Average Temperature") +
  theme_minimal()
print(boxplot_3)

#04/08/2024
#redoing temperature t-tests, pollination-temperature regression

lastpairedtemp <- t.test(merged_data_temp_with_treatment_full$`Temperature (C)` ~ merged_data_temp_with_treatment_full$Treatment, paired = TRUE)
print(lastpairedtemp)
#unpaired = pvalue 0.05616

View(result_8_first_month)
pairedtempfirstmonth <- t.test(result_8_first_month$average_temp_8 ~ result_8_first_month$Treatment, paired = TRUE)
print(pairedtempfirstmonth)
#p value in the first month, paired = 0.3434, df = 123, t = -0.95113, with a mean difference of -0.1430238

View(result_8)
lastpaired_1 <- t.test(result_8$average_temp_8 ~ result_8$Treatment, paired = TRUE)
print(lastpaired_1)
#averages for each day in a t.test, unpaired, p value = 0.54, df = 537.93, t = -0.61324
#mean in group Ambient = 13.28028 unpaired
#mean in group Warm = 13.42158 unpaired

#fin: 04/29/2024