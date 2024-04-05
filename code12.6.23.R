
#mayapple project data entry into R and analysis
#lee matthew (leematthew@vt.edu)
#10/30/2023

plot(MayappleData_ChamberAttributes)
na.omit(MayappleData_Temperature)

plot(MayappleData_Temperature$Temperature(C), MayappleData_Temperature$Ibutton_ID)
plot(MayappleData_Temperature$Ibutton_ID, MayappleData_Temperature$`Temperature (C)')'


str(MayappleData_Temperature)



#11/01/2023
merged_data_temp_with_treatment <- merge(MayappleData_ChamberTreatments_Ibutton_ID_good, MayappleData_Temperature, by = "Ibutton_ID")
View(MayappleData_ChamberTreatments_Ibutton_ID)
#to do: merge treatments onto data sets- start with temp
#ttest of temperature and treatment; treatment and pollinator visitation


merged_data_observations_with_treatment <- merge(MayappleData_ChamberTreatments_Unit_ID, MayappleData_VisitorObservations, by = "Unit ID")
#note!!! in visitor observations, it is 'Unit ID' instead of 'Ibutton_ID', and so seperate treatment mini data was used, with titles changed so it could be merged

merged_data_attributes_with_treatment <- merge(MayappleData_ChamberTreatments_UnitID_Nospace, MayappleData_ChamberAttributes, by = "UnitID")
#note!!! same as observations.... but without the space. So made UnitID nospace and merged that instead


#somehow i named the merged data sets wrong.... ummm... lemme fix that
#yay!! ttest time??
merged_data_temp_with_treatment <- na.omit(merged_data_temp_with_treatment)
t_test_temp_with_treatment <- t.test(merged_data_temp_with_treatment$Treatment, merged_data_temp_with_treatment$`Temperature (C)`)
View(merged_data_temp_with_treatment)
View(MayappleData_Temperature)
View(MayappleData_ChamberTreatments_Ibutton_ID)


ambient_value <- "Ambient"


warm_value <- "Warm"
warm_factor <- as.factor(warm_value)

t_test_temp_with_treatment <- t.test(merged_data_temp_with_treatment$Treatment, merged_data_temp_with_treatment$`Temperature (C)`)


# Check if ambient_factor is a factor
if (is.factor(ambient_factor)) {
  print("ambient_factor is a factor variable.")
} else {
  print("ambient_factor is not a factor variable.")
}

#uhhhh it says its not a factor


summary(merged_data_temp_with_treatment)


#threw some themes in
rstudioapi::addTheme("https://raw.githubusercontent.com/jnolis/synthwave85/master/Synthwave85.rstheme", TRUE, TRUE, FALSE)

oceanic_theme <- "https://raw.githubusercontent.com/gadenbuie/oceanic-eighties/master/oceanic-eighties.rstheme"
rstudioapi::addTheme("https://raw.githubusercontent.com/gadenbuie/oceanic-eighties/master/oceanic-eighties.rstheme", TRUE, TRUE, FALSE)


#ok back to chatgpt trying to get ambient and warm to be conditions

View(MayappleData_ChamberTreatments_Ibutton_ID)
#it seems ive made it much much worse but im not sure how to reverse it ummmmmm
#i took back the code i wrote but still the effects are there


#but this was basically it >>>
# Sample data frame
df <- data.frame(Category = c("A1", "A2", "W1", "W2", "A1"))

# Create factors for "ambient" and "warm"
df$Category <- factor(df$Category, levels = c("A1", "A2", "W1", "W2"),
                      labels = c("ambient", "ambient", "warm", "warm"))

#<<<<<
#ok so i think i broke the dataset ummm
#im going to throw the mayapple..._Ibutton_ID away and replace it with mayapple..._Ibutton_ID_good
#ok i deleted the messed up Ibutton_ID and replace with _Ibutton_ID_good



merged_data_temp_with_treatment <- merge(MayappleData_ChamberTreatments_Ibutton_ID_good, MayappleData_Temperature, by = "Ibutton_ID")
View(merged_data_temp_with_treatment)
#wait it worked now !! yippee!

plot(merged_data_temp_with_treatment$Ibutton_ID, merged_data_temp_with_treatment$`Temperature (C)`, na.action = na.pass)

summary(merged_data_temp_with_treatment)

#ok now i am going to try to change the class of 'Treatment' from 'character' to 'factor'

factor(merged_data_temp_with_treatment$Treatment)

#yippee! i think it worked! now i will check
class(merged_data_temp_with_treatment$Treatment)
#even though it says 'levels: Ambient Warm' it still says there are "character"!
#imma check again

# Check if ambient_factor is a factor
if (is.factor(merged_data_temp_with_treatment$Treatment)) {
  print("treatment is a factor variable.")
} else {
  print("treatment is not a factor variable.")
}
#it says it is not a factor this is heartbreaking

summary(merged_data_temp_with_treatment)
#although it says treatment has two levels - ambient and warm.... so maybe it did work

plot(merged_data_temp_with_treatment$Ibutton_ID, merged_data_temp_with_treatment$`Temperature (C)`)
summary(merged_data_temp_with_treatment, na.rm = TRUE)

#i try to seperate the ambient and warm once again
subset_ambient <- subset(merged_data_temp_with_treatment, Treatment == "Ambient")
subset_warm <- subset(merged_data_temp_with_treatment, Treatment == "Warm")
#did i do it? i see no error

plot(merged_data_temp_with_treatment$Ibutton_ID, merged_data_temp_with_treatment$`Temperature (C)`)
summary(merged_data_temp_with_treatment)
merged_data_temp_with_treatment$...5 <- NULL
#idk what ...5 is so throw away

barplot(merged_data_temp_with_treatment$Ibutton_ID, merged_data_temp_with_treatment$`Temperature (C)`)
table(merged_data_temp_with_treatment)
#this table is very concerning

#11/06/2023
#had to reupload and remerge data whyyyyy
#ok saved yippee!
table(merged_data_temp_with_treatment)
merged_data_temp_with_treatment <- na.omit(merged_data_temp_with_treatment)
plot(merged_data_temp_with_treatment$Ibutton_ID, merged_data_temp_with_treatment$`Temperature (C)`, na.exclude())

merged_data_temp_with_treatment <- merged_data_temp_with_treatment[is.finite(merged_data_temp_with_treatment$Treatment) & is.finite(merged_data_temp_with_treatment$`Temperature (C)`), ]

median(merged_data_temp_with_treatment$`Temperature (C)`, na.rm = TRUE)
#dude how did i delete all of the temperature data 

merged_data_temp_treatment_good <- merge.data.frame(MayappleData_Temperature, MayappleData_ChamberTreatments_Ibutton_ID_good)
merged_data_temp_with_treatment <- NULL
merged_data_temp_treatment_good$...5 <- NULL

plot(merged_data_temp_treatment_good$Ibutton_ID, merged_data_temp_treatment_good$`Temperature (C)`, xlim = c(-50, 100))
plot(na.exclude(merged_data_temp_treatment_good$Ibutton_ID), na.exclude(merged_data_temp_treatment_good$`Temperature (C)`))

temp_na_omitted <- na.omit(merged_data_temp_treatment_good$`Temperature (C)`)

plot(merged_data_temp_treatment_good$Ibutton_ID, x)

summary.data.frame(merged_data_temp_treatment_good)
t.test(merged_data_temp_treatment_good$Ibutton_ID, x)

ibutton_id_na_omitted <- na.omit(merged_data_temp_treatment_good$Ibutton_ID)

t.test(temp_na_omitted, ibutton_id_na_omitted)

merged_temp_na_omitted <- na.omit(merged_data_temp_treatment_good)
summary(merged_temp_na_omitted)
#ok yay! no na in data set anymore hehe

plot(merged_temp_na_omitted)
temp_split_by_ambient <- split(merged_temp_na_omitted$`Temperature (C)`, "ambient_factor", drop = FALSE)
summary(temp_split_by_factor)
levels(temp_split_by_ambient)
temp_split_by_warm <- split(merged_temp_na_omitted$`Temperature (C)`, "warm_factor", drop = FALSE)
summary(temp_split_by_warm)
median(temp_split_by_ambient, na.rm = TRUE)
sum(temp_split_by_ambient)
na.omit(temp_split_by_ambient)
na.omit(temp_split_by_warm)
median(temp_split_by_ambient)

summary(temp_split_by_ambient)





# Create an empty vector to store the medians
medians_ambient <- numeric(length(temp_split_by_ambient))

# Calculate the median for each group and store it in the 'medians' vector
for (i in seq_along(temp_split_by_ambient)) {
  medians_ambient[i] <- median(temp_split_by_ambient[[i]], na.rm = TRUE)
}

View(medians_ambient
     )




#chatgpt said so
# Create an empty vector to store the medians
medians_warm <- numeric(length(temp_split_by_warm))

# Calculate the median for each group and store it in the 'medians' vector
for (i in seq_along(temp_split_by_warm)) {
  medians_warm[i] <- median(temp_split_by_warm[[i]], na.rm = TRUE)
}

View(medians_warm
)


#11/08/2023
# ok remember to load back the "mayapple_all_datasets" every time opening project i suppose

summary(merged_data_observations_with_treatment)
merged_data_observations_with_treatment$Pollinator_YorN_binary <- ifelse(merged_data_observations_with_treatment$Pollinator_YorN == "Y", 1, 0)
merged_data_observations_with_treatment$Antagonist_YorN_binary <- ifelse(merged_data_observations_with_treatment$Antagonist_YorN == "Y", 1, 0)
merged_data_observations_with_treatment$Treatment_binary <- ifelse(merged_data_observations_with_treatment$Treatment == "Warm", 1, 0)

observations_by_binary_treatment <- tapply(merged_data_observations_with_treatment$`Count of Observations`, merged_data_observations_with_treatment$Treatment_binary, sum, na.rm = TRUE)
View(observations_by_binary_treatment)

#again with the NAs! only the ambient has NA the warm has a value
#yay! i fixed it with the na.rm = true! now i have results!


#this is how observations of ambient vs warm, 45 vs 19

observant_warm <- merged_data_observations_with_treatment$`Count of Observations`[merged_data_observations_with_treatment$Treatment_binary == 1]
observant_ambient <- merged_data_observations_with_treatment$`Count of Observations`[merged_data_observations_with_treatment$Treatment_binary == 0]
View(observant_warm)
View(observant_ambient)

t_test_observant_warm_vs_ambient <- t.test(observant_warm, observant_ambient)
print(t_test_observant_warm_vs_ambient)
#yasss slay 0.007032 p value!!! hooray!!!

#ok time for multiple binary condition time ummmm
#ok this is kind of a lot ummmmm 

observations_by_all_binary_treatments <- merged_data_observations_with_treatment$`Count of Observations`[(merged_data_observations_with_treatment$Pollinator_YorN_binary == 1) & 
    (merged_data_observations_with_treatment$Antagonist_YorN_binary == 0) & 
    (merged_data_observations_with_treatment$Treatment_binary == 0)]

View(observations_by_all_binary_treatments)


#huh i wonder if i can do this with ambient and warm instead of and make them kind of factors like that

merged_data_temp_with_treatment$Treatment_binary <- ifelse(merged_data_temp_with_treatment$Treatment == "Warm", 1, 0)
temp_by_binary_treatment <- tapply(merged_data_temp_with_treatment$`Temperature (C)`, merged_data_temp_with_treatment$Treatment_binary, median, na.rm = TRUE)

View(temp_by_binary_treatment)
na.omit(merged_data_temp_with_treatment)

"NA" %in% merged_data_temp_with_treatment
#ok is says NA successful out of temp data ! yippee !

"NA" %in% merged_data_observations_with_treatment
#yipee! also says no NAs here either

"NA" %in% merged_data_attributes_with_treatment
#yay!

temp_by_binary_treatment <- tapply(merged_data_temp_with_treatment$`Temperature (C)` , merged_data_temp_with_treatment$Treatment_binary , mean)

#NA still the result of the ambient 
#NA this NA that omg it ruins everything
#how can only ambient be NA but warm has a mean????


#11/13/2023
#still on the multiple conditions 

observations_by_all_binary_treatments <- merged_data_observations_with_treatment[
  (merged_data_observations_with_treatment$Pollinator_YorN_binary == 1) & 
    (merged_data_observations_with_treatment$Antagonist_YorN_binary == 0) & 
    (merged_data_observations_with_treatment$Treatment_binary == 0), 
  "Count of Observations"
]

summary(na.omit(observations_by_all_binary_treatments))


temp_by_binary_treatment <- tapply(merged_data_temp_with_treatment$`Temperature (C)` , merged_data_temp_with_treatment$Treatment_binary )
summary(temp_by_binary_treatment)

#um

#11/15/2023
#ok run through multiple conditions another time
#work on temp or attributes


observant_warm <- merged_data_observations_with_treatment$`Count of Observations`[merged_data_observations_with_treatment$Treatment_binary == 1]
observant_ambient <- merged_data_observations_with_treatment$`Count of Observations`[merged_data_observations_with_treatment$Treatment_binary == 0]
View(observant_warm)
View(observant_ambient)

t_test_observant_warm_vs_ambient <- t.test(observant_warm, observant_ambient)
print(t_test_observant_warm_vs_ambient)



str(merged_data_temp_with_treatment)
merged_data_temp_with_treatment$Treatment <- as.factor(merged_data_temp_with_treatment$Treatment)

str(merged_data_temp_with_treatment)

t_test_temp_warm_vs_ambient <- pairwise.t.test(merged_data_temp_with_treatment$Temperature, merged_data_temp_with_treatment$Treatment, data = merged_data_temp_with_treatment)

library(tidyr)
merged_data_temp_with_treatment$Ibutton_ID <- as.factor(merged_data_temp_with_treatment$Ibutton_ID)
str(merged_data_temp_with_treatment)

#11/29/2023
#we trying to get those p values today ! >:)

pairwise_t_test_temp_warm_vs_ambient <- pairwise.t.test(merged_data_temp_with_treatment$`Temperature (C)`, merged_data_temp_with_treatment$Treatment, data = merged_data_temp_with_treatment)
print(pairwise_t_test_temp_warm_vs_ambient)
#number give : 0.012 ! yippee?? it says P value adjustment method:holm

#ok i think i got it but i still havent figured out how to put data through
#multiple conditions to see what affects it
#how do i adjust for placement and time?
#this p value also preliminary
#ok now try to connect github to r studio

system("git config --global user.name 'leematthewlab'")
library(usethis)

#nevermind oof accidentally added copilot though?

#12/1/2023
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



#12/4/2023
#divide pollinator observations with time in r
merged_data_observations_with_treatment$`Count of Observations` / merged_data_attributes_with_treatment$TotalObservationLength
sum(merged_data_observations_with_treatment$`Unit ID`, "1A")

#making ibutton ids into factors? should i?
sum_of_observations <- aggregate(merged_data_observations_with_treatment$`Count of Observations` ~ merged_data_observations_with_treatment$`Unit ID`, data = merged_data_observations_with_treatment, sum)

merged_data_observations_with_treatment$`Unit ID` <- as.factor(merged_data_observations_with_treatment$`Unit ID`)
str(merged_data_observations_with_treatment)

print(sum_of_observations)
str(sum_of_observations)

library(tidyr)
library(dplyr)
sum_of_observations_by_factor <- merged_data_observations_with_treatment %>%
  filter(!is.na(`Count of Observations`)) %>%
  group_by(`Unit ID`) %>%
  summarise(sum_count = sum(`Count of Observations`, na.rm = TRUE))

print(sum_of_observations_by_factor)
#now i have the sum of observation counts for each factor, ie, ibutton ID
#ok now that the ibutton ids are a factor i want to connect to attributes

merged_data_attributes_with_treatment$UnitID <- as.factor(merged_data_attributes_with_treatment$UnitID)
str(merged_data_attributes_with_treatment)
#yippee! now both data sets have identical factors

merged_observations_to_time_by_factor <- left_join(sum_of_observations_by_factor, merged_data_attributes_with_treatment, by = "UnitID")
#omg one unit id has a space between and one doesn't

merged_data_observations_with_treatment <- merged_data_observations_with_treatment %>%
  rename('UnitID' = 'Unit ID')
str(merged_data_observations_with_treatment)

sum_of_observations_by_factor <- merged_data_observations_with_treatment %>%
  filter(!is.na(`Count of Observations`)) %>%
  group_by(`UnitID`) %>%
  summarise(sum_count = sum(`Count of Observations`, na.rm = TRUE))



merged_observations_to_time_by_factor <- merge(sum_of_observations_by_factor, merged_data_attributes_with_treatment, by = "UnitID")



#12/6/2023
#note
library(tidyr)
library(dplyr)
str(sum_of_observations_by_factor)
str(merged_data_observations_with_treatment)

merged_data_observations_with_treatment <- merged_data_observations_with_treatment %>%
  rename('UnitID' = 'Unit ID')
str(merged_data_observations_with_treatment)

sum_of_observations_by_factor <- merged_data_observations_with_treatment %>%
  filter(!is.na(`Count of Observations`)) %>%
  group_by(`UnitID`) %>%
  summarise(sum_count = sum(`Count of Observations`, na.rm = TRUE))

View(merged_observations_to_time_by_factor)
merged_observations_to_time_by_factor <- merge(sum_of_observations_by_factor, merged_data_attributes_with_treatment, by = "UnitID")
observations_by_time <- merged_observations_to_time_by_factor$sum_count / merged_observations_to_time_by_factor$TotalObservationLength
str(merged_observations_to_time_by_factor)


totalobserservationlengthparts = t(as.data.frame(strsplit(merged_observations_to_time_by_factor$TotalObservationLength)))

totalobserservationlengthtimes = chron(times = merged_observations_to_time_by_factor$TotalObservationLength, format=c('h:m:s'))

totalobservationlengthtimes = strptime(merged_observations_to_time_by_factor$TotalObservationLength, format = "%H:%M:%S")

str(merged_observations_to_time_by_factor)
str(totalobservationlengthtimes)

#chatgpt help?
install.packages("hms")
library(hms)
#chat gpt no help
library(dplyr)

merged_observations_to_time_by_factor %>% select(-TotalObservationLength_as_duration)

#splitting column with tidyr package by :
library("tidyr")


merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  separate(TotalObservationLength, c("hours", "minutes", "seconds"), ":")
str(merged_observations_to_time_by_factor)
View(merged_observations_to_time_by_factor)

#1/29/2024
#i haven't been dating my entries my bad
#merged_observations_to_time_by_factor$minutes / 60
#merged_observations_to_time_by_factor$seconds / 60 / 60

merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  mutate(minutes_by_hour = minutes / 60)

str(merged_observations_to_time_by_factor)

merged_observations_to_time_by_factor$hours = as.numeric(merged_observations_to_time_by_factor$hours)
merged_observations_to_time_by_factor$minutes = as.numeric(merged_observations_to_time_by_factor$minutes)
merged_observations_to_time_by_factor$seconds = as.numeric(merged_observations_to_time_by_factor$seconds)
str(merged_observations_to_time_by_factor)

#now hours, minutes, and seconds columns are numbers when before they were characters, which is why the division wasn't working
merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  mutate(minutes_by_hour = minutes / 60)
#yipee! it worked!
merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  mutate(seconds_by_hour = seconds / 3600)

#now i add the minutes and seconds into the hours column

merged_observations_to_time_by_factor$total_time_duration_hours = rowSums(merged_observations_to_time_by_factor[, c("hours", "minutes_by_hour", "seconds_by_hour")], na.rm = TRUE)
#yippe it worked! now time to divide the count by time

merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  mutate(count_divided_by_time = sum_count / total_time_duration_hours)

merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>% 
  select(-TotalObservationLength_as_duration) 

merged_observations_to_time_by_factor = merged_observations_to_time_by_factor %>%
  select(-Notes)
#there was only one note, that was that videos of 2 seconds or less were not counted


#time to compare ambient and warm _divided_by_time's

ambient_count_divided_by_time = merged_observations_to_time_by_factor$count_divided_by_time[merged_observations_to_time_by_factor$Treatment == "Ambient"]
warm_count_divided_by_time = merged_observations_to_time_by_factor$count_divided_by_time[merged_observations_to_time_by_factor$Treatment == "Warm"]

t_test_observation_count_divided_by_time = t.test(ambient_count_divided_by_time, warm_count_divided_by_time)
print(t_test_observation_count_divided_by_time)
#the results are "t = 1.8867, df = 6.2357, p-value = 0.1063"
#"alternative hypothesis: true difference in means is not equal to 0"
#i feel like with such small numbers, a normal t test may not be the best test
#i'm going to do a paired t test too

paired_t_test_observation_count_divided_by_time = t.test(ambient_count_divided_by_time, warm_count_divided_by_time, paired = TRUE)
print(paired_t_test_observation_count_divided_by_time)
#a paired t test gives: "t = 1.9263, df = 5, p-value = 0.112"
#but it should also be noted the average number in count_divided_by_time looks something like 0.010003404 so

# now it is time divide temperature data by timeline for effectiveness of warming chambers 
View(merged_data_temp_with_treatment_full)
str(merged_data_temp_with_treatment_full)
#first i want to change the date column from characters to actual time dates

#this is what chatgpt says to do :

# Assuming your_data is your data frame
# Assuming Date is the column with dates in "mm/dd/yy" format
# Assuming Values is the column with the numeric data points
# Assuming Treatment is the column with treatment labels

library(dplyr)

# Convert the Date column to a Date object
your_data$Date <- as.Date(your_data$Date, format = "%m/%d/%y")

# Extract month and year information
your_data <- your_data %>%
  mutate(Month = format(Date, "%Y-%m"))

# Group by Treatment and Month, and calculate summary statistics (e.g., mean)
summary_by_treatment_and_month <- your_data %>%
  group_by(Treatment, Month) %>%
  summarise(Mean_Value = mean(Values))

# Print the results
print(summary_by_treatment_and_month)

#end of chatgpt.



#2/2/2024
str(merged_data_temp_with_treatment_full)
#currently, the dates and ibutton_ID are characters
#the treatments are a factor with 2 binary levels, ambient and warm
#first i will try to make date from character into date

merged_data_temp_with_treatment_full$Date <- as.Date(merged_data_temp_with_treatment_full$Date, format = "%m/%d/%y")
str(merged_data_temp_with_treatment_full)
#yipee! date is no longer character it was date that was easy :)
#now for hard part dividing it by month

merged_data_temp_with_treatment_full <- merged_data_temp_with_treatment_full %>%
  mutate(Month = format(Date, "%Y-%m"))
#ok that worked!

summary_by_treatment_and_month <- merged_data_temp_with_treatment_full %>%
  group_by(Treatment, Month) %>%
  summarise(mean_temp_by_month = mean(`Temperature (C)`))

View(summary_by_treatment_and_month)
#ok this worked and now i have the averages of the warmed and ambient for the months 4 and 5

#i think i want the ibutton id to be 12 level factors rather than characters
#im pretty sure ive done this before tbh
#i also want to t-test rather than find the means
#or a test that can be done when the difference is like 0.07 between


str(merged_observations_to_time_by_factor)
#see here, the ID is factor of 12 levels
merged_data_temp_with_treatment_full$Ibutton_ID = as.factor(merged_data_temp_with_treatment_full$Ibutton_ID)
str(merged_data_temp_with_treatment_full)
#yipee! ibutton_ID is now factor with 12 levels! huzzah!


summary_by_Ibutton_ID_and_month <- merged_data_temp_with_treatment_full %>%
  group_by(Ibutton_ID, Month) %>%
  summarise(mean_temp_by_month = mean(`Temperature (C)`))

View(summary_by_Ibutton_ID_and_month)
#ok now i have the averages for both months for each ibutton
#1W, 3W, and 4W are NAs though

#ok more chatgpt now

merged_data_temp_with_treatment_full <- merged_data_temp_with_treatment_full %>%
  mutate(Day = format(Date, "%Y-%m-%d"))

summary_by_treatment_and_day <- merged_data_temp_with_treatment_full %>%
  group_by(Treatment, Day) %>%
  summarise(mean_temp_by_day = mean(`Temperature (C)`))
View(summary_by_treatment_and_day)
#ok now i have the average of the temperature for each day for ambient and warm

#now i do it for ibutton_ID hehe!
summary_by_Ibutton_ID_and_day <- merged_data_temp_with_treatment_full %>%
  group_by(Ibutton_ID, Day) %>%
  summarise(mean_temp_by_day_ibutton = mean(`Temperature (C)`))
View(summary_by_Ibutton_ID_and_day)

#now how do i test these against eachother
#oh this might be a lot
#i think it would be good to test out different tests that are better for smaller samples sizes with smaller differences
#wilcoxon signed rank test ayo?

#ok ok paired t test by treatment using day
mean_temp_by_day_ambient = summary_by_treatment_and_day$mean_temp_by_day[summary_by_treatment_and_day$Treatment == "Ambient"]
mean_temp_by_day_warm = summary_by_treatment_and_day$mean_temp_by_day[summary_by_treatment_and_day$Treatment == "Warm"]

paired_t_test_mean_temp_by_day = t.test(mean_temp_by_day_ambient, mean_temp_by_day_warm, paired = TRUE, na.omit = TRUE)
#so there is a missing value

wilcox_test_mean_temp_by_day = wilcox.test(mean_temp_by_day_ambient, mean_temp_by_day_warm, paired = TRUE)

is.na(merged_data_temp_with_treatment_full)

any_missing <- anyNA(merged_data_temp_with_treatment_full)

if (any_missing) {
  print("There are missing values in the dataset.")
} else {
  print("No missing values found.")
}
#there are missing values in the dataset
#but where?

na_matrix <- is.na(merged_data_temp_with_treatment_full)
col_na_count <- colSums(na_matrix)
row_na_count <- rowSums(na_matrix)
print(col_na_count)
print(row_na_count)


#ok data breach !! T-T


#2/12/2024
#time is currently " 'hms' num "

#make date from character to dates




#have to divide it into year, month, day
MayappleData_Fixed_Temp_2$Date = as.Date(MayappleData_Fixed_Temp_2$Date,format = "%m/%d/%y")

str(MayappleData_Fixed_Temp_2)
#how did i mess it up omg it made all of the years 2020 when it should be 2023...???
#idk how it even happened tbh
#how to undo..?

#add ambient and warm as factors, and take ibutton id from characters to factor with 12 levels
merged_MayappleData_Fixed_Temp_2 <- merge(MayappleData_ChamberTreatments_Ibutton_ID_good, MayappleData_Fixed_Temp_2, by = "Ibutton_ID")

#omg i literally just broke it??
#idk what happened so i reuploaded it as mayapple_fixed_temp_2
#ok omg i know what happened lmao
str(MayappleData_Fixed_Temp_2)
MayappleData_Fixed_Temp_2$Ibutton_ID = as.factor(MayappleData_Fixed_Temp_2$Ibutton_ID)
str(MayappleData_Fixed_Temp_2)
#ibuttons as factor complete

#Make chamber id from number into factor with however many levels
MayappleData_Fixed_Temp_2$Chamber_ID = as.factor(MayappleData_Fixed_Temp_2$Chamber_ID)
str(MayappleData_Fixed_Temp_2)
#chamber id as factor with 10 levels complete


#prep for time period divisions and testing 

#2/16/24
#time period divisions 
str(MayappleData_Fixed_Temp_2)

library(dplyr)
library(tidyr)

#add treatments
merged_mayappledata_fixed_temp_2 <- merge(MayappleData_Fixed_Temp_2, MayappleData_ChamberTreatments_Ibutton_ID_good, by = "Ibutton_ID")
str(merged_mayappledata_fixed_temp_2)
merged_mayappledata_fixed_temp_2$Treatment = as.factor(merged_mayappledata_fixed_temp_2$Treatment)
str(merged_mayappledata_fixed_temp_2)

#ok now grouping
merged_mayappledata_fixed_temp_2 <- merged_mayappledata_fixed_temp_2 %>%
  mutate(Difference = )

merged_mayappledata_fixed_temp_2 <- merged_mayappledata_fixed_temp_2 %>%
  group_by(Date)

View(merged_mayappledata_fixed_temp_2)

#erm ok chatgpt?
#XX WHATEVER IM STARTING OVER!!

str(merged_mayappledata_fixed_temp_2)

merged_mayappledata_fixed_temp_2_filter <- merged_mayappledata_fixed_temp_2 %>%
  filter(Treatment %in% c("Ambient", "Warm"))

merged_mayappledata_fixed_temp_2_filter_result <- merged_mayappledata_fixed_temp_2 %>%
  pivot_wider(names_from = 'Treatment', values_from = 'Temperature (C)') %>%
  mutate(Difference = 'Warm' - 'Ambient') %>%
  select(Date, Difference)
#doesnt show the merged_mayappledata_fixed_temp_2_filter_result

library(dplyr)
library(tidyr)
library(ggplot2)

merged_mayappledata_fixed_temp_2_averaged <- merged_mayappledata_fixed_temp_2_filter %>%
  group_by(Date, Treatment) %>%
  summarise(Avg_Temperature = mean(`Temperature (C)`))


plot <- ggplot(merged_mayappledata_fixed_temp_2_averaged, aes(x = Date, y = Avg_Temperature, color = Treatment)) +
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

plot2 <- ggplot(average_temp_weekly, aes(x = as.factor(week), y = Avg_Temperature, color = Treatment)) +
  geom_line(size = 1.5) +
  geom_point() +  # Add points for each data point
  labs(title = "Average Ambient and Warm Temperatures Each Week",
       x = "Week",
       y = "Average Temperature (C)",
       color = "Treatment") +
  scale_color_manual(values = c("#4CAF50", "#008000")) +  # Set line colors
  theme_minimal()

print(plot2)

t_test_results <- average_temp_weekly %>%
  group_by(week) %>%
  summarise(p_value = t.test(Avg_Temperature ~ Treatment, data = .)$p.value)
print(t_test_results)
View(t_test_results)


wilcox_test_results <- average_temp_weekly %>%
  group_by(week) %>%
  summarise(p_value = wilcox.test(Avg_Temperature ~ Treatment, data = .)$p.value)
print(wilcox_test_results)
#all p values the exact same looks weird

average_temp_daily <- merged_mayappledata_fixed_temp_2_filter %>%
  group_by(Treatment, Date) %>%
  summarise(Avg_Temperature = mean(`Temperature (C)`))

t_test_results_2 <- average_temp_daily %>%
  group_by(Date) %>%
  summarise(p_value = t.test(Avg_Temperature ~ Treatment, data = .)$p.value)

print(t_test_results_2)


#2/23/2024
library(dplyr)
library(tidyr)
library(ggplot2)



na.omit(merged_observations_to_time_by_factors_hms)
plot3 <- ggplot(merged_observations_to_time_by_factors_hms, aes(x = factor(UnitID), fill = factor(sum_of_observations_by_factor), color = Treatment)) +
  geom_bar() +
  labs(title = "Pollinator observations for different treatments",
       x = "Treatment",
       y = "pollinator count") 


print(plot3)
str(merged_observations_to_time_by_factors_hms)
View(merged_observations_to_time_by_factors_hms)

plot4 <- ggplot(merged_observations_to_time_by_factors_hms, aes(x = UnitID, y = sum_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Graph of sum_count by UnitID",
       x = "UnitID",
       y = "sum_count") +
  scale_fill_manual(values = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#1abc9c", "#9b59b6", "#34495e", "#e67e22", "#95a5a6", "#d35400", "#27ae60", "#c0392b")) +
  theme_minimal()

plot4 <- ggplot(merged_observations_to_time_by_factors_hms, aes(x = Treatment, y = sum_count, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Pollinator Count of Ambient and Warm",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))

print(plot4)

#the merging of the factors was incorrect and cut out 4 sets of data so we restart
str(MayappleData_VisitorObservations)

as.factor(MayappleData_VisitorObservations$`Unit ID`)
str(MayappleData_VisitorObservations)


summary(merged_data_observations_with_treatment)
MayappleData_VisitorObservations$Pollinator_YorN_binary <- ifelse(MayappleData_VisitorObservations$Pollinator_YorN == "Y", 1, 0)
MayappleData_VisitorObservations$Antagonist_YorN_binary <- ifelse(MayappleData_VisitorObservations$Antagonist_YorN == "Y", 1, 0)

observations_by_binary_treatment <- tapply(MayappleData_VisitorObservations$`Count of Observations`, MayappleData_VisitorObservations$Treatment_binary, sum, na.rm = TRUE)
str(MayappleData_VisitorObservations)


library(tidyr)
library(dplyr)
sum_of_observations_by_factor_2 <- MayappleData_VisitorObservations %>%
  filter(!is.na(`Count of Observations`)) %>%
  group_by(`Unit ID`) %>%
  summarise(sum_count = sum(`Count of Observations`, na.rm = TRUE))

print(sum_of_observations_by_factor_2)

MayappleData_VisitorObservations$`Unit ID` <- as.factor(MayappleData_VisitorObservations$`Unit ID`)
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

#trying to re-duration the time data
install.packages("hms")
library(hms)

MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength = as.numeric(MayappleData_VisitorObservations_with_time_treatment_5$TotalObservationLength)

library("tidyr")



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

#03/25/2024

sum_observations_1 <- sum_observations_1 %>%
  rename('UnitID' = 'MayappleData_VisitorObservations_with_time_treatment_5$UnitID')

treatments_3_25_2024_Sheet1 <- treatments_3_25_2024_Sheet1 %>%
  rename('UnitID' = 'MayappleData_VisitorObservations_with_time_treatment_5$UnitID')

sum_observations_2 = merge(sum_observations_1, treatments_3_25_2024_Sheet1, by = "UnitID")
View(sum_observations_2)

sum_observations_2 <- sum_observations_2 %>%
  rename('Count' = 'MayappleData_VisitorObservations_with_time_treatment_5$\`Count of Observations\`')

plot30 <- ggplot(sum_observations_2, aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Sum of Pollinator Count in Ambient and Warm Treatments",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))

print(plot30)

sum_observations_2_ttest = t.test(sum_observations_2$Count ~ sum_observations_2$Treatment, paired = FALSE)
print(sum_observations_2_ttest)
#unpaired: t = 2.1871, df = 11.746, p-value = 0.04973
#unpaired:mean in group Ambient=10.222222    mean in group Warm=2.857143 

sum(is.na(sum_observations_2$Count))
sum(is.na(sum_observations_2$Treatment))

length(sum_observations_2$Count)
length(sum_observations_2$Treatment)

levels(sum_observations_2$Treatment)

sum_observations_2 = sum_observations_2 %>%
  select(-Notes.y)


#to merge, we make 6
#3/1/2024

library(dplyr)
library(tidyr)
library(ggplot2)


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

str(MayappleData_VisitorObservations_with_time_treatment_6)

plot6 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = count_divided_by_time, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Rate of Pollinator Count in Ambient and Warm Treatments",
       x = "Treatment",
       y = "Rate of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))

print(plot6)

View(MayappleData_VisitorObservations_with_time_treatment_6)

plot7 <- ggplot(MayappleData_VisitorObservations_with_time_treatment_6, aes(x = Treatment, y = `MayappleData_VisitorObservations_with_time_treatment_5$\`Count of Observations\``, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Sum of Pollinator Count in Ambient and Warm",
       x = "Treatment",
       y = "Sum of Pollinator Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000"))

print(plot7)

#03/11/2024
#seed counting figures

library(dplyr)
library(tidyr)
library(ggplot2)




controlseed <- c(46, 32)
ambientseed <- c(49, 47, 12)
warmseed <- c(0)

seed_data <- data.frame(Treatment = rep(c("Control", "Ambient", "Warm"), 
                                        times = c(length(controlseed), 
                                                  length(ambientseed), 
                                                  length(warmseed))),
                        Count = c(controlseed, ambientseed, warmseed))
print(seed_data)

plot20 <- ggplot(seed_data, aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Seed Count",
       x = "Treatment",
       y = "Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000", "#FF5733"))

print(plot20)

plot21 <- ggplot(seed_data, aes(x = Treatment, y = Count)) +
  geom_point(position = position_jitterdodge(), size = 2) + # Add points with jitter to spread out points horizontally
  labs(title = "Seed Count",
       x = "Treatment",
       y = "Count") +
  scale_fill_manual(values = c("#4CAF50", "#008000", "#FF5733"))

print(plot21)

plot22 <- ggplot(seed_data, aes(x = Treatment)) +
  geom_bar(position = "dodge", fill = c("#4CAF50", "#008000", "#FF5733")) +
  geom_point(position = position_jitterdodge(), size = 2, color = "black") +
  labs(title = "Seed Count",
       x = "Treatment",
       y = "Count")

print(plot22)

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

merged_data_temp_with_treatment_full <- merged_data_temp_with_treatment_full %>%
  rename(Temperature = 'Temperature (C)')

plot8 <- ggplot(merged_data_temp_with_treatment_full, aes(x = Date, y = Temperature, color = factor(Chamber_ID))) +
  geom_line() +
  geom_point() +
  labs(title = " Ambient and Warm Temperatures",
       x = "Date",
       y = "Temperature, not averaged",
       color = "Chamber") +
  theme_minimal()

print(plot8)

str(merged_data_temp_with_treatment_full)
#chat time
library(dplyr)
library(lubridate)

# Assuming your dataset is named 'my_data'
# and your columns are named 'factor1', 'factor2', 'date_column', and 'data_points'

# Convert the date column to a Date objectxx
merged_data_temp_with_treatment_full$Date <- mdy(merged_data_temp_with_treatment_full$Date)

# Group by factors and date, then calculate the average of data points
result_8 <- merged_data_temp_with_treatment_full %>%
  group_by(Treatment, Chamber_ID, Date) %>%
  summarize(average_temp_8 = mean(Temperature))

# If you want to arrange the result by date, you can add the following line:
result_8 <- result_8 %>% arrange(Date)

# Print the result
print(result_8)
View(result_8)
#result_8 is successful, will use for figures now in plot 

plot8 <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = factor(Chamber_ID))) +
  geom_line() +
  geom_text(aes(label = sprintf("%.1f", average_temp_8)), 
            hjust = 0, vjust = 1, position = position_dodge(0.5), size = 2) + 
  labs(title = " Ambient and Warm Temperatures",
       x = "Date",
       y = "Temperature, not averaged",
       color = "Chamber") +
  theme_minimal()

print(plot8)

plot9 <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = factor(Treatment))) +
  geom_line() +
  geom_point() +
  labs(title = " Ambient and Warm Temperatures",
       x = "Date",
       y = "Temperature, the average for each date",
       color = "Chamber") +
  theme_minimal()

print(plot9)

library(ggplot2)
install.packages("ggrepel")

library(ggrepel)

# Assuming your dataset is named 'result_8' and contains columns 'Date', 'average_temp_8', 'Treatment', and 'Chamber_ID'

plot_all <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = Treatment, linetype = factor(Chamber_ID))) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = round(average_temp_8, 1)), 
                  size = 3, color = "black", show.legend = FALSE) + 
  labs(title = "Temperature over Time",
       x = "Date",
       y = "Temperature",
       color = "Treatment",
       linetype = "Chamber") +
  theme_minimal()

print(plot_all)

plot_all_1 <- ggplot(result_8, aes(x = Date, y = average_temp_8, color = factor(Chamber_ID), linetype = Treatment)) +
  geom_line(size = 0.5) +
  geom_point() +
  geom_text_repel(aes(label = round(average_temp_8, 1)), 
                  size = 3, color = "black", show.legend = FALSE) +
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
