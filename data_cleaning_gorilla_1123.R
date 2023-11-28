# packages ----
library(dplyr)

#Set working directory
setwd("Z:/Biobank_Vascular/Windows/Gorilla_Mark_1123/shortform_unblind_mark")

#reading in data and renaming according to task-type
visual_ps <- read.csv("data_exp_71706-v1_task-fbg8.csv")
semantic_ps <- read.csv("data_exp_71706-v1_task-jmh6.csv")
basic_rt <- read.csv("data_exp_71706-v1_task-suhp.csv")
motor_ps <- read.csv("data_exp_71706-v1_task-z7s8.csv")

#cleaning data - getting rid of unused/unnecessary columns and practice trials
#cleaning visual processing speed task

#deleting all rows that aren't experimental trials
visual_exp <- visual_ps[grep("Exp trial", visual_ps$display),]

#deleting all rows that contain data from the fixation screen
visual_exp <- visual_exp[!grepl("fixation", visual_exp$Zone.Name),]

#deleting columns that aren't useful
visual_exp <- visual_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#visually checked data to ensure "correct" answers are actually correct - which they are!

#if the screen timed out, changing the value from incorrect to NA
visual_exp$Incorrect[visual_exp$Timed.Out == 1] <- NA
visual_exp$Correct[visual_exp$Timed.Out == 1] <- NA

#condensing data down to tabular form - one ppt per row
visual_condensed <- visual_exp[-c(2:3, 5, 9:13) ]

visual_sums <- visual_condensed %>% group_by(Participant.Public.ID) %>% 
  summarise(vis_mean_ps = mean(Reaction.Time), vis_total_attempt = sum(Attempt, na.rm = TRUE), 
            vis_total_correct = sum(Correct, na.rm = TRUE),
            vis_total_incorrect = sum(Incorrect, na.rm = TRUE))


#cleaning semantic processing speed task

#deleting all rows that aren't experimental trials
semantic_exp <- semantic_ps[grep("Task", semantic_ps$display),]

#deleting all rows that contain data from the fixation screen
semantic_exp <- semantic_exp[!grepl("fixation", semantic_exp$Zone.Name),]

#deleting columns that aren't useful
semantic_exp <- semantic_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#if the screen timed out, changing the value from incorrect to NA
semantic_exp$Incorrect[semantic_exp$Timed.Out == 1] <- NA
semantic_exp$Correct[semantic_exp$Timed.Out == 1] <- NA

#visually checked data to ensure "correct" answers are actually correct - which they are!

#condensing data down to relevant columns
semantic_condensed <- semantic_exp[-c(2:3, 5, 9:13) ]

semantic_sums <- semantic_condensed %>% group_by(Participant.Public.ID) %>% 
  summarise(sem_mean_ps = mean(Reaction.Time), sem_total_attempt = sum(Attempt, na.rm = TRUE), 
            sem_total_correct = sum(Correct, na.rm = TRUE),
            sem_total_incorrect = sum(Incorrect, na.rm = TRUE))

#cleaning basic RT task
#deleting all rows that aren't experimental trials
motor_exp <- motor_ps[grep("Exp", motor_ps$display),]

#deleting all rows that contain data from the fixation screen
motor_exp <- motor_exp[!grepl("fixation", motor_exp$Zone.Name),]
motor_exp <- motor_exp[!grepl("Zone1", motor_exp$Zone.Name),]

#deleting columns that aren't useful
motor_exp <- motor_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#checking to ensure correct responses were actually recorded as being correct
#not in this case - all the incorrectly coded answers were actually correct - checked by sums for sanity
#adjusting column names to reflect this
motor_exp <- motor_exp %>% rename(temp = "Incorrect")
motor_exp <- motor_exp %>% rename(Incorrect = "Correct")
motor_exp <- motor_exp %>% rename(Correct = "temp")

#correcting for timed out trials, where they were coded as incorrect 
motor_exp$Incorrect[motor_exp$Timed.Out == 1] <- NA
motor_exp$Correct[motor_exp$Timed.Out == 1] <- NA

#condensing data down to relevant columns
motor_condensed <- motor_exp[-c(2:3, 5, 9:13) ]

motor_sums <- motor_condensed %>% group_by(Participant.Public.ID) %>% 
  summarise(motor_mean_ps = mean(Reaction.Time), motor_total_attempt = sum(Attempt, na.rm = TRUE), 
            motor_total_correct = sum(Correct, na.rm = TRUE),
            motor_total_incorrect = sum(Incorrect, na.rm = TRUE))

#cleaning BRT task

#deleting all rows that aren't experimental trials
basic_rt_exp <- basic_rt[grep("Experimental trial", basic_rt$display),]

#deleting all rows that contain data from the fixation screen
basic_rt_exp <- basic_rt_exp[!grepl("fixation", basic_rt_exp$Zone.Name),]

#deleting columns that aren't useful
basic_rt_exp <- basic_rt_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#correcting for timed out trials, where they were coded as incorrect 
basic_rt_exp$Incorrect[basic_rt_exp$Timed.Out == 1] <- NA
basic_rt_exp$Correct[basic_rt_exp$Timed.Out == 1] <- NA

#creating a correct rejection column - ppt doesn't respond to incorrect stimuli
basic_rt_exp <- mutate(basic_rt_exp, correct_rejection = ifelse(Timed.Out == 1 & is.na(Response), 1, 0))

#creating a correct hits column - ppt responds to correct stimuli
basic_rt_exp <- basic_rt_exp %>% rename(correct_hits = "Correct")
basic_rt_exp$correct_hits <- ifelse(is.na(basic_rt_exp$correct_hits), 0, basic_rt_exp$correct_hits)

#creating a false alarm column - ppt responds to incorrect stimuli
basic_rt_exp <- basic_rt_exp %>% rename(false_alarm = "Incorrect")
basic_rt_exp$false_alarm <- ifelse(is.na(basic_rt_exp$false_alarm), 0, basic_rt_exp$false_alarm)

#creating a miss column - ppt doesn't respond to correct stimuli
basic_rt_exp <- mutate(basic_rt_exp, miss = ifelse(is.na(Response) & Letters == "H.png", 1, 0))

#condensing data down to relevant columns
basic_rt_condensed <- basic_rt_exp[-c(2:3, 5, 9:12) ]


basic_rt_sums <- basic_rt_condensed %>% group_by(Participant.Public.ID) %>% 
  summarise(brt_mean_ps = mean(Reaction.Time), 
            brt_total_correct_hits = sum(correct_hits, na.rm = TRUE),
            brt_total_false_alarms = sum(false_alarm, na.rm = TRUE),
            brt_total_correct_rej = sum(correct_rejection, na.rm = TRUE),
            brt_total_miss = sum(miss, na.rm = TRUE))

