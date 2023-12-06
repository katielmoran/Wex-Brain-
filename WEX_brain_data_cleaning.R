# packages ----
library(dplyr)

#reading in data and renaming according to task-type
visual_ps <- read.csv("data_exp_71706-v1_task-fbg8.csv")
semantic_ps <- read.csv("data_exp_71706-v1_task-jmh6.csv")
basic_rt <- read.csv("data_exp_71706-v1_task-suhp.csv")
motor_ps <- read.csv("data_exp_71706-v1_task-z7s8.csv")

#reading in participants who were originally 'live'
#and not included in first download
visual_ps_p21 <- read.csv("data_exp_71706-v1_task-fbg8-5211411.csv")
semantic_ps_p21 <- read.csv("data_exp_71706-v1_task-jmh6-5211411.csv")
basic_rt_p21 <- read.csv("data_exp_71706-v1_task-suhp-5211411.csv")
motor_ps_p21 <- read.csv("data_exp_71706-v1_task-z7s8-5211411.csv")

visual_ps_p18 <- read.csv("data_exp_71706-v1_task-fbg8-5211409.csv")
semantic_ps_p18 <- read.csv("data_exp_71706-v1_task-jmh6-5211409.csv")
basic_rt_p18 <- read.csv("data_exp_71706-v1_task-suhp-5211409.csv")
motor_ps_p18 <- read.csv("data_exp_71706-v1_task-z7s8-5211409.csv")

visual_ps_p26 <- read.csv("data_exp_71706-v1_task-fbg8-5211417.csv")
semantic_ps_p26 <- read.csv("data_exp_71706-v1_task-jmh6-5211417.csv")
basic_rt_p26 <- read.csv("data_exp_71706-v1_task-suhp-5211417.csv")
motor_ps_p26 <- read.csv("data_exp_71706-v1_task-z7s8-5211417.csv")

visual_ps_p22 <- read.csv("data_exp_71706-v1_task-fbg8-5211413.csv")
semantic_ps_p22 <- read.csv("data_exp_71706-v1_task-jmh6-5211413.csv")
basic_rt_p22 <- read.csv("data_exp_71706-v1_task-suhp-5211413.csv")
motor_ps_p22 <- read.csv("data_exp_71706-v1_task-z7s8-5211413.csv")

#attaching their data to each task-specific DF
visual_ps <- rbind(visual_ps, visual_ps_p18, visual_ps_p21, 
                   visual_ps_p22, visual_ps_p26)

semantic_ps <- rbind(semantic_ps, semantic_ps_p18, semantic_ps_p21,
                     semantic_ps_p22, semantic_ps_p26)

basic_rt <- rbind(basic_rt, basic_rt_p18, basic_rt_p21,
                  basic_rt_p22, basic_rt_p26)

motor_ps <- rbind(motor_ps, motor_ps_p18, motor_ps_p21,
                  motor_ps_p22, motor_ps_p26)

#cleaning data - getting rid of unused/unnecessary columns and practice trials
#cleaning visual processing speed task

#deleting all rows that aren't experimental trials
visual_exp <- visual_ps[grep("Exp trial", visual_ps$display),]

#deleting all rows that contain data from the fixation screen
visual_exp <- visual_exp[!grepl("fixation", visual_exp$Zone.Name),]

#deleting columns that aren't useful
visual_exp <- visual_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#visually checked data to ensure "correct" answers are actually correct - which they are!

#if the screen timed out, changing the values to NA
visual_exp$Incorrect[visual_exp$Timed.Out == 1] <- NA
visual_exp$Correct[visual_exp$Timed.Out == 1] <- NA
visual_exp$Reaction.Time[visual_exp$Timed.Out == 1] <- NA

#changing RT from character to numeric
visual_exp$Reaction.Time <- as.numeric(visual_exp$Reaction.Time)

#condensing down to total attempted, total correct, total incorrect and mean reaction time 
visual_sums <- visual_exp %>% group_by(Participant.Public.ID) %>% 
  summarise(vis_mean_ps = mean(Reaction.Time, na.rm = TRUE), vis_total_attempt = sum(Attempt, na.rm = TRUE), 
            vis_total_correct = sum(Correct, na.rm = TRUE),
            vis_total_incorrect = sum(Incorrect, na.rm = TRUE))

#cleaning semantic processing speed task

#deleting all rows that aren't experimental trials
semantic_exp <- semantic_ps[grep("Task", semantic_ps$display),]

#deleting all rows that contain data from the fixation screen
semantic_exp <- semantic_exp[!grepl("fixation", semantic_exp$Zone.Name),]

#deleting columns that aren't useful
semantic_exp <- semantic_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#if the screen timed out, changing the value to NA
semantic_exp$Incorrect[semantic_exp$Timed.Out == 1] <- NA
semantic_exp$Correct[semantic_exp$Timed.Out == 1] <- NA
semantic_exp$Reaction.Time[semantic_exp$Timed.Out == 1] <- NA

#visually checked data to ensure "correct" answers are actually correct - which they are!

#condensing down to total attempted, total correct, total incorrect and mean reaction time 
semantic_sums <- semantic_exp %>% group_by(Participant.Public.ID) %>% 
  summarise(sem_mean_ps = mean(Reaction.Time, na.rm = TRUE), sem_total_attempt = sum(Attempt, na.rm = TRUE), 
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
motor_exp$Reaction.Time[motor_exp$Timed.Out == 1] <- NA

#creating correct, incorrect and attempted totals, as well as average reaction times 
motor_sums <- motor_exp %>% group_by(Participant.Public.ID) %>% 
  summarise(motor_mean_ps = mean(Reaction.Time, na.rm = TRUE), motor_total_attempt = sum(Attempt, na.rm = TRUE), 
            motor_total_correct = sum(Correct, na.rm = TRUE),
            motor_total_incorrect = sum(Incorrect, na.rm = TRUE))

#cleaning BRT task

#deleting all rows that aren't experimental trials
basic_rt_exp <- basic_rt[grep("Experimental trial", basic_rt$display),]

#deleting all rows that contain data from the fixation screen
basic_rt_exp <- basic_rt_exp[!grepl("fixation", basic_rt_exp$Zone.Name),]

#deleting columns that aren't useful
basic_rt_exp <- basic_rt_exp[-c(1:11, 13:14, 16:36, 39:40, 45:47, 49:50) ]

#correcting for timed out trials, where they were coded as incorrect to NA
basic_rt_exp$Incorrect[basic_rt_exp$Timed.Out == 1] <- NA
basic_rt_exp$Correct[basic_rt_exp$Timed.Out == 1] <- NA
basic_rt_exp$Reaction.Time[basic_rt_exp$Timed.Out == 1] <- NA

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

#creating a total correct hits, false alarms, misses and correct rejection sums and average reaction time
basic_rt_sums <- basic_rt_exp %>% group_by(Participant.Public.ID) %>% 
  summarise(brt_mean_ps = mean(Reaction.Time, na.rm = TRUE), 
            brt_total_correct_hits = sum(correct_hits, na.rm = TRUE),
            brt_total_false_alarms = sum(false_alarm, na.rm = TRUE),
            brt_total_correct_rej = sum(correct_rejection, na.rm = TRUE),
            brt_total_miss = sum(miss, na.rm = TRUE))

#exporting tables
#"-_ps" = full data downloaded from gorilla
write.csv(visual_ps, file = "visual_ps_full_data_061223.csv")
write.csv(semantic_ps, file = "semantic_ps_full_data_061223.csv")
write.csv(motor_ps, file = "motor_ps_full_data_061223.csv")
write.csv(basic_rt, file = "basic_rt_full_data_061223.csv")

#"-_exp" = relevant experimental data - mainly used to double check whether 'correct' 
#answers were actually correct and whether or not participants completed the tasks, 
#main cleaning edits done here too - i.e. RT data deleted where screen has timed out 
#this is trial-by-trial data, as ppt spans multiple rows
write.csv(visual_exp, file = "visual_clean_trials_061223.csv")
write.csv(semantic_exp, file = "semantic_clean_trials_061223.csv")
write.csv(motor_exp, file = "motor_clean_trials_061223.csv")
write.csv(basic_rt_exp, file = "basic_rt_clean_trials_061223.csv")

#"-_sums" = one participant per row, contains trial averages and totals for each ppt. 
write.csv(visual_sums, file = "visual_ppt_avgs_061223.csv")
write.csv(semantic_sums, file = "semantic_ppt_avgs_061223.csv")
write.csv(motor_sums, file = "motor_ppt_avgs_061223.csv")
write.csv(basic_rt_sums, file = "basic_rt_ppt_avgs_061223.csv")
