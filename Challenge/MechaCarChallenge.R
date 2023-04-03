library("dplyr")
challenge_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data= challenge_table)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data= challenge_table))

suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- summarize(suspension_table, mean=mean(PSI), median=median(PSI),variance=var(PSI),SD=sd(PSI))
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(mean= mean(PSI),median = median(PSI),variance=var(PSI),SD = sd(PSI))

t.test(suspension_table$PSI,mu=1500)
t.test(subset(suspension_table$PSI,suspension_table$Manufacturing_Lot == 'Lot1'), mu=1500)
t.test(subset(suspension_table$PSI,suspension_table$Manufacturing_Lot == 'Lot2'), mu=1500)
t.test(subset(suspension_table$PSI,suspension_table$Manufacturing_Lot == 'Lot3'), mu=1500)
