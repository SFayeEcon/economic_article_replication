library(ggplot2)
library(tidyverse)
library(lubridate)
path = "C:/Users/soule/Documents/ProjectEconometrics/"
pums <- read.table(paste(path,"QOB.raw",sep=""),
                   header           = FALSE,
                   stringsAsFactors = FALSE)
colnames(pums)[c(1,2,4,5,6,9:13,16,18,19:21,24,25,27)] <- c("AGE", "AGEQ", "EDUC",
                                                            "ENOCENT","ESOCENT", "LWKLYWGE", 
                                                            "MARRIED", "MIDATL", "MT", "NEWENG", "CENSUS", "QOB", "RACE",
                                                            "SMSA", "SOATL", "WNOCENT", "WSOCENT", "YOB")
pums <- as_tibble(pums)
pums
pums %>%
  mutate(cohort = factor(1*(YOB<=39 & YOB >=30) +
                           2*(YOB<=49 & YOB >=40),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> pums
ymd(paste("19",30,4 * 3, sep=""),truncated = 2)
pums_age <- pums %>%
  group_by(QOB, YOB) %>%
  summarise(LWKLYWGE = mean(LWKLYWGE), EDUC = mean(EDUC)) %>%
  mutate(q4 = (QOB == 4))
ggplot(pums_age, aes(x = YOB+ (QOB-1) / 4, y = EDUC)) +
  geom_line(colour = "black") +
ggtitle("Average education by quarter of birth") +
  geom_label(mapping = aes(label = QOB),size=3,label.r=unit(0.2,"lines"),label.padding = unit(0.16, "lines"),label.size=0.2, colour="white", fill="black") +
  theme(legend.position = "none") +
  scale_x_continuous("Year of birth", breaks = seq(30,40, by=2),
                     limits = c(30, 40)) +
  scale_y_continuous("Years of Education", breaks = seq(12.2, 13.2, by = 0.2),
                     limits = c(12.2,13.2))

#renaming each variables of data
colnames(pums)[c(1,2,4,5,6,9:13,16,18,19:21,24,25,27)] <- c("AGE", "AGEQ", "EDUC",
                                                            "ENOCENT","ESOCENT", "LWKLYWGE",
                                                            "MARRIED", "MIDATL", "MT", "NEWENG", "CENSUS", "QOB", "RACE",
                                                            "SMSA", "SOATL", "WNOCENT", "WSOCENT", "YOB")
pums <- as_tibble(pums)
pums

pums %>%
  mutate(cohort = factor(1*(YOB<=39 & YOB >=30) +
                           2*(YOB<=49 & YOB >=40),
                         levels=c(1,2), labels=c("30-39","40-49")) ) -> pums
ymd(paste("19",30,4 * 3, sep=""),truncated = 2)

pums_age <- pums %>%
  group_by(QOB, YOB) %>%
  summarise(LWKLYWGE = mean(LWKLYWGE), EDUC = mean(EDUC)) %>%
  mutate(q4 = (QOB == 4))

ggplot(pums_age, aes(x = YOB + (QOB - 1) / 4, y = LWKLYWGE)) +
  geom_line() +
  geom_point() +
  ggtitle("Average weekly wage by quarter of birth") +
  geom_label(mapping = aes(label = QOB),size=3,label.r=unit(0.2,"lines"),label.padding = unit(0.16, "lines"),label.size=0.2, colour = "white", fill = "black") +
  theme(legend.position = "none") +
  scale_x_continuous("Year of birth", breaks = seq(30,50, by=5),
                     limits = c(30, 50)) +
  scale_y_continuous("Log Weekly Earnings", breaks = seq(5.65, 5.90, by = 0.05),
                     limits = c(5.65, 5.92))
