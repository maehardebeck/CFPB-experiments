## CFPB Student Loan Complaint Visualization
##
## Mae Hardebeck, 10/15/16

## INITIAL SETTING

rm(list = ls())
# setwd("~/Dartmouth/Jobs/17X/CFPB")

library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)

data <- read.csv("Student_Loan_Complaints.csv", stringsAsFactors = FALSE)

## ANALYSES

## Analysis 1: Which companies have the most complaints?

analysis1 <- data %>% 
  select(Company, Issue) %>% 
  group_by(Company) %>% 
  summarise(nissuesperco = n()) %>% 
  arrange(desc(nissuesperco)) %>% 
  ungroup()

## Retrieve the Top 10
analysis1.1 <- analysis1 %>% 
  filter(nissuesperco >=400)

tbl_df(analysis1.1)

## 1. Navient Solutions, Inc. (8290)
## 2. AES/PHEAA (2481)
## 3. Wells Fargo & Company (1111)
## 4. Sallie Mae (910)
## 5. JPMorgan Chase & Co. (753)
## 6. Discover (696)
## 7. ACS Education Services (597)
## 8. Citibank (548)
## 9. Nelnet (473)
## 10. Genesis Lending (459)

## Conclusion: Navient Solutions by far has the most complaints (quadruple
## that of AES/PHEAA). Is this because of bad business practices, or 
## because they serve the most students? This is not included in the dataset,
## but could be retrieved from elsewhere for further inquiry.


## Analysis 2: When are the complaints coming in? (graph)

analysis2 <- data %>% 
  select(`Date.received`) %>% 
  separate(`Date.received`, c("month", "day", "year"),
           sep = "/") %>% 
  arrange(year, month, day) %>% 
  group_by(year, month) %>% 
  summarise(complaints = n())

year12 <- analysis2 %>% 
  filter(year == 2012)
year13 <- analysis2 %>% 
  filter(year == 2013)
year14 <- analysis2 %>% 
  filter(year == 2014)
year15 <- analysis2 %>% 
  filter(year == 2015)
year16 <- analysis2 %>% 
  filter(year == 2016, month < 10)

ggplot() +
  geom_line(data = year12, mapping = aes(x = month, 
                                         y = complaints, group = year)) +
  geom_line(data = year13, mapping = aes(x = month,
                                         y = complaints, group = year)) +
  geom_line(data = year14, mapping = aes(x = month,
                                         y = complaints, group = year)) +
  geom_line(data = year15, mapping = aes(x = month,
                                         y = complaints, group = year)) +
  geom_line(data = year16, mapping = aes(x = month,
                                         y = complaints, group = year),
            color = "red") +
  facet_wrap(~ year, ncol = 5)

## Analysis 3: What are the most common issues from complaints?

analysis3 <- data %>% 
  select(Issue)
 
analysis3.1 <- analysis3
analysis3.1$Issue <- factor(analysis3$Issue, levels = c("Dealing with my lender or servicer",
                                                        "Can't repay my loan",
                                                        "Repaying your loan",
                                                        "Problems when you are unable to pay",
                                                        "Getting a loan"))

ggplot(data = analysis3, mapping = aes(x = Issue, fill = Issue)) +
  geom_bar() +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
