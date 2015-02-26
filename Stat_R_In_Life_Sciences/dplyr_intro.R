### Intro to dplyr in HarvardX Statistics and R for the life sciences.
#
library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

##Starting with dplyr verbs
#Select
sleepData <- select(msleep, name, sleep_total) #Select columns from a data frame 
head(sleepData)
head(select(msleep, -name)) #Select columns from data frame except 'x'
head(select(msleep, name:order))  #Select a range of columns
head(select(msleep, starts_with("sl"))) #Select columns starting with 'x'
##Some additional options to select columns based on a specific criteria include
#ends_with() = Select columns that end with a character string
#contains() = Select columns that contain a character string
#matches() = Select columns that match a regular expression
#one_of() = Select columns names that are from a group of names

#Filter
filter(msleep, sleep_total >= 16) #Filter on one criteria
filter(msleep, sleep_total >= 16, bodywt >= 1) #Filter on multiple criteria

#Pipe    #dyplr imports pipe from another package and allows for inline(left to right) reading
head(select(msleep, name, sleep_total)) ### base method
msleep %>% select(name, sleep_total) %>% head  ### Pipe method

#Arrange
msleep %>% arrange(order) %>% head
msleep %>% select(name, order, sleep_total) %>% arrange(order, sleep_total) %>% head
msleep %>% select(name, order, sleep_total) %>% arrange(order, sleep_total) %>% filter(sleep_total >= 16)
msleep %>% select(name, order, sleep_total) %>% arrange(order, desc(sleep_total)) %>% filter(sleep_total >= 16)

#Mutate  #Add calculated columns to data frame
msleep %>% mutate(rem_proportion = sleep_rem / sleep_total) %>% head
msleep %>% mutate(rem_proportion = sleep_rem / sleep_total, bodywt_grams = bodywt *1000) %>% head

#Summarise
msleep %>% summarise(avg_sleep = mean(sleep_total))
#There are many other summary statistics you could consider such sd(), min(), max(), median(), sum(), 
#n() (returns the length of vector), first() (returns first value in vector), last() (returns last value in vector) 
#and n_distinct() (number of distinct values in vector).
msleep %>% summarise(avg_sleep = mean(sleep_total), min_sleep = min(sleep_total),
                max_sleep = max(sleep_total), total = n())

# Group_by
msleep %>% group_by(order) %>% summarise(avg_sleep = mean(sleep_total), min_sleep = min(sleep_total),
                                         max_sleep = max(sleep_total), total = n())
###
msleep %>% mutate(rem_proportion = sleep_rem / sleep_total) %>% group_by(order) %>% 
    summarise(med_rem = median(rem_proportion)) %>% arrange(med_rem) %>% head
























