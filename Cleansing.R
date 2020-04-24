library(tidyverse)
library(rvest)

#Specifying the URL
url <- 'https://www.thebalancecareers.com/2016-federal-state-minimum-wage-rates-2060062'

#Reading entire HTML content from the webpage
webpage <- read_html(url)

#Reading the desired HTML content with different xpaths
vec <- webpage %>% html_nodes(xpath ='//div[contains(@id, "mntl-sc-block_1-0-16")]') %>% html_text()
vec1 <- webpage %>% html_nodes(xpath ='//div[contains(@id, "mntl-sc-block_1-0-20")]') %>% html_text()
vec2 <- webpage %>% html_nodes(xpath ='//div[contains(@id, "mntl-sc-block_1-0-24")]') %>% html_text()
vec3 <- webpage %>% html_nodes(xpath ='//div[contains(@id, "mntl-sc-block_1-0-28")]') %>% html_text()

vecf <- c(vec, vec1, vec2, vec3)

#cleaning the data to remove redundant characters
clean_vecf = gsub("[*]", "", vecf)
clean_vecf = gsub("[(]", "", clean_vecf)
clean_vecf = gsub("[)]", "", clean_vecf)
clean_vecf = gsub("[ - ]", "", clean_vecf)
clean_vecf = gsub("[ -]", "", clean_vecf)
clean_vecf = gsub("[0-9][0-9][0-9][0-9]", "", clean_vecf)
clean_vecf = gsub("[$$]", "", clean_vecf)
clean_vecf = gsub(",", "", clean_vecf)
clean_vecf = gsub("\\s+", "", clean_vecf)

#putting the content of vector in a data frame
x <- data.frame(clean_vecf)

#writing the data frame in xlsx file
write.csv(x, "H:\\Mimimum_Wages.csv")
