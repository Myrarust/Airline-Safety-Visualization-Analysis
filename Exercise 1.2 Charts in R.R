# Exercise 1.2: Charts

# Set working directory
setwd("C:\\Users\\myraw\\RStudio\\DSC640")

# Import Library to read excel files
install.packages("tidyverse")
library(tidyverse)
library(readxl)

# Read in the excel files
df1 <- read_excel("hotdog-contest-winners.xlsm")
df1

df2 <- read_excel("obama-approval-ratings.xls")
df2

# Import library to create visualizations
library(ggplot2)

# Clean up columns with spaces in the names
names(df1)<-make.names(names(df1),unique = TRUE)
df1

# Create bar chart
bar <- ggplot(data=df1, aes(x=Year, y=Dogs.eaten)) +
    ggtitle("Number of Hotdogs Eaten for 1st Place by Year") +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
bar

# Prepare to create stacked bar chart
# Factor the Issue column to prepare to change data format
df2$Issue <- factor(df2$Issue)

# Change the format of the data from wide to long
library(tidyr)
df2_long <- gather(df2, sentiment, score, Approve:None, factor_key=TRUE)
df2_long

# Refactor levels for sentiment to change the order of the fill
df2_long$sentiment <- relevel(df2_long$sentiment,"Approve")
df2_long$sentiment <- relevel(df2_long$sentiment,"None")
df2_long$sentiment <- relevel(df2_long$sentiment,"Disapprove")

# Create stacked bar chart
stacked3 <- ggplot(data=df2_long, aes(x=Issue, y=score, fill=sentiment)) +
    ggtitle("Obama Approval Rating by Issue") +
    geom_bar(stat="identity")
stacked3 + coord_flip() + scale_fill_manual(values=c('indianred', 'gray70', 'steelblue'))

# Prepare data to create pie chart
df_counts <- df1 %>% count(Country, sort = TRUE)

# Add a percent column
df_counts <- df_counts %>%
    mutate(per = n/31 * 100)
df_counts

# Add label position
df_counts <- df_counts %>%
    arrange(desc(Country)) %>%
    mutate(lab.ypos = cumsum(per) - 0.5*per)
df_counts

# Create pie chart
ggplot(df_counts, aes(x = "", y = per, fill = Country)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = n), color = "white") +
    ggtitle("Number of 1st Place wins by Country") +
    theme_void()

# Use pie chart and make adjustments to create donut chart
ggplot(df_counts, aes(x = 2, y = per, fill = Country)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = n), color = "white") +
    ggtitle("Number of 1st Place wins by Country") +
    theme_void() +
    xlim(0.5, 2.5)
