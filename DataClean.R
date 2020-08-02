
library(tidyverse)

df <- read.csv("US suicide rates since 1990.csv")

df[df=="Unreliable"]<-NA
df[df=="*"]<-NA
df[df==""]<-NA

df <- df %>%
    gather(key = Age, value = Rate , -Year, -Sex, -Race, - Means)

df$Age <- str_remove_all(df$Age, "^X")

df$Age[which(df$Age == "5.14.years")] <- "05-14 years"
df$Age[which(df$Age == "15.24.years")] <- "15-24 years"
df$Age[which(df$Age == "25.34.years")] <- "25-34 years"
df$Age[which(df$Age == "35.44.years")] <- "35-44 years"
df$Age[which(df$Age == "45.54.years")] <- "45-54 years"
df$Age[which(df$Age == "55.64.years")] <- "55-64 years"
df$Age[which(df$Age == "65.74.years")] <- "65-74 years"
df$Age[which(df$Age == "75.84.years")] <- "75-84 years"
df$Age[which(df$Age == "85.years.and.over")] <- "85+ years"

df$Race[which(df$Race == "Black")] <- "Black or African American"

df$Rate <- as.numeric(df$Rate)

write.csv(df, "USrates1900clean.csv", row.names=FALSE)    
