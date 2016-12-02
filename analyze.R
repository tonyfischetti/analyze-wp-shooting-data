#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   analyze.R                                           ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################

# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)
options(datatable.fread.datatable=FALSE)

# cli args
args <- commandArgs(trailingOnly=TRUE)

# libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)


dat <- read_csv("./data/fatal-police-shootings-data.csv") %>%
  mutate(race=ifelse(race=='A', "Asian",
                     ifelse(race=='B', "Black",
                            ifelse(race=='H', "Hispanic",
                                   ifelse(race=='N', "AmerIndian",
                                          ifelse(race=='O', "Other",
                                                 ifelse(race=='W', "White",
                                                        NA)))))))

dat %>% head
dat %>% data.frame %>% head


dat$race %>% table -> raw.by.race

by.race <- raw.by.race %>% data.frame
names(by.race)[1] <- "race"
names(by.race)[2] <- "raw.count"
by.race %<>% filter(race!="Other")

by.race$percent <- by.race$raw.count / sum(by.race$raw.count)

by.race$percent.pop <- c(.007, .047, .122, .163, .637)
by.race$controlled <- by.race$percent / by.race$percent.pop
by.race$controlled <- by.race$controlled / sum(by.race$controlled)


dat %>% data.frame %>% head

dat %>% filter(armed=="unarmed") -> unarmed
unarmed$race %>% table %>% data.frame -> unarmed
names(unarmed)[1] <- "race"
names(unarmed)[2] <- "unarmed.count"


by.race %<>% left_join(unarmed)
by.race$unarmed.count[is.na(by.race$unarmed.count)] <- 0
by.race %<>% mutate(perc.unarmed=unarmed.count/sum(unarmed.count))

by.race


ggplot(by.race, aes(x=race, y=percent)) +
  geom_bar(stat="identity") +
  ggtitle("percent of police shootings by race") +
  ggsave("./plots/1percent.png") +
  ggsave("./plots/1percent.pdf")


by.race %>% select(race, percent) -> tmp
by.race %>% select(race, perc.unarmed) -> tmp2
names(tmp)[2] <- "perc"
names(tmp2)[2] <- "perc"
tmp$kind <- "armed and unarmed"
tmp2$kind <- "unarmed"
rbind(tmp, tmp2) -> tmp

ggplot(tmp, aes(x=race, y=perc, fill=kind)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("percent of police shootings by race") +
  ggsave("./plots/2percent.png") +
  ggsave("./plots/2percent.pdf")



ggplot(by.race, aes(x=race, y=controlled)) +
  geom_bar(stat="identity") +
  ggtitle("percent of police shootings by race after\ncontrolling for US racial breakdown") +
  ggsave("./plots/3control.png") +
  ggsave("./plots/3control.pdf")


