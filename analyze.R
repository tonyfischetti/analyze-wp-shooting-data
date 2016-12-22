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
library(tidyr)


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


dat %<>% mutate(armp = ifelse(dat$armed=="unarmed", FALSE, TRUE))
dat %>%
  filter(race!="Other") %>%
  filter(!is.na(race)) %>%
  group_by(race) %>%
  summarise(raw.count = n(),
            raw.know.armp=sum(!is.na(armp)),
            raw.armed=sum(armp, na.rm=TRUE)) %>%
  mutate(percent=raw.count/sum(raw.count),
         percent.armed=raw.armed/raw.know.armp,
         percent.unarmed=(raw.know.armp-raw.armed)/raw.know.armp) %>%
  arrange(race) %>% 
  mutate(percent.pop=c(.007, .047, .122, .163, .637),
         controlled_percent=percent/percent.pop) %>%
  mutate(controlled_percent=controlled_percent/sum(controlled_percent)) -> by.race


ggplot(by.race, aes(x=race, y=percent, fill=race)) +
  geom_bar(stat="identity") +
  ggtitle("percent of police shootings by race") +
  ggsave("./plots/1.png") +
  ggsave("./plots/1.pdf")


ggplot(by.race, aes(x=race, y=controlled_percent, fill=race)) +
  geom_bar(stat="identity") +
  ggtitle("percent of police shootings by race (controlled by size of race)") +
  ylab("") + theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  ggsave("./plots/2.png") +
  ggsave("./plots/2.pdf")




by.race %>%
  select(race, percent.armed, percent.unarmed) %>%
  gather(-race, key = "key", value = "value") %>%
  mutate(key=ifelse(key=="percent.armed", "armed", "unarmed"))-> tmp

ggplot(tmp, aes(x=race, y=value, fill=key)) +
  geom_bar(stat="identity") + #, position="dodge") +
  ggtitle("percent of police shootings by race by armed status") +
  ggsave("./plots/3.png") +
  ggsave("./plots/3.pdf")


#--------------------------------------------------#
# bootstrapping percentages
dat %>% filter(!is.na(armp)) %>% filter(race=="Black") %>% {.$armp} -> black
dat %>% filter(!is.na(armp)) %>% filter(race=="White") %>% {.$armp} -> white

library(boot)
das.boot <- boot(black, function(x, i){ sum(!x[i])/length(black) }, 100000, 
                 parallel="multicore", ncpus=4)
boot.ci(das.boot, type="bca") -> bcablack

das.boot <- boot(white, function(x, i){ sum(!x[i])/length(white) }, 100000, 
                 parallel="multicore", ncpus=4)
boot.ci(das.boot, type="bca") -> bcawhite


limits <- aes(ymax = c(bcablack$bca[5], bcawhite$bca[5]),
              ymin=c(bcablack$bca[4], bcawhite$bca[4]))
limits <- aes(ymax = c(bcablack$bca[5]*100, bcawhite$bca[5]*100),
              ymin=c(bcablack$bca[4]*100, bcawhite$bca[4]*100))

tmp %>% filter(race=="White" | race=="Black") %>% filter(key=="unarmed") %>%
ggplot(aes(x=race, y=value*100, fill=race)) +
  geom_bar(stat="identity") + #, position="dodge") +
  geom_errorbar(limits, width=.25) +
  ylab("percent of unarmed victims") +
  ggtitle("              Percent of unarmed victims shot by police by race") +
  ggsave("./plots/4.png") +
  ggsave("./plots/4.pdf")





