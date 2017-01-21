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
options(mc.cores = parallel::detectCores())

# cli args
args <- commandArgs(trailingOnly=TRUE)

# libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(parallel)


download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv",
              destfile="./data/fatal-police-shootings-data.csv")

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


ggplot(by.race, aes(x=race, y=percent*100)) +
  # geom_bar(stat="identity") +
  geom_bar(stat="identity", fill="#0072B2") +
  ggtitle("                              Percent of police shootings by race") +
  ylab("percent") +
  ggsave("./plots/1.png") +
  ggsave("./plots/1.pdf")


ggplot(by.race, aes(x=race, y=controlled_percent)) +
  # geom_bar(stat="identity") +
  geom_bar(stat="identity", fill="#0072B2") +
  ggtitle("             Percent of police shootings by race (controlled by size of race)") +
  ylab("") + theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  ggsave("./plots/2.png") +
  ggsave("./plots/2.pdf")




by.race %>%
  select(race, percent.armed, percent.unarmed) %>%
  gather(-race, key = "key", value = "value") %>%
  mutate(key=ifelse(key=="percent.armed", "armed", "unarmed"))-> tmp

ggplot(tmp, aes(x=race, y=value*100, fill=key)) +
  geom_bar(stat="identity") + #, position="dodge") +
  ggtitle("          Percent of police shootings by race by armed status") +
  ylab("percentage") +
  ggsave("./plots/3.png") +
  ggsave("./plots/3.pdf")



#--------------------------------------------------#
# bootstrapping percentages (for "armed" status only)

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
  ylab("percent of unarmed victims within race") +
  ggtitle("              Percent of unarmed victims shot by police by race") +
  theme(legend.position="none") +
  ggsave("./plots/4.png") +
  ggsave("./plots/4.pdf")


# White: 4.2% - 7.1%
# Black: 8.5% - 14.2%

#--------------------------------------------------#


#--------------------------------------------------#
# bootstrapping percentages (for "armed" and "not attacking")

dat %<>% mutate(armed.or.fleeing.p = ifelse(dat$armed=="unarmed" & dat$threat_level!="attack", FALSE, TRUE))

dat %>% filter(!is.na(armed.or.fleeing.p)) %>% filter(race=="Black") %>% {.$armed.or.fleeing.p} -> black
dat %>% filter(!is.na(armed.or.fleeing.p)) %>% filter(race=="White") %>% {.$armed.or.fleeing.p} -> white

das.boot <- boot(black, function(x, i){ sum(!x[i])/length(black) }, 100000, 
                 parallel="multicore", ncpus=4)
boot.ci(das.boot, type="bca") -> bcablack

das.boot <- boot(white, function(x, i){ sum(!x[i])/length(white) }, 100000, 
                 parallel="multicore", ncpus=4)
boot.ci(das.boot, type="bca") -> bcawhite


# White: 2.3% - 4.5%
# Black: 4.7% - 9.1%

#--------------------------------------------------#








#--------------------------------------------------#
#--------------------------------------------------#

# bayesian modeling of the probabilities that a white or
# black target of a shooting would be unarmed

library(rstan)
library(rethinking)
rstan_options(auto_write = FALSE)


by.race %<>% mutate(raw.unarmed=raw.know.armp-raw.armed)
by.race$racenum <- 1:nrow(by.race)
xwalk <- by.race %>% select(race, racenum)
by.race %>% transmute(racenum, raw_unarmed=raw.unarmed,
                      raw_know_armp=raw.know.armp) -> d2



nhmod <- stan(file='./models/non-heirarchical-model.stan',
              data=append(data.frame(d2), list(N=5, N_racenum=5)),
              iter=1000000, chains=1)

traceplot(nhmod, pars=c("race[1]", "race[2]", "race[3]",
                        "race[4]", "race[5]"))
plot(nhmod)


get_sampler_params(nhmod, inc_warmup=FALSE)
summary(nhmod)$summary


nhsamps <- extract.samples(nhmod, n=100000)
nhsrace <- data.frame(logistic(nhsamps$race))
names(nhsrace) <- xwalk$race


options(warn=1)


heir <- stan(file='./models/heirarchical-model.stan',
             data=append(data.frame(d2), list(N=5, N_racenum=5)),
             iter=1000000, chains=1,
             control=list(adapt_delta=0.9999))


traceplot(heir, pars=c("races[1]", "races[2]", "races[3]",
                       "races[4]", "races[5]", "a", "sigma"))


samps <- extract.samples(heir, n=100000)
srace <- data.frame(logistic(samps$races))
names(srace) <- xwalk$race
sa <- logistic(samps$a)




HPDI(srace$White, prob=0.95) -> cihwhite
HPDI(srace$Black, prob=0.95) -> cihblack

HPDI(nhsrace$White, prob=0.95) -> cinhwhite
HPDI(nhsrace$Black, prob=0.95) -> cinhblack



plot(density(nhsrace$White), lwd=2, lty=1, col="red", xlim=c(0, .2),
     ylim=c(-4, 55),
     main="posterior distribution of probability between target race",
     # ylab="probability density",
     ylab=NA,
     xlab="probability of targets being unarmed",
     yaxt='n')
lines(density(nhsrace$Black), lwd=2, lty=1, col="blue")


lines(density(srace$White), lwd=2, lty=2, col="red")
lines(density(srace$Black), lwd=2, lty=2, col="blue")
lines(density(sa), lwd=1, lty=3)

lines(c(cinhwhite[1], cinhwhite[2]), c(-2, -2), col="red", lwd=2)
lines(c(cinhwhite[1], cinhwhite[1]), c(-3, -1), col="red", lwd=2)
lines(c(cinhwhite[2], cinhwhite[2]), c(-3, -1), col="red", lwd=2)

lines(c(cinhblack[1], cinhblack[2]), c(-2, -2), col="blue", lwd=2)
lines(c(cinhblack[1], cinhblack[1]), c(-3, -1), col="blue", lwd=2)
lines(c(cinhblack[2], cinhblack[2]), c(-3, -1), col="blue", lwd=2)


lines(c(cihwhite[1], cihwhite[2]), c(-4, -4), col="red", lwd=1, lty=2)
lines(c(cihblack[1], cihblack[2]), c(-4, -4), col="blue", lwd=1, lty=2)


text(x=mean(cinhwhite), y=-2, labels="█████", col="white")
text(x=mean(cinhwhite), y=-2, labels="white")
text(x=mean(cinhblack), y=-2, labels="█████", col="white")
text(x=mean(cinhblack), y=-2, labels="black")

dev.copy(png,'plots/post.png')
dev.off()
dev.copy(pdf,'plots/post.pdf')
dev.off()




# how many unarmed black people might be alive
# today if there were no racial bias?

num.ppl.no.bias <- nhsrace$White * 460
num.ppl.bias <- nhsrace$Black * 460

num.ppl.bias - num.ppl.no.bias -> difference
hist(difference)
plot(density(difference))


