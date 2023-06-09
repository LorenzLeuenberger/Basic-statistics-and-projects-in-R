# R Script used for exercises in the Basic statistics in R course

library(tidyverse)
library(here)
library(forcats)
library(unibeCols)
library(cowplot)
library(TH.data)
library(epitools)

# Monday afternoon ------

# exercise 1 insurance data
insurance_with_date <- read_csv(here("data","raw","insurance_with_date.csv"))
str(insurance_with_date)


insurance_reformatted <- insurance_with_date |> 
  mutate(
    across(c(sex, region), factor),
    # sex = factor(sex),
    # region = factor(region),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
    # date_6m = date + 30.4 * 6
  )

str(reformatted)

# Exercise 4A

ebola <- read_csv(here("data","raw","ebola.csv"))

ebola <- arrange(ebola, Date)
str(ebola)

ebola_2015 <- ebola %>% 
  filter((Country =="Liberia" | Country=="Guinea" | Country=="Sierra Leone") &
         Date <= as.Date("2015-05-31")) %>% 
  select(Country, Date, Cum_conf_cases)

str(ebola_2015)

ebola_2015_point <- ggplot(data = ebola_2015, mapping = aes(y=Cum_conf_cases, x=Date, colour = Country, fill = Country))+
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5)+
  scale_color_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  scale_fill_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  scale_x_date(breaks = as.Date(c("2014-08-29","2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01" )),
               labels = c("29 August","1 October", "1 December", "1 February", "1 April"))+
  ggtitle(label = "Cumulative ebola cases in 3 countries") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")+
  theme_bw()+ theme(legend.position = "bottom")+
  facet_grid(cols = vars(Country))

print(ebola_2015_point)

ebola_2015_line <- ggplot(data = ebola_2015, mapping = aes(y=Cum_conf_cases, x=Date, colour = Country, fill = Country))+
  geom_line(alpha = 0.7, linetype = "solid", linewidth = 1.5)+
  scale_color_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000),
                     limits = c(0,10000))+
  scale_x_date(breaks = as.Date(c("2014-08-29","2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01" )),
               labels = c("29 August","1 October", "1 December", "1 February", "1 April"))+
  scale_fill_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  ggtitle(label = "Cumulative ebola cases in 3 countries") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")+
  theme_bw()+ theme(legend.position = "bottom")+
  facet_grid(cols = vars(Country))

print(ebola_2015_line)

ebola_2015_col <- ggplot(data = ebola_2015, mapping = aes(y=Cum_conf_cases, x=Date, colour = Country, fill = Country))+
  geom_col(alpha = 0.7, linetype = "solid", linewidth = 0.5, width = 0.7)+
  scale_color_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000,12500,15000),
                     limits = c(0,15000))+
  scale_fill_manual(values = c(unibeRed(), unibeGreen(), unibeOcean()))+
  scale_x_date(breaks = as.Date(c("2014-08-29","2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01" )),
               labels = c("29 August","1 October", "1 December", "1 February", "1 April"))+
  ggtitle(label = "Cumulative ebola cases in 3 countries") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")+
  theme_bw()+ theme(legend.position = "bottom")+
  facet_grid(cols = vars(Country))

print(ebola_2015_col)

plot_ebola_2015_grid <- plot_grid(plotlist = list(ebola_2015_point, ebola_2015_line, ebola_2015_col),
                                  labels = c("A1", "A2", "A3"), label_size=12, nrow=2)

plot_ebola_2015_grid

# Friday morning ----

# import data

breastCancer <- GBSG2
str(breastCancer)
head(breastCancer)

# Exercise 1.1
tbl_number <- breastCancer %>% 
  select(horTh,cens) %>%
  table()

tbl_number

Risk <- breastCancer %>% 
  select(cens,horTh) %>%
  group_by(horTh) %>%
  summarise_at(vars(cens),
               list(proportion=mean))
Risk

# Exercise 1.2

binom.test(x=tbl_number[1,2], n=tbl_number[1,1]+tbl_number[1,2])

binom.test(x=tbl_number[2,2], n=tbl_number[2,1]+tbl_number[2,2])

# Exercise 1.3

prop.test(cbind(tbl_number[,2],tbl_number[,1]))

chisq.test(cbind(tbl_number[,2],tbl_number[,1]))

# Exercise 2

RR <- Risk[2,2]/Risk[1,2]

OR <- tbl_number[2,2]/tbl_number[2,1]/(tbl_number[1,2]/tbl_number[1,1])
OR

# CI RR
z <- qnorm(0.975)
se_lnRR <- sqrt(1/tbl_number[2,2]-1/(tbl_number[2,2]+tbl_number[2,1])+1/tbl_number[1,2]-1/(tbl_number[1,2]+tbl_number[1,1]))
lower_RR_CI <- exp(log(RR)-(z*se_lnRR))
upper_RR_CI <- exp(log(RR)+(z*se_lnRR))

cbind(RR, lower_RR_CI, upper_RR_CI)

# CI OR
se_lnOR <- sqrt(1/tbl_number[2,2]+1/tbl_number[2,1]+1/tbl_number[1,2]+1/tbl_number[1,1])
lower_OR_CI <- exp(log(OR)-(z*se_lnOR))
upper_OR_CI <- exp(log(OR)+(z*se_lnOR))

cbind(OR,lower_OR_CI, upper_OR_CI)

# automatic with epitools package

riskratio(x=tbl_number, correction=TRUE)

oddsratio(x=tbl_number, correction=TRUE)  
