library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

d <- read.table("http://www.mbnet.com.pl/dl.txt", sep=" ", dec=".")
names(d) <- c("obs", "date", "numbers")

d <- d %>%
  separate(numbers, into = c("number1", "number2", "number3", "number4", "number5", "number6")) %>%
  separate(date, into = c("day", "month", "year"), remove = F) %>%
  mutate(date_f=as.POSIXct(strptime(date, "%d.%m.%Y")))

freqs <- d %>%
  select(starts_with("number")) %>%
  gather(number, value) %>%
  mutate(value=as.numeric(value))

# frequency of each number

counts_num <- freqs %>%
  count(value)

ggplot(counts_num, aes(x=as.factor(value), n)) +
  geom_bar(stat="identity", fill="#FCBD10") +
  xlab("Liczba") + ylab("Częstość") + 
  theme_light() +
  theme(axis.text.x = element_text(size = 7))
# distribution of numbers

ggplot(freqs, aes(x=number, y=value)) + 
  geom_violin(fill="#FCBD10") +
  xlab("Pozycja liczby") + ylab("Wylosowana liczba") + 
  scale_x_discrete(labels = 1:6) +
  theme_light()

# number of draws by year

draw_y <- d %>%
  # filter(year!="2017") %>%
  group_by(year) %>%
  count()

ggplot(draw_y, aes(x=year, y=n, group = 1)) +
  geom_line(color="#FCBD10", size=2) +
  geom_point(color="#FCBD10", size=3) +
  xlab("Rok") + ylab("Liczba losowań") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# weekdays

week_d <- d %>%
  mutate(weekday=weekdays(date_f)) %>%
  group_by(year, weekday) %>%
  count() %>%
  ungroup() %>%
  mutate(weekday=ordered(weekday, 
                         levels=rev(c("wtorek", "środa", "czwartek", "sobota", "niedziela")),
                         labels=rev(c("wtorek", "środa", "czwartek", "sobota", "niedziela"))))

ggplot(week_d, aes(year, weekday)) + 
  geom_tile(aes(fill = n), colour = "white") +
  # geom_text(aes(label = n), size=3, color = "white") +
  scale_fill_gradient("Liczba losowań", limits=c(0,max(week_d$n))) + 
  xlab("Rok") + ylab("Dzień tygodnia") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position="bottom")

# number of draws by day and month

draw_dm <- d %>%
  group_by(month, day) %>%
  count()

ggplot(draw_dm, aes(day, month)) + 
  geom_tile(aes(fill = n), colour = "white") +
  geom_text(aes(label = n), size=3, color = "white") +
  scale_fill_gradient("Liczba losowań", limits=c(0,max(draw_dm$n))) + 
  xlab("Dzień") + ylab("Miesiąc") +
  theme_light() +
  theme(legend.position="bottom")

