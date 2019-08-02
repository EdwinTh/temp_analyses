library(tidyverse)
library(padr)
system(
  'wget -O ~/Desktop/deBilt.txt --post-data="stns=260&vars=TEMP&byear=1930&bmonth=1&bday=1&eyear=2019&emonth=7&eday=28" http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi'
)
db_raw <- readLines("~/Desktop/deBilt.txt")
db_cl  <- tibble(all = db_raw[-c(1:18)]) %>% 
  separate(all, into = c(NA, "dag", NA, NA, NA, "max_temp", NA, NA, NA), sep = ",") %>% 
  mutate(dag = lubridate::ymd(dag), max_temp = as.numeric(max_temp) / 10) %>% 
  padr::thicken("year", "year") %>% 
  filter(year < as.Date("2019-01-01"))

dgn30pl <- db_cl %>% 
  group_by(year) %>% 
  summarise(dgn_30_pl =sum(max_temp > 30))

gem30pl <- dgn30pl %>% 
  mutate(before91 = year < as.Date("1990-01-01")) %>% 
  group_by(before91) %>% 
  summarise(gem30pl = mean(dgn_30_pl))

dgn30pl %>% 
  filter(year < as.Date("1991-01-01")) %>% 
  ggplot(aes(year, dgn_30_pl)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Dagen met maximum temperatuur boven de 30",
          subtitle = "De Bilt, 1930-1990") +
  ylab("Aantal dagen") +
  xlab("Jaar") +
  geom_hline(aes(yintercept = gem30pl$gem30pl[2]), col = "blue")
  
dgn30pl %>% 
  ggplot(aes(year, dgn_30_pl)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Dagen met maximum temperatuur boven de 30",
          subtitle = "De Bilt, 1930-2018") +
  ylab("Aantal dagen") +
  xlab("Jaar") +
  geom_segment(aes(y = gem30pl$gem30pl[2], yend = gem30pl$gem30pl[2],
               x = as.Date("1930-01-01"), xend = as.Date("1990-01-01")), col = "blue") +
  geom_segment(aes(y = gem30pl$gem30pl[1], yend = gem30pl$gem30pl[1],
                   x = as.Date("1991-01-01"), xend = as.Date("2018-01-01")), col = "red") 

dgn20min <- db_cl %>%
  filter(lubridate::month(dag) %in% 6:8) %>% 
  group_by(year) %>% 
  summarise(dgn_20_1 = sum(max_temp < 20)) %>% 
  group_by(year < as.Date("1991-01-01")) %>% 
  summarise(dgn_20_min = mean(dgn_20_1))

db_cl %>% 
  filter(lubridate::month(dag) %in% 6:8) %>% 
  group_by(year) %>% 
  summarise(dgn_20_1 = sum(max_temp < 20)) %>%
  ggplot(aes(year, dgn_20_1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Dagen met maximum temperatuur onder de 30",
          subtitle = "De Bilt, 1930-2018, Juni-Juli-Augustus") +
  ylab("Aantal dagen") +
  xlab("Jaar") +
  geom_segment(aes(y = dgn20min$dgn_20_min[2], yend = dgn20min$dgn_20_min[2],
                   x = as.Date("1930-01-01"), xend = as.Date("1990-01-01")), col = "blue") +
  geom_segment(aes(y = dgn20min$dgn_20_min[1], yend = dgn20min$dgn_20_min[1],
                   x = as.Date("1991-01-01"), xend = as.Date("2018-01-01")), col = "red") 

  

  