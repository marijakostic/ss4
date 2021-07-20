# PRVI ZADATAK

# (a)

#install.packages("tidyverse")
library(tidyverse) 

# Ucitacemo bazu sa kojom cemo raditi

baza <- read.csv("C:/Users/Korisnik/Desktop/SS4 seminarski/baza.csv")
head(baza,5) # prikazacemo prvih pet elemenata baze
# baza sadrzi podatke o policijskom zaustavljanju vozaca u drzavi Misisipi

class(baza) #vidimo da je baza oblika data.frame pa je moramo konvertovati u tibble 
trafficstops<-as_tibble(baza)
head(trafficstops,5) #ispis prvih 5 elemenata tibbla
attach(trafficstops)

filter(trafficstops, county_name == "Leake County") # prikazujemo zaustavljanja u okrugu "Leake County"
filter(trafficstops,violation == "Speeding") # prikazujemo zaustavljanja zbog prebrze voznje
filter(trafficstops,violation == "Speeding",driver_gender =="female") # prikazuje zenske vozace zaustavljene zgog prebrze voznje

select(trafficstops, police_department, officer_id) # prikazujemo samo trazene kolone
select(trafficstops, starts_with("driver")) # prikazujemo kolone ciji nazivi pocinju sa driver
select(trafficstops, violation,everything()) # prkazujemo prvo prestup pa sve ostalo

arrange(trafficstops, driver_gender, driver_race) #sortiramo prvo po polu, pa zatim svaku od dve kategorije sortiramo po rasi
arrange(trafficstops, driver_gender, desc(driver_age)) #sortiramo prvo po polu, pa zatim po godinama opadajuce

#filtriramo tibble tako da nema NA vrednosti
trafficstops<-filter(trafficstops,!is.na(county_fips),!is.na(officer_id),!is.na(driver_age)) 
trafficstops<-mutate(trafficstops,adult=(ifelse(driver_age < 18, 1, 0))) 
#dodajemo novu kolonu koja prikazuje da li je vozac punoletan
trafficstops

# ilustracija operatora cevi

senior_drivers <- trafficstops %>%
  filter(driver_age > 85) %>%
  select(violation_raw, driver_gender, driver_race)

senior_drivers # izdvajamo manju bazu podataka

# Dodajemo novu kolonu sa godinom rodjenja vozaca
trafficstops %>% mutate(birth_year = substring(driver_birthdate, 1, 4))

library(lubridate)
trafficstops %>% 
  mutate(birth_date = ymd(driver_birthdate),
         birth_year = year(driver_birthdate),
         birth_cohort = round(birth_year/10)*10) %>% 
  head()

# Crtamo histogram kohorte o rodjenju vozaca
trafficstops %>% 
  mutate(birth_date = ymd(driver_birthdate),
         birth_year = year(driver_birthdate),
         birth_cohort = round(birth_year/10)*10,
         birth_cohort = factor(birth_cohort)) %>%
  select(birth_cohort) %>% 
  plot()

# Pregled prosecne starosti za vozace crne i bele rase
trafficstops %>%
  group_by(driver_race) %>%
  summarize(mean_age = mean(driver_age, na.rm=TRUE))

# Pregled prosecne starosti vozaca za razlicite okruge
trafficstops %>%
  group_by(county_name) %>%
  summarize(mean_age = mean(driver_age, na.rm=TRUE))

# Takodje mozemo grupisati i po vise zahteva
trafficstops %>% 
  group_by(county_name, driver_race) %>%
  summarize(mean_age = mean(driver_age, na.rm=TRUE))

# ako zelimo da uklonimo NA vrednosti
trafficstops %>%
  filter(!is.na(driver_race)) %>% 
  group_by(county_name, driver_race) %>%
  summarize(mean_age = mean(driver_age, na.rm=TRUE))

# koliko je svaki sluzbenik zabelezio zaustavljanja
trafficstops %>%
  group_by(officer_id) %>%
  tally()

# (c)

# sredicemo sada malo nasu bazu koriscenjem paketa tidyrd

library(tidyr)

# Kreiramo okvir sa srednjom staroscu svakog vozaca prema polu,rasi i okrugu

trafficstops_ma <- trafficstops %>%
  filter(!is.na(driver_gender)) %>%
  group_by(county_name, driver_gender) %>%
  summarize(mean_age = mean(driver_age, na.rm = TRUE))

head(trafficstops_ma)

# Svaku opservaciju cemo prosiriti na dva reda-muski i zenski pol 
trafficstops_ma_wide <- trafficstops_ma %>%
  spread(key=driver_gender,value =  mean_age) 

head(trafficstops_ma_wide)

# uporedjivanje prosecne starosti muskih i zenskih vozaca
trafficstops_ma_wide %>% 
  mutate(agediff = male - female) %>% 
  ungroup() %>%
  filter(agediff %in% range(agediff))

#skupljanje podataka
trafficstops_ma_long <- trafficstops_ma_wide %>%
  gather(gender, mean_age, -county_name)

head(trafficstops_ma_long)

# (b)

# Ucitavamo paket za rad sa graficima

library(ggplot2)
MS_county_stops <- trafficstops_ma_wide


# Crtamo prazan ggplot
ggplot(data = MS_county_stops)
# navodimo sta da se nalazi na x, a sta na y-osi
ggplot(data = MS_county_stops, aes(x = female, y = male))
# dodajemo tacke
ggplot(data = MS_county_stops, aes(x = female, y = male)) +
  geom_point()

# dodelicemo ime kako bi nam bilo lakse za rad
MS_plot <- ggplot(data = MS_county_stops, aes(x = female, y = male)) + geom_point() + geom_smooth(method="lm")
MS_plot

# naslov grafika i naziv x i y ose

MS_plot <- MS_plot+ labs(title = "Prosecna starost zausavljenih vozaca po okruzima",
                         subtitle = "drzava Misisipi",
                         y="vozaci zenskog pola",x="vozaci muskog pola",
                         caption = "Period: od januara 2013.god do jula 2016.godine")
theme_set(theme_classic())
MS_plot

# Bar-plot prestupa po polovima
ggplot(trafficstops, aes(violation)) + 
  geom_bar(aes(fill = driver_gender))

ggplot(trafficstops, aes(violation)) + 
  geom_bar(aes(fill = driver_gender), position = "fill")


# Bar chart prestupa u odnosu na prosecnu starost vozaca
trafficstops %>% 
  group_by(violation) %>% 
  summarize(mean_age = mean(driver_age, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(violation, mean_age), y = mean_age)) + 
  geom_col() + 
  coord_flip()

# Crtacemo box plot samo za okrug Yazoo, pa ga prvo moramo izdvojiti iz baze
Yazoo_stops <- filter(trafficstops, county_name == "Yazoo County")

#Raspodela uzrasta vozaca u okviru svakog saobracajnog prestupa
ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot()

# dodavanjem tacaka imamo bolju predstavu o broju merenja i raspodeli
ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot() +
  geom_jitter()

# grafik izgleda prilicno neuredno pa cemo ga malo srediti
ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, color = "tomato")

ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_jitter(alpha = 0.1, color = "tomato") +
  geom_boxplot(alpha = 0) 

# vremenske serije

library(lubridate)

theme_set(theme_classic())

# kako bi olaksali, konvertovacemo kolonu stop_date u format datuma
trafficstops <- trafficstops %>% 
  mutate(stop_date = ymd(stop_date),
         wk_day = wday(stop_date, label = TRUE))

# broj prekrsaja po radnom datumu
trafficstops %>%
  count(wk_day, violation)

# prikazujemo podatke kao vremensku seriju
trafficstops %>%
  count(wk_day, violation) %>% 
  ggplot(aes(wk_day, n)) +
  geom_line()

# Nismo dobili ono sto smo ocekivali
# Moramo zadati da se povuce posebna linije za svaki prekrsaj

trafficstops %>%
  count(wk_day, violation) %>% 
  ggplot(aes(wk_day, n, group = violation, color = violation)) +
  geom_line()

# Umesto da koristimo bojenje za razlicite prestupe, mozemo crtati vise grafika po kategorijama
trafficstops %>%
  count(wk_day, violation) %>% 
  ggplot(aes(wk_day, n, group = violation)) +
  geom_line() +
  facet_wrap(~ violation)

# delimo liniju na osnovu rase
trafficstops %>%
  count(wk_day, violation, driver_race) %>%
  ggplot(aes(wk_day, n, color = driver_race, group = driver_race)) +
  geom_line() +
  facet_wrap(~ violation)

#  promenicemo temu
stops_facet_plot <- trafficstops %>%
  count(wk_day, violation, driver_race) %>% 
  ggplot(aes(wk_day, n, color = driver_race, group = driver_race)) +
  geom_line() +
  facet_wrap(~ violation)

stops_facet_plot +
  theme_bw()

# mozemo promeniti imena osama i dodati naslov
stops_facet_plot +
  labs(title = 'Prekrsaji po danima u nedelji',
       x = 'Posmatranja radnim danima',
       y = 'Broj prekrsaja') +
  theme_bw() + 
  theme(axis.text.x = element_text(colour="grey40", size=12, angle=90, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey40", size=12),
        strip.text = element_text(size=14),
        text = element_text(size=16))

# (d)

# Ucitavamo novu bazu podataka
MS_bw_pop <- read.csv("C:/Users/Korisnik/Desktop/SS4 seminarski/baza2.csv")
head(MS_bw_pop)

# Brojimo sva saobracajna zaustavljanja po okruzima

trafficstops %>% 
  group_by(county_name) %>% 
  summarise(n_stops = n())

# Novoj bazi podataka cemo dodati novu kolonu, koja predstavlja broj zaustavljanja po okruzima iz prve baze
trafficstops %>% 
  group_by(county_name) %>% 
  summarise(n_stops = n()) %>% 
  left_join(MS_bw_pop, by = c("county_name" = "County")) %>% 
  head()

# Spajamo baze po nazivu okruga i zadrzavamo samo preseke
trafficstops %>% 
  inner_join(MS_bw_pop, by = c("county_name" = "County"))

# Brojimo saobracajna zaustavljanja po drzavama i rasama
trafficstops_2 <- trafficstops %>% 
  filter(!is.na(driver_race)) %>%
  group_by(county_name,driver_race) %>% 
  summarise(n_stops = n())

# Svaku opservaciju cemo prosiriti na dva reda-black i white rasa
trafficstops_race <- trafficstops_2 %>%
  spread(key=driver_race,value =  n_stops) 

head(trafficstops_race)

# Rezultat je broj zausavljenih crnaca, odnosno belaca po okruzima

# Bazi podataka koja sadrzi broj stanovnika po okruzima, dodacemo broj zaustavljenih crnaca i belaca
trafficstops_race %>% 
  group_by(county_name) %>% 
  full_join(MS_bw_pop, by = c("county_name" = "County")) %>% 
  head()



