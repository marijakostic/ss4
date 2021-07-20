# DRUGI ZADATAK

library(stringr)

# Ucitavamo tekst kao string
string <- 'Paradoksi kretanja
Ahil i kornjaca
"U utrci, najbrzi trkac nikada ne moze prestici najsporijeg, zato sto gonitelj prvo mora doci do tacke odakle je gonjeni posao, pa prema tome najsporiji uvijek ima prednost."-Aristotelova Fizika VI:9, 239b15
Zamislite da Ahil trci protiv kornjace. Ahil trci 10 puta brze od kornjace, ali pocinje od tacke A, 100 metara iza kornjace koja je u tacki K1 (kornjaci , koja je sporija, data je prednost). Da bi prestigao kornjacu, Ahil mora prvo doci do tacke K1. Medutim, kada je Ahil stigao do tacke K1, kornjaca je presla 10 metara i dosla do tacke K2. Ponovo Ahil trci do K2. Ali, kao i prije, kada je presao 10 metara kornjaca je metar ispred njega, kod tacke K3, i tako dalje (kornjaca ce uvijek imati prednost nad Ahilom, bez obzira na to koliko mala ona bila). Prema tome Ahil nikada ne moze prestici kornjacu.
A----------------------------K1----------------K2---K3
Paradoks dihotomije
"Kretanje je nemoguce jer ono sto je u pokretu mora prvo preci pola puta prije nego sto stigne do cilja".-Aristotelova Fizika VI:9, 239b10
Zamislite stvar koja treba ici od tacke A do tacke B. Da bi dosla do tacke B stvar prvo mora doci do srednje tacke B1 koja je izmedu tacaka A i B. Ali, prije nego sto se ovo dogodi stvar mora doci do tacke B2 koja je izmedu tacaka A i B1. Slicno, prije nego sto moze i to uraditi, mora prvo doci do tacke B3 koja je izmedu A i B2, i tako dalje. Prema tome kretanje nikada ne moze poceti.
A-----B3-----B2-----------B1-------------------------B

Paradoks strijele
Zenon je dokazivao da je strijela u letu nepokretna.
"Ako je sve nepomicno sto zauzima prostor, i ako sve sto je u pokretu zauzima takav prostor u nekom vremenu, onda je leteca strijela nepokretna."-Aristotelova Fizika VI:9, 239b5
Zamislite da strijela leti neprestano naprijed, tokom jednog vremenskog intervala. Uzmite svaki momenat u tom vremenskom intervalu. Nemoguce je da se strijela mice u takvom momentu, jer trenutak ima trajanje 0, i strijela ne moze biti na dva mjesta u isto vrijeme. Prema tome, u svakom trenutku je strijela nepomicna, i tako strijela je nepomicna tokom citavog intervala.

Predlozena rjesenja za Ahila i kornjacu

Aristotel je istakao da kao sto se udaljenost smanjuje, vrijeme potrebno da se ta udaljenost prede takode se smanjuje.Takav pristup rijesavanju paradoksa bi doveo do demanta tvrdnje da je potrebno beskonacno mnogo vremena da se prede preko beskonacno mnogo udaljenosti, iako neki to spore.Prije 212. p. n. e., Arhimed je razvio metod da izvede konacni odgovor za beskonacno mnogo clanova koji postaju progresivno manji. Teoreme su razvijene u modernijim oblicima da bi postigle isti rezultat, ali sa tacnijom metodom za dokazivanje. Ove metode dozvoljavaju konstrukciju rijesenja koje kazu da (pod normalnim uslovima) ako se udaljenosti stalno smanjuju, vrijeme je konacno.Ova rijesenja su u biti geometrijski nizovi.
Predlozena rjesenja za paradoks dihotomije
Aristotel je istakao da kao sto se udaljenost smanjuje, vrijeme potrebno da se ta udaljenost prede takode se smanjuje. Takav pristup rijesavanju paradoksa bi doveo do demanta tvrdnje da je potrebno beskonacno mnogo vremena da se prede preko beskonacno mnogo udaljenosti.
Predlozena rjesenja za paradoks strijele
Paradoks o strijeli postavlja pitanja o prirodi kretanja koja nisu odgovorena na matematicki nacin, kao u slucaju Ahila i kornjace i Dihotomije.Ovaj paradoks se moze rijesiti matematicki na slijedeci nacin: u granicnoj vrijednosti, duzina momenta tezi nuli, trenutacna stopa mijenjanja ili brzine (koja je kolicnik predenog puta u odredenom vremenu) ne mora teziti nuli. Ova nenultna granicna vrijednost je brzina strijele u trenutku.Problem sa racunskim rijesenjem je taj da racunska radnja moze opisati samo kretanje dok se granicna vrijednost priblizava, bazirano na vanjskoj observaciji da se strijela krece naprijed. Medutim, u Zenonovom paradoksu, koncepti kao brzina gube svoje znacenje i nepostoji cinilac, koji nije pod djelovanjem paradoksa, koji bi mogao strijeli omoguciti letenje.Drugo glediste je to da premisa kaze da je u svakom trenutku, strijela nepomicna. Medutim, ne kretati se- je relativan pojam. Niko ne moze suditi, posmatrajuci jedan trenutak, da strijela stoji u mjestu. Tacnije, potrebni su drugi, slicni trenuci koji bi odredili, poredeci se sa drugim trenucima, da je strijela u jednom trenutku nepomicna. Prema tome, u poredenju sa drugim trenucima, strijela bi bila na drugom mjestu nego sto je bila i sto ce biti u vremenu prije i poslije. Uzevsi ovo u obzir, strijela se krece.
'
# Stampani prikaz stringa (nije isti kao sam string, jer stampani prikaz prikazuje i escape karaktere)
writeLines(string)

# Koliko karaktera ima u stringu
str_length(string)

# Ispis stringa, tako da sve bude ispisano malim slovima
str_to_lower(string) 

# Iz stringa izdvajamo podstringove oblike "Aristotelova Fizika "
str_view_all(string, "Aristotelova Fizika ") 

# Pravimo podstring od 800 do 1000 karaktera
substr(string,800,1000 )

# Menjamo rec paradoks sa problem
str_replace(string, "paradoks", "problem") 

# Izdvajanje podudaranja
# Recimo da zelimo da pronademo sve recenice koje sadrze imena
# Prvo kreiramo vektor koji sadrzi imena , a zatim ga pretvaramo u jedan regularni izraz
names <- c("Ahil","Aristotel","Dihotomije","Zenon")
names_match <- str_c(names, collapse = "|")
names_match

# Sada mozemo da izaberemo recenice koje sadrze imena, a zatim i da je izvucemo
has_names <- str_subset(string, names_match)

matches <- str_extract_all(has_names,names_match)
head(matches)

# Funkcija strsplit vraca listu reci na koje je podelila string
podeljen_string<-strsplit(string," ")[[1]] 
# Od liste pravimo vektor reci koji i dalje ima posebne karaktere, tj. nije bas vektor reci
reci<-unlist(podeljen_string, recursive = TRUE, use.names = TRUE) 
# Izbacujemo sve zareze iz vektora reci, ti zarezi se nalaze spojeni uz prethodnu rec, i analogno za \n, \. i \?
reci<-str_remove_all(reci, ",") 
reci<-str_remove_all(reci, "\\n") 
reci<-str_remove_all(reci, "\\.")
reci<-str_remove_all(reci, "\\?")
# Broj reci u tekstu je zapravo duzina vektora reci
broj_reci<-length(reci) 

# Zelimo da izdvojimo sve reci koje sadrze samo jedan samoglasnik
# Posmatramo tri slucaja
# rec pocinje sa samoglasnikom i ima odredjen broj suglasnika,tacno jedan samoglasnik a zatim sve suglasnike
# rec pocinje sa samoglasnikom i sve ostale suglasnike 
# rec ima samo suglasnike i na kraju samo samoglasnik
str_view_all(reci, "(^[^aeiou][^aeiou]{0,}+[aeiou]{1}+[^aeiou]{0,}[^aeiou]$)|(^[aeiou][^aeiou]{0,}[^aeiou]$)|(^[^aeiou][^aeiou]{0,}[aeiou]$)")


# Broj reci koje se zavrsavaju na a
sum(str_detect(reci, "a$")) 
# prosecan broj samoglasnika u recima
mean(str_count(reci, "[aeiou]")) 

# Ilustrujemo rad sa tibblovima i stringovima
df <- tibble(
  reci = reci, 
  i = seq_along(reci)
)

df %>% 
  filter(str_detect(reci, "a$")) #izdvaja sve reci koje se zavrsavaju na a

df %>% 
  mutate(
    sa_m = str_count(reci, "m"), #izdvaja koliko karaktera u reci je m, a koliko nije d
    nema_d= str_count(reci, "[^d]")
  )

