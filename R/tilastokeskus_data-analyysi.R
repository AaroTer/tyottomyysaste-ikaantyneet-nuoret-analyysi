# Tämän koodin tarkoitus on siivota ensin Tilastokeskuksen data "Työttömyysaste ikäryhmän mukaan 2009-2025"
# ja tarkastella, onko nuorten (15 - 24-vuotiaiden) ja ikääntyneiden (55 - 64-vuotiaiden) työttömyysasteella yhteyttä

# koodin toimiakseen, pitää asentaa tidyverse -kokoelma, ja ladata dplyr- ja ggplot2 -paketit
# luen tiedoston "Työttömyysaste ikäryhmän mukaan 2009-2025"
# ja teen siitä taulukon df ja tarkastelen sitä

df = read.csv2("13aj_20260730_161729.csv", skip = 2)
head(df)
glimpse(df) 

# seuraavaksi datan siistimistä
# R ei suostu lukemaan numerolla alkavia otsikoita
# poistan ne gsub -komennolla

names(df) <- gsub("x", "", names(df), 
                  ignore.case = TRUE)

# nimetään tyhjä sarakenimi nimellä "ikä"

names(df) <- gsub("^$", "ikä", names(df), 
                  ignore.case = TRUE)

names(df)

# poistan turhat sarakkeet 7 ja 8

df <- df %>% 
  slice(-c(7, 8))

df <- df %>% 
  filter(ikä %in% c("15 - 24", "55 - 64"))

# seuraavaksi tutkitaan nuorten ja ikääntyneiden työttömyysasteiden yhteyttä
# suodatan omiksi taulukoikseen ikäryhmät 15-24v ja 55-64v

df_15_24 <- df %>% 
  filter(ikä == "15 - 24")

df_55_64 <- df %>% 
  filter(ikä == "55 - 64")

df_15_24_num <- as.numeric(df_15_24[1, -1])
  
df_55_64_num <- as.numeric(df_55_64[1, -1])

# lasken korrelaation ja teen regressioanalyysin

cor(df_15_24_num,
    df_55_64_num)

malli <- lm(
  df_15_24_num~df_55_64_num
)

summary(malli)

# nähdään, että muuttujien välillä on positiivinen yhteys
# t-arvo df_55_64 on pieni
# mallin kerroin on positiivinen, mutta yhteys ei ole tilastollisesti merkitsevä

# tehdään tästä kuvaaja

ggplot(
  data.frame(
    Vuosi = 2009:2025,
    Nuoret = df_15_24_num,
    Ikääntyneet = df_55_64_num), 
    aes(x = Ikääntyneet, y = Nuoret, color = Vuosi)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw()

# kuvaajassa jokainen piste kuvaa yhtä vuotta
# x-akseli kuvaa ikääntyneiden työttömyysastetta ja y-akseli nuorten
# väri kuvaa vuotta
# viiva on lineaarinen regressiosuora
