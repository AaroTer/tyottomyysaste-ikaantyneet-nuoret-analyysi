# Luen tiedoston "Työttömyysaste ikäryhmän mukaan 2009-2025"
# Ja teen siitä taulukon df ja tarkastelen sitä

df = read.csv2("13aj_20260730_161729.csv", skip = 2)
head(df)
glimpse(df) 

# R ei suostu lukemaan numerolla alkavia otsikoita
# poistan ne gsub -komennolla

names(df) <- gsub("x", "", names(df), 
                  ignore.case = TRUE)

names(df) <- gsub("^$", "ikä", names(df), 
                  ignore.case = TRUE)

names(df)

# poistan turhat rivit 7 ja 8

df <- df %>% 
  slice(-c(7, 8))

df <- df %>% 
  filter(ikä %in% c("15 - 24", "55 - 64"))

# Suodatan ikäryhmät 15-24v ja 55-64v

df_15_24 <- df %>% 
  filter(ikä == "15 - 24")

df_55_64 <- df %>% 
  filter(ikä == "55 - 64")

df_15_24_num <- as.numeric(df_15_24[1, -1])
  
df_55_64_num <- as.numeric(df_55_64[1, -1])

cor(df_15_24_num,
    df_55_64_num)

malli <- lm(
  df_15_24_num~df_55_64_num
)

summary(malli)

# Nähdään, että muuttujien välillä on positiivinen yhteys
# t-arvo df_55_64 on matala
# tarkoittaen, ettei muuttuja ole tilastollisesti
# merkittävä selittäjä

# Tehdään tästä kuvaaja

ggplot(
  data.frame(
    Vuosi = 2009:2025,
    Nuoret = df_15_24_num,
    Ikääntyneet = df_55_64_num), 
    aes(x = Ikääntyneet, y = Nuoret, color = Vuosi)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_bw()

# Seuraavaksi tutki"Miten nuorten ja ikääntyneiden työttömyys on 
# kehittynyt Suomessa vuosina 2009–2025, ja onko niiden kehityksen välillä yhteyttä?"
# tee ensin pivot_longer