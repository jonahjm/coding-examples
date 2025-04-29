#Getting Started#
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(psych)
library(nnet)
library(stargazer)
library(texreg)

setwd("~/Desktop/Hawkins-RA/Environment and Populism Project")

#Loading the data-----------------------------------
polat <- read.csv("POLAT_dataset_1-12_v1.csv", header = TRUE, sep = ",")
polat <- as.data.frame(polat)

#Data Cleaning---------------------------------------
#Create a new binary variable for those who express support for the party VOX
##Use the variables partyid_str, partyidrepesca_str, voteintentionspain_str, and vote2019nov
polat$vox_support <- ifelse(polat$partyid_str == "VOX" | polat$partyidrepesca_str == "VOX" | polat$voteintentionspain_str == "VOX" | polat$vote2019nov == "VOX"| polat$probvotevox >= 5, 1, 0)
#Check a new binary variable for those who express support for the party Podemos or Unidas Podemos
##Use the variables partyid_str, partyidrepesca_str, voteintentionspain_str, and vote2019nov
polat$podemos_support <- ifelse(polat$partyid_str == "Podemos" | polat$partyidrepesca_str == "Podemos" | polat$voteintentionspain_str == "Podemos" | polat$vote2019nov == "Podemos" |
                                polat$partyid_str == "Unidas Podemos" | polat$partyidrepesca_str == "Unidas Podemos" | polat$voteintentionspain_str == "Unidas Podemos" | polat$vote2019nov == "Unidas Podemos" | polat$probvotePodemos >= 5, 1, 0)

#Select only the variables that we are interested in
polat_c <- polat %>%
  select(c(id, wave, sex, age,
           partyid_str, partyidintens, lrself, lrpodemos, lrvox, 
           polintr, partyidrepesca_str, voteintentionspain_str, vote2019nov, 
           vote2019, vote2016, vote2015, voteintentioneuro,
           probvotevox, probvotePodemos,
           education, ecositpersonal, ecositpast, socialtrust, trustparl,
           pop1willpeople, pop2compromise, pop3differences, pop4represent, pop5decisions, pop6muchtalk,
           polgoodevil, pollead, govbusiness, govexperts, 
           riskaversion2, efintunderstand, efintopinion, efintpart, satdem, 
           indeprespect, selfconfobed, empathybehave, curiosmanners,  
           problem, climatechange, threatclimhealth, threatclimecpers, threatclimeccountry, 
           threatclimvalpers, threatclimvalcountry, immigeco, immicult,
           spanishnat, spanishnatpodemos, spanishnatvox
           ))

#Filter to only include waves 6 onwards
polat_c <- polat_c %>%
  filter(wave >= 6)

#Recoding the variables-----------------------------------
##Populist Attitudes
polat_c$pop1willpeople <- match(
  polat_c$pop1willpeople,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

polat_c$pop2compromise <- match(
  polat_c$pop2compromise,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

polat_c$pop3differences <- match(
  polat_c$pop3differences,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

polat_c$pop4represent <- match(
  polat_c$pop4represent,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

polat_c$pop5decisions <- match(
  polat_c$pop5decisions,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

polat_c$pop6muchtalk <- match(
  polat_c$pop6muchtalk,
  c("Muy en desacuerdo", "Bastante en desacuerdo", "Más bien en desacuerdo", 
    "Ni de acuerdo ni en desacuerdo", 
    "Más bien de acuerdo", "Bastante de acuerdo", "Muy de acuerdo"))

#Redefine the environment variables to numeric values -------------------
polat_c$climatechange <- case_when(
  polat_c$climatechange == "0 Den prioridad al crecimiento económico, aunque esto tenga consecuencias negativas sobre el cambio climático" ~ 0,
  polat_c$climatechange == "10 Den prioridad al cambio climático, aunque esto tenga consecuencias negativas sobre el crecimiento económico" ~ 10,
  polat_c$climatechange == "1" ~ 1,
  polat_c$climatechange == "2" ~ 2,
  polat_c$climatechange == "3" ~ 3,
  polat_c$climatechange == "4" ~ 4,
  polat_c$climatechange == "5" ~ 5,
  polat_c$climatechange == "6" ~ 6,
  polat_c$climatechange == "7" ~ 7,
  polat_c$climatechange == "8" ~ 8,
  polat_c$climatechange == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$threatclimhealth <- match(
  polat_c$threatclimhealth,
  c("Mucho", "Bastante", "Poco", "Nada"))

polat_c$threatclimecpers <- match(
  polat_c$threatclimecpers,
  c("Mucho", "Bastante", "Poco", "Nada"))

polat_c$threatclimeccountry <- match(
  polat_c$threatclimeccountry,
  c("Mucho", "Bastante", "Poco", "Nada"))

polat_c$threatclimvalpers <- match(
  polat_c$threatclimvalpers,
  c("Mucho", "Bastante", "Poco", "Nada"))

polat_c$threatclimvalcountry <- match(
  polat_c$threatclimvalcountry,
  c("Mucho", "Bastante", "Poco", "Nada"))

#Redefine the immigration variables to numeric values -------------------
polat_c$immigeco <- case_when(
  polat_c$immigeco == "Efecto muy negativo" ~ 0,
  polat_c$immigeco == "Efecto muy positivo" ~ 10,
  polat_c$immigeco == "1" ~ 1,
  polat_c$immigeco == "2" ~ 2,
  polat_c$immigeco == "3" ~ 3,
  polat_c$immigeco == "4" ~ 4,
  polat_c$immigeco == "5" ~ 5,
  polat_c$immigeco == "6" ~ 6,
  polat_c$immigeco == "7" ~ 7,
  polat_c$immigeco == "8" ~ 8,
  polat_c$immigeco == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$immicult <- case_when(
  polat_c$immicult == "Nada importante" ~ 0,
  polat_c$immicult == "Muy importante" ~ 10,
  polat_c$immicult == "1" ~ 1,
  polat_c$immicult == "2" ~ 2,
  polat_c$immicult == "3" ~ 3,
  polat_c$immicult == "4" ~ 4,
  polat_c$immicult == "5" ~ 5,
  polat_c$immicult == "6" ~ 6,
  polat_c$immicult == "7" ~ 7,
  polat_c$immicult == "8" ~ 8,
  polat_c$immicult == "9" ~ 9,
  TRUE ~ NA_real_)

#Redefine the left-right self placement variable to numeric values -------------------
polat_c$lrself <- case_when(
  polat_c$lrself == "Extrema izquierda" ~ 0,
  polat_c$lrself == "Extrema derecha" ~ 10,
  polat_c$lrself == 1 ~ 1,
  polat_c$lrself == 2 ~ 2,
  polat_c$lrself == 3 ~ 3,
  polat_c$lrself == 4 ~ 4,
  polat_c$lrself == 5 ~ 5,
  polat_c$lrself == 6 ~ 6,
  polat_c$lrself == 7 ~ 7,
  polat_c$lrself == 8 ~ 8,
  polat_c$lrself == 9 ~ 9,
  TRUE ~ NA_real_)
polat_c$lrself <- as.numeric(polat_c$lrself)

polat_c$lrvox <- case_when(
  polat_c$lrvox == "Extrema izquierda" ~ 0,
  polat_c$lrvox == "Extrema derecha" ~ 10,
  polat_c$lrvox == 1 ~ 1,
  polat_c$lrvox == 2 ~ 2,
  polat_c$lrvox == 3 ~ 3,
  polat_c$lrvox == 4 ~ 4,
  polat_c$lrvox == 5 ~ 5,
  polat_c$lrvox == 6 ~ 6,
  polat_c$lrvox == 7 ~ 7,
  polat_c$lrvox == 8 ~ 8,
  polat_c$lrvox == 9 ~ 9,
  TRUE ~ NA_real_)
polat_c$lrvox <- as.numeric(polat_c$lrvox)

polat_c$lrpodemos <- case_when(
  polat_c$lrpodemos == "Extrema izquierda" ~ 0,
  polat_c$lrpodemos == "Extrema derecha" ~ 10,
  polat_c$lrpodemos == 1 ~ 1,
  polat_c$lrpodemos == 2 ~ 2,
  polat_c$lrpodemos == 3 ~ 3,
  polat_c$lrpodemos == 4 ~ 4,
  polat_c$lrpodemos == 5 ~ 5,
  polat_c$lrpodemos == 6 ~ 6,
  polat_c$lrpodemos == 7 ~ 7,
  polat_c$lrpodemos == 8 ~ 8,
  polat_c$lrpodemos == 9 ~ 9,
  TRUE ~ NA_real_)
polat_c$lrpodemos <- as.numeric(polat_c$lrpodemos)

#Replace blanks with NA in the party identification variables -------------------
polat_c$partyidtens <- ifelse(polat_c$partyidintens == " ", 
                              NA, polat_c$partyidintens)

#Redefine the education variable to numeric values -------------------
polat_c$education <- trimws(polat_c$education)
polat_c$education <- iconv(polat_c$education, to = "UTF-8")

polat_c$education <- case_when(
  polat_c$education == "Tengo menos de 5 años de escolarización" ~ 1,
  polat_c$education == "Educación primaria de LOGSE" ~ 2,
  polat_c$education == "Educación General Básica (EGB)" ~ 3,
  polat_c$education == "Educación Secundaria Obligatoria (ESO)" ~ 4,
  polat_c$education == "Formación Profesional de grado medio" ~ 5,
  polat_c$education == "Bachillerato de LOGSE" ~ 6,
  polat_c$education == "BUP/COU" ~ 7,
  polat_c$education == "Formación Profesional de grado superior" ~ 8,
  polat_c$education == "Diplomado, Arquitecto e Ingeniero Técnico" ~ 9,
  polat_c$education == "Licenciado, Arquitecto e Ingeniero Superior" ~ 10,
  polat_c$education == "Estudios de Postgrado o especialización" ~ 11,
  TRUE ~ NA_real_)

#This was creating a new variable so we could see if someone had a degree beyond high school #
polat_c$edu_high <- ifelse(polat_c$education > 6, 1, 0)

#Recoded so that things listed as text will actually be the numbers they represent#
#This question is coded in reverse as compared to the BES survey and also is on a 4 point scale rather than a 10 point scale#
polat_c$polintr <- case_when(
  polat_c$polintr == "Mucho" ~ 1,
  polat_c$polintr == "Bastante" ~ 2,
  polat_c$polintr == "Poco" ~ 3,
  polat_c$polintr == "Nada" ~ 4,
  TRUE ~ NA_real_)

#This question is coded in reverse as compared to the BES survey and also is on a 3 point scale rather than a 5 point scale#
polat_c <- polat_c %>%
  mutate(ecositpersonal = case_when(
    ecositpersonal == "Mejor que hace un año" ~ 1,
    ecositpersonal == "Igual que hace un año" ~ 2,
    ecositpersonal == "Peor que hace un año" ~ 3,
    TRUE ~ NA_real_))

#This question is coded in reverse as compared to the BES survey and also is on a 3 point scale rather than a 5 point scale#
polat_c <- polat_c %>% 
  mutate(ecositpast = case_when(
    ecositpast == "Mejor" ~ 1,
    ecositpast == "Igual" ~ 2,
    ecositpast == "Peor" ~ 3,
    TRUE ~ NA_real_))

polat_c <- polat_c %>%
  mutate(socialtrust = case_when(
    socialtrust == "Se puede confiar en la gente" ~ 1,
    socialtrust == "Hay que ir con cuidado" ~ 2,
    TRUE ~ NA_real_))

polat_c$trustparl <- case_when(
  polat_c$trustparl == "Ninguna confianza" ~ 0,
  polat_c$trustparl == "Total Confianza" ~ 10,
  polat_c$trustparl == "1" ~ 1,
  polat_c$trustparl == "2" ~ 2,
  polat_c$trustparl == "3" ~ 3,
  polat_c$trustparl == "4" ~ 4,
  polat_c$trustparl == "5" ~ 5,
  polat_c$trustparl == "6" ~ 6,
  polat_c$trustparl == "7" ~ 7,
  polat_c$trustparl == "8" ~ 8,
  polat_c$trustparl == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$indeprespect <- case_when(
  polat_c$indeprespect == "Independencia" ~ 1,
  polat_c$indeprespect == "Respeto por los mayores" ~ 2)

polat_c$selfconfobed <- case_when(
  polat_c$selfconfobed == "Obediencia" ~ 1,
  polat_c$selfconfobed == "Confianza en sí mismo" ~ 2)

polat_c$empathybehave <- case_when(
  polat_c$empathybehave == "Ser considerado" ~ 1,
  polat_c$empathybehave == "Portarse bien" ~ 2)

polat_c$curiosmanners <- case_when(
  polat_c$curiosmanners == "Curiosidad" ~ 1,
  polat_c$curiosmanners == "Buenos modales" ~ 2)

polat_c$riskaversion2 <- case_when(
  polat_c$riskaversion2 == "0 Me siento muy cómodo asumiendo riesgos" ~ 0,
  polat_c$riskaversion2 == "10 Me siento muy incómodo asumiendo riesgos" ~ 10,
  polat_c$riskaversion2 == "1" ~ 1,
  polat_c$riskaversion2 == "2" ~ 2,
  polat_c$riskaversion2 == "3" ~ 3,
  polat_c$riskaversion2 == "4" ~ 4,
  polat_c$riskaversion2 == "5" ~ 5,
  polat_c$riskaversion2 == "6" ~ 6,
  polat_c$riskaversion2 == "7" ~ 7,
  polat_c$riskaversion2 == "8" ~ 8,
  polat_c$riskaversion2 == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$efintunderstand <- case_when(
  polat_c$efintunderstand == "Me cuesta entender lo que pasa en la política" ~ 0,
  polat_c$efintunderstand == "Creo que entiendo bien lo que pasa en la política" ~ 10,
  polat_c$efintunderstand == "1" ~ 1,
  polat_c$efintunderstand == "2" ~ 2,
  polat_c$efintunderstand == "3" ~ 3,
  polat_c$efintunderstand == "4" ~ 4,
  polat_c$efintunderstand == "5" ~ 5,
  polat_c$efintunderstand == "6" ~ 6,
  polat_c$efintunderstand == "7" ~ 7,
  polat_c$efintunderstand == "8" ~ 8,
  polat_c$efintunderstand == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$efintopinion <- case_when(
  polat_c$efintopinion == "Nunca tengo opiniones sobre cuestiones políticas" ~ 0,
  polat_c$efintopinion == "Siempre tengo opiniones sobre cuestiones políticas" ~ 10,
  polat_c$efintopinion == "1" ~ 1,
  polat_c$efintopinion == "2" ~ 2,
  polat_c$efintopinion == "3" ~ 3,
  polat_c$efintopinion == "4" ~ 4,
  polat_c$efintopinion == "5" ~ 5,
  polat_c$efintopinion == "6" ~ 6,
  polat_c$efintopinion == "7" ~ 7,
  polat_c$efintopinion == "8" ~ 8,
  polat_c$efintopinion == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$efintpart <- case_when(
  polat_c$efintpart == "Me considero poco capacitado/a para participar en política" ~ 0,
  polat_c$efintpart == "Me considero muy capacitado/a para participar en política" ~ 10,
  polat_c$efintpart == "1" ~ 1,
  polat_c$efintpart == "2" ~ 2,
  polat_c$efintpart == "3" ~ 3,
  polat_c$efintpart == "4" ~ 4,
  polat_c$efintpart == "5" ~ 5,
  polat_c$efintpart == "6" ~ 6,
  polat_c$efintpart == "7" ~ 7,
  polat_c$efintpart == "8" ~ 8,
  polat_c$efintpart == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$satdem <- case_when(
  polat_c$satdem == "0 Completamente insatisfecho/a" ~ 10,
  polat_c$satdem == "10 Completamente satisfecho/a" ~ 0,
  polat_c$satdem == "1" ~ 1,
  polat_c$satdem == "2" ~ 2,
  polat_c$satdem == "3" ~ 3,
  polat_c$satdem == "4" ~ 4,
  polat_c$satdem == "5" ~ 5,
  polat_c$satdem == "6" ~ 6,
  polat_c$satdem == "7" ~ 7,
  polat_c$satdem == "8" ~ 8,
  polat_c$satdem == "9" ~ 9,
  TRUE ~ NA_real_)

#Redefine the national identity variables to numeric values -------------------
polat_c$spanishnat <- case_when(
  polat_c$spanishnat == "Mínimo españolismo" ~ 0,
  polat_c$spanishnat == "Máximo españolismo" ~ 10,
  polat_c$spanishnat == "1" ~ 1,
  polat_c$spanishnat == "2" ~ 2,
  polat_c$spanishnat == "3" ~ 3,
  polat_c$spanishnat == "4" ~ 4,
  polat_c$spanishnat == "5" ~ 5,
  polat_c$spanishnat == "6" ~ 6,
  polat_c$spanishnat == "7" ~ 7,
  polat_c$spanishnat == "8" ~ 8,
  polat_c$spanishnat == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$spanishnatpodemos <- case_when(
  polat_c$spanishnatpodemos == "Mínimo españolismo" ~ 0,
  polat_c$spanishnatpodemos == "Máximo españolismo" ~ 10,
  polat_c$spanishnatpodemos == "1" ~ 1,
  polat_c$spanishnatpodemos == "2" ~ 2,
  polat_c$spanishnatpodemos == "3" ~ 3,
  polat_c$spanishnatpodemos == "4" ~ 4,
  polat_c$spanishnatpodemos == "5" ~ 5,
  polat_c$spanishnatpodemos == "6" ~ 6,
  polat_c$spanishnatpodemos == "7" ~ 7,
  polat_c$spanishnatpodemos == "8" ~ 8,
  polat_c$spanishnatpodemos == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$spanishnatvox <- case_when(
  polat_c$spanishnatvox == "Mínimo españolismo" ~ 0,
  polat_c$spanishnatvox == "Máximo españolismo" ~ 10,
  polat_c$spanishnatvox == "1" ~ 1,
  polat_c$spanishnatvox == "2" ~ 2,
  polat_c$spanishnatvox == "3" ~ 3,
  polat_c$spanishnatvox == "4" ~ 4,
  polat_c$spanishnatvox == "5" ~ 5,
  polat_c$spanishnatvox == "6" ~ 6,
  polat_c$spanishnatvox == "7" ~ 7,
  polat_c$spanishnatvox == "8" ~ 8,
  polat_c$spanishnatvox == "9" ~ 9,
  TRUE ~ NA_real_)

#Redefine the probvote variables to numeric values -------------------
polat_c$probvotevox <- case_when(
  polat_c$probvotevox == "0 Ninguna probabilidad, nunca le votaría" ~ 0,
  polat_c$probvotevox == "10 Todas las probabilidades, con seguridad le votaría" ~ 10,
  polat_c$probvotevox == "1" ~ 1,
  polat_c$probvotevox == "2" ~ 2,
  polat_c$probvotevox == "3" ~ 3,
  polat_c$probvotevox == "4" ~ 4,
  polat_c$probvotevox == "5" ~ 5,
  polat_c$probvotevox == "6" ~ 6,
  polat_c$probvotevox == "7" ~ 7,
  polat_c$probvotevox == "8" ~ 8,
  polat_c$probvotevox == "9" ~ 9,
  TRUE ~ NA_real_)

polat_c$probvotePodemos <- case_when(
  polat_c$probvotePodemos == "0 Ninguna probabilidad, nunca le votaría" ~ 0,
  polat_c$probvotePodemos == "10 Todas las probabilidades, con seguridad le votaría" ~ 10,
  polat_c$probvotePodemos == "1" ~ 1,
  polat_c$probvotePodemos == "2" ~ 2,
  polat_c$probvotePodemos == "3" ~ 3,
  polat_c$probvotePodemos == "4" ~ 4,
  polat_c$probvotePodemos == "5" ~ 5,
  polat_c$probvotePodemos == "6" ~ 6,
  polat_c$probvotePodemos == "7" ~ 7,
  polat_c$probvotePodemos == "8" ~ 8,
  polat_c$probvotePodemos == "9" ~ 9,
  TRUE ~ NA_real_)


#Factor Analysis: Populist Attitudes#
df_pop <- data.frame(polat_c$pop1willpeople, polat_c$pop2compromise, polat_c$pop3differences, polat_c$pop4represent, polat_c$pop5decisions, polat_c$pop6muchtalk)

#There are no "Don't Know" values in this data set to clean for and replace with NAs#
df_pop$polat_c.pop1willpeople <- as.numeric(df_pop$polat_c.pop1willpeople)
df_pop$polat_c.pop2compromise <- as.numeric(df_pop$polat_c.pop2compromise)
df_pop$polat_c.pop3differences <- as.numeric(df_pop$polat_c.pop3differences)
df_pop$polat_c.pop4represent <- as.numeric(df_pop$polat_c.pop4represent)
df_pop$polat_c.pop5decisions <- as.numeric(df_pop$polat_c.pop5decisions)
df_pop$polat_c.pop6muchtalk <- as.numeric(df_pop$polat_c.pop6muchtalk)

##FACTOR ANALYSIS w/ Hubers comments##
fa.parallel(df_pop, fa = "fa")
## Test to assess the number of factors with 5 different rule:
#1 Only Factors above eigenvalue 1
#2 Only Factors before the "elbow": Strong difference beween Factor i and i+1
#3 Only Factors above Simulated or resampled data
VSS(df_pop,  3, rotate="promax", fm="mle")
#4 Only number of factors close to CSS Complexity 1 and 2
#5 Only number of factors mentioned by the MAP criterion

fa_pop <- fa(df_pop, 1, fm="ml", rotate = "varimax")
#Factor analysis by psych package. fm="ml" == factanal in R

print(fa_pop, digits = 2, cut = .5, sort = TRUE)
#Output from Factor analysis. Round for 2 digits and no loadings below .2 sorted by loading

hist(fa_pop$scores)

#and then adding into the database, plus some other Huber code
polat_c$populism <- fa_pop$scores

chronbachalpha <- alpha(df_pop, check.keys = TRUE)
chronbachalpha

str(fa_pop$loadings)
class(fa_pop$loadings)

# Factor Analysis: AUTH --------------------------------------------------------------------
#More Huber data cleaning
df_auth <- data.frame(polat_c$indeprespect, polat_c$selfconfobed, polat_c$empathybehave, polat_c$curiosmanners)

#There are no "Don't Know" values in this data set to clean for and replace with NAs#
df_auth$polat_c.indeprespect <- as.numeric(df_auth$polat_c.indeprespect)
df_auth$polat_c.selfconfobed <- as.numeric(df_auth$polat_c.selfconfobed)
df_auth$polat_c.empathybehave <- as.numeric(df_auth$polat_c.empathybehave)
df_auth$polat_c.curiosmanners <- as.numeric(df_auth$polat_c.curiosmanners)

##FACTOR ANALYSIS w/ Hubers comments##
fa.parallel(df_auth, fa = "fa")
## Test to assess the number of factors with 5 different rule:
#1 Only Factors above eigenvalue 1
#2 Only Factors before the "elbow": Strong difference beween Factor i and i+1
#3 Only Factors above Simulated or resampled data
VSS(df_auth,  3, rotate="promax", fm="mle")
#4 Only number of factors close to CSS Complexity 1 and 2
#5 Only number of factors mentioned by the MAP criterion

fa_auth <- fa(df_auth, 1, fm="ml", rotate = "varimax")
#Factor analysis by psych package. fm="ml" == factanal in R

print(fa_auth, digits = 2, cut = .3, sort = TRUE)
#Output from Factor analysis. Round for 2 digits and no loadings below .2 sorted by loading

hist(fa_auth$scores)

#and again, same as above
polat_c$auth <- fa_auth$scores


chronbachalpha <- alpha(df_auth, check.keys = TRUE)
chronbachalpha

str(fa_auth$loadings)
class(fa_auth$loadings)

#Create a new variable that is an index of the "efint" variables#
polat_c$efintindex <- (polat_c$efintunderstand + polat_c$efintopinion + polat_c$efintpart)/3

# Final Data Cleaning ---------------------------------------------------
polat_c$populism <- as.vector(polat_c$populism)
polat_c$populism <- as.numeric(polat_c$populism)

#Create an new variable that is an index of the climate threat variable#
polat_c$threatclimindex <- (polat_c$threatclimhealth + polat_c$threatclimecpers + polat_c$threatclimeccountry + polat_c$threatclimvalpers + polat_c$threatclimvalcountry)/5

#Create a new variable that is an index of the six populist attitudes#
polat_c$popindex <- (polat_c$pop1willpeople + polat_c$pop2compromise + polat_c$pop3differences + polat_c$pop4represent + polat_c$pop5decisions + polat_c$pop6muchtalk)/6

#Create a new binary variable for those who do not express populist attitudes#
polat_c$low_populism <- case_when(
  polat_c$popindex <= 5 ~ 1,
  polat_c$popindex > 5 ~ 0)

#Save the cleaned data set as a new csv file#
#Specify the file path where the cleaned data set will be saved#
csv_file_path <- "C:/Users/Owner/Desktop/Hawkins-RA/Environment and Populism Project"
write.csv(polat_c, "POLAT_clean_2.csv", row.names = FALSE)

