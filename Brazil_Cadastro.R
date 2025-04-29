#Getting Started#
rm(list=ls())
library(tidyverse)
library(readr)
library(knitr)
library(dplyr)
setwd("~/Downloads/202408_Servidores_SIAPE")

#Reading the data####
cadastro <- read.csv("202408_Cadastro.csv", 
                     sep = ";", 
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     encoding = "latin1")

#Filter by position and job, which we can do by using DESCRIÇÃO_CARGO####
count_table <- table(cadastro$DESCRICAO_CARGO)
#convert to data frame#
count_table <- as.data.frame(count_table)
nrow(count_table) #There are 2,289 unique cargos#

table(is.na(cadastro$ORG_EXERCICIO))
#There are no NAs, this is because they are listed as "Sem informação"#

org <- table(cadastro$ORG_EXERCICIO)
org <- as.data.frame(org)
nrow(org)
length(unique(cadastro$ORG_EXERCICIO)) #There are 349 unique ministerios#

list_orgsup <- list(unique(cadastro$ORGSUP_EXERCICIO))

#List of ministires####


##Min Da integ e do desenv regional####
Min_da_integ <- cadastro %>% 
  filter(ORG_EXERCICIO == "MIN DA INTEG E DO DESENV REGIONAL")
nrow(Min_da_integ) #There are 1,000 employees in the ministry of integration and regional development#

sum(Min_da_integ$ATIVIDADE=="Sem informaç" & Min_da_integ$DESCRICAO_CARGO!= "Sem informaç")
sum(Min_da_integ$ATIVIDADE!="Sem informaç" & Min_da_integ$DESCRICAO_CARGO== "Sem informaç") 
sum(Min_da_integ$ATIVIDADE=="Sem informaç" & Min_da_integ$DESCRICAO_CARGO== "Sem informaç") 
sum(Min_da_integ$ATIVIDADE!="Sem informaç" & Min_da_integ$DESCRICAO_CARGO!= "Sem informaç")

##MIN DESENVOLV IND COMERCIO E SERVICOS####
Min_desenvolv <- cadastro %>% 
  filter(ORG_EXERCICIO == "MIN DESENVOLV IND COMERCIO E SERVICOS")
nrow(Min_desenvolv) #There are 1,047 employees in the ministry of development, industry, commerce and services#

sum(Min_desenvolv$ATIVIDADE=="Sem informaç" & Min_desenvolv$DESCRICAO_CARGO!= "Sem informaç")
sum(Min_desenvolv$ATIVIDADE!="Sem informaç" & Min_desenvolv$DESCRICAO_CARGO== "Sem informaç")
sum(Min_desenvolv$ATIVIDADE=="Sem informaç" & Min_desenvolv$DESCRICAO_CARGO== "Sem informaç")
sum(Min_desenvolv$ATIVIDADE!="Sem informaç" & Min_desenvolv$DESCRICAO_CARGO!= "Sem informaç")


#For loop the process to streamline the process#

ministries <- unique(cadastro$ORGSUP_EXERCICIO)
# Create an empty data frame to store the results
result_df <- data.frame(ORGSUP_EXERCICIO = character(), 
                        nativ_ycarg = numeric(),
                        yativ_ncarg = numeric(),
                        yativ_ycarg = numeric(),
                        nativ_ncarg = numeric(),
                        stringsAsFactors = FALSE)

# Loop through each ministry
for (ORGSUP_EXERCICIO in ministries) {
  
  # Calculate the sum when atividade == "sem informacao" and cargo != "sem informacao"
  sum1 <- sum(cadastro$ATIVIDADE=="Sem informaç" & cadastro$DESCRICAO_CARGO!= "Sem informaç" & cadastro$ORGSUP_EXERCICIO == ORGSUP_EXERCICIO)
  sum2 <- sum(cadastro$ATIVIDADE!="Sem informaç" & cadastro$DESCRICAO_CARGO== "Sem informaç" & cadastro$ORGSUP_EXERCICIO == ORGSUP_EXERCICIO)
  sum3 <- sum(cadastro$ATIVIDADE=="Sem informaç" & cadastro$DESCRICAO_CARGO== "Sem informaç" & cadastro$ORGSUP_EXERCICIO == ORGSUP_EXERCICIO)
  sum4 <- sum(cadastro$ATIVIDADE!="Sem informaç" & cadastro$DESCRICAO_CARGO!= "Sem informaç" & cadastro$ORGSUP_EXERCICIO == ORGSUP_EXERCICIO)
  
  # Append the results to the data frame
  result_df <- rbind(result_df, data.frame(Ministry = ORGSUP_EXERCICIO, 
                                           nativ_ycarg = sum1,
                                           yativ_ncarg = sum2,
                                           yativ_ycarg = sum3,
                                           nativ_ncarg = sum4,
                                           stringsAsFactors = FALSE))
}

# Print the results
print(result_df)

#Save the results to an excel file#
#export it into excel
library(openxlsx)
install.packages("openxlsx")

# Specify the file path and name for the Excel file#
excel_file <- "~/Downloads/summary_data_202408.xlsx"

# Write the data frame to an Excel file
write.xlsx(result_df, file = excel_file)



#Number of unique positions withing the ministry of education#

cad_edu <- cadastro %>% 
  filter(ORGSUP_EXERCICIO == "Ministério da Educação")

count_table_edu <- table(cad_edu$DESCRICAO_CARGO)
count_table_edu <- as.data.frame(count_table_edu)
nrow(count_table_edu) #There are 846 unique positions within the ministry of education#
view(count_table_edu)

#Creating a for loop to find the number of unique cargos within each ministry#
#Create an empty data frame to store the results#
cargos_df <- data.frame(ORGSUP_EXERCICIO = character(), 
                        unique_positions = numeric(),
                        stringsAsFactors = FALSE)

#Loop through each ministry#

for (ministry_name in ministries) {
  
  # Filter the data for the specific ministry
  cad_ministry <- cadastro %>% 
    filter(ORGSUP_EXERCICIO == ministry_name)
  
  # Count the number of unique positions within the ministry
  count_positions <- nrow(as.data.frame(table(cad_ministry$DESCRICAO_CARGO)))
  
  # Append the results to the data frame
  cargos_df <- rbind(cargos_df, data.frame(Ministry = ministry_name, 
                                           unique_positions = count_positions,
                                           stringsAsFactors = FALSE))
}


#Print the results#
print(cargos_df)

#Save the results to an excel file#
# Specify the file path and name for the Excel file#
excel_file_cargos <- "~/Downloads/unique_positions_202408.xlsx"

# Write the data frame to an Excel file
write.xlsx(cargos_df, file = excel_file_cargos)

#Split the names into first and last name####
#The first name is the first name and the last name of all is the last name#
cadastro_E <- cadastro %>%
  mutate(First_Name = word(NOME, 1),
         Last_Name = word(NOME, -1)) 

#List out the domain names for each ministry --------------
view(ministries)
#Create a list of domain names for each ministry#
ministy_domains <- list(
  "Ministério da Educação" = "mec.gov.br",
  "MIN DESENVOLV IND COMERCIO E SERVICOS" = "mdic.gov.br",
  "Ministério do Desenvolvimento Regional" = "mdr.gov.br",
  "MIN GESTAO E INOV EM SERV PUBLICOS" = "gestao.gov.br",
  "MINISTERIO DA DEFESA" = "defesa.gov.br",
  "MINISTERIO DO PLANEJAMENTO E ORCAMENTO" = "planejamento.gov.br",
  "MIN DO DESENV AGR E AGRIC FAMILIAR" = "mda.gov.br",
  "MINISTERIO DAS CIDADES" = "cidades.gov.br",
  "Ministério da Saúde" = "saude.gov.br",
  "MINISTERIO DOS TRANSPORTES" = "transportes.gov.br",
  "Ministério de Minas e Energia" = "mme.gov.br",
  "MINISTERIO DA PREVIDENCIA SOCIAL" = "previdencia.gov.br",
  "Ministério da Defesa" = "defesa.gov.br",
  "MINISTERIO DOS POVOS INDIGENAS" = "povosindigenas.gov.br",
  "MINISTERIO DO MEIO AMBIENTE" = "mma.gov.br",
  "MIN DA INTEG E DO DESENV REGIONAL" = "mdr.gov.br",
  "MINISTERIO DA AGRICULTURA E PECUARIA" = "agro.gov.br",
  "Presidência da República" = "presidencia.gov.br",
  "Ministério da Ciência, Tecnologia, Inovações e Comunicações" = "mcti.gov.br",  
  #Return here to fix; the webstie has this as two separate ministries#
  
  "MINISTERIO DA CULTURA" = "cultura.gov.br",
  "Ministério das Relações Exteriores" = "itamaraty.gov.br",
  "MINISTERIO DE PORTOS E AEROPORTOS" = "mpor.gov.br",
  "MINISTERIO DA FAZENDA" = "fazenda.gov.br",
  "Ministério da Economia" = "economia.gov.br",
  #Return here to fix; could not find on website or quem e quem document#
  
  "MINISTERIO DO TRABALHO E EMPREGO" = "trabalho.gov.br",
  #Return here to fix; some domains are mte.gov.br#
  
  "Ministério da Justiça e Segurança Pública" = "mj.gov.br",
  "Poder Executivo Federal" = "mj.gov.br",
  #Return here to fix; could not find on website or quem e quem document#
  #Based off of what Mariana found, I will use the domain name of Ministério da Justiça e Segurança Pública#
  
  "Ministério da Previdência Social" = "previdencia.gov.br",
  #Return here to fix; some domains are mtp.gov.br#
  
  "Ministério do Planejamento, Desenvolvimento e Gestão" = "planejamento.gov.br",
  #could not find on website or quem e quem document#
  #Based off of what Mariana found, I will use the domain name of Ministério da Planejamento e Orçamento#
  
  "Ministério do Meio Ambiente" = "mma.gov.br",
  "Ministério da Infraestrutura" = "gestao.gov.br")
  #could not find on website or quem e quem document#
  #Based off of what Mariana found, I will use the domain name of Ministério da Gestão e da Inovação em Serviços Públicos#
# Emails --------------------------------------------

#Create unique emails for each individial with domain names based off of their ministry#
cadastro_E <- cadastro_E %>%
  mutate(Email = paste0(tolower(First_Name), ".", tolower(Last_Name), "@", ministy_domains[ORGSUP_EXERCICIO]))

#Clean the data####
cad_E_clean <- cadastro_E %>%
  mutate(ORGSUP_EXERCICIO = na_if(ORGSUP_EXERCICIO, "Sem informação"))

#Remove the rows with missing values#
cad_E_clean <- cad_E_clean %>%
  drop_na(ORGSUP_EXERCICIO)

cad_E_clean <- cad_E_clean %>%
  select(c(Id_SERVIDOR_PORTAL, First_Name, Last_Name, Email, NOME, ATIVIDADE, UORG_EXERCICIO, ORG_EXERCICIO, ORGSUP_EXERCICIO, UORG_LOTACAO, ORG_LOTACAO, ORGSUP_LOTACAO))

#Find emails for individuals with "Sem informação" in the ORGSUP_EXERCICIO column####
#Filter the original cadastro data to only include observations with "Sem informação" in the ORGSUP_EXERCICIO column#
cad_sem <- cadastro %>%
  filter(ORGSUP_EXERCICIO == "Sem informação")

#Are there any "Sem informação" values in the ORG_EXERCICIO column?#
cad_sem2 <- cad_sem %>%
  filter(ORG_EXERCICIO == "Sem informação")
#There are no observations with "Sem informação" in the ORG_EXERCICIO column#

#Filter the cad_sem data to only include observations in the ORG_EXERCICIO column that appear in the list of ministries#
cad_org <- cad_sem %>%
  filter(ORG_EXERCICIO %in% ministries)

#Split the names into first and last name####
#The first name is the first name and the last name of all is the last name#
cad_org_E <- cad_org %>%
  mutate(First_Name = word(NOME, 1),
         Last_Name = word(NOME, -1)) 

#Create unique emails for each individial with domain names based off of their ministry listed in the ORG_EXERCICIO column#
cad_org_E <- cad_org_E %>%
  mutate(Email = paste0(tolower(First_Name), ".", tolower(Last_Name), "@", ministy_domains[ORG_EXERCICIO]))

#Clean the data#
cad_org_E_clean <- cad_org_E %>%
  select(c(Id_SERVIDOR_PORTAL, First_Name, Last_Name, Email, NOME, ATIVIDADE, UORG_EXERCICIO, ORG_EXERCICIO, ORGSUP_EXERCICIO, UORG_LOTACAO, ORG_LOTACAO, ORGSUP_LOTACAO))

#Filter the cad_sem data to only include observations in the ORG_EXERCICIO column that DO NOT appear in the list of ministries####
cad_exer <- cad_sem %>%
  filter(!ORG_EXERCICIO %in% ministries)
#Note 1: This data represents employees whose ORGSUP_EXERCICIO is "Sem informação" and ORG_EXERCICIO is not a ministry#
#Note 2: It seems as though I can use the ORG_LOTACAO column to find the ministry of the employees in the Health Ministry#
#Note 3: It seems as though I can use the ORGSUP_LOTACAO column to find the ministry of the employees in all the other ministries#

#First, I will filter the data to only include observations in the ORG_LOTACAO column that appear in the list of ministries#
cad_lota <- cad_exer %>%
  filter(ORG_LOTACAO %in% ministries)

#Check to see if there is any observation in the ORG_LOTACAO column that is not "Ministério da Saúde"#
cad_lota2 <- cad_lota %>%
  filter(ORG_LOTACAO != "Ministério da Saúde")

#This data does include observations not in the Health Ministry, however, the ministries present are in the list of ministries#

#Split the names into first and last name####
#The first name is the first name and the last name of all is the last name#
cad_lota_E <- cad_lota %>%
  mutate(First_Name = word(NOME, 1),
         Last_Name = word(NOME, -1))

#Create unique emails for each individial with domain names based off of their ministry listed in the ORG_LOTACAO column#
cad_lota_E <- cad_lota_E %>%
  mutate(Email = paste0(tolower(First_Name), ".", tolower(Last_Name), "@", ministy_domains[ORG_LOTACAO]))

#Clean the data#
cad_lota_E_clean <- cad_lota_E %>%
  select(c(Id_SERVIDOR_PORTAL, First_Name, Last_Name, Email, NOME, ATIVIDADE, UORG_EXERCICIO, ORG_EXERCICIO, ORGSUP_EXERCICIO, UORG_LOTACAO, ORG_LOTACAO, ORGSUP_LOTACAO))

#Next, I will filter the data to only include observations in the ORGSUP_LOTACAO column that appear in the list of ministries#
cad_supl <- cad_exer %>%
  filter(ORGSUP_LOTACAO %in% ministries) %>% 
  filter(ORGSUP_LOTACAO != "Sem informação")

#Split the names into first and last name####
#The first name is the first name and the last name of all is the last name#
cad_supl_E <- cad_supl %>%
  mutate(First_Name = word(NOME, 1),
         Last_Name = word(NOME, -1))

#Create unique emails for each individial with domain names based off of their ministry listed in the ORGSUP_LOTACAO column#
cad_supl_E <- cad_supl_E %>%
  mutate(Email = paste0(tolower(First_Name), ".", tolower(Last_Name), "@", ministy_domains[ORGSUP_LOTACAO]))

#Clean the data#
cad_supl_E_clean <- cad_supl_E %>%
  select(c(Id_SERVIDOR_PORTAL, First_Name, Last_Name, Email, NOME, ATIVIDADE, UORG_EXERCICIO, ORG_EXERCICIO, ORGSUP_EXERCICIO, UORG_LOTACAO, ORG_LOTACAO, ORGSUP_LOTACAO))

#How many observation in cad_exer are not present in cad_lota or cad_supl?#
cad_imp <- cad_exer %>%
  filter(!Id_SERVIDOR_PORTAL %in% cad_lota$Id_SERVIDOR_PORTAL) %>%
  filter(!Id_SERVIDOR_PORTAL %in% cad_supl$Id_SERVIDOR_PORTAL)

#cad_imp includes observations whose values in the ORG_EXERCICIO, ORGSUP_EXERCICIO, ORG_LOTACAO, and ORGSUP_LOTACAO columns are not ministries#
#I can not think of another way to create emails for these people, so I will leave them out of the final dataset#



#Combine the cleaned dataframes into one final dataframe####
final_emails <- rbind(cad_E_clean, cad_org_E_clean, cad_lota_E_clean, cad_supl_E_clean)
view(head(final_emails, 500))

#Export the final dataframe to a csv file#
write.csv(final_emails, file = "final_emails.csv", row.names = FALSE)







