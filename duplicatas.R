data<- read.csv(here::here("/", "naiveeresults.xlsx"), sep = ";")
#abrimos os pacotes
library(devtools)
library(revtools)
library(stringi)
library(stringr)
library(remotes)
library(litsearchr)
library(synthesisr)
library(data.table)
library(openxlsx)
#Tentei instalar litsearchr mas nao funcionou, entao instalei pelo github
remotes::install_github("ropensci/litsearchr")
install.packages("remotes")
install.packages("litsearchr")
library(remotes)
install_github("elizagrames/litsearchr", ref="main")
search_directory<-("/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Mestrado/mestrado/Diretorio R/Primeiras buscas meta analise/buscas_so_leucaena")
#importamos os dados
naiveimport<-litsearchr::import_results(directory = "/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Mestrado/mestrado/Diretorio R/Primeiras buscas meta analise/buscas_so_leucaena", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naiveresults <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naiveresults)
#salvamos o arquivo
write.xlsx (naiveresults, "naiveeresults.xlsx")


