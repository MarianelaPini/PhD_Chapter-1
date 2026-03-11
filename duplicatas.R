#Coloquei assim mas está errada, é pra salvar a função here
data<- read.csv(here::here("", "naiveeresults.xlsx"), sep = ";")
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
library(dplyr)
#Tentei instalar litsearchr mas nao funcionou, entao instalei pelo github
library(remotes)
install.packages(c(
  "igraph",
  "dplyr",
  "tidytext",
  "ggplot2",
  "tm",
  "stringr",
  "SnowballC"
))
remotes::install_github("elizagrames/litsearchr")
search_directory<-("C:/Users/maria/OneDrive/Documentos/PhD_Chapter-1/search/28-07")
#importamos os dados
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive/Documentos/PhD_Chapter-1/search/28-07", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_results <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_results)
naive_results
colnames(naive_results)
write.xlsx (naive_results, "naiveeresults.xlsx")
#Criando uma planilha da busca sem regenerant*, para teste
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_resultsnoreg <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_resultsnoreg)
naive_resultsnoreg
colnames(naive_resultsnoreg)
write.xlsx (naive_resultsnoreg, "naiveeresultsnoreg.xlsx")
#Criando uma planilha da busca com regenerant*
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_resultsreg <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_resultsreg)
naive_resultsreg
colnames(naive_resultsreg)
write.xlsx (naive_resultsreg, "naiveeresultsreg.xlsx")
#teste com secondar* forest
library(remotes)
search_directory<-("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test/secondary forest")
#importamos os dados
# Nesses dados coloquei as duas buscas de WoS, com secondary forest e sem
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test/secondary forest", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_results <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_results)
naive_results
colnames(naive_results)
write.xlsx (naive_results, "naiveeresults_secforest.xlsx")
#Criando uma planilha da busca sem secondar* forest, para teste, para saber quantos são e comparar
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test/secondary forest", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_resultsnosec <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_resultsnosec)
naive_resultsnosec
colnames(naive_resultsnosec)
write.xlsx (naive_resultsnosec, "naiveeresultsnosec.xlsx")
#Criando uma planilha da busca com secondary forest para checar*
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/test/secondary forest", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_resultssec <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_resultssec)
naive_resultssec
colnames(naive_resultssec)
write.xlsx (naive_resultssec, "naiveeresultssec.xlsx")
#Sorteio para avaliar quem são os artigos que aparecem na busca com secondary forest*
numeros_sorteados <- sample(1:725, 100, replace = FALSE)
print(numeros_sorteados)
sort(numeros_sorteados)

#Sorteio para avaliar quem são os artigos que aparecem na busca com regenerant*
numeros_sorteados <- sample(1:1915, 100, replace = FALSE)
print(numeros_sorteados)
sort(numeros_sorteados)

##script mari
####conferencia pontos

local2 <- "G:/Other computers/Dell Eclipse/Doutorado/Analise_de_dados/"

data <- read.table("Data_doc2.txt",  sep="\t", header = T)
n_species <- length(unique(data$Species))
sp <- sort(unique(data$Species))

#criando a lista das especies q quero comparar

especie_name1 <- sp[[1]]
especie1 <- data[data$Species==especie_name1,]

especie_name2 <- sp[[19]]
especie2 <- data[data$Species==especie_name2,]


a <- especie1$Site_study
b <- especie2$Site_study

#comparando pontos entre especies
pontos_dif <- setdiff(a, b)
View(pontos_dif)

#intersect(a,b)

write.table(pontos_dif,"C:/Documentos/Doutorado/Analise_de_dados/complist.csv", sep=",",dec=".")

##Tentativas para melhorar as palavras chaves na busca
#titulo do primeiro trabalho
naive_results[1, "title"]
#two differents ways of searching of new terms
naive_results[5, "keywords"]
#how many articles are missing keywords?
sum(is.na(naive_results[, "keywords"]))
#The method="tagged" argument lets extract_terms() know that we are getting keywords that article authors
#themselves have provided (or ‘tagged’ the article with).
extract_terms(keywords=naive_results[, "keywords"], method="tagged")
#min_freq=2. Only get keywords that appear at least twice in the full set of results. This is good for making sure that we are only getting keywords that are related to more than just one article in our field of interest. But it might also miss out some important extra suggestions.
#min_n=2. Only get keywords that consist of at least two words. This is why we only see multi-word phrases in the keywords we just got.
#max_n=5. Get keywords up to five words long. Maybe this is longer than we need.
keywords <- extract_terms(keywords=naive_results[, "keywords"], method="tagged", min_n=1,min_freq=20)
keywords
#extract by title
title_keywords<-extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=20, min_n=1)
title_keywords
all_stopwords <- c(get_stopwords("English"), title_keywords)
title_terms <- extract_terms(
  text=naive_results[, "title"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=all_stopwords
)
all_stopwords
title_terms
#fix later, nao deu certo
terms <- unique(c(keywords, title_terms))
terms
#network analysis
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1]
#creating matrix of words
dfm <- create_dfm(elements=docs, features=terms)
#Each entry in the matrix records how many times that article contains that term. For example, 
#if we look at the first three articles we see that adherence does not occur in any of them, adolescents occurs in the third, antidepressant occurs in the first two, and anxiety occurs in all of them.
dfm[1:3, 1:4]
g <- create_network(dfm, min_studies=3)
install.packages("ggraph")
library(ggplot2)
library(ggraph)
plot<-ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) 
plot
#ranking
install.packages("igraph")
library(igraph)
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths
term_strengths
View(term_strengths)
write.table(term_strengths, "terms_strengths.txt", sep="\t", row.names=FALSE, quote=FALSE)
#check terms united
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

# Definindo as datas
data_inicial <- as.Date("2003-03-18")
data_final <- as.Date("2005-06-30")

# Calculando o número de dias
dias <- as.numeric(data_final - data_inicial)
dias

##Busca Scielo
#importamos os dados
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive/Documentos/PhD_Chapter-1/search/Scielo", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_results <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naiveimport)
naive_results
colnames(naive_results)
write.xlsx (naiveimport, "BuscaScielo.xlsx")
#Agora uma vez que juntei em uma planilha só os dados do scielo e a busca anterior vou adicionar aqui a planilha
search_directory<-("C:/Users/maria/OneDrive/Documentos/PhD_Chapter-1/search/Scielo")
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive/Documentos/PhD_Chapter-1/search/Scielo", verbose = TRUE)
colnames(naiveimport)
##não funcionou abrir o arquivo pelo litsearchr, mas ai abri o csv manualmente, 
# e apos isso removo as duplicatas, porque estava dando problema para abrir o 
#csv
scielo <- read.csv("scielo_busca.csv",
                   sep = ",",
                   quote = "\"",
                   fill = TRUE,
                   stringsAsFactors = FALSE)
#Remover duplicatas apos ler a planilha, que tem o wos que são os artigos que ja 
#estao sendo triados e o scielo que sao os novos, no grupo
naive_results <- 
  litsearchr::remove_duplicates(scielo, field = "title", method = "string_osa")
nrow(naive_results)
naive_results
colnames(naive_results)
write.xlsx (naive_results, "Buscatotal.xlsx")
