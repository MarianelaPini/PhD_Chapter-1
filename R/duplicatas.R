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
install.packages("Rtools")
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
#Remover duplicatas para busca com todos os termos, 28-07, em WoS e Scopus
library(remotes)
search_directory<-("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07")
#importamos os dados
# Nesses dados coloquei as duas buscas, em Wos e em Scopus
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_results <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_results)
naive_results
colnames(naive_results)
write.csv(naive_results, "naiveeresults_secforest28-07.csv")
write.xlsx (naive_results, "naiveresults_secforest28-07.xlsx")
####
####

##script mari
#ja triados, colocar na nova planilha
#A planilha tem os artigos novos, e os artigos já triados, todos juntos

local2 <- "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search"
data<- read_excel("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/hola.xlsx") 
#criando a lista das especies q quero comparar
#nessa lista vão ser os trabalhos que já foram triados
lista1 <- data[data$grupo=="triado",]
#nessa lista vão ser os trabalhos novos
lista2 <- data[data$grupo=="novo",]
#Crio objetos para ter só os titulos de cada grupo
a <- lista1$title
b <- lista2$title

#comparando pontos entre grupos, triados e novos
pontos_dif <- setdiff(a,b)
View(pontos_dif)
pontos_dif
#intersect(a,b)

write.table(pontos_dif,"C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/complist.csv", sep=",",dec=".")

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
<<<<<<< HEAD
####Atualizando a planilha, os triados e os 4194 do WoS e Scopus
##Criei uma planilha .csv, com uma coluna de triado e novo
#Agora vou remover os duplicados
library(remotes)
library(litsearchr)
library(dplyr)
# Specify the directory
search_directory <-"C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1"
#Lendo a planilha
tudo <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/tudo_corrigido.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = TRUE,
                 na.strings = "NA",
                 strip.white = TRUE)
head(tudo)
nrow(tudo) 
#Remover as duplicatas
tudo.dedupli <-litsearchr::remove_duplicates(tudo, field = "title", method = "string_osa")
nrow(tudo.dedupli)
#Salvar o arquivo 
write.csv(tudo.dedupli, "tudo.dedupli.csv")
####Triando sistemáticamente os de solo
###Lendo a planilha que tem todos os de solo e o tree
tree <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/soil_tudo.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = TRUE,
                 na.strings = "NA",
                 strip.white = TRUE)
head(tree)
nrow(tree) 
#Remover as duplicatas
tudo.deduplitree <-litsearchr::remove_duplicates(tree, field = "title", method = "string_osa")
nrow(tudo.deduplitree)
#Salvar o arquivo 
write.csv(tudo.deduplitree,"tre.dedupli.csv")
#Agora vou colocar os de plant, com os que não tem tree, os que sobram, vai ser soil sem tree sem plant
plant <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/tree_dedupli_plant.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = TRUE,
                 na.strings = "NA",
                 strip.white = TRUE)
#Remover as duplicatas
tudo.dedupliplant <-litsearchr::remove_duplicates(plant, field = "title", method = "string_osa")
nrow(tudo.dedupliplant)
#Salvar o arquivo 
write.csv(tudo.dedupliplant,"tudo.dedupliplant.csv")
#Agora vou colocar os de vegetation, com os que não tem tree,não tem plant, os que sobram, vai ser soil sem tree sem plant sem vegetation
veg <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/tudo_dedupliveg.csv",
                  header = TRUE,
                  sep = ",",
                  stringsAsFactors = TRUE,
                  na.strings = "NA",
                  strip.white = TRUE)
#Remover as duplicatas
tudo.dedupliveg <-litsearchr::remove_duplicates(veg, field = "title", method = "string_osa")
nrow(tudo.dedupliveg)
#Salvar o arquivo 
write.csv(tudo.dedupliveg,"tudo.dedupliveg.csv")
#Agora vou colocar os de só solo com o total, para colocar o resto como veg/tree/plant
justsoil <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/just_soil.csv",
                header = TRUE,
                sep = ",",
                stringsAsFactors = TRUE,
                na.strings = "NA",
                strip.white = TRUE)
#Remover as duplicatas
tudo.dedupli <-litsearchr::remove_duplicates(justsoil, field = "title", method = "string_osa")
nrow(tudo.dedupli)
#Salvar o arquivo 
write.csv(tudo.dedupli,"tudo.dedupli.csv")
#Agora tenho a planilha com os triados e vou colocar os de solo, pra saber quais são de solo, quais triados
#não deu certo, então um por um, vendo quais triados são "soil" e apago esses
triadossoil <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/soil_triagem.csv",
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = TRUE,
                     na.strings = "NA",
                     strip.white = TRUE)
#Remover as duplicatas
tudo.dedupli <-litsearchr::remove_duplicates(triadossoil, field = "title", method = "string_osa")
nrow(tudo.dedupli)
#Salvar o arquivo 
write.csv(tudo.dedupli,"triados_soil.csv")
#não deu certo,Agora um por um, vendo quais triados são "plant/tree/veg" e apago esses
triadosplant <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/triados_plant.csv",
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = TRUE,
                        na.strings = "NA",
                        strip.white = TRUE)
#Remover as duplicatas
triadoplant.dedupli <-litsearchr::remove_duplicates(triadosplant, field = "title", method = "string_osa")
nrow(triadoplant.dedupli)
#Salvar o arquivo 
write.csv(triadoplant.dedupli,"triados_plant.csv")
#Finalmente junto os de soil e os novos,agora pensando os de planta vou ler, então não precisa
soil_novos <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/novo_soil.csv",
                         header = TRUE,
                         sep = ",",
                         stringsAsFactors = TRUE,
                         na.strings = "NA",
                         strip.white = TRUE)
#Remover as duplicatas
soil_novosdedupli <-litsearchr::remove_duplicates(soil_novos, field = "title", method = "string_osa")
nrow(soil_novosdedupli)
#Salvar o arquivo 
write.csv(soil_novosdedupli,"soil_novos.csv")
#Agora mesmo procedimento com os de Scopus, soil com plant, soil 1544, plant 1275
plantscopus <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/Tudo_soil_plant_scopus.csv",
                  header = TRUE,
                  sep = ",",
                  stringsAsFactors = TRUE,
                  na.strings = "NA",
                  strip.white = TRUE)
plantscopus
#Remover as duplicatas
tudo.dedupliplant <-litsearchr::remove_duplicates(plantscopus, field = "Title", method = "string_osa")
nrow(tudo.dedupliplant)
#Salvar o arquivo 
write.csv(tudo.dedupliplant,"dedupliplant_scopus.csv")
#Agora mesmo procedimento com os de Scopus, soil com veg, sem plant
vegscopus <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/veg_scopus.csv",
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = TRUE,
                        na.strings = "NA",
                        strip.white = TRUE)
#Remover as duplicatas
tudo.dedupliveg <-litsearchr::remove_duplicates(vegscopus, field = "Title", method = "string_osa")
nrow(tudo.dedupliveg)
#Salvar o arquivo 
write.csv(tudo.dedupliveg,"dedupliveg_scopus.csv")
#Agora mesmo procedimento com os de Scopus, soil com tree, sem plant, sem veg
treescopus <- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/tree_scopus.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE,
                      na.strings = "NA",
                      strip.white = TRUE)
#Remover as duplicatas
tudo.deduplitree <-litsearchr::remove_duplicates(treescopus, field = "Title", method = "string_osa")
nrow(tudo.deduplitree)
#Salvar o arquivo 
write.csv(tudo.deduplitree,"deduplitree_scopus.csv")
#Agora vou comparar só solo na WoS e no Scopus
compsw<- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/comp_wos_scopus.csv",
                       header = TRUE,
                       sep = ",",
                       stringsAsFactors = TRUE,
                       na.strings = "NA",
                       strip.white = TRUE)
#Remover as duplicatas
tudo.deduplicompsw <-litsearchr::remove_duplicates(compsw, field = "title", method = "string_osa")
nrow(tudo.deduplicompsw)
#Salvar o arquivo 
write.csv(tudo.deduplicompsw,"comp_wos_scopus.csv")
#Agora adicionar os de scopus (FORAM 36) na planilha com os novos sem triagem
novosscopus<- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/novos_scopus.csv",
                  header = TRUE,
                  sep = ",",
                  stringsAsFactors = TRUE,
                  na.strings = "NA",
                  strip.white = TRUE)
#Remover as duplicatas
tudo.deduplisscopus<-litsearchr::remove_duplicates(novosscopus, field = "title", method = "string_osa")
nrow(tudo.deduplisoil)
#Salvar o arquivo 
write.csv(tudo.deduplisoil,"novos_soil_scopus.csv")
#na verdade junto com os de WoS, adicionar eles, de scopus, e os novos
novossoil<- read.csv("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search/28-07-soil/novos_soil_wosscopus.csv",
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = TRUE,
                     na.strings = "NA",
                     strip.white = TRUE)
#Remover as duplicatas
tudo.deduplisoil<-litsearchr::remove_duplicates(novossoil, field = "title", method = "string_osa")
nrow(tudo.deduplisoil)
#Salvar o arquivo 
write.csv(tudo.deduplisoil,"novos_soil_wos_scopus.csv")
#Sorteio para checar dos 434 de just soil, os titulos, 86, ver se é só solo mesmo
numeros_sorteados <- sample(1:2790, 2790, replace = FALSE)
print(numeros_sorteados)
sort(numeros_sorteados)
saving<-data.frame(numeros_sorteados)
write.csv(saving, "numerossorteados.csv")
=======

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
>>>>>>> 531b6f93e775ad692d89b371cd20b3d020fd68ff
