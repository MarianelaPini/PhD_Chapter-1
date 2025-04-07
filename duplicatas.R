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
search_directory<-("C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search")
#importamos os dados
naiveimport<-litsearchr::import_results(directory = "C:/Users/maria/OneDrive - Questindustries/Documentos/Brasil/Doutorado/R/PhD_Chapter-1/search", verbose = TRUE)
colnames(naiveimport)
#Remover duplicatas
naive_results <- 
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
nrow(naive_results)
naive_results
colnames(naive_results)
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
keywords <- extract_terms(keywords=naive_results[, "keywords"], method="tagged", min_n=1,min_freq=10)
keywords
#extract by title
title_keywords<-extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=10, min_n=1)
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
#ranking
install.packages("igraph")
library(igraph)
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths
term_strengths
write.table(term_strengths, "terms_strengths.txt", sep="\t", row.names=FALSE, quote=FALSE)
#check terms united
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig
