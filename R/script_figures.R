##Figures for qualification
####Open data####
library(readr)
Search_data_processed <- read_csv("Data/Processed/Search_data_processed.csv")
data<-read_csv("Data/Processed/Search_data_processed.csv")
data
str(data)
sum(data$obj_plant,na.rm= TRUE)
sum(!is.na(data$obj_plant))
table(data$obj_plant)
168+379+427+841
sum(totals)
#Somando o numero de artigos por categoria
totals<-c((sum(data$plant_seeds,na.rm= TRUE)),(sum(data$plant_juvenile,na.rm= TRUE)),
          (sum(data$plant_adults,na.rm= TRUE)),(sum(data$plant_not_specified,na.rm= TRUE)),
         (sum(data$obj_animal,na.rm= TRUE)),(sum(data$obj_interactions,na.rm= TRUE)),
         (sum(data$obj_litter,na.rm= TRUE)),(sum(data$obj_soil,na.rm= TRUE)),
         (sum(data$obj_climate,na.rm= TRUE)),(sum(data$obj_others,na.rm= TRUE)))
totals
categories<-c("seeds","juvenile","adults","not specified","animal","interactions",
              "litter","soil","climate","others")
categories
#criando o data frame para o total das categorias, primeiro grafico
data_total<-data.frame(categories,totals)
data_total
# Load ggplot2
library(ggplot2)
library(hrbrthemes) # for style
library(tidyr)
library(dplyr)
#
# Criar grupo principal
data_total <- data_total %>%
  mutate(bar_group = case_when(
    categories %in% c("seeds","juvenile",
                      "adults","not specified") ~ "Plant",
    TRUE ~ categories
  ))
ordem <- data_total %>%
  group_by(bar_group) %>%
  summarise(total_barra = sum(totals)) %>%
  arrange(desc(total_barra)) %>%
  pull(bar_group)

data_total$bar_group <- factor(
  data_total$bar_group,
  levels = ordem
)
data_total
#
##data frame para a legenda de plantas
total<-c(10,600,1100,1600)
legend<-c("se","ns","re","ad")
plant<-c("Plant","Plant","Plant","Plant")
legendplant<-data.frame(total,legend,plant)
legendplant
#### Gráfico   ####
grafico_total<-ggplot(data_total,
       aes(x = bar_group,
           y = totals,
           fill = categories)) +
  
  geom_col(width = 0.7) +
  geom_text(data = legendplant,
            aes(x = plant,
                y = total,
                label = legend),
            vjust = -0.5,
            size = 5,
            inherit.aes = FALSE) +
  scale_fill_manual(
    
    values = c(
      "seeds" = "#d9f0d3",
      "juvenile" = "#a6dba0",
      "adults" = "#5aae61",
      "not specified" = "#1b7837",
      
      "animal" = "#bc80bd",
      "interactions" = "darkorange1",
      "litter" = "sienna",
      "soil" = "turquoise4",
      "climate" = "dodgerblue",
      "others" = "#8c8c8c"
    ),
    breaks = c("seeds",
               "juvenile",
               "adults",
               "not specified")
  ) +
  scale_x_discrete(labels = c(
    "Plant" = "Plant (1815)",
    "animal" = "Animal (474)",
    "soil" = "Soil (460)",
    "litter" = "Litter (146)",
    "interactions" = "Interactions (133)",
    "climate" = "Climate (72)",
    "others" = "Others (145)")) +
  labs(title="Object of study",
    x = "",
    y = "Number of studies that cite each category",
    fill = ""
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(size = 16,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 17),
    legend.title = element_text(size = 17),
  
    
    # remove grade
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # adiciona linhas dos eixos
    axis.line = element_line(color = "black"),
    # marquinhas dos eixos
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,   # centraliza
      size = 18,
      face = "bold")
  )
grafico_total
ggsave("plot_total.png",
       grafico_total,
       width = 12,
       height = 10,
       dpi = 300)

#### Vamos agora com o grafico por anos ####
only_plant<-data[data$obj_plant == "1",]
sum(data$plant_seeds,na.rm= TRUE)
tempos_total<-aggregate(only_plant[,9:12],
          by = list(period_of_year = only_plant$period_of_year),
          FUN = sum)
tempos_total
##
#Vamos ver se da certo
library(tidyr)
library(ggplot2)
library(dplyr)
library(grid)
tempos_total
# dados
# transformar para formato longo
dados_grafico <- pivot_longer(
  tempos_total,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)

# totais para colocar no topo 
totais <- dados_grafico %>%
group_by(period_of_year) %>%
summarise(total = sum(total))
totais
# gráfico
ggplot(dados_grafico,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  geom_col(width = 0.7) +
  geom_text(data = totais,
            aes(x = period_of_year,
                y = total,
                label = total),
            vjust = -0.5,
            size = 5,
            inherit.aes = FALSE)+
  scale_fill_manual(values = c(
    "communities_populations" = "#66a61e",
    "obj_ecosystem_int" = "darkorange1",
    "obj_landscape" = "turquoise4",
    "obj_socioecological" = "#d73027"), 
  labels = c(
    "Communities & populations",
    "Ecosystem",
    "Landscape",
    "Socioecological")) +
  
  labs(
    x = "Period",
    y = "Number of studies that cite each category",
    fill = "Level of organization"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
ggsave("total_tempon.png",
       width = 8,
       height = 6,
       dpi = 300)
#### Deu certo, agora por bioma ####
#Preciso do patchowrk, pacote para criar painel em ggplot
###annotate("text",
#x = -Inf,
#y = Inf,
#label = "b)",
#hjust = -0.5,
#vjust = 1.5,
#size = 5,
#fontface = "bold")##
install.packages("patchwork")
library(patchwork)

only_amazon<-only_plant[only_plant$amazon == "1",]
#somando amazon
tempos_amazon<-aggregate(only_amazon[,9:12],
                        by = list(period_of_year = only_amazon$period_of_year),
                        FUN = sum)

#somando o total de artigos para colocar no titulo
totalamazon<-sum(colSums(tempos_amazon[, c("communities_populations", "obj_ecosystem_int", 
                              "obj_landscape", "obj_socioecological")]))

# transformar para formato longo
dados_gamazon <- pivot_longer(
  tempos_amazon,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
# totais para colocar no topo 
totais <- dados_gamazon %>%
  group_by(period_of_year) %>%
  summarise(total = sum(total))
# gráfico
p2<-ggplot(dados_gamazon,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(
    "communities_populations" = "#66a61e",
    "obj_ecosystem_int" = "darkorange1",
    "obj_landscape" = "turquoise4",
    "obj_socioecological" = "#d73027"),
    labels = c(
    "Communities & populations",
    "Ecosystem",
    "Landscape",
    "Socioecological")) +
  
  labs(title="b) Amazon (776)",
    x = "",
    y = "",
    fill = "Level of organization"
  ) +
  
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 350))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    plot.title = element_text(
      hjust = 0.5,   # centraliza
      size = 12,
      face = "bold")
  )
p2
#Agora mata atlantica
only_af<-only_plant[only_plant$atlantic_forest == "1",]
#somando mata atlantica
tempos_af<-aggregate(only_af[,9:12],
                          by = list(period_of_year = only_af$period_of_year),
                          FUN = sum)
tempos_af
totalaf<-sum(colSums(tempos_af[, c("communities_populations", "obj_ecosystem_int", 
                                           "obj_landscape", "obj_socioecological")]))
totalaf
# transformar para formato longo
dados_gaforest <- pivot_longer(
  tempos_af,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
dados_gaforest
# totais para colocar no topo 
totais <- dados_gaforest %>%
  group_by(period_of_year) %>%
  summarise(total = sum(total))
# gráfico
p1<-ggplot(dados_gaforest,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  geom_col(width = 0.7) +
  scale_y_continuous(limits = c(0, 360))+

  scale_fill_manual(values = c(
    "communities_populations" = "#66a61e",
    "obj_ecosystem_int" = "darkorange1",
    "obj_landscape" = "turquoise4",
    "obj_socioecological" = "#d73027"
  ), labels = c(
    "Communities & populations",
    "Ecosystem",
    "Landscape",
    "Socioecological")) +
  
  labs(title = "a) Atlantic Forest (1029)",
    x = "",
    y = "",
    fill = ""
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    plot.title = element_text(
      hjust = 0.5,   # centraliza
      size = 12,
      face = "bold"),
    legend.position = "none")
p1
#testando para cerrado
only_cerrado<-only_plant[only_plant$cerrado == "1",]
#somando cerrado
tempos_cerrado<-aggregate(only_cerrado[,9:12],
                          by = list(period_of_year = only_cerrado$period_of_year),
                          FUN = sum)
tempos_cerrado
totalcerrado<-sum(colSums(tempos_cerrado[, c("communities_populations", "obj_ecosystem_int", 
                                           "obj_landscape", "obj_socioecological")]))
totalcerrado
#Adicionado o 1971-1990 que falta 
# todos os períodos desejados
todos_periodos <- data.frame(
  period_of_year = c(
    "1971-1990",
    "1991-1995",
    "1996-2000",
    "2001-2005",
    "2006-2010",
    "2011-2015",
    "2016-2020",
    "2021-2025"
  )
)

# adicionar períodos ausentes
tempos_cerrado <- merge(
  todos_periodos,
  tempos_cerrado,
  by = "period_of_year",
  all.x = TRUE
)

# substituir NAs por 0
tempos_cerrado[is.na(tempos_cerrado)] <- 0
tempos_cerrado
# transformar para formato longo
dados_gcerrado <- pivot_longer(
  tempos_cerrado,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
# totais para colocar no topo 
totais <- dados_gcerrado %>%
  group_by(period_of_year) %>%
  summarise(total = sum(total))
totais
# gráfico
p3<-ggplot(dados_gcerrado,
           aes(x = period_of_year,
               y = total,
               fill = category)) +
  geom_col(width = 0.7) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = c(
    "communities_populations" = "#66a61e",
    "obj_ecosystem_int" = "darkorange1",
    "obj_landscape" = "turquoise4",
    "obj_socioecological" = "#d73027"
  )) +
  
  labs(title = "c) Cerrado (165)",
    x = "",
    y = "",
    fill = ""
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    plot.title = element_text(
      hjust = 0.5,   # centraliza
      size = 12,
      face = "bold"),
    legend.position = "none"
    )
p3
#Agora caatinga
only_caatinga<-only_plant[only_plant$caatinga == "1",]
#somando caatinga
tempos_caatinga<-aggregate(only_caatinga[,9:12],
                     by = list(period_of_year = only_caatinga$period_of_year),
                     FUN = sum)
tempos_caatinga
totalcaatinga<-sum(colSums(tempos_caatinga[, c("communities_populations", "obj_ecosystem_int", 
                                           "obj_landscape", "obj_socioecological")]))
totalcaatinga
#Adicionar tempos que faltam
tempos_caatinga <- merge(
  todos_periodos,
  tempos_caatinga,
  by = "period_of_year",
  all.x = TRUE
)

# substituir NAs por 0
tempos_caatinga[is.na(tempos_caatinga)] <- 0
tempos_caatinga
# transformar para formato longo
dados_gcaatinga <- pivot_longer(
  tempos_caatinga,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
dados_gcaatinga
# totais para colocar no topo 
totais <- dados_gcaatinga %>%
  group_by(period_of_year) %>%
  summarise(total = sum(total))

# gráfico
p4<-ggplot(dados_gcaatinga,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(
    "communities_populations" = "#66a61e",
    "obj_ecosystem_int" = "darkorange1",
    "obj_landscape" = "turquoise4",
    "obj_socioecological" = "#d73027")) +
  
  labs(title="d) Caatinga (154)",
    x = "",
    y = "",
    fill = ""
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    plot.title = element_text(
      hjust = 0.5,   # centraliza
      size = 12,
      face = "bold"),
    legend.position="none"
  )
p4
#colocando os graficos no mesmo painel
library(grid)
library(ggplot2)
#axis.text.x = element_text(
#angle = 45,
#hjust = 1)#


# painel 2x2
painel <- (p1 + p2 + p3 + p4) +
  plot_layout(ncol = 2)
painel
# título lateral
titulo_lateral <- wrap_elements(
  textGrob("Number of studies that cite each category",
           rot = 90,
           gp = gpar(fontsize = 16,
                     fontface = "bold"))
)
#titulo inferior
titulo_inferior <- wrap_elements(
  textGrob("Periods of publication",
           gp = gpar(fontsize = 16,
                     fontface = "bold"))
)


# combinar tudo
grafico_final<-(
  titulo_lateral + painel +
    plot_layout(widths = c(0.05, 1))
) /
  titulo_inferior +
  
  plot_layout(heights = c(1, 0.08))
grafico_final
ggsave("painel2.png",
       grafico_final,
       width = 10,
       height = 10,
       dpi = 300)
##graficos testes
# painel 1x3
painel <- (p1 + p3 + p4) +
  plot_layout(ncol = 1)
painel
#so amazonia e mata atlantica
painel <- (p1 + p2) +
  plot_layout(ncol = 2)
painel
#testando pampa
only_pampa<-only_plant[only_plant$pampa == "1",]
#somando pampa
tempos_pampa<-aggregate(only_pampa[,9:12],
                           by = list(period_of_year = only_pampa$period_of_year),
                           FUN = sum)
tempos_pampa
# transformar para formato longo
dados_gpampa <- pivot_longer(
  tempos_pampa,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
# gráfico
ggplot(dados_gpampa,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  
  geom_col(width = 0.7) +
  
  scale_fill_manual(values = c(
    "communities_populations" = "#1b9e77",
    "obj_ecosystem_int" = "#66a61e",
    "obj_landscape" = "#7570b3",
    "obj_socioecological" = "#e7298a"
  )) +
  
  labs(
    x = "Period",
    y = "Number of papers",
    fill = "Level of organization"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
#### Graficos suplementares #####
##Categorias exclusivas
dataexc<-data[data$exclusives == "1",]
dataexc
numbers<-totals<-c((sum(dataexc$obj_plant,na.rm= TRUE)),(sum(dataexc$obj_animal,na.rm= TRUE)),
                   (sum(dataexc$obj_soil ,na.rm= TRUE)),
                   (sum(dataexc$obj_litter,na.rm= TRUE)),(sum(dataexc$obj_climate,na.rm= TRUE)),
                   (sum(data$obj_interactions,na.rm= TRUE)))
numbers
categories<-c("Plant","Animal","Soil","Litter",
              "Climate","Interactions")
dados_gexc<-data.frame(categories,numbers)
dados_gexc
#ordenando de maior a menor
dados_gexc$categories <- factor(
  dados_gexc$categories,
  levels = dados_gexc$categories[order(dados_gexc$numbers, decreasing = TRUE)]
)
##Grafico exclusivos ##
grafico_exclusivos<-ggplot(dados_gexc,
                      aes(x = categories,
                          y = numbers,
                          fill = categories)) +
  
  geom_col(width = 0.7) +
  
  scale_fill_manual(
        values = c(
      "Plant" = "#5aae61",
      "Animal" = "#bc80bd",
      "Interactions" = "darkorange1",
      "Litter" = "sienna",
      "Soil" = "turquoise4",
      "Climate" = "dodgerblue",
      "Landscape" = "turquoise4",
      "Socioecological"="#d73027"
    )) +
  scale_x_discrete(labels = c(
    "Plant" = "Plant (675)",
    "Animal" = "Animal (285)",
    "Soil" = "Soil (148)",
    "Interactions" = "Interactions (133)",
    "Litter"= "Litter (13)",
    "Climate" = "Climate (11)"
    )) +
  labs(
    x = "",
    y = "Number of studies that cite each category"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(size = 16,
                               angle = 45,
                               hjust = 1
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    
    
    # remove grade
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # adiciona linhas dos eixos
    axis.line = element_line(color = "black"),
    # marquinhas dos eixos
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none"
  )
grafico_exclusivos
ggsave("graficoexclusivos.png",
       grafico_exclusivos,
       width = 12,
       height = 10,
       dpi = 300)
## Vamos com nivel mais alto ##
only_plant$high_level<-as.factor(only_plant$high_level)
tempos_highlevel<-aggregate(only_plant[,20:23],
                        by = list(period_of_year = only_plant$period_of_year),
                        FUN = sum)
tempos_highlevel
# gráfico
## # transformar para formato longo
dados_ghigh <- pivot_longer(
  tempos_highlevel,
  cols = -period_of_year,
  names_to = "category",
  values_to = "total"
)
##colocar numeros
totais2 <- dados_ghigh %>%
group_by(period_of_year) %>%
summarise(total = sum(total))

#grafico
ggplot(dados_ghigh,
       aes(x = period_of_year,
           y = total,
           fill = category)) +
  
  geom_col(width = 0.7) +
  geom_text(data = totais2,
  aes(x = period_of_year,
  y = total,
  label = total),
  vjust = -0.5,
  size = 5,
  inherit.aes = FALSE) +
  scale_fill_manual(values = c(
    "high_com" = "#66a61e",
    "high_ecos" = "darkorange1",
    "high_landscape" = "turquoise4",
    "high_socio" = "#d73027"), 
    labels = c(
      "Communities & populations",
      "Ecosystem",
      "Landscape",
      "Socioecological")) +
  
  labs(
    x = "Period",
    y = "Number of studies",
    fill = "Level of organization"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black"),
    
    axis.ticks = element_line(color = "black"),
    
    axis.ticks.length = unit(0.2, "cm"),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
ggsave("total_highleveln.png",
       width = 8,
       height = 6,
       dpi = 300)
# Grafico com cada categoria exclusiva mas não ao longo do tempo
