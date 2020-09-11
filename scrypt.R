library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(kableExtra)
library(forcats)
library(plotly)


recursos_CB_EC <- tibble(read_excel("./data/Recursos digitales del curso básico.xlsx", sheet = "EC", na = "0"))

recursos_CB_HN <- tibble(read_excel("./data/Recursos digitales del curso básico.xlsx", sheet = "HN", na = "0"))

recursos_CB_SV <- tibble(read_excel("./data/Recursos digitales del curso básico.xlsx", sheet = "SV", na = "0"))


recursos_cb <- bind_rows(recursos_CB_EC, recursos_CB_HN, recursos_CB_SV)


recursos_mas_usados = as.data.frame(table(select(recursos_cb, Tipo)))
recursos_mas_usados <- arrange(recursos_mas_usados, -Freq)
recursos_mas_usados

recursos_mas_usados %>%
  mutate(name = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x=name, y=Freq, fill=name)) +
  geom_bar(stat="identity",  alpha=.6, width=.6) +
  coord_flip() +
  xlab("Recursos usados") +
  ylab("Número de veces utilizados")+
  ggtitle("Tipo de recursos utilizados")+
  theme_bw()+theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(fig)


transformar_variables <- function(dataframe){
  dataframe$'Experiencia'[dataframe$'Descripción de la experiencia' %in% "Buena"] <- 3
  dataframe$'Proposito'[dataframe$'Cumplio con su proposito' %in% "SI"] <- 3
  dataframe$'Dificultad'[dataframe$'Nivel de dificultad' %in% "Alto"] <- 3
  dataframe$'Dificultad'[dataframe$'Nivel de dificultad' %in% "Bajo"] <- 1
  return(dataframe)
}

new_df <- transformar_variables(recursos_cb)[, c("Tipo","Número de comentarios positivos",
                                                 "Número de comentarios negativos",
                                                 "Número de audiencia captada",
                                                 "Experiencia", "Proposito", "Dificultad",
                                                 "Tema enseñado con el recurso", 'Link al recurso')]

colnames(new_df) <- c("Tipo", "Comentarios_Positivos", "Comentarios_Negativos",
                                   "Audiencia", "Experiencia", "Proposito", 
                                   "Dificultad", "Tema", "Link")


data_radial_chart <- new_df%>%
  group_by(Tipo)%>%
  summarise(Experiencia=sum(Experiencia),proposito = sum(Proposito), 
            dificultad = sum(Dificultad), Comentarios_Positivos = sum(Comentarios_Positivos), 
            Comentarios_Negativos = sum(Comentarios_Negativos), audiencia = sum(Audiencia))


data_radial_chart <- data_radial_chart %>% replace(is.na(.), 0)
colSums(data_radial_chart[,-1])
data_radial_chart

recursos_top = head(recursos_mas_usados, 3)['Var1']

recursos_top

data_radial_chart <- data_radial_chart[data_radial_chart$Tipo %in% unlist(recursos_top, use.names = FALSE),]
max_values = colSums(data_radial_chart[,-1])
recurso = data_radial_chart[data_radial_chart$Tipo %in% recursos_top[[1]][1],]

names_columns = colnames(recurso[,-1])

rule_of_3 <- function(df_recurso, max_values, list_columns){
  result <- list()
  for (val in list_columns){
    result <- append(result, (df_recurso[val][[1]] *3)/ max_values[val][[1]])
  }
  result <- unlist(result) %>% replace(is.na(.), 0)
  return(result)
}

colSums(recurso[,-1])
max_values

resultado = rule_of_3(colSums(recurso[,-1]), colSums(data_radial_chart[,-1]), names_columns)
resultado
fig <- plot_ly(
  type = 'scatterpolar',
  r = resultado,
  theta = names_columns,
  fill = 'toself'
) 

fig

