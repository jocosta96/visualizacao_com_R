# Setting directory
setwd("C:/Users/JOC/Documents/curso_r/dataviz_mult_var-dados")
  # Verifying 
getwd()

# Installing libs
#install.packages("data.table")
#install.packages("reshape2")

# Activating libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)

# Loading contents
enem_2010 <- fread("enem_2010.csv", encoding = "UTF-8")
enem_2011 <- fread("enem_2011.csv", encoding = "UTF-8")
enem_2012 <- fread("enem_2012.csv", encoding = "UTF-8")
enem_2013 <- fread("enem_2013.csv", encoding = "UTF-8")
enem_2014 <- fread("enem_2014.csv", encoding = "UTF-8")
enem_2015 <- fread("enem_2015.csv", encoding = "UTF-8")
enem_2016 <- fread("enem_2016.csv", encoding = "UTF-8")
enem_2017 <- fread("enem_2017.csv", encoding = "UTF-8")

#Merging data
merge_enem <- rbind(enem_2017,
                    enem_2016,
                    enem_2015,
                    enem_2014,
                    enem_2013,
                    enem_2012,
                    enem_2011,
                    enem_2010,
                    fill = TRUE
                    )

#Eliminating objects
rm(enem_2017,
      enem_2016,
      enem_2015,
      enem_2014,
      enem_2013,
      enem_2012,
      enem_2011,
      enem_2010
)

#View data
str(merge_enem)

#Filtering columns
  #Creating a list with Column Names 
colunas <- c("NUMERO_INSCRICAO",
             "ANO","CO_MUNICIPIO_RESIDENCIA","MUNICIPIO_RESIDENCIA",
             "UF_RESIDENCIA", "UF_RESIDENCIA", "UF_ESCOLA", "IDADE",
             "SEXO", "SITUACAO_CONCLUSAO","BRAILLE","MUNICIPIO_PROVA",
             "UF_PROVA","PRESENCA_CIENCIAS_NATUREZA","PRESENCA_CIENCIAS_HUMANAS",
             "PRESENCA_LINGUAGENS_CODIGOS","PRESENCA_MATEMATICA",
             "NOTA_CIENCIAS_NATUREZA","NOTA_CIENCIAS_HUMANAS",
             "NOTA_LINGUAGENS_CODIGOS","NOTA_MATEMATICA","TIPO_LINGUA",
             "STATUS_REDACAO","NOTA_REDACAO")
  #Filtering based on column list
enem <- merge_enem %>% select_(.dots = colunas)


# Deleting merge_enem
rm(merge_enem)

#View data
str(enem)

#Normalizing sex
  #Count Feature
table(enem$SEXO)
  #Substituting 1 to FEMININO
enem$SEXO <- gsub("1", "FEMININO", enem$SEXO)
  #Substituting F to FEMININO
enem$SEXO <- gsub("^F$", "FEMININO", enem$SEXO)
  #Substituting 0 to MASCULINO
enem$SEXO <- gsub("0", "MASCULINO", enem$SEXO)  
  #Substituting M to MASCULINO
enem$SEXO <- gsub("^M$", "MASCULINO", enem$SEXO)  

#Normalizing language
  #VIEW options
table(enem$TIPO_LINGUA)
  #Substituting 0 to INGLES and 1 to ESPANHOL
enem$TIPO_LINGUA <- gsub("0", "INGLÊS", enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub("1", "ESPANHOL", enem$TIPO_LINGUA)

#Normalizing uf_prova
  #View "Must be 27 UF´s"
length(table(enem$UF_PROVA))

#Normalizing situation
enem$SITUACAO_CONCLUSAO <-
  gsub("1", "CONCLUIDO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <-
  gsub("2", "CONCLUIRÁ NO ANO", enem$SITUACAO_CONCLUSAO)  
enem$SITUACAO_CONCLUSAO <-
  gsub("3", "CONCLUIDÁ APOS(ANO)", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <-
  gsub("4", "NÃO CONC. NÃO CURSANDO", enem$SITUACAO_CONCLUSAO)


#Converting test grade to number
  #Converting ratings
enem$NOTA_CIENCIAS_HUMANAS <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_NATUREZA <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_MATEMATICA <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_REDACAO <- as.numeric(enem$NOTA_REDACAO)


#Generating language and sex graphic
  #Filtering data errors
tp_lingua_sexo <- enem %>%
  filter(TIPO_LINGUA != '.') %>%
  select_(.dots = c('SEXO', 'TIPO_LINGUA'))
  #Plotting the graphic
plot_idioma_sexo <- ggplot(data = tp_lingua_sexo) +
  geom_bar(aes(x = SEXO, fill = TIPO_LINGUA),
           stat = 'count', position = position_dodge()) +
  ggtitle("Notas Por Sexo e Por Lingua") + xlab('Sexo') + ylab("Quantidade") +
  theme_linedraw() + theme(plot.title = element_text(hjust = 0.5))


#Generating schooling situation
  #view
  ggplot(data = enem) +
    geom_bar(aes(x = UF_PROVA), stat = 'count')
  #removing scientific notation
  options(scipen = 9999)
  #filtering
  uf_prova <- enem %>%
    filter(UF_PROVA != "") %>%
    select_(.dots = c("UF_PROVA", "SITUACAO_CONCLUSAO"))
  #printing filtered data
  plot_uf_conclusao <- ggplot(data = uf_prova) + 
    geom_bar(aes(x = UF_PROVA, fill = SITUACAO_CONCLUSAO),
             position = position_dodge()) +
    facet_grid(SITUACAO_CONCLUSAO~.)
  #Addicting title and axis labels
  plot_uf_conclusao <- plot_uf_conclusao +
    ggtitle("Situação Escolar por estado") + 
    ylab("Quantidade") + xlab("Estado")
  #Changing theme and centralizing title
  plot_uf_conclusao <- plot_uf_conclusao + 
    theme_linedraw() + labs(fill = "Situação") +
    theme(plot.title = element_text(hjust = 0.5))
  
#
  #Verifying age data
  summary(enem$IDADE)
  idade_uf <- enem %>%
    filter(!is.na(IDADE))
  #View
  summary(idade_uf)
  media_idade_sexo_uf <- idade_uf %>%
                          group_by(UF_PROVA, SEXO) %>%
                          summarise(media = mean(IDADE))
  View(media_idade_sexo_uf)
  media_idade_sexo_uf <- media_idade_sexo_uf %>%
                        filter(UF_PROVA != "")
  # 
  ggplot(data = media_idade_sexo_uf) + 
    geom_bar(aes(x = UF_PROVA, y = media, fill = SEXO),
             position = position_dodge(), stat = 'identity') +
    coord_flip()
  
  #Making a pyramid graphic
  plot_piram_idade <- ggplot(data = media_idade_sexo_uf,
         aes(x = reorder(UF_PROVA, -media),
             y = ifelse(SEXO == "MASCULINO", -media, media),
             fill = SEXO)) +
    geom_bar(stat = 'identity')+
    coord_flip()
  #removing negative   
  plot_piram_idade + scale_y_continuous(labels = abs)
  #formating graphic title, axis labels, theme and title aligment
  plot_piram_idade + 
    ggtitle("Média de Idade por UF e Sexo") + 
    ylab("Média de Idade") +
    xlab("Estado") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  #changing graphic color
  plot_piram_idade <- plot_piram_idade +
    scale_fill_manual(values = c('hotpink', 'dodgerblue3'))
  #Inserting data labels
  plot_piram_idade <- plot_piram_idade +
    geom_text(aes(label = round(media, digits = 2),
                                   hjust = 0.5),
                               size = 4.5,
                               colour = "black",
                               fontface = "bold"
                               )
  
  #Making points graphic to human sciences
    #Create new sub conjunct
  notas_ciencias_humanas <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS)&
                    !is.na(IDADE) &
                    IDADE > 17)
    #calculating age average to human sciences
  notas_ciencias_humanas_idade <-
  notas_ciencias_humanas %>%
    group_by(IDADE) %>%
    summarise(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))
    #printing points graphic to human sciences
  ggplot(data = notas_ciencias_humanas_idade)+
    geom_point(aes(x = IDADE, y = media_nota_ciencias_humanas))

  #Making points graphic to math
  notas_mt <- enem %>%
    filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE >17)
   #Calculating punctuation average per age
  notas_matematica_idade <- notas_mt %>%
      group_by(IDADE) %>%
      summarise(media_nota_matematica = mean(NOTA_MATEMATICA))
    #Plotting math punctuation graphic 
  ggplot(data = notas_matematica_idade) +
    geom_point(aes(x = IDADE, y = media_nota_matematica))
  
  
#Merging math and human sciences 
  notas_ciencias_humanas_matematica_idade <-
    merge(notas_ciencias_humanas_idade,
          notas_matematica_idade, all = T)
  
# Analyzing data
  View(notas_ciencias_humanas_matematica_idade)

#Pivoting axis
  notas_ciencias_humanas_matematica_idade <-
    melt(notas_ciencias_humanas_matematica_idade,
         id.vars = "IDADE")

#Mixed Graphics
  plot_scatter_mt_ch <- ggplot(data = notas_ciencias_humanas_matematica_idade) + 
    geom_point(aes(x = IDADE,y = value, color = variable))


#Formatting graphics
  #Addicting title and axis labels
  plot_scatter_mt_ch <- 
    plot_scatter_mt_ch +
    ggtitle("Média Notas por Idade e Matéria")+
    xlab("Idade")+
    ylab("Nota(média)")
  
  #Changind theme
  plot_scatter_mt_ch <- plot_scatter_mt_ch + theme_bw()
  
  #Changing subtitle colors and labels
  plot_scatter_mt_ch <-
    plot_scatter_mt_ch +
    scale_color_manual(name = "Matéria", values = c('blue', 'red'),
                       labels = c('Ciências \nHumanas', 'Matemática'))
  
  #Create custom data
media_anos <- 
  enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS)&
                  !is.na(NOTA_CIENCIAS_NATUREZA)&
                  !is.na(NOTA_LINGUAGENS_CODIGOS)&
                  !is.na(NOTA_MATEMATICA)&
                  !is.na(NOTA_REDACAO)) %>%
                  group_by(ANO) %>%
                             summarise(media_ch = mean(NOTA_CIENCIAS_HUMANAS),
                                       media_cn = mean(NOTA_CIENCIAS_NATUREZA),
                                       media_lc = mean(NOTA_LINGUAGENS_CODIGOS),
                                       media_mt = mean(NOTA_MATEMATICA),
                                       media_rd = mean(NOTA_REDACAO))

#Pivoting axis
media_anos_2 <- melt(data = media_anos, id.vars = "ANO")
ggplot(data = media_anos_2)
#Plotting lines graphic
plot_line_notas <- ggplot(data = media_anos_2)+
  geom_line(aes(x = ANO, y = value, color = variable))
#Improving lines graphic
plot_line_notas <- plot_line_notas + ggtitle("Média de Notas por Matéria")+
  ylab("Média") +
  geom_point(aes(ANO,value,color = variable), size = 3)
#Labels on points
plot_line_notas <- plot_line_notas + geom_text(aes(x = ANO, y = value,
                                color = variable,
                                label = round(value, digits = 2),
                                hjust = -0.15,
                                vjust = 0.2))
#Subtitle Labels
plot_line_notas <- 
  plot_line_notas +
  scale_color_discrete(name = "Matérias",
                       labels = c("Ciênc. Natureza", "Ciênc. Hum",
                                  "Matemática", "Letras/Cod", "Redação"))

#Changing theme
plot_line_notas <- plot_line_notas + theme_bw()

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  










