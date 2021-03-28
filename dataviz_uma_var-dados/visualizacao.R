# Setting work path
path <- 
  "C:/Users/JOC/Documents/curso_r/dataviz_uma_var-dados/googleplaystore.csv"


# Getting data source
dados <- read.csv(file = path)


# View table
View(dados)


# get data info 6th lines
head(dados)


# get data info of last 6 lines
tail(dados)


# get column types
str(dados)


# convert factor types
dados <- read.csv(file = path, stringsAsFactors = FALSE)


# install libraries
  # install.packages("dplyr")
  # install.packages("ggplot2")


# use libraries
library(dplyr)
library(ggplot2)


# construct histogram (r native)
hist(dados$Rating)

# create frequency table
table(dados$Rating)


# formatting histogram (x axis range)
hist(dados$Rating, xlim = c(1,5))


#printing graphics with ggplot lib
ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5))+
  xlim(c(1,5))


# attributing graphics to objects
rating.histogram <- ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) +
  xlim(c(1,5))


# plotting objects
rating.histogram


#changing bins (columns number)
ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating), bins = 6)


#changing breaks (legends interval)
ggplot(data = dados) + 
  geom_histogram(mapping = aes(x = Rating), breaks = seq(1,5))


#creating bar graphics
ggplot(data = dados) +
  geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip()


# getting top 10
category.freq <- data.frame(table(dados$Category))
  #printing object
ggplot(data = category.freq) +
  geom_bar(mapping = aes(x = Var1, y = Freq), stat = "identity") + coord_flip()
  # Order (-Freq to decrease order)
ggplot(data = category.freq) + 
  geom_bar(mapping = aes(x = reorder(Var1,Freq), y = Freq), stat = "identity") +
  coord_flip()
  # order table
category.Top10 <- category.freq[(order(-category.freq$Freq)), ]
  # select top 10
category.Top10 <- category.Top10[1:10, ]
  # plot top 10
freq.category.plot <- ggplot(data = category.Top10) +
  geom_bar(mapping = aes(x = reorder(Var1,Freq), y = Freq), stat = "identity") +
  coord_flip()


#concatenating data with function
dados_2 <- dados %>% filter(Category != "1.9")


#verifying min e max
min(dados_2$Rating)
max(dados_2$Rating)


#counting rows with data error
dados_2 %>% filter(is.na(Rating)) %>% count()


#summarizing data
summary(dados_2$Rating)


#group and count errors per Category
dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% count()


#attributing medium values to Category
mean.category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>% 
  summarise(media = mean(Rating))


#substituting N/A values per medium values
for (i in 1:nrow(dados_2)){
  if(is.na(dados_2[i, "Rating"])){
    dados_2[i, "newrating"] <-
      mean.category[mean.category$Category == dados_2[i, "Category"], "media"]
  }else{
    dados_2[i, "newrating"] <- dados_2[i, "Rating"]
  }
}


#Consulting new rating data
summary(dados_2$newrating)


#Classifying new rating
#<2 = ruim
#>4 = bom
#2.1 e 3.9 regular
dados_2 <- dados_2 %>%
    mutate(rating_class = if_else(newrating < 2, "ruim",
                                  if_else(newrating > 4, "bom", "Regular")))


#Plotting classified rating
rating_class_plot <- ggplot(dados_2) +
  geom_bar(aes(rating_class), stat = "count")


#Verifying types proportion
type.freq <- data.frame(table(dados_2$Type))


#Plotting pizza graphics
type.plot <- ggplot(type.freq) +
  geom_bar(aes(x = "", y = Freq, fill = Var1), stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0)


#Analyzing size column
freq.size <- data.frame(table(dados_2$Size))


#Converting to same scale
  #K or M frequency
teste <- dados_2$Size[1]
grepl(pattern = "m", x = teste, ignore.case = T)
grepl(pattern = "k", x = teste, ignore.case = T)
  #drop letter M or k
gsub(pattern = "M", replacement = "", x = teste)
gsub(pattern = "K", replacement = "", x = teste)
  #Looping with sapply
sapply(X = dados_2$Size, FUN = function(y){
  print(y)
})
  #Converting Mb to Kb
dados_2$kb <- sapply(X = dados_2$Size,FUN = function(y){
  if(grepl("M",y,ignore.case = T)){
    y <- as.numeric(gsub(pattern = "M", replacement = "", x = y)) * 1024
  }else if(grepl("k|\\+",y,ignore.case = T)){
    y <- gsub("k|\\+",replacement = "", x = y)
  }else{
    y <- "nd"
  }
})


#Plotting
hist(as.numeric(dados_2$kb))
  #Removing scientific notation on x axis
options(scipen = 999)
  #Saving filtered table
size.app <- dados_2 %>% filter( kb != "nd") %>% mutate(kb = as.numeric(kb))
  #Saving graphic
size.app.plot <- ggplot(size.app) + geom_histogram(aes(kb))


#Installing pack "lubridate"
 #install.packages("lubridate")


#Activating lubridate
library(lubridate)


#Convert text to date dmy -> "Day. Month, Year"
dmy("05-07-2018")
  #OR
dmy("05072018")
  #Other convertion mask...
ymd("19961229")
  #OR
ymd("1996/12/29")
  #OR
ymd("20-12-13")


#Working with hours
ymd_hms("2018-01-24 12:00:00")
  #or
ymd_hms('2018-12-2512:00:00')
  #or
ymd_h("2018/12/25 05")
  #checking type
typeof(hms("12:30:00"))
  #or
hours("12")


#Other lubritade functions
data_hora <- "2018-12-25 14:05:15"
data_hora <- ymd_hms(data_hora)
  #extract the month
month(data_hora)
  #extract the day
mday(data_hora)
  #extract the year
year(data_hora)
  #extract the hour
hour(data_hora)
  #extract the minute
minute(data_hora)
  #extract the seconds
second(data_hora)
  #extract the week day "char"
wday(data_hora, label = T)
  #extract the week day "integer"
wday(data_hora)
  #extract the month "char"
month(data_hora, label = T)
#extract the month "integer"
month(data_hora)


#Creating graphics with date and hour
notas <- read.csv("C:/Users/JOC/Documents/curso_r/dataviz_uma_var-dados/user_reviews.csv")
  #Analyzing data
summary(notas)
  #Convert data
notas$data_2 <- ymd_hms(notas$data)
  #Convert data to YYYY-MM  
notas$data_2 <- parse_date_time(format(notas$data_2, "%y-%m"), "ym")
  #Plotting graphics
ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))
  #Improving graphics
media_nota <- notas %>% group_by(data_2) %>%
  summarise(media = mean(Sentiment_Polarity))
nota_plot <- ggplot(media_nota) + geom_line(aes(x = data_2, y = media))


#Improving rating.histogram graphic
  #Attributing title
rating.histogram <- rating.histogram + ggtitle("Histograma Rating")
  #Centralizing title 
rating.histogram <- rating.histogram +
  theme(plot.title = element_text(hjust = 0.5))
  #Changing layout color
rating.histogram <- rating.histogram + theme_bw()


#Improving freq.Category.plot
  #Writing a Title
freq.category.plot <- freq.category.plot +
  ggtitle("Quantidade de Apps por Categora")
  #Changing axes labels (rotated axis)
freq.category.plot <- freq.category.plot +
  ylab("Quantidade") + xlab("Categoria")
  #Bars Dynamic Colors and Legends
freq.category.plot + geom_bar(aes(Var1, Freq, fill = Freq), stat = "identity")
  #Bars Fixed Colors
freq.category.plot <- freq.category.plot +
  geom_bar(aes(Var1, Freq), fill = "darkcyan", stat = "identity")
  #Changing layout
freq.category.plot <- freq.category.plot + theme_bw()

  
#Improving type.plot (pizza graphic)
  #Creating a custom blank theme
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )
  #Concatenating 
type.plot <- type.plot + blank_theme
  #Addicting data labels
library(scales)
type.plot <- type.plot + geom_text(aes(x = "", y = Freq/2, label =
                                         percent(Freq/sum(Freq))), size = 5)
  #Changing legend
type.plot <- type.plot + scale_fill_discrete(name = "Tipo App")
  #Add Title
type.plot <- type.plot + ggtitle("Tipos de App´s") +
  theme(plot.title = element_text(hjust = 0.5))


#Personalizing size.app.plot (histogram)
  #addicting title
size.app.plot <- size.app.plot + ggtitle("Histograma Tamanho dos Aplicativos")
  # changing the color
size.app.plot <- size.app.plot + geom_histogram(aes(kb, fill = ..x..)) +
  scale_fill_gradient(low = "blue", high = "yellow") + guides(fill = FALSE)
  #changing axis labels
size.app.plot <- size.app.plot + xlab("Tamanho do App (em kb)") +
  ylab("Quantidade de Apps")
  #modifying theme
size.app.plot <- size.app.plot + theme_bw()


#Personalizing rating_class_plot (bar graphic)
  #addicting title and axis labels
rating_class_plot <- rating_class_plot +
  ggtitle("Categoria de Notas do App") + xlab("Categoria") + ylab("Quantidade")
  #changing the color
rating_class_plot +
  geom_bar(aes(rating_class, fill = rating_class)) +
  scale_fill_manual("legend", values =
                      c("bom" = "green4", "Regular" = "yellow2", "ruim" = "red"))
  # OR
rating_class_plot <- rating_class_plot +
  geom_bar(aes(rating_class), fill = c("green4", "yellow2", "red")) +
  guides(fill = FALSE)
  #changing theme
rating_class_plot <- rating_class_plot + theme_bw()


#Personalizing nota_plot (lines graphic)
  #addicting title and axis label
nota_plot <- nota_plot + ggtitle("Evolução das Notas por Período") +
  xlab ("Período") + ylab ("Nota")
  #changing theme
nota_plot <- nota_plot + theme_bw()


#making a dashboard
  #install.packages("gridExtra")
library(gridExtra)
  #two graphics at dashboard
dashboard <- grid.arrange(rating.histogram,
             freq.category.plot,
             nota_plot,
             rating_class_plot,
             type.plot,
             size.app.plot,
             ncol = 3,
             nrow = 2
             )










































