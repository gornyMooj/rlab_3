#Na zajęciach pobraliśmy dane z ogłoszeń dla jednego miasta( Lublin)

#Zestaw3:
#1. Zebrać dane dotyczące ofert sprzedaży 10 miast, za pomocą web scrappingu.
#   Miasta takie jak w linku:https://www.bankier.pl/wiadomosc/Ceny-ofertowe-wynajmu-mieszkan-maj-2022-Raport-Bankier-pl-8337421.html
#2. Odpowiedzieć na pytania:
#a czy i jak różnią się średnie ceny ofertowe sprzedaży mieszkań dla miast - warszawa i lublin.
#b czy i jak różnią się średnie ceny ofertowe sprzedaży mieszkań  dla wszystkich miast .
#3. Wykonać raport wskazując nalepsze miasto do zakupu mieszkania biorąc pod uwagę średnie ceny wynajmu z linku.



library(dplyr)
library(stringr)
library(gtools)
library(rvest)
library(xml2)


# Part One - SCRAPING DATA
# data is scraped to separate CSV files created for each of the 10 cities

miasta <- list("lublin","bydgoszcz","gdansk","katowice", "krakow", "lodz", "poznan","szczecin", "warszawa", "wroclaw")


for (miasto in miasta) {
  
  
  url1 <- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/",miasto,"?areaMax=38&page=1")
  ileOgloszen<-read_html(url1)%>%html_node(".css-klxieh.e1ia8j2v9")%>%html_text()
  ileOgloszen<-as.numeric(ileOgloszen)
  
  print('ileOgloszen')
  ileOgloszen
  
  wektorLinkow<-c()
  liczbaStron<-(ceiling(ileOgloszen/36))
    
  for( i in 1:liczbaStron){
        #url<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/",miasto,"?areaMax=38&page=1",i)
        url<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/",miasto,"?areaMax=38&page=",i)
        page<-read_html(url)
        temp<- page%>%html_nodes(".css-p74l73.es62z2j17")%>%html_node("a")%>%xml_attr("href")
        wektorLinkow<-c(wektorLinkow,temp)
  }  
  
   
  
  zrobWierszRvest<-function(w,wektorLinkow,miasto){
    urlo<-paste0("https://www.otodom.pl",wektorLinkow[w])
    page<-read_html(urlo)
    cena<-page%>%html_node('.css-8qi9av.eu6swcv19')%>%html_text()
    v<-   page%>% html_nodes(".css-1qzszy5.estckra8")%>%html_text()
    v<- gsub('.css.*}','',v) # removes all characters before and including curly bracket ' }', to be specific all characters are replaces with empty string ''
    indexy<- seq(1,length(v),1)
    nazwyKolumn<- v[indexy%%2==1 ] 
    wartosci<- v[indexy%%2==0 ] 
    df1<-  data.frame( matrix(wartosci,nrow=1,ncol=length(nazwyKolumn)) ) # creates a datafram from matrix
    names(df1)<-nazwyKolumn # adds column names to the dataframe
    df1<-cbind(df1,cena) # this is how you add a custom row to the dataframe 
    df1<-cbind(df1,urlo)
    df1<-cbind(df1,miasto=miasto)
    df1
  }
  
  
  wektorLinkow <- unique(wektorLinkow)
  
  mieszkania <- NULL
  
  liczbaLinkow<-length(wektorLinkow)
  
  for( w in 1:liczbaLinkow ){
    print(paste0(w,"/",liczbaLinkow))
        skip <- FALSE
          tryCatch(
            dm <- zrobWierszRvest(w,wektorLinkow,miasto),error=function(e){
              print(e);
              print(wektorLinkow[w])
              skip<<-TRUE} # if we have error in tryCatch() error handler then we change var skip to True 
          )
        if(skip){next}  # if True - meaning there was an error - skip the rest of the code in this loop and start with a new one
        if(is.null(mieszkania)){
          mieszkania<-dm # if dataframe is null use dm instead
        }
        mieszkania<-smartbind(mieszkania,dm)
    }
  
  


  path = paste("C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB3\\",miasto,".csv", sep="")
  write.csv(x=mieszkania, file=path,fileEncoding = "UTF-8")
  
} 

miasta <- c("lublin","bydgoszcz","gdansk","katowice", "krakow", "lodz", "poznan","szczecin", "warszawa", "wroclaw")

# having data forall 10 cities 
df  <- NULL
srednie_cenny <- NULL
srednie_cenny_za_metr <- NULL
mediana_ceny <- NULL
media_ceny_za_metr <- NULL

for (miasto in miasta) {
  path = paste("C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB3\\",miasto,".csv", sep="")
  csv <-read.csv(path,fileEncoding = "UTF-8")

  
  
  # cleaning cena column 
  csv$cena <- gsub(' ', '', csv$cena)  # removing empty spaces
  csv$cena <- gsub('zl', '', csv$cena) # removing zl
  csv <- subset(csv, grepl('^\\d+$', csv$cena))
  csv$cena <- as.numeric(as.character(csv$cena)) # converting string to numeric
  
  
  
  # cleaning Powierzchnia
  csv$Powierzchnia <- gsub(' ', '', csv$Powierzchnia)  # removing empty spaces
  csv$Powierzchnia <- gsub('m²', '', csv$Powierzchnia) # removing m²
  csv$Powierzchnia <- gsub(',', '.', csv$Powierzchnia) # removing zl
  csv$Powierzchnia <- as.numeric(as.character(csv$Powierzchnia)) # converting string to numeric
  csv$Powierzchnia <- round(csv$Powierzchnia, digits = 0)
  
  # creating a new column for square meters
  csv$SQRM <- round( csv$cena /  csv$Powierzchnia , digits = 0) 
  
  if(is.null(df)){
    df<-csv
  }
  df<-smartbind(df,csv)
  
  
  srednia_cena <- mean(csv[["cena"]])
  srednie_cenny <- c(srednie_cenny, srednia_cena)
  
  srednie_cena_za_metr <- mean(csv[["SQRM"]])
  srednie_cenny_za_metr <- c(srednie_cenny_za_metr, srednie_cena_za_metr)
  
  mediana_cena <- median(csv[["cena"]])
  mediana_ceny <- c(mediana_ceny, mediana_cena)
  
  media_cena_za_metr <- median(csv[["SQRM"]])
  media_ceny_za_metr <- c(media_ceny_za_metr, media_cena_za_metr)
  
}

write.csv(x=df, file="C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB3\\miasta10new.csv",fileEncoding = "UTF-8")

# creating dataframe from scratch basic summarizing for each model
df_basic_stats <- data.frame (
  miasta = miasta,
  srednie_cenny  = srednie_cenny,
  srednie_cenny_za_metr  = srednie_cenny_za_metr,
  mediana_ceny  = mediana_ceny,
  media_ceny_za_metr  = media_ceny_za_metr
)


# saving df_basic_miasta
write.csv(x=df_basic_stats, file="C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB3\\df_basic_stats.csv",fileEncoding = "UTF-8")


#b czy i jak różnią się średnie ceny ofertowe sprzedaży mieszkań  dla wszystkich miast .
df_basic_stats <- df_basic_stats[order(df_basic_stats$miasta, decreasing = FALSE), ]
df_basic_stats
View(df_basic_stats)

# # # # # # # # # # # #
# WYKRESY LICZBY
# Srednie ceny  - MIASTA
x <- df_basic_stats$miasta
options(scipen=999)
y <- df_basic_stats$srednie_cenny

barplot(y, names.arg = x, density = 10, main="Srednie ceny ",las=2)
avg <- mean(df_basic_stats[["srednie_cenny"]])
abline( h = avg, col = "Pink", lty = 4)

# Srednie cena za metr - MIASTA
x <- df_basic_stats$miasta
options(scipen=999)
y <- df_basic_stats$srednie_cenny_za_metr

barplot(y, names.arg = x, density = 10, main="Srednie ceny za m²",las=2)
avg <- mean(df_basic_stats[["srednie_cenny_za_metr"]])
abline( h = avg, col = "Pink", lty = 4)


#a czy i jak różnią się średnie ceny ofertowe sprzedaży mieszkań dla miast - warszawa i lublin.
vc <- c('lublin', 'warszawa')
dt <- df_basic_stats[df_basic_stats$miasta %in% vc,]

# Srednie ceny  - MIASTA
x <- dt$miasta
options(scipen=999)
y <- dt$srednie_cenny

barplot(y, names.arg = x, density = 10, main="Srednie ceny ")

# Srednie cena za metr - MIASTA
x <- dt$miasta
options(scipen=999)
y <- dt$srednie_cenny_za_metr

barplot(y, names.arg = x, density = 10, main="Srednie ceny za m²")


# Getting renting data from Bankier.pl
url = "https://www.bankier.pl/wiadomosc/Ceny-ofertowe-wynajmu-mieszkan-maj-2022-Raport-Bankier-pl-8337421.html"
content <- read_html(url) 
web <- content %>%
  html_nodes("table tr")%>% html_text()


miasta <-c("Bydgoszcz","Gdańsk","Katowice","Kraków","Lublin","Łódź","Poznań","Szczecin","Warszawa","Wrocław")

content <- web[5:length(web) - 2]
content <- content[seq(from=1, to=length(content), by=3)]   # create a collection with every 3rd element

target_rows <- NULL
names <- gsub("\n\n\n", "|", web[2])
names <- gsub("\n", "", names)
names <- as.list(unlist(strsplit(names,split='|', fixed=TRUE)))
names # what means ==>  [[1]]

for (row in content) {
  target <- gsub("\n\n\n", "|", row)
  target <- gsub("\n", "", target)
  target <- strsplit(target,split='|', fixed=TRUE) 
  
  target_rows <- c(target_rows, target)
}

target_rows

my_name_vector<- names
my_data_vector <-  target_rows[[1]]

#create a data frame out of a transposed vector
table = as.data.frame(t(my_data_vector));
#change the names of the dataframe to be the titles
colnames(table) <- my_name_vector;

for (row in target_rows) {
  supplemental_data_frame <- data.frame(t(row));
  colnames(supplemental_data_frame) <- my_name_vector;
  table <- rbind(table, supplemental_data_frame);
}
table <- unique(table)
table <- table[order(table$Miasto, decreasing = FALSE), ]
View(table)

colnames(table)

# Srednie ceny  - MIASTA
y <- table[["Średnia cena [w zł/m-c]"]]
y <-  gsub(" ", "", y)
y <- as.numeric(y)

options(scipen=999)
x <- table$Miasto

barplot(y, names.arg = x, density = 10, main="Srednie ceny wynajmu",las=2)

# Change compared to last year in %
y <- table[["Zmiana r/r [w proc.]"]]
y <-  gsub(" ", "", y)
y <-  gsub(",", ".", y)
y <-  gsub("+", "", y)
y <- as.numeric(y)

options(scipen=999)
x <- table$Miasto

barplot(y, names.arg = x, density = 10, main="Zmiana r/r [w proc.]",las=2)


# Change compared to the previous month in%
y <- table[["Zmiana m/m [w proc.]"]]
y <-  gsub(" ", "", y)
y <-  gsub(",", ".", y)
y <-  gsub("+", "", y)
y <- as.numeric(y)

options(scipen=999)
x <- table$Miasto

barplot(y, names.arg = x, density = 10, main="Zmiana m/m [w proc.]",las=2)


# saving table from bankier.pl to CSV
write.csv(x=table, file="C:\\Users\\Ukasz\\Desktop\\BIG DATA\\_R\\LAB3\\bankier_pl_table.csv",fileEncoding = "UTF-8")
