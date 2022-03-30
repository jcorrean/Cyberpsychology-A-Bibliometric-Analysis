library(bibliometrix)
load("~/Documents/GitHub/Cyberpsychology-A-Bibliometric-Analysis/CyberBehaviorDataSet.RData")
# Raw data is in bibtex format 
# Download the "All.bib" file, save it somewhere
# in your local hard disk and open it like this:
# all <- readFiles("/home/juan/Documents/Ciberconducta/All.bib")
# Replace the address in between double quotes by 
# the local address of your hard disk where you saved
# the All.bib file
# todos <- convert2df(all, dbsource = "scopus", format = "bibtex")
# library(dplyr)
# Let's select the bibliographic records from 2000  
# analizables <- todos %>% filter(PY >=2000)
# let's save preliminary results in a new object
resultados <- biblioAnalysis(analizables)
# Now let's see some of these results
Resultados <- summary(resultados)
Resultados$MainInformation
# Let's build a data frame with most productive authors
Autores <- Resultados$MostProdAuthors
# Let's keep the first two rows of this dataframe
Autores <- Autores[1:2]
# Let's see most productive countries
Paises <- Resultados$MostProdCountries
# Let's keep the first and thirs column of this dataframe
Paises <- Paises[c(1, 3)]
# Let's change the name of the first column
names(Paises)[1] <- "Country"
# Pongamos los nombres en Español
# Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")
Paises$Freq <- as.numeric(Paises$Freq)
# Let's see the production
Produccion <- Resultados$AnnualProduction
# Let's change the name of the first column
names(Produccion)[1] <- "Year"
# Let's set as numeric the records of the second column
Produccion$Articles <- as.numeric(Produccion$Articles)

# Let's plot our first figure
library(ggplot2)
Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="green") + coord_flip() + xlab("Country") + ylab("Relative Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
library(ggpubr)
ggarrange(Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)

# Ahora miremos la distribución de formatos de publicaciones
# Formatos <- data.frame(resultados$Documents)
# ggplot(Formatos, aes(x="", y=Freq, fill=Var1))+ geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)

# Now let's see the following results:
# Average interannual growth
Resultados$AnnualGrowthRate
# Average number of citations 
Resultados$MainInformation[7]
# Scientific community size
redautores <- biblioNetwork(analizables, analysis = "collaboration", network = "authors", sep = ";")
autores <- networkStat(redautores)
autores$network$networkSize
# We created Figure 2 with biblioshiny 
# -> "Social Structure" -> "Network Collaboration" -> "Countries" (layout: "World Map")
# For every year we loaded a .zip file containing
# the cummulative records. These files can be downloaded 
# as well from our GitHub repository
biblioshiny()
# Let's create our Figure 3
Fig3 <- networkPlot(redautores, n = 60, type = "sphere", size=15,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=60,label.cex=F, cluster = "none", Title = "")
# Let's see how many different institutions
reduniversidades <- biblioNetwork(analizables, analysis = "collaboration", network = "universities", sep = ";")
reduni <- networkStat(reduniversidades)
reduni$network$networkSize
networkPlot(reduniversidades, n = 60, type = "sphere", size=15,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=60,label.cex=F, cluster = "none", Title = "")
# Number of countries
analizables <- metaTagExtraction(analizables, Field = "AU_CO", sep = ";")
redpaises <- biblioNetwork(analizables, analysis = "collaboration", network = "countries", sep = ";")
redpais <- networkStat(redpaises)
redpais$network$networkSize
networkPlot(redpaises, n = 82, type = "kamada", size=15,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=82,label.cex=F, cluster = "none", Title = "")
listapaises <- data.frame(redpaises@Dimnames)
names(listapaises)[1] <- "Pais"
listapaises[2] <- NULL
listapaises$Region <- c("Europe","North-America","Europe","Europe","Asia","Asia","North-America","Europe","Europe","Europe","Europe","Oceania","Europe","Europe","Oceania","Europe","Asia","Europe","Asia","Europe","Asia","Europe","Europe","Europe","Europe","Asia","Asia","North-America","Asia","Europe","Asia","Asia","South-America","Europe","Europe","Asia","Africa","Europe","Europe","South-America","Europe","Europe","Europe","Africa","Europe","South-America","South-America","Asia","Africa","Asia","Asia","Europe","Europe","Africa","Europe","Africa","Europe","Europe","Europe","Africa","Asia","South-America","Africa","South-America","Europe","Asia","Asia", "South-America","Asia","Asia","Europe", "South-America", "Africa", "South-America", "Europe", "Asia", "Oceania","Europe","South-America","Africa","Africa","Africa")
table(listapaises$Region)

# Authors per document
Resultados$MainInformation[14]
# Documents per author
Resultados$MainInformation[13]
# Collaboration index
Resultados$MainInformation[16]
# Total of published articles
Resultados$MainInformation[2]
# Total of single-authored documents
Resultados$MainInformation[10]
# Articles with two or more authors
Resultados$MainInformation[11]
# Percentage of articles with two or more authors
(1-(696/6761))*100

# Top20 of most frequent keywords
Top20 <- KeywordGrowth(analizables, Tag = "DE", sep = ";", top = 20)
topicos <- variable.names(Top20)[-1]
library(tidyr)
Temas <- Top20 %>% gather(topicos, key = "Temas", value = "Articulos")
# Let's create figure 4
Fig4 <- ggplot(Temas, aes(x=Year, y=Articulos)) + geom_line(colour="blue") + facet_wrap(. ~ Temas) + theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_rect(fill="lightblue", colour="black", size=1)) + xlim(2010, 2018)


# Now let's apply text mining to the 
# abstracts of the papers
abstracts <- analizables$AB
library(quanteda)
mycorpus <- corpus(abstracts)
summary(mycorpus)
docvars(mycorpus, "Citations") <- analizables$TC
docvars(mycorpus, "Year") <- analizables$PY
docvars(mycorpus, "Journal") <- analizables$SO
tokens(mycorpus)
my_dfm <- dfm(mycorpus, remove = stopwords("english"), remove_punct = TRUE)
topfeatures(my_dfm, 20)
mylsa <- textmodel_lsa(my_dfm)
library(topicmodels)
ap_lda <- LDA(my_dfm, k = 2, control = list(seed = 1234))
