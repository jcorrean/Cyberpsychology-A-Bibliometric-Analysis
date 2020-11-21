library(bibliometrix)
load("~/Documents/GitHub/Cyberpsychology-A-Bibliometric-Analysis/CyberBehaviorDataSet.RData")
resultados <- biblioAnalysis(analizables)
Resultados <- summary(resultados)
Resultados$MainInformation
Autores <- Resultados$MostProdAuthors
Autores <- Autores[1:2]
Paises <- Resultados$MostProdCountries
Paises <- Paises[c(1, 3)]
names(Paises)[1] <- "Country"
Paises$Freq <- as.numeric(Paises$Freq)
Produccion <- Resultados$AnnualProduction
names(Produccion)[1] <- "Year"
Produccion$Articles <- as.numeric(Produccion$Articles)
library(ggplot2)
Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Relative Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
library(ggpubr)
ggarrange(Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
Resultados$AnnualGrowthRate
Resultados$MainInformation[7]
redautores <- biblioNetwork(analizables, analysis = "collaboration", network = "authors", sep = ";")
autores <- networkStat(redautores)
autores$network$networkSize
Fig3 <- networkPlot(redautores, n = 60, type = "sphere", size=15,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=60,label.cex=F, cluster = "none", Title = "")
reduniversidades <- biblioNetwork(analizables, analysis = "collaboration", network = "universities", sep = ";")
reduni <- networkStat(reduniversidades)
reduni$network$networkSize
networkPlot(reduniversidades, n = 60, type = "sphere", size=15,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=60,label.cex=F, cluster = "none", Title = "")
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
Resultados$MainInformation[14]
Resultados$MainInformation[13]
Resultados$MainInformation[16]
Resultados$MainInformation[2]
Resultados$MainInformation[10]
Resultados$MainInformation[11]
(1-(696/6761))*100
Top20 <- KeywordGrowth(analizables, Tag = "DE", sep = ";", top = 20)
topicos <- variable.names(Top20)[-1]
library(tidyr)
Temas <- Top20 %>% gather(topicos, key = "Temas", value = "Articulos")
Fig4 <- ggplot(Temas, aes(x=Year, y=Articulos)) + geom_line(colour="blue") + facet_wrap(. ~ Temas) + theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_rect(fill="lightblue", colour="black", size=1)) + xlim(2010, 2018)
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
