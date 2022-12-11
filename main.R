librarian::shelf("dplyr", "ggplot2", "RColorBrewer")

# https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_covidggstatus_GGSTATUS_1

# load data
url <- "https://data.statistik.gv.at/data/"
meta.header <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_HEADER.csv"), encoding = "UTF-8")[,1:3]
meta.b00 <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_C-B00-0.csv"), encoding = "UTF-8")
meta.b00$name <- lapply(meta.b00$name, function(x) unlist(strsplit(x, " "))[[1]]) %>% unlist
meta.bilimst <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_C-BILIMST-0.csv"), encoding = "UTF-8")
meta.altgrimst <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_C-ALTGRIMST-0.csv"), encoding = "UTF-8")
meta.esimst <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_C-ESIMST-0.csv"), encoding = "UTF-8")
meta.imst <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1_C-IMST-0.csv"), encoding = "UTF-8")

rawdata <- read.csv2(paste0(url, "OGD_covidggstatus_GGSTATUS_1.csv"))

# create a list of the metadata of rawdata
meta <- list(meta.b00, meta.bilimst, meta.altgrimst, meta.esimst, meta.imst)

data <- rawdata %>% `colnames<-`(c("Bundesland","Bildung","Altersgruppe",
                                   "Erwerbsstatus","Status","Anzahl"))

for(i in 1:5){
  data[,i] <- factor(rawdata[,i],levels=meta[[i]]$code, labels = meta[[i]]$name)
}

###### Plotdata

#plotdata <- data %>% group_by(Status, Bundesland) %>%
#  summarise(Anzahl = sum(Anzahl)) 

plotdata <- data %>% group_by(Status, Bildung) %>%
  summarise(Anzahl = sum(Anzahl)) %>% subset(Bildung != 'Unbekannt')

#plotdata <- data %>% group_by(Status, Altersgruppe) %>% 
#  summarise(Anzahl = sum(Anzahl)) 
#
#plotdata <- data %>% group_by(Status, Erwerbsstatus) %>% 
#  summarise(Anzahl = sum(Anzahl))

###### Plot

gg <- ggplot(plotdata, aes_string(x=colnames(plotdata)[2], y=colnames(plotdata)[3], fill=colnames(plotdata)[1]))
gg + geom_col(position="fill") + 
  ylab("Anteil") + # position=fill => Anteil; position="dodge2" => Anzahl
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")+ #display.brewer.all() 
  labs(title=paste("Anteile der COVID-19 Status nach", colnames(plotdata)[2]), 
       subtitle="letzte Aktualisierung: 15.07.2022")
