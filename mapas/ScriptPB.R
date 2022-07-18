#Dados: 
#De mortalidade no estado de Pernambuco 


#links:
#http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/obt10pe.def

#install.packages("ggplot2")
#install.packages("gdal") 
#install.packages("rgeos")
#install.packages("gpclib")
#install.packages("maptools")
#install.packages("RColorBrewer",dependencies = T)

library(rgeos)
library(ggplot2)
library(rgdal)
library(gpclib)
library(maptools)
library(RColorBrewer)

#Ler arquivo
PB <- readOGR("D:/Documents/#Técnico/TI/mapas", "PE_Municipios_2021")
Obitos_xlsx <- readxl::read_xlsx(file.choose())

PB$CD_MUN <- substr(PB$CD_MUN,1,6)

head(Obitos_xlsx)

Obitos_xlsx <- na.omit(Obitos_xlsx)

names(Obitos_xlsx) <- c("Municipio", "Obitos")

head(PB@data)

Obitos_xlsx$CD_MUN <- substr(Obitos_xlsx$Municipio,1,6)

head(Obitos_xlsx)

dim(Obitos_xlsx)
dim(PB@data)

head(PB@data)

Obitos_xlsx <- Obitos_xlsx[order(Obitos_xlsx$CD_MUN),]
malhaPB <- PB@data[order(PB@data$CD_MUN),]

head(malhaPB)
dim(Obitos_xlsx)
dim(malhaPB)

linhas <- c(1,2)
malhaPB = malhaPB[-linhas,]
dim(Obitos_xlsx)
dim(malhaPB)

#Dica
#malhaPB = subset(malhaPB,CD_MUN!="430000")
head(malhaPB)
head(Obitos_xlsx)

PB2 <- merge(malhaPB,Obitos_xlsx)

head(PB2)

head(PB)
#Até aqui OK
PB.PBf <- fortify(PB, region = "CD_MUN")

head(PB.PBf)

PB.PBf <- subset(PB.PBf,id!="430000")

PB.PBf <- merge(PB.PBf, PB@data, by.x = "id", by.y = "CD_MUN")

PB2$ObitosCat <- cut(PB2$ObitosCat, breaks = c(0,20000,40000,60000,80000,100000,2000000),
                        labels = c('0-20000',
                                   '20000-40000',
                                   '40000-60000',
                                   '60000-80000',
                                   '80000-100000',
                                   '+100000'),
                        include.lowest = T)

head(PB2)

#rm(PB2)
#rm(PB.PBf)

PB.PBf <- merge(PB.PBf, PB2, by.x = "id", by.y = "CD_MUN")

head(PB.PBf)

#names(PB2)[1]=c("id")

#install.packages("RColorBrewer",dependencies = T)
library(RColorBrewer)

ggplot(PB.PBf, aes(PB.PBf$long,PB.PBf$lat, group=PB.PBf$group,fill=PB.PBf$ObitosCat)) +
  geom_polygon(colour='green') + coord_equal() + ggtitle("Obitos") +
  labs(x = "Longitude", y = "Latitude", fill="Obitos") +
  scale_fill_manual(values = brewer.pal(9,'Reds')[4:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))
