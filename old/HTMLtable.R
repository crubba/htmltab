rm(list=ls())

#pkgs
library(dplyr)
library(magrittr)

#source functions
source("~/Dropbox/HTMLtable/HTMLtable-fun.R")

#URLS

url <- "http://en.wikipedia.org/wiki/Demographics_of_Pakistan"
url <- "http://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom"
url <- "http://de.wikipedia.org/wiki/Bundestagswahlkreis_Gro%C3%9F-Gerau"
url <- "http://de.wikipedia.org/wiki/Bundestagswahlkreis_Waldeck"
url <- "http://en.wikipedia.org/wiki/New_Zealand_general_election,_2002"
url <- "http://en.wikipedia.org/wiki/List_of_countries_by_population"
url <- "http://en.wikipedia.org/wiki/Arundel_and_South_Downs_%28UK_Parliament_constituency%29"
url <- "http://en.wikipedia.org/wiki/Game_of_Thrones"
url <- "http://en.wikipedia.org/wiki/Great_Recession"
url <- "http://de.wikipedia.org/wiki/Bundestagswahlkreis_Frankfurt_am_Main_I"
url <- "http://en.wikipedia.org/wiki/List_of_countries_by_population"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)



#HTMLtable(doc, xpath, header = [thead, rows])

#table node
table.node <- getNodeSet(parsed, table.xp)


#get orientation from header dimension
#MAIN CELLS (without column header)
#cells <- getCells(table.node, header) #"//table[@class = 'wikitable sortable']"          //table[@width='95%']             //table[./caption[contains(., 'Electorate results of the New Zealand general election, 2002')]]

#SETWD
setwd("~/Dropbox/HTMLtable/data")

header = NULL
url <- "List_of_countries_by_population.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[1]" #population
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL, elFun = function(node)str_trim(xmlValue(node)))


url <- "Bundestagswahlkreis_Frankfurt_am_Main_I.html"
parsed <- htmlParse(file = url, encoding = "utf-8") #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[20]" #Frankfurt
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL, elFun = function(node)str_trim(xmlValue(node)))

url <- "Bundestagswahlkreis_Gro%C3%9F-Gerau.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[15]" #giessen
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL, elFun = function(node)str_trim(xmlValue(node)))

url <- "Jamie_xx.html"
parsed <- htmlParse(file = url, encoding = "utf-8") #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[2]" #jamie
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL,  body = "tr[./td[not(@colspan = '9')]]", elFun = function(node)str_trim(xmlValue(node)))

url <- "Jamie_xx.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- '//*[@id="mw-content-text"]/table[3]' #jamie
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL, body = 1, elFun = function(node)str_trim(xmlValue(node)))

url <- "Pakistan.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "//table[3]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL, body = NULL, elFun = function(node)str_trim(xmlValue(node)))

url <- "Afghanistan.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "//table[5]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL,  body = NULL, elFun = function(node)str_trim(xmlValue(node)))

url <- "Federal_Constitutional_Court_of_Germany.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[4]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = NULL,  body = NULL, elFun = function(node)str_trim(xmlValue(node)))

elFun <- function(node) {
	x <- xmlValue(node)
  
  if(x == ""){
    x <- xmlGetAttr(node, "style")
    
    if(str_detect(x, "#098137")){
      x <- "Greens"}
    if(str_detect(x, "#00529F")){
      x <- "National"}
    if(str_detect(x, "#FF0000")){
      x <- "Labour"}
    if(str_detect(x, "#501557")){
      x <- "United Future"}
    if(str_detect(x, "#000000")){
      x <- "New Zealand First"}
    if(str_detect(x, "#AB0616")){
      x <- "Progressive"}
    if(str_detect(x, "#9E9E9E")){
      x <- "independent"}      
  }
  
  return(x)
}

#header <- c("tr[td[@bgcolor = '#FFDEAD']]", "tr[th]")
#header <- "tr[td[@bgcolor = '#FFDEAD']]"
url <- "New_Zealand_general_election,_2002.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[4]"
table.node <- getNodeSet(parsed, table.xp)
(nz <- HTMLtable(table.node, header = NULL, body = "tr[./td[not(@colspan = '10')]]", elFun = elFun))


url <- "Demography_of_the_United_Kingdom.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[12]"
table.node <- getNodeSet(parsed, table.xp)
elFun = function(node) str_replace_all(xmlValue(node), "[,%]", "") 
ukLang <- HTMLtable(table.node, elFun = elFun)

ukLang <- ukLang %>% gather(key, value, -Ability)

ukLang %>% separate(key, into = c("region", "language", "statistic"), sep = " >[0-9]> ")


url <- "Liste_deutscher_W%C3%B6rter_aus_dem_Hebr%C3%A4ischen_und_Jiddischen.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[1]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node)

url <- "Indian_general_election,_2014.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[5]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = 1:2, body = "tr[position() > 2]")

url <- "German_federal_election,_2009.html"
parsed <- htmlParse(file = url) #readHTMLTable(purl, which = 20)
table.xp <- "/html/body/div[3]/div[2]/div[4]/table[3]"
table.node <- getNodeSet(parsed, table.xp)
HTMLtable(table.node, header = 1:2, body = "tr[position() > 2]")