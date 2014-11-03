header <- NULL
body <- NULL
headerSep = c(" >1> ", " >2> ", " >3> ", " >4> ")
elFun = function(node)XML::xmlValue(node)

doc <- "http://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom"
num = "//table[12]"

ukLang <- htmltable(doc = doc, num = "//table[12]")



#INDIAN

headerSep = c(" >1> ", " >2> ", " >3> ", " >4> ")
url <- "http://en.wikipedia.org/wiki/Indian_general_election,_2014"
doc = url
which = "/html/body/div[3]/div[2]/div[4]/table[5]"
header = 1:2
body = "tr[position() > 2]"
elFun = function(node)XML::xmlValue(node)


#GERMAN ELECTION 2009 - hard case need to revise span_header
url <- "http://en.wikipedia.org/wiki/German_federal_election,_2009"
doc = url
which = "/html/body/div[3]/div[2]/div[4]/table[3]"
header = 1:2
body = "tr[position() > 2]"
headerSep = c(" >> ")
elFun = function(node)XML::xmlValue(node)
htmltable(doc = url, which = "/html/body/div[3]/div[2]/div[4]/table[3]", header = 1:2, body = "tr[position() > 2]")


# #NZ TOPLEVEL
which = "/html/body/div[3]/div[2]/div[4]/table[3]"
doc = "~/Dropbox/PhD Zurich/new zealand/candidate_info/toplevel/government/Fifth National Government of New Zealand - Wikipedia, the free encyclopedia.html"
header = "tbody/tr[th]"
body = "tbody/tr[td]" #Check this layout, all under the tbody
national <- htmltable(doc = doc, which = which, header = header, body = body)

#AFGHANISTAN
url <- "Afghanistan.html"
doc = url
num = "//table[3]"
header = NULL
body = NULL
elFun = function(node)str_trim(xmlValue(node))

#JAMIE XX
url <- "http://en.wikipedia.org/wiki/Jamie_xx"
doc = url
num = "/html/body/div[3]/div[2]/div[4]/table[2]"
header = NULL
body = "tr[./td[not(@colspan = '9')]]"
elFun = function(node)str_trim(xmlValue(node)))
htmltable(doc = url, which = "/html/body/div[3]/div[2]/div[4]/table[2]")
