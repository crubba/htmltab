#pkgs
library(XML)
library(magrittr)
library(stringr)


#TODO
# replace sapply with vapply
#replace lapply (over indexes) with Map
# correct elFun in NZ table
#correct span_header. also see NZ table
#replace %in% with any()
#remove list from table.node

#functions
has_spans <- function(x){
	unlist(x) %>% max %>% `>`(1)
}	


#Extracts the cell XML value (default)
get_cell_element <- function(cells, tag = "td | th", elFun = elFun) { #"td for cell values, th for header values

	cell.element <- lapply(cells, function(tr) { 
			xpathSApply(tr, tag, elFun)
			}
		)
		
	return(cell.element)
}


#Extracts rowspan information
get_rowspans <- function(cells, tag = "td"){
	
	rowspans <- lapply(cells, function(tr) {
		xpathSApply(tr, tag, function(node) {
			rs <- xmlGetAttr(node, "rowspan")
			value <- ifelse(is.null(rs), 1, rs) %>% as.numeric
			return(value)
			}
		)
	}
)	
	return(rowspans)
}


#Extracts colspan information
get_colspans <- function(cells, tag = "td"){
	
	colspans <- lapply(cells, function(tr) {
		xpathSApply(tr, tag, function(node) {
			cs <- xmlGetAttr(node, "colspan")
			value <- ifelse(is.null(cs), 1, cs) %>% as.numeric
			return(value)
			}
		)
	}
)
	return(colspans)
}


#get cell xpath
get_cell_xpath <- function(body){
	
	if(is.character(body)){
		cell.xpath <- body}
		
	tbody <- has_tag(table.node, "tbody") #does table node has tbody?
	td <- has_tag(table.node, "td") #does table node has <td> tags?
	
	if (is.numeric(body) && tbody){ #check to have header checked (cell.xpath <- sprintf("tr[position() > %s]", max(header)))
		cell.xpath <- lapply(body, function(x) sprintf("tbody/tr[position() = %s]", x)) %>% paste(., collapse= " | ")
		}
	if (is.numeric(body) && td){
		cell.xpath <- lapply(body, function(x) sprintf("tr[td][position() = %s]", x)) %>% paste(., collapse= " | ")
		}
	if (is.null(body) && tbody) {
		cell.xpath <- "tbody/tr"
		} 
	 if (is.null(body)) {
	 	cell.xpath <- "tr[td]"}
		
		return(cell.xpath)
}

#Extracts cell values
get_cells <- function(table.node, body) {
	
	cell.xpath <- get_cell_xpath(body = body)
	
	cells <- lapply(1:length(cell.xpath), function(xpath) {
		xpathSApply(table.node[[1]], cell.xpath[[xpath]])
		})  %>% unlist
		
		# sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos])) %>% paste(., collapse = " or ")
		# cell.xpath <- sprintf("tr[%s]", header.xpath)
		
	# }
	# if(tbody && !is.numeric(header)){
		# cell.xpath <- "tbody/tr"
	# } else {
		# if(is.numeric(header)){
			# cell.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos])) %>% paste(., collapse = " or ")
			# cell.xpath <- sprintf("tr[%s]", header.xpath)
		# }
	# }
	
	# if(tbody && !is.numeric(header)){
		# elements <- ifelse(all, "/tr[td] | /tr[th]", "/tr[td]")
		# xpath <- paste0(xpath, elements)
		# cells <- getNodeSet(doc = table.node, path = xpath)
		# } else {}
	
	return(cells)
}


#Assess which XPath to use
get_xpath_header <- function(table.node, header){
	
		
	if(is.character(header)){
		header.xpath = header}
		
	if(is.numeric(header)) {
		header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos])) %>% paste(., collapse = " or ")
		header.xpath <- sprintf("tr[%s]", header.xpath)}
		
		thead <- has_tag(table.node, "thead") #check if has thead
		th <- has_tag(table.node, "th") #check if has th
		
	if(thead && is.null(header)) { #check sequence of statements
		header.xpath <- "thead/tr"}
		
	if (!thead && is.null(header) && th){
		header.xpath <- "tr[th]"}
		
	# if(exists(header.xpath)) {
		# warnings("No usuable header information. Skipping header generation")
		# header.name.table <- vector()
		# return(header.name.table)
		# }
		
		return(header.xpath)
}



#Retrieve Head
get_head <- function(table.node, header = header) {
	
	#Produce XPATH
	header.xpath <- get_xpath_header(table.node, header = header) #return: vector (char, 1-)
	
	#Retrieve header elements, account for weird header structures
	head <- lapply(1:length(header.xpath), function(xp) {
		xpath.return <- xpathSApply(table.node[[1]], header.xpath[xp]) 
		return(xpath.return)
		}
		) %>% unlist 
	
	return(head)
}


#Assert if table has <tag>
has_tag <- function(table.node, tag) {
	x <- xpathSApply(table.node[[1]], "//*", xmlName) %>% unlist #probably wrong
	any(x == tag) #tag %in% x
}


#Main assembly function
span_body <- function(vals, colspans, rowspans) {	
	
	n.cols <- colspans[[1]] %>% unlist %>% as.numeric %>% sum #get idea from first row column length, should be right usually
	n.rows <- length(rowspans) #guess, better make flexible #lapply(rowspans, function(x) x[1]) %>% unlist %>% as.numeric %>% sum
	
	#Create empty Matrix
	mat <- matrix(NA, ncol = n.cols, nrow = n.rows)
	col = 1
	
while(col <= n.cols){
	
		row <- 1
			
		while(row <= n.rows){
		
		col.span.length <- colspans[[row]][col]
		row.span.length <- rowspans[[row]][col]
		cel.val <- vals[[row]][col]
		
		if(row.span.length < 2) {
			mat[row, col] <- cel.val
		} else {
			if(row != n.rows){ #Control for situation: specified rows (main), last row demands col/rowspans
			index <- (row + 1) : (row + row.span.length - 1)
			
			for(counter in index){
			vals[[counter]] <- append(vals[[counter]], cel.val, (col - 1 )) #append col val is after
			rowspans[[counter]] <- append(rowspans[[counter]], 1, (col - 1))
			colspans[[counter]] <- append(colspans[[counter]], 1, (col - 1))
			}
			rowspans[[row]] <- rowspans[[row]][-col]
			rowspans[[row]] <- append(rowspans[[row]], 1, (col-1))
			
			mat[row:(row + row.span.length-1), col] <- cel.val
		}
	}
		
		if(col.span.length > 1){
			vals[[row]] <- append(vals[[row]], rep(cel.val, (col.span.length - 1)), col) #append
			colspans[[row]] <- colspans[[row]][-col]
			colspans[[row]] <- append(colspans[[row]], rep(1, col.span.length), (col-1))
			rowspans[[row]] <- append(rowspans[[row]], rep(1, col.span.length), col)
		} else{}
		
		row <- row + 1 #row <- row + row.span.length
	}
	
	col <- col + 1
}

return(mat) 

} #function end




#Create header with spans
span_header <- function(header.names, header.colspans, header.rowspans, headerSep) {
	
	header.name.table <- vector()
	
	col <- 1
	n.row <- length(header.names) #How many rows has the table? Maybe wrong under some circumstances
	
	repeat{ #column loop, change to while??
	
	row <- 1 #kann weg
	
	if(header.rowspans[[row]][1] == n.row){#Does the cell span entire column?
		
		#create the header cell name
		header.name.table <- append(header.name.table, header.names[[row]][1], length(header.name.table))
		
		#expand along columnes
		length.col <- header.colspans[[row]][1]
		header.colspans[[row]] <- append(header.colspans[[row]], rep(1, length.col - 1), 1)
		header.rowspans[[row]] <- append(header.rowspans[[row]], rep(header.rowspans[[row]][1], length.col - 1), 1)
		header.names[[row]] <- append(header.names[[row]], rep(header.names[[row]][1], length.col - 1), 1)
		
		#remove the just introduced cell infos
		header.colspans[[row]] <- header.colspans[[row]][-1] #check for colspans different
		header.rowspans[[row]] <- header.rowspans[[row]][-1] #check for colspans different
		header.names[[row]] <- header.names[[row]][-1]
		
		} else{
			
			#Take a look at colspans
			#times <- header.colspans[[row]][1]
			name <- header.names[[1]][1]
			keep.on <- TRUE			
			
			for(level in 1:n.row) {
				
				#expand along columnes
				length.col <- header.colspans[[level]][1]
				length.row <- header.rowspans[[level]][1]
				
				header.colspans[[level]] <- append(header.colspans[[level]], rep(1, length.col - 1), 1)
				header.rowspans[[level]] <- append(header.rowspans[[level]], rep(header.rowspans[[row]][1], length.col - 1), 1) # replace header.rowspans[[row]][1] by 1
				header.names[[level]] <- append(header.names[[level]], rep(header.names[[level]][1], length.col - 1), 1)
				
				if(level > 1) {name <- paste(name, header.names[[level]][1], sep = headerSep[level])}
								
				#remove the just introduced cell infos
				header.colspans[[level]] <- header.colspans[[level]][-1] #check for colspans different
				header.rowspans[[level]] <- header.rowspans[[level]][-1] #check for colspans different
				header.names[[level]] <- header.names[[level]][-1]
				}
				
				header.name.table <- append(header.name.table, name, length(header.name.table))				
				}
				
			if(length(header.names[[1]]) == 0) {break}
			
			}
		return(header.name.table)				
}
				


#Consecutive sequence?
consecutive.seq <- function(index){
	result <- rle(diff(numbers))
	any(result$lengths>=2 & result$values==1)
}




#Main Function
HTMLtable <- function(table.node, which = NULL, tableSplit = NULL, header = NULL, headerSep = c(" >1> ", " >2> ", " >3> ", " >4> "), body = NULL, elFun = function(node)xmlValue(node), colClasses = NULL, as.data.frame = TRUE, ...){ #followFootnotes
  
  #Check if table xpath specified
  if(is.character(which)){table.node <-getNodeSet(table.node, which)}
	
# ################
# # TABLE SPLIT
# if(is.character(tableSplit)){	
	# table.node <- split_table(table.node)
# }

# table <- list()


# for(i in 1:length(table.node)){

#####################
#HEADER	

#Retrieve Head Elements
head <- get_head(table.node, header = header)

# Header Position
#header.position <- get_header_position(header = header, header.)

header.colspans <- get_colspans(head, tag = "td | th") #colspans for relevant row values
header.rowspans <- get_rowspans(head, tag = "td | th") #rowspans so far ignored
header.names <- get_cell_element(head, tag = "td | th", elFun = function(node)xmlValue(node))

#Span head
header.name.table <- span_header(header.names, header.colspans, header.rowspans, headerSep = headerSep)



#####################
#BODY

#Get Body Cell Nodes
cells <- get_cells(table.node, body = body) #change header to header.location

#Extract and transform body cell elements
vals <- get_cell_element(cells, elFun = elFun)

#Produce rowspans and colspans lists from body cell 
body.rowspans <- get_rowspans(cells)
body.colspans <- get_colspans(cells)	

#Produce table body 
tab <- span_body(vals, colspans = body.colspans, rowspans = body.rowspans) #used to be "span.table"



#####################
#FINISH

#Give Column Names
colnames(tab) <- header.name.table

tab <- as.data.frame(tab, stringsAsFactors = F, ...)


return(tab)
}