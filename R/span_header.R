
#Create header with spans
span_header <- function(header.names, header.colspans, header.rowspans, headerSep) {

  header.name.table <- vector()

  col <- 1
  n.row <- length(header.names) #How many rows has the table? Maybe wrong under some circumstances

  repeat{ #column loop, change to while??

    row <- 1 #kann weg, ersetze row durch 1

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
