
#Create header with spans
span_header <- function(header.names, header.colspans, header.rowspans, headerSep) {

  header.name.table <- vector()

  col <- 1
  n.row <- length(header.names) #How many rows has the table? Maybe wrong under some circumstances when last row has rowspans > 2

  repeat{#tests if misspecified table rows

    if(length(header.names[[2]]) == 0){break} #test for misspecified tables

    if(header.rowspans[[1]][1] == n.row){ #Does the cell span entire column?

      #create the header cell name
      header.name.table <- append(header.name.table, header.names[[1]][1], length(header.name.table))

      #expand along columnes
      length.col <- header.colspans[[1]][1]
      header.colspans[[1]] <- append(header.colspans[[1]], rep(1, length.col - 1), 1)
      header.rowspans[[1]] <- append(header.rowspans[[1]], rep(header.rowspans[[1]][1], length.col - 1), 1)
      add.cols <- rep(header.names[[1]][1], length.col - 1) #paste(header.names[[1]][1], 1:length.col - 1, sep = ".")
      header.names[[1]] <- append(header.names[[1]], add.cols, 1)

      #remove the just introduced cell infos
      header.colspans[[1]] <- header.colspans[[1]][-1] #check for colspans different
      header.rowspans[[1]] <- header.rowspans[[1]][-1] #check for colspans different
      header.names[[1]] <- header.names[[1]][-1]

    } else{

      #Take a look at colspans
      #times <- header.colspans[[1]][1]
      name <- header.names[[1]][1]
      keep.on <- TRUE

      for(level in 1:n.row) {#start at 1, correct?

        #expand along columnes
        length.col <- header.colspans[[level]][1]
        length.row <- header.rowspans[[level]][1]

        header.colspans[[level]] <- append(header.colspans[[level]], rep(1, length.col - 1), 1)
        header.rowspans[[level]] <- append(header.rowspans[[level]], rep(header.rowspans[[1]][1], length.col - 1), 1) # replace header.rowspans[[row]][1] by 1
        header.names[[level]] <- append(header.names[[level]], rep(header.names[[level]][1], length.col - 1), 1)

        if(level > 1) {name <- paste(name, header.names[[level]][1], sep = headerSep)}

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
