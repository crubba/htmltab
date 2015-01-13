expand_header <- function(header.names, header.colspans, header.rowspans){

  header.name.table <- list()
  col <- 1
  n.row <- length(header.names)

  repeat{

    #Break when there are no header information or when last column is missspecified
    if(is_empty(header.names[[1]])){break} # || length(header.names) > 1 && length(header.names[[2]]) == 0

    header.row <- vector()

      for(row in 1:n.row) {

        length.col <- header.colspans[[row]][1]
        length.row <- header.rowspans[[row]][1]
        name <- header.names[[row]][1]
        name <- ifelse(name == "", NA, name)

        if(is.na(length.col)){break}

        #Remove cell info
        header.colspans[[row]] <- header.colspans[[row]][-1]
        header.rowspans[[row]] <- header.rowspans[[row]][-1]
        header.names[[row]] <- header.names[[row]][-1]

        #Expand along columnes
        header.colspans[[row]] <- append(header.colspans[[row]], rep(1, length.col - 1), 0)
        header.rowspans[[row]] <- append(header.rowspans[[row]], rep(length.row, length.col - 1), 0)
        header.names[[row]] <- append(header.names[[row]], rep(name, length.col - 1), 0)

        #Expand along rows
        these.rows <- row:(row + (length.row - 1))
        header.rowspans[these.rows] <- lapply(header.rowspans[these.rows], append, 1, after = 0) #an erster Stelle
        header.colspans[these.rows] <- lapply(header.colspans[these.rows], append, 1, after = 0)
        header.names[these.rows] <- lapply(header.names[these.rows], append, NA, after = 0)

        #remove the first colum info
        header.colspans[[row]] <- header.colspans[[row]][-1] #check for colspans different
        header.rowspans[[row]] <- header.rowspans[[row]][-1] #check for colspans different
        header.names[[row]] <- header.names[[row]][-1]

        #Add cell info to column name vector
        header.row <- c(header.row, name)
      }

    #Break if
    if(col > 1 && length(header.row) < length(header.name.table[[col-1]])){break}

    header.name.table[[col]] <- vector()
    header.name.table[[col]] <- header.row

    col <- col + 1
    }

  return(header.name.table)
}
