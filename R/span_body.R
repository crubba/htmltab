#Main assembly function
span_body <- function(vals, colspans, rowspans) {

  n.cols <- sum(as.numeric(unlist(colspans[[1]]))) #get idea from first row column length, should be right usually
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
