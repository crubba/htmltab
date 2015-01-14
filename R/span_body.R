#' Creates body using span information
#'
#' @param vals a list of body cell values
#' @param colspans a list of body colspans
#' @param rowspans a list of body rowspans
#' @return a matrix of the assembled body
span_body <- function(vals, colspans, rowspans) {

  #Remove rows which have all empty cells
  empty.rows <- which(sapply(vals, function(x) all(x == "")))
  if(!is_empty(empty.rows)){
    vals <- vals[-empty.rows]
    colspans <- colspans[-empty.rows]
    rowspans <- rowspans[-empty.rows]
  }

  #span body
  n.cols <- sum(as.numeric(unlist(colspans[[1L]]))) #get idea from first row column length, should be right usually
  n.rows <- length(rowspans) #guess, better make flexible #lapply(rowspans, function(x) x[1]) %>% unlist %>% as.numeric %>% sum

  #Create empty Matrix
  mat <- matrix(NA, ncol = n.cols, nrow = n.rows)
  col = 1L

  while(col <= n.cols){ # col start

    row <- 1L

    while(row <= n.rows){ # row start

      col.span.length <- colspans[[row]][col]
      row.span.length <- rowspans[[row]][col]
      cel.val <- vals[[row]][col]

      #This block controls for undefined col- and rowspans (hacky)
      if(is.na(row.span.length) && row < n.rows){
        col.span.length <- 1L
        row.span.length <- 1L
        cel.val <- mat[row, col -1L]
      }

      if(row.span.length < 2L) {
        mat[row, col] <- cel.val
      } else {
        if(row != n.rows){ #Control for situation: specified rows (main), last row demands col/rowspans
          index <- (row + 1L) : (row + row.span.length - 1L)

          for(counter in index){
            vals[[counter]] <- append(vals[[counter]], cel.val, (col - 1L )) #append col val is after
            rowspans[[counter]] <- append(rowspans[[counter]], 1L, (col - 1L))
            colspans[[counter]] <- append(colspans[[counter]], 1L, (col - 1L))
          }
          rowspans[[row]] <- rowspans[[row]][-col]
          rowspans[[row]] <- append(rowspans[[row]], 1L, (col-1L))

          mat[row:(row + row.span.length-1L), col] <- cel.val
        }
      }

      if(col.span.length > 1L){
        vals[[row]] <- append(vals[[row]], rep(cel.val, (col.span.length - 1L)), col) #append
        colspans[[row]] <- colspans[[row]][-col]
        colspans[[row]] <- append(colspans[[row]], rep(1, col.span.length), (col-1L))
        rowspans[[row]] <- append(rowspans[[row]], rep(1, col.span.length), col)
      } else{}

      row <- row + 1L #row <- row + row.span.length
    } #row end

    col <- col + 1L
  } #col end

  return(mat)

} #function end
