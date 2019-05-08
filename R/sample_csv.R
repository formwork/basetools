#' Sample lines from csv
#'
#' @param file a csv file to sample
#' @param prob probability of reading a line, defaults to 1/1000
#' @param header does file have a header? Defaults to TRUE
#'
#' @keywords sample lines
#' @return a data frame
#' @export
sample_csv <- function(file, prob = 1/1000, header = T){

  conn <- file(file,  "r")
  buff <- vector("list")
  indx <- 0

  if (header){
    indx <- indx + 1
    buff[[indx]] <- readLines(conn, n = 1)
  }

  while(TRUE){
    inrec <- readLines(conn, n = 10000)
    n_lin <- length(inrec)
    if (n_lin == 0) break

    rand <- stats::runif(n_lin)
    indx <- indx + 1
    buff[[indx]] <- inrec[rand <= prob]
    if (n_lin < 10000) break
  }

  close(conn)
  return(utils::read.csv(textConnection(unlist(buff)),
                         stringsAsFactors = FALSE,
                         header = header))
}
