#' dfba_table_gamma
#
#' Plots cross-tabulated gamma data
#'
#' @param x a dfba_gamma object
#' @param row.label Row header for cross-tabulation (default is "x")
#' @param column.label Column header for cross-tabulation (default is "y")
#'
#' @return Table
#'

## Function to format two (raw) vectors as a gamma table

#' @export
dfba_table_gamma<-function(x,
                           row.label="X",
                           column.label="Y"){
  table(x$table.row,
        x$table.column,
        dnn = c(row.label,
                column.label)
  )
}
