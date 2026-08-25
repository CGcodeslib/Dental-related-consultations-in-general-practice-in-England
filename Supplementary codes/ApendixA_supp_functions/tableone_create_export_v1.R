# Create and export table one
# Author: CG

####################################

table1 <- function(data,
                   vars,
                   strata = NULL,
                   factorVars = NULL,
                   nonnormal = NULL,
                   addOverall = FALSE,
                   showAllLevels = TRUE,
                   quote = TRUE,
                   noSpaces = TRUE,
                   directory = NULL,
                   filename = NULL) {
  
  # Load required packages
  library(tableone)
  library(writexl)
  
  # Check that all specified columns exist
  missing_vars <- setdiff(unique(c(vars, strata, factorVars)), names(data))
  if (length(missing_vars)) stop("These columns are missing in `data`: ", paste(missing_vars, collapse = ", "))
  
  # Build the TableOne object
  tbl <- CreateTableOne(
    vars       = vars,
    strata     = strata,
    data       = data,
    factorVars = factorVars,
    addOverall = addOverall
  )
  
  # Convert to data frame
  tbl_df <- as.data.frame(
    print(tbl,
          showAllLevels = showAllLevels,
          nonnormal     = nonnormal,
          quote         = F,
          noSpaces      = noSpaces)
  )
  
  # Add row names as first column
  tbl_df <- cbind(RowName = rownames(tbl_df), tbl_df, row.names = NULL)
  
  # Optional export to Excel
  out_path <- NULL
  if (!is.null(directory) && !is.null(filename)) {
    if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(directory, filename)
    write_xlsx(tbl_df, out_path)
  }
  
  # Return the results
  return(list(tableone_object = tbl, table_df = tbl_df, path = out_path))
}



