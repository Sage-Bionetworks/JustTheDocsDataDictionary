
# jsonschema2csv
# This script converts JSON Schema files into CSV format
jsonschema2csv <- function(json_schema, csv_file) {
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  
  # Read the JSON schema
  schema <- fromJSON(json_schema, flatten = TRUE)
  
  # Extract properties
  properties <- schema$properties
  property_titles <- unlist(lapply(properties, function(x) {return(x$title)}))
  
  # Convert properties to a data frame
  df <- as.data.frame(matrix(ncol = length(property_titles), nrow = 0))
  colnames(df) <- property_titles
  
  # Write to CSV
  write.csv(df, file = csv_file, row.names = FALSE)
}