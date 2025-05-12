attribute_myTable <- readLines("attribute_myTable")
#usethis::use_data(attribute_myTable, internal = TRUE, overwrite = TRUE)
save(attribute_myTable, file = "../data/attribute_myTable.rda")
