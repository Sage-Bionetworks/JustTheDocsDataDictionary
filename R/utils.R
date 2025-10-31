#' inverse the logical boolean response of %in%
#' @noRd
`%notin%` <- Negate(`%in%`)

#' make necessary subdirs and parent md files needed to create site content
#' @noRd
configure_space <- function() {
  # config subdirs
  purrr::walk(c("_includes/content/",
                "_data/csv/attributes/",
                "_data/csv/metadata_templates/",
                "docs/metadata_templates/",
                "docs/attributes/"),
              make_subdir)
  
  # write parent markdown files
  header <- templates_md()
  writeLines(header, con = "docs/metadata_templates/metadata_templates.md", sep = "\n")
  
  header <- attributes_md()
  writeLines(header, con = "docs/attributes/attributes.md", sep = "\n")
}

#' make a directory if it does not exist
#' @param d a string indicating the name of the directory to make
#' @noRd
make_subdir <- function(d) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

#' Return all rows in model that define metadata templates
#' @description A util function that selects all rows in the data model that define metadata templates.
#' @param model a data.frame object containing the data model.
#' @return a subset of `model` that contains all rows that define metadata templates.
#' @importFrom rlang .data
selectMetadataTemplates <- function(model) {
  dplyr::filter(model, grepl("template", .data$Attribute, ignore.case = TRUE) |
    grepl("^Component", .data$DependsOn))
}

#' Return character vector of all valid value strings defined in the data model
#' @description A util function that parses all valid values from a data model
#' @param model a data.frame object containing the data model
#' @return a character vector of all valid values defined in the data model
#' @importFrom rlang .data
get_validVals <- function(model){
  temp <- dplyr::filter(model, !grepl("^$", .data$Valid.Values) & !is.na(.data$Valid.Values))
  valid_vals <- purrr::map(temp$Valid.Values, function(d){
    unlist(strsplit(d, ", "))
  })
  valid_vals <- unique(unlist(valid_vals))
}

#' Return all rows in model that define a model attribute that isn't a template
#' @description A util function that selects all rows in the data model that define metadata attributes
#' @param model a data.frame object containing the data model
#' @return a subset of `model` that contains all rows that define metadata attributes with `rank` column for ordering attribute md pages on sidebar.
#' @importFrom rlang .data
selectMetadataAttributes <- function(model) {
  # DependsOn defined templates and conditionally required relationships
  model_attributes <- dplyr::filter(model, .data$DependsOn == "")

  # order alphabetically and add nav_order rank
  model_attributes$rank <- stringr::str_to_lower(model_attributes$Attribute)
  model_attributes <- dplyr::arrange(model_attributes, .data$rank)
  model_attributes$rank <- 1:nrow(model_attributes)

  return(model_attributes)
}

#' convert a Attribute string to desired snake case syntax
#' @param x a string indicating the attribute string
#' @return a string with the attribute name in snake case
get_title_snake <- function(x) {
  title_snake <- snakecase::to_snake_case(x)
  title_snake <- stringr::str_replace(title_snake, "sc_rna_seq", "scrnaseq")
  return(title_snake)
}

#' write csv in desired format
#' @param df a data.frame object to be written to csv
#' @param fid a string indicating the filename to write to
write_model_csv <- function(df, fid) {
  df[is.na(df)] <- ""
  colnames(df) <- stringr::str_replace_all(colnames(df), "\\.", " ")
  write.csv(df, file = fid, quote = TRUE, row.names = FALSE, na = "")
}
