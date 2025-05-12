#' inverse the logical boolean response of %in%
#' @noRd
`%notin%` <- Negate(`%in%`)

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
selectMetadataTemplates <- function(model) {
  dplyr::filter(model, grepl("template", Attribute, ignore.case = TRUE) |
    grepl("^Component", DependsOn))
}

#' Return character vector of all valid value strings defined in the data model
#' @description A util function that parses all valid values from a data model
#' @param model a data.frame object containing the data model
#' @return a character vector of all valid values defined in the data model
get_validVals <- function(df){
  temp <- dplyr::filter(df, !grepl("^$", Valid.Values) & !is.na(Valid.Values))
  valid_vals <- purrr::map(temp$Valid.Values, function(d){
    unlist(strsplit(d, ", "))
  })
  valid_vals <- unique(unlist(valid_vals))
}

#' Return all rows in model that define a model attribute that isn't a template
#' @description A util function that selects all rows in the data model that define metadata attributes
#' @param model a data.frame object containing the data model
#' @return a subset of `model` that contains all rows that define metadata attributes with `rank` column for ordering attribute md pages on sidebar.
selectMetadataAttributes <- function(model) {
  # prep
  model_templates <- selectMetadataTemplates(model)

  ### select rows in model for attributes
  # first remove all rows that define templates
  model_attributes <- dplyr::filter(model,
                                    Attribute %notin% model_templates$Attribute)
  # then remove rows that define a valid value based on presence of conditional dependency
  model_attributes <- dplyr::filter(model_attributes, DependsOn == "")

  # order alphabetically and add nav_order rank
  model_attributes$rank <- stringr::str_to_lower(model_attributes$Attribute)
  model_attributes <- dplyr::arrange(model_attributes, rank)
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
