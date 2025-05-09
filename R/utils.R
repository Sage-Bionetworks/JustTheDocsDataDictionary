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
selectMetadataTemplates <- function(model) {
  dplyr::filter(model, grepl("template", Attribute, ignore.case = TRUE) |
    grepl("^Component", DependsOn))
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
