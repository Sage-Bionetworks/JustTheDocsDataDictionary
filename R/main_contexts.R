#' Main
#' @description A function that executes the whole workflow of creating/updating gh-pages content from a data model.
#' @param data_model_url a string indicating the https://raw.githubusercontent.com URL of the data model csv to be used.
#' @return NULL
#' @importFrom rlang .data
#' @export

main_contexts <- function(repo_raw_url) {
  # config repo space
  configure_space()
  
  # download latest version of all_attributes.csv
  url <- glue::glue("{repo_raw_url}/ark.all_attributes.csv")
  all <- read.csv(url)
  # create/update metadata attribute content
  makeAttributeContent(all)

  # remove mock templates
  model <- dplyr::filter(model, !grepl("mock|test ", .data$Attribute, ignore.case = TRUE))

  ## archive content for attributes no longer in the model
  archive_content(model)

  # create/update metadata collection template content
  makeTemplateContent(model)
}
