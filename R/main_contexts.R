#' Main
#' @description A function that executes the whole workflow of creating/updating gh-pages content from a data model.
#' @param portal a string indicating the abbreviation of the portal: 'ark', 'veoibd'
#' @param template_dir a string specifying the subdir where template files are stored, default = "model_templates"
#' @param branch a OPTIONAL string indicating subdir to which the model main branch has been downloaded to, default = "./"
#' @return NULL
#' @importFrom rlang .data
#' @export

main_contexts <- function(portal,
                          template_dir = "model_templates",
                          branch = "./") {
  # config repo space
  configure_space()

  # download latest version of all_attributes.csv
  fid <- file.path(branch, paste0(portal, ".all_attributes.csv", sep = ""))
  all <- read.csv(fid)
  # create/update metadata attribute content
  makeAttributeContent(all)

  # create/update metadata collection template content
  contexts_path = file.path(branch, "model_contexts")
  fids <- list.files(contexts_path, full.names = TRUE, recursive = TRUE)
  fids <- fids[grepl("model\\.csv$", fids)]
  models <- purrr::map(fids, read.csv)
  # process each context model separately so that each template page renders context-specific details
  purrr::walk(models, function(model, template_dir_path, portal) {
    makeTemplateContent(model, template_dir_path, portal)
  }, template_dir_path = file.path(branch, template_dir), portal)

  ## archive content that is not longer defined in the data model
  models <- dplyr::bind_rows(models)
  models <- unique(models)
  archive_content(models)
}
