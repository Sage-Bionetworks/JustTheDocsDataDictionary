#' Main
#' @description A function that executes the whole workflow of creating/updating gh-pages content from a data model.
#' @param portal a string indicating the abbreviation of the portal: 'ark', 'veoibd'
#' @param template_dir a string specifying the subdir where template files are stored, default = "model_templates"
#' @param template_list a header-less txt file listing out all of the templates defined by the model
#' @param branch a OPTIONAL string indicating subdir to which the model main branch has been downloaded to, default = "./"
#' @return NULL
#' @importFrom rlang .data
#' @export

main_contexts <- function(portal, template_dir = "model_templates", template_list, branch = "./") {
  # config repo space
  configure_space()

  # download latest version of all_attributes.csv
  all <- read.csv(all_attr_url)
  # create/update metadata attribute content
  makeAttributeContent(all)

  # create/update metadata collection template content
  fids <- list.files(file.path(branch, "model_contexts"), full.names = TRUE, recursive = TRUE)
  fids <- fids[grepl("model\\.csv$", fids)]
  models <- purrr::map(fids, read.csv)
  # process each context model separately so that each template page renders context-specific details
  purrr::walk(models, function(model, template_dir_path) {
    makeTemplateContent(model, template_dir)
  }, template_dir_path = template_dir)

  ## archive content that is not longer defined in the data model
  archive_content("???")
}
