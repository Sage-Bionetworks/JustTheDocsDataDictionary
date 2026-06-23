#' archive content for attributes no longer in the model
#' @description Main function for archiving content for attributes no longer included in the data model
#' @param model a data.frame object containing all data model content.
#' @return NULL
#' @importFrom rlang .data
archive_content <- function(model){
  ## get catalog of existing md and _data/csv files
  content_catalog <- get_content_cat()
  # ignore parent md files
  content_catalog <- dplyr::filter(content_catalog, .data$Attribute %notin% c("attributes", "metadata_templates"))

  # prep for finding files to archive
  model_templates <- selectMetadataTemplates(model)
  template_str <- unlist(purrr::map(model_templates$Attribute, get_title_snake))
  # build character vector of all attributes/templates defined in a model
  ref <- c(unique(model$Attribute), template_str)
  # select md content for attr/templates no longer in model
  content_catalog <- dplyr::filter(content_catalog, .data$Attribute %notin% ref)

  # archive files that remain in content_catalog
  if (nrow(content_catalog) > 0) {
    purrr::walk(c(".archived/", ".archived/_includes/",
                  ".archived/_includes/content/",
                  ".archived/docs/",
                  ".archived/docs/metadata_templates/",
                  ".archived/docs/attributes/",
                  ".archived/_data/csv/attributes/",
                  ".archived/_data/csv/metadata_templates/"),
                make_subdir)
    purrr::walk(content_catalog$full_name, archive_file)
  } else {
    message("No files to archive")
  }
}

#' Get catalog of markdown files
#' @description This utils function returns a data frame with the full path and name of all markdown files in the specified directories.
#' @return data.frame with columns: full_name, Attribute
#' @noRd
get_content_cat <- function(){
  content_dirs <- c("_includes/content/",
               "docs/metadata_templates/",
               "docs/attributes/",
               "_data/csv/attributes/",
               "_data/csv/metadata_templates/")
  content_catalog <- purrr::map(content_dirs, function(dir) {
    out <- data.frame(full_name = list.files(dir, full.names = TRUE))
    return(out)
  })
  content_catalog <- dplyr::bind_rows(content_catalog)
  content_catalog$Attribute <- unlist(purrr::map(content_catalog$full_name,
                                            function(fid) {
                                              fid <- basename(fid)
                                              fid <- stringr::str_remove_all(fid, pattern = "\\.md|\\.csv")
                                              return(fid)
                                            }))
  return(content_catalog)
}

#' Move content file to archive location
#' @description This utils function moves a markdown file to the corresponding archive location.
#' @param fid a string representing the full path and name of the markdown file to be archived.
#' @return NULL
#' @noRd
archive_file <- function(fid) {
  message(glue::glue("Archiving {fid}"))
  file.rename(from = fid, to = glue::glue(".archived/{fid}"))
}
