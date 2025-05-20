#' archive content for attributes no longer in the model
#' @description Main function for archiving content for attributes no longer included in the data model
#' @param model a data.frame object containing the data model.
#' @return NULL
#' @importFrom rlang .data
archive_content <- function(model){
  ## get catalog of existing md files
  md_catalog <- get_md_cat()
  # ignore parent md files
  md_catalog <- dplyr::filter(md_catalog, .data$Attribute %notin% c("attributes", "metadata_templates"))

  # prep for finding files to archive
  model_templates <- selectMetadataTemplates(model)
  template_str <- unlist(purrr::map(model_templates$Attribute, get_title_snake))
  # select those attributes/templates no longer in model
  md_catalog <- dplyr::filter(md_catalog, .data$Attribute %notin% c(model$Attribute, template_str))

  # archive files that remain in md_catalog
  if (nrow(md_catalog) > 0) {
    purrr::walk(c(".archived/", ".archived/_includes/",
                  ".archived/_includes/content/",
                  ".archived/docs/",
                  ".archived/docs/metadata_templates/",
                  ".archived/docs/attributes/"),
                make_subdir)
    purrr::walk(md_catalog$full_name, archive_md)
  } else {
    message("No files to archive")
  }
}

#' Get catalog of markdown files
#' @description This utils function returns a data frame with the full path and name of all markdown files in the specified directories.
#' @return data.frame with columns: full_name, Attribute
#' @noRd
get_md_cat <- function(){
  md_dirs <- c("_includes/content/",
               "docs/metadata_templates/",
               "docs/attributes/")
  md_catalog <- purrr::map(md_dirs, function(dir) {
    out <- data.frame(full_name = list.files(dir, full.names = TRUE))
    return(out)
  })
  md_catalog <- dplyr::bind_rows(md_catalog)
  md_catalog$Attribute <- unlist(purrr::map(md_catalog$full_name,
                                            function(fid) {
                                              fid <- basename(fid)
                                              fid <- stringr::str_remove(fid, pattern = "\\.md")
                                              return(fid)
                                            }))
  return(md_catalog)
}

#' Move md file to archive location
#' @description This utils function moves a markdown file to the corresponding archive location.
#' @param fid a string representing the full path and name of the markdown file to be archived.
#' @return NULL
#' @noRd
archive_md <- function(fid) {
  message(glue::glue("Archiving {fid}"))
  file.rename(from = fid, to = glue::glue(".archived/{fid}"))
}
