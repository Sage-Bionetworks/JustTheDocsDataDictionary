#' archive content for attributes no longer in the model

archive_content <- function(){
  md_catalog <- get_md_cat()
}


#' Get catalog of markdown files
#' @description This utils function returns a data frame with the full path and name of all markdown files in the specified directories.
#' @param NULL
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
  md_catalog <- bind_rows(md_catalog)
  md_catalog$Attribute <- unlist(purrr::map(md_catalog$full_name,
                                            function(fid) {
                                              basename(fid) %>% str_remove(pattern = "\\.md")
                                            }))
  return(md_catalog)
}
