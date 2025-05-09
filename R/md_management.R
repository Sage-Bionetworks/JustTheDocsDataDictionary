#' write md yaml header
#' @param title data model attribute string
#' @param parent class of markdown file that is being created, options c('Metadata Templates', 'Attributes')
#' @return character vector of yaml header content
get_yaml_header <- function(title, parent) {
  title_snake <- get_title_snake(title)
  docs_dir <- stringr::str_replace_all(parent, " ", "_")
  docs_dir <- stringr::str_to_lower(docs_dir)

  header <- c("---",
             glue::glue("title: {title}"),
             glue::glue("parent: {parent}"),
             "datatable: true",
             "layout: page",
             glue::glue("permalink: docs/{docs_dir}/{title_snake}.html"),
             glue::glue("date: {lubridate::as_date(Sys.time(), tz = 'UTC')}"),
             "---")
  header <- paste(header, collapse = "\n")
  return(header)
}

#'
content_md <- function(attr, desc, vals_note = FALSE, title = NULL) {
  if (desc == ""){
    desc = "Content TBD"
  }

  fid <- glue::glue("_includes/content/{attr}.md")
  md_lines <- glue::glue("# {attr}")
  if (!is.null(title)) {
    md_lines <- glue::glue("# {title}")
  }

  if (!file.exists(fid)) {
    md_lines <- c(md_lines, desc)
    if (vals_note){
      md_lines <- c(md_lines, "\n",
                    "{: .note }",
                    "There are no defined valid values for this model attribute.")
    }
  } else {
    # replace existing description text with what's in the model just in case there have
    # been any changes
    md_lines <- readLines(fid)
    md_lines[2] <- desc
  }
  writeLines(md_lines, con = fid)
}
