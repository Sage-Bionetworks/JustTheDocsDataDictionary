#' write md yaml header
#' @description A function that creates a yaml header for the Attributes parent markdown file.
#' @noRd
attributes_md <- function() {
  header <- get_yaml_header(title = "Attributes",
                            parent = NULL,
                            nav_order = 3)
  return(header)
}

#' write md yaml header
#' @description A function that creates a yaml header for the Metadata Templates parent markdown file.
#' @noRd
templates_md <- function() {
  header <- get_yaml_header(title = "Metadata Templates",
                            parent = NULL,
                            nav_order = 2)
  return(header)
}

#' Quick string transform for html string
#' @param x the string to transform
#' @return a string
#' @noRd
get_docs_dir <- function(x) {
  docs_dir <- stringr::str_replace_all(x, " ", "_")
  docs_dir <- stringr::str_to_lower(docs_dir)
  return(docs_dir)
}

#' write md yaml header
#' @param title data model attribute string
#' @param parent class of markdown file that is being created, options c('Metadata Templates', 'Attributes')
#' @param nav_order default NULL, else specify an integer to add to the yaml header to control display order on site
#' @return A multi-line string detailing the markdown yaml header content
get_yaml_header <- function(title, parent = NULL, nav_order = NULL) {
  title_snake <- get_title_snake(title)
  if (is.null(parent)) {
    #docs_dir <- get_docs_dir(title)
    docs_dir <- get_title_snake(title)
  } else {
    #docs_dir <- get_docs_dir(parent)
    docs_dir <- get_title_snake(parent)
  }

  header <- c("---",
             glue::glue("title: {title}"),
             ifelse(is.null(parent),
                    "has_children: true",
                    glue::glue("parent: {parent}")),
             ifelse(is.null(parent), NA, "datatable: true"),
             "layout: page",
             ifelse(is.null(parent),
                    glue::glue("permalink: docs/{docs_dir}/{stringr::str_replace_all(title, ' ', '_')}.html"),
                    ifelse(parent == "Attributes",
                           glue::glue("permalink: docs/{docs_dir}/{title}.html"),
                           glue::glue("permalink: docs/{docs_dir}/{title_snake}.html"))),
             glue::glue("date: {lubridate::as_date(Sys.time(), tz = 'UTC')}"),
             "---")
  header <- header[!is.na(header)]

  if (!is.null(nav_order)) {
    header <- paste(c(header[1:5],
                      glue::glue("nav_order: {nav_order}"),
                      header[6:length(header)]),
                    collapse = "\n")
  } else {
    header <- paste(header, collapse = "\n")
  }
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
