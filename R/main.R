#' Main
#' @description A function that executes the whole workflow of creating/updating gh-pages content from a data model.
#' @param data_model_url a string indicating the https://raw.githubusercontent.com URL of the data model csv to be used.
#' @return NULL
#' @export

main <- function(data_model_url) {
  # config subdirs
  purrr::walk(c("_includes/content/",
                "_data/csv/attributes/",
                "_data/csv/metadata_templates/",
                "docs/metadata_templates/",
                "docs/attributes/"),
              make_subdir)

  # write parent markdown files
  header <- templates_md()
  writeLines(header, con = "docs/metadata_templates/metadata_templates.md", sep = "\n")

  header <- attributes_md()
  writeLines(header, con = "docs/attributes/attributes.md", sep = "\n")

  # download latest version of data model
  model <- read.csv(data_model_url)

  # remove mock templates
  model <- dplyr::filter(model, !grepl("mock|test ", Attribute, ignore.case = TRUE))

  ## archive content for attributes no longer in the model

  # create/update metadata collection template content
  makeTemplateContent(model)

  # create/update metadata attribute content
  makeAttributeContent(model)

}

#' Make content detailing metadata attributes in the data model
#' @description A function that executes a series of steps to create/update content detailing metadata attributes.
#' @param model a data.frame object containing the data model.
#' @export
makeAttributeContent <- function(model) {
  # get df of metadata attributes with nav_order rank added
  model_attributes <- selectMetadataAttributes(model)
  # add column to df for content_md vals_note param
  model_attributes <- dplyr::mutate(model_attributes,
                                    note = ifelse(Valid.Values == "",
                                                  TRUE, FALSE))
  # note TRUE -> no valid values, note FALSE -> valid values

  # df with attributes with valid values
  model_valid_val <- dplyr::filter(model_attributes, Valid.Values != "")

  #### create csv detailing all valid values for a given attribute _data/csv/attributes/
  purrr::walk2(model_valid_val$Attribute, model_valid_val$Valid.Values,
               function(attr, vals) {
                 # build tibble of all valid vals for this attribute
                 vals <- unlist(stringr::str_split(vals, ", "))
                 vals <- sort(vals)
                 out <- dplyr::tibble('Valid Values' = as.character(vals))

                 # check for existing definitions
                 fid <- glue::glue("_data/csv/attributes/{attr}.csv")
                 if (file.exists(fid)) {
                   pre <- read.csv(fid, colClasses = rep("character", 3))
                   pre <- dplyr::tibble(pre)
                   colnames(pre) <- c("Valid Values", "Description", "Source")
                   # add any existing definitions to out tibble
                   out <- dplyr::left_join(out, pre, by = "Valid Values")
                 } else {
                   out$Description <- NA
                   out$Source <- NA
                 }
                 out <- dplyr::arrange(out, `Valid Values`)
                 out <- unique(out)
                 fid <- glue::glue("_data/csv/attributes/{attr}.csv")
                 write_model_csv(out, fid)
               })

  # make or update _includes/content/md file for each attribute
  df <- dplyr::select(model_attributes, Attribute, Description, note)
  purrr::pwalk(df, function(Attribute, Description, note) {
                 content_md(attr = Attribute,
                            desc = Description,
                            vals_note = note,
                            title = Attribute)
               })

  ### make markdown file for attributes with valid values
  purrr::pwalk(dplyr::select(model_attributes, Attribute, Description, rank, note),
               function(Attribute, Description, rank, note){
                 yaml_header <- get_yaml_header(title = Attribute,
                                                parent = "Attributes",
                                                nav_order = rank)
                 content <- c(paste(c("{% assign mydata=site.data.csv.attributes.",
                                      Attribute, " %}"), collapse = ""),
                              paste(c("{% include content/", Attribute, ".md %}"), collapse = ""))
                 # write md file
                 fid <- glue::glue("docs/attributes/{Attribute}.md")
                 if (note) { # note TRUE -> no valid values
                   # if TRUE there are NO valid values, don't need javascript dataTable
                   writeLines(c(yaml_header, content[2]), con = fid, sep = "\n")
                 } else {
                   #note FALSE -> valid values, need javascript dataTable
                   writeLines(c(yaml_header, content, attribute_myTable), con = fid, sep = "\n")
                 }
               })
}

#' Make metadata collection template content
#' @description A function that executes a series of steps to create/update metadata collection template pages.
#' @param model a data.frame object containing the data model.
#' @export
makeTemplateContent <- function(model) {
  # select all rows that define templates for metadata collection
  model_templates <- selectMetadataTemplates(model)

  # make or update _includes/content/md file for each template
  purrr::walk2(model_templates$Attribute,
               model_templates$Description,
               function(attribute, description) {
                 content_md(attr = get_title_snake(attribute),
                            desc = description,
                            vals_note = FALSE,
                            title = attribute)
               })

  # add column to df for title_snakecase
  model_templates$title_snake <- unlist(purrr::map(model_templates$Attribute,
                                                   get_title_snake))

  # create csv detailing each metadata template
  purrr::walk2(model_templates$title_snake, model_templates$DependsOn,
               function(title_snake, depends, df) {
                 depends <- unlist(strsplit(depends, ", "))
                 out <- dplyr::filter(df, Attribute %in% depends)
                 out$Attribute <- factor(out$Attribute, levels = depends)
                 out <- dplyr::select(out, Attribute, Description, Required, Valid.Values)
                 out <- dplyr::arrange(out, Attribute)
                 fid = glue::glue("_data/csv/metadata_templates/{title_snake}.csv")
                 write_model_csv(out, fid)
               }, df = model)

  ### write md page for each template to docs/metadata_templates/
  purrr::pwalk(dplyr::select(model_templates, Attribute, Description, title_snake),
               function(Attribute, Description, title_snake){
                 yaml_header <- get_yaml_header(title = Attribute,
                                                parent = "Metadata Templates")
                 content <- c(paste(c("{% assign mydata=site.data.csv.metadata_templates.",
                              title_snake, " %}"), collapse = ""),
                              paste(c("{% include content/", title_snake, ".md %}"), collapse = ""))
                 #content <- paste(content, collapse = "\n")

                 fid <- glue::glue("docs/metadata_templates/{title_snake}.md")
                 writeLines(c(yaml_header, content, template_myTable), con = fid, sep = "\n")
               })
}
