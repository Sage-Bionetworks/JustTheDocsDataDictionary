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

  # download latest version of data model
  model <- read.csv(data_model_url)

  # remove mock templates
  model <- dplyr::filter(model, !grepl("mock|test ", Attribute, ignore.case = TRUE))

  ## archive content for attributes no longer in the model

  # process template content
  makeTemplateContent(model)

  # process valid values content

  # process attribute content
}

#' Make template content
#' @description A function that executes a series of steps to create/update template pages.
#' @param model a data.frame object containing the data model.
#' @return data.frame containing all data model rows that define metadata templates. This is used by TBD to ensure templates are not included in subset of metadata attributes.
#' @export

makeTemplateContent <- function(model) {
  # select all rows that define templates for metadata collection
  model_templates <- selectMetadataTemplates(model)

  # make or update _includes/content/md file for each attribute
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
  #purrr::pwalk(select(model_templates, Attribute, DependsOn, Description, title_snake),
  #             function(Attribute, DependsOn, Description, title_snake){
                 #depends <- str_replace_all(DependsOn, ", ", "', '")
                 #depends <- glue::glue("'{depends}'")
  purrr::pwalk(dplyr::select(model_templates, Attribute, Description, title_snake),
               function(Attribute, Description, title_snake){
                 yaml_header <- get_yaml_header(title = Attribute,
                                                parent = "Metadata Templates")
                 content <- c(paste(c("{% assign mydata=site.data.csv.metadata_templates.",
                              title_snake, " %}"), collapse = ""),
                              paste(c("{% include content/", title_snake, ".html %}"), collapse = ""))
                 #content <- paste(content, collapse = "\n")

                 fid <- glue::glue("docs/metadata_templates/{title_snake}.md")
                 writeLines(c(yaml_header, content, template_myTable), con = fid, sep = "\n")
               })


}
