#' Main
#' @description A function that executes the whole workflow of creating/updating gh-pages content from a data model.
#' @param portal a string indicating the data model portal abbreviation, e.g., <portal>.model.csv: 'ark', 'veoibd'
#' @param template_dir a string specifying the subdir where template files are stored, default = "model_templates"
#' @param template_list a header-less txt file listing out all of the templates defined by the model
#' @param branch a OPTIONAL string indicating subdir to which the model main branch has been downloaded to, default = "./"
#' @return NULL
#' @importFrom rlang .data
#' @export

main <- function(portal,
                 template_dir = "model_templates",
                 template_list,
                 branch = "./") {
  # config subdirs
  configure_space()

  # download latest version of data model
  fid <- glue::glue("{branch}/{portal}.model.csv")
  model <- read.csv(fid)

  # remove mock templates
  model <- dplyr::filter(model, !grepl("mock|test ", .data$Attribute, ignore.case = TRUE))

  ## archive content for attributes no longer in the model
  archive_content(model)

  # create/update metadata collection template content
  makeTemplateContent(model, template_dir_path = file.path(branch, template_dir))

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
#' @param template_dir_path a string specifying the subdir where template files are stored
#' @export
makeTemplateContent <- function(model, template_dir_path) {
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
  # add column for UpperCamelCase to enable template file finding
  model_templates$camel <-
    unlist(purrr::map(model_templates$Attribute, get_camel_case))

  # create csv detailing each metadata template
  purrr::walk2(model_templates$title_snake, model_templates$camel,
              function(title_snake, camel, df, template_dir) {
                template_fid <- list.files(template_dir,
                                           pattern = glue::glue("^{camel}"),
                                           full.names = TRUE)
                # open either xlsx or csv template file, which ever is 1st in vector
                if (grepl("\\.xlsx$", template_fid[1])) {
                  template_df <- openxlsx::read_excel(template_fid[1], sheet = 1)
                } else if (grepl("\\.csv$", template_fid[1])) {
                  template_df <- read.csv(template_fid[1])
                } else {
                  stop(glue::glue("No template file found for {camel}"))
                }
                out <- data.frame(Attribute = colnames(template_df))
                out <- dplyr::left_join(out, model, by = "Attribute")
                fid = glue::glue("_data/csv/metadata_templates/{title_snake}.csv")
                write_model_csv(out, fid)
              }, df = dplyr::select(model, Attribute, Description, Required, Valid.Values),
              template_dir = template_dir_path)

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
