library(tidyverse)

library(dynplot)
library(dynwrap)
library(dynutils)


devtools::load_all("package")

# get content path
content_path <- function(path, ext = "html") {
  map_chr(path, function(path) {
    if (endsWith(path, "/")) {
      path <- paste0(path, "_index", ".", ext)
    } else if (nchar(fs::path_ext(path)) == 0) {
      path <- paste0(path, ".", ext)
    }
    path <- paste0("content/", path)
    fs::dir_create(fs::path_dir(path))
    path
  })
}

packages <- tribble(
  ~id, ~folder,
  "dynwrap", "../../dynwrap",
  "dynplot", "../../dynplot"
)


#   ____________________________________________________________________________
#   General content                                                         ####

blogdown::stop_server()
if (fs::dir_exists("content")) fs::dir_delete("content")
processx::run("cp", c("-Rl", paste0(getwd(), "/content_raw/"), "content/"), echo = TRUE) # symlink content_raw

#   ____________________________________________________________________________
#   Vignettes                                                               ####

# define vignettes
vignettes <- tribble(
  ~package, ~file_original, ~folder_output, ~ix, ~title,
  "dynwrap", "create_ti_method_barebones.Rmd", "developers/creating-ti-method/", 1, "Bare-bones",
  "dynwrap", "create_ti_method_script.Rmd", "developers/creating-ti-method/", 2, "Wrapper script",
  "dynwrap", "create_ti_method_container.Rmd", "developers/creating-ti-method/", 4, "Container"
) %>%
  mutate(
    file_original = fs::path(deframe(select(packages, id, folder))[package], "vignettes", file_original),
    file_rmd = content_path(fs::path(folder_output, fs::path_file(file_original)), "Rmd"),
    file_out = file_rmd %>% fs::path_ext_remove()
  )

# copy vignettes
fs::file_copy(vignettes$file_original, vignettes$file_rmd, overwrite = FALSE)

vignette <- extract_row_to_list(vignettes, 1)
adapt_frontmatter <- function(vignette) {
  front <- rmarkdown::yaml_front_matter(vignette$file_rmd)

  front$title <- vignette$title
  front$weight <- vignette$ix

  vignette$file_rmd %>%
    readr::read_file() %>%
    gsub("---\n.*\n---", paste0("---\n", yaml::as.yaml(front), "\n---"), .) %>%
    readr::write_file(vignette$file_rmd)
}
purrr::transpose(vignettes) %>% walk(adapt_frontmatter)


# rmarkdown::render(
#   vignette$file_rmd,
#   blogdown::html_page()
# )
#
# vignette$file_in


#   ____________________________________________________________________________
#   Reference                                                               ####

package <- "dynwrap"
pkg <- as_pkgdown(paste0("../../", package))
pkg_data <- get_topics_and_sections(pkg)

get_sections_data <- function(pkg_data) {
  list(
    package = package,
    sections = transpose(pkg_data$sections)
  )
}

get_section_data <- function(pkg_data, section_id) {
  c(
    list(package = package),
    pkg_data$sections %>% filter(id == !!section_id) %>% dynutils::extract_row_to_list(1)
  )
}

get_topic_data <- function(pkg_data, topic_name) {
  topic <- pkg_data$topics %>% filter(name == !!topic_name) %>% dynutils::extract_row_to_list(1)
  topic_data <- pkg_data$topics_data[[topic_name]]

  c(topic, topic_data)
}

# render package index
template <- system.file("templates2/content-reference-index.html", package = "dynsite") %>% readr::read_lines()
whisker::whisker.render(template, get_sections_data(pkg_data)) %>%
  readr::write_lines(content_path(paste0("reference/", package, "/")))

# render sections
walk(transpose(pkg_data$sections), function(section) {
  template <- system.file("templates2/content-reference-section.html", package = "dynsite") %>% readr::read_lines()
  whisker::whisker.render(template, get_section_data(pkg_data, section$id)) %>%
    readr::write_lines(content_path(section$path))

  # render topics
  topic_names <- section$contents %>% map_chr("name")
  topics <- map(topic_names, get_topic_data, pkg_data = pkg_data)

  template <- system.file("templates2/content-reference-topic.html", package = "dynsite") %>% readr::read_lines()
  walk(topics, function(topic) {
    whisker::whisker.render(template, topic) %>%
      readr::write_lines(content_path(topic$path))
  })
})


#   ____________________________________________________________________________
#   Render                                                                  ####
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0 && args[1] == "build") {
  blogdown::build_site()
} else {
  blogdown::serve_site()
}

blogdown::stop_server()
