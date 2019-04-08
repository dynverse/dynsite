library(tidyverse)

library(dynplot)
library(dynwrap)
library(dynutils)

devtools::load_all("package")

# get content path
content_path <- function(path, ext = "html", raw = FALSE) {
  map_chr(path, function(path) {
    if (endsWith(path, "/")) {
      path <- paste0(path, "_index", ".", ext)
    } else if (nchar(fs::path_ext(path)) == 0) {
      path <- paste0(path, ".", ext)
    }
    if (!raw) {
      path <- paste0("content/", path)
    } else {
      path <- paste0("content_raw/", path)
    }
    fs::dir_create(fs::path_dir(path))
    path
  })
}

packages <- tribble(
  ~id, ~folder,
  "dynwrap", "../dynwrap",
  "dynplot", "../dynplot",
  "dynmethods", "../dynmethods",
  "dyneval", "../libraries/dyneval",
  "dynutils", "../libraries/dynutils",
  "dyncli", "../libraries/dyncli"
) %>%
  mutate(ix = row_number())

base_url <- "/"


#   ____________________________________________________________________________
#   General content                                                         ####

blogdown::stop_server()
if (fs::dir_exists("content")) fs::dir_delete("content")
processx::run("rsync", c("-r", "--checksum", "--update", paste0(getwd(), "/content_raw/"), "content"), echo = TRUE) # copy raw content
processx::run("rsync", c("-r", "--checksum", "--update", paste0(getwd(), "/static_raw/"), "static"), echo = TRUE) # copy raw static

#   ____________________________________________________________________________
#   Vignettes                                                               ####

# define vignettes
vignettes <- tribble(
  ~package, ~file_original, ~folder_output, ~ix, ~title,
  "dynwrap", "create_ti_method_wrappers.Rmd", "developers/creating-ti-method/", 1, "Creating a trajectory",
  "dynwrap", "create_ti_method_r.Rmd", "developers/creating-ti-method/", 2, "Wrapping in R",
  "dyncli", "create_ti_method_script.Rmd", "developers/creating-ti-method/", 3, "Wrapper script",
  "dynwrap", "create_ti_method_container.Rmd", "developers/creating-ti-method/", 4, "Containerised wrapping"
) %>%
  mutate(
    file_original = fs::path(deframe(select(packages, id, folder))[package], "vignettes", file_original),
    file_rmd = content_path(fs::path(folder_output, fs::path_file(file_original)), "Rmd", raw = TRUE),
    file_out = file_rmd %>% fs::path_ext_remove()
  )

# copy vignettes
fs::file_copy(vignettes$file_original, vignettes$file_rmd, overwrite = TRUE)

vignette <- extract_row_to_list(vignettes, 1)
adapt_frontmatter <- function(rmd, adapt, front = rmarkdown::yaml_front_matter(rmd)) {
  front <- purrr::list_modify(front, !!!adapt)
  rmd %>%
    readr::read_file() %>%
    gsub("---\n.*\n---", paste0("---\n", yaml::as.yaml(front), "\n---"), .) %>%
    readr::write_file(rmd)
}
adapt_frontmatter_vignette <- function(vignette) {
  front <- rmarkdown::yaml_front_matter(vignette$file_rmd)

  adapt_frontmatter(vignette$file_rmd, list(title = vignette$title, weight = vignette$ix))
}
purrr::transpose(vignettes) %>% walk(adapt_frontmatter_vignette)


# rmarkdown::render(
#   vignette$file_rmd,
#   blogdown::html_page()
# )
#
# vignette$file_in


#   ____________________________________________________________________________
#   Reference                                                               ####

get_sections_data <- function(pkg_data) {
  data <- pkg_data
  data$sections <- transpose(data$sections)
  data$recent_changes <- fs::path_abs(pkg_data$folder) %>%
    dynutils:::process_news() %>%
    slice(1:2) %>%
    mutate(text = map_chr(text, ~markdown::markdownToHTML(text = ., fragment.only = TRUE))) %>%
    transpose()

  data$logo <- fs::file_copy(paste0(pkg_data$folder, "/man/figures/logo.png"), paste0("static/images/logos/", pkg_data$id, ".png"), overwrite = TRUE) %>% fs::path_rel("static/")
  data
}

get_section_data <- function(pkg_data, section_id) {
  c(
    list(package = pkg_data$package),
    pkg_data$sections %>% filter(id == !!section_id) %>% dynutils::extract_row_to_list(1)
  )
}

get_topic_data <- function(pkg_data, topic_name) {
  topic <- pkg_data$topics %>% filter(name == !!topic_name) %>% dynutils::extract_row_to_list(1)
  topic_data <- pkg_data$topics_data[[topic_name]]

  c(topic, topic_data)
}


package <- packages %>% extract_row_to_list(1)
# walk(transpose(packages %>% filter(id == "dynplot")), function(package) {
topics <- map_dfr(transpose(packages), function(package) {
  print(package$id)

  pkg <- as_pkgdown(package$folder)
  pkg_data <- get_topics_and_sections(pkg, package = package)

  # render package index
  output <- content_path(paste0("reference/", package$id, "/_index.html"))
  template <- system.file("templates2/content-reference-index.html", package = "dynsite") %>% readr::read_lines()
  whisker::whisker.render(template, get_sections_data(pkg_data)) %>%
    readr::write_lines(output)

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

  pkg_data$topics
})

#   ____________________________________________________________________________
#   Render                                                                  ####
processx::run("rsync", c("-r", "--checksum", "--update", paste0(getwd(), "/content_raw/"), "content"), echo = TRUE) # copy raw content
processx::run("rsync", c("-r", "--checksum", "--update", paste0(getwd(), "/static_raw/"), "static"), echo = TRUE) # copy raw static
knitr::opts_chunk$set(collapse=TRUE, results="hold")
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0 && args[1] == "build") {
  blogdown::build_site()
} else {
  blogdown::serve_site()
}

blogdown::stop_server()


link_references(topics, "public")
