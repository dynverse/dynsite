# Convert Rd to list ------------------------------------------------------

list_reference_topic <- function(topic,
                                 pkg,
                                 examples = TRUE,
                                 run_dont_run = FALSE
                                 ) {
  tag_names <- purrr::map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)

  out <- list()

  # Single top-level converted to string
  out$name <- flatten_text(tags$tag_name[[1]][[1]])
  out$title <- extract_title(tags$tag_title)

  out$pagetitle <- paste0(out$title, " \u2014 ", out$name)

  # File source
  out$source_link <- github_source_links(pkg$github_url, topic$source)
  out$filename <- topic$file_in
  out$path <- topic$file_out

  # Multiple top-level converted to string
  out$author <- purrr::map_chr(tags$tag_author %||% list(), flatten_para)
  out$aliases <- purrr::map_chr(tags$tag_alias %||% list(), flatten_text)
  out$keywords <- purrr::map_chr(tags$tag_keyword %||% list(), flatten_text)

  # Sections that contain arbitrary text and need cross-referencing
  out$description <- as_data(tags$tag_description[[1]])
  out$opengraph <- list(description = strip_html_tags(out$description$contents))
  out$usage <- as_data(tags$tag_usage[[1]])
  out$arguments <- as_data(tags$tag_arguments[[1]])
  if (length(out$arguments)) {
    out$has_args <- TRUE # Work around mustache deficiency
  }

  out$examples <- as_data(
    tags$tag_examples[[1]],
    env = new.env(parent = globalenv()),
    topic = tools::file_path_sans_ext(topic$file_in),
    examples = examples,
    run_dont_run = run_dont_run
  )

  # Everything else stays in original order, and becomes a list of sections.
  section_tags <- c(
    "tag_details", "tag_references", "tag_source", "tag_format",
    "tag_note", "tag_seealso", "tag_section", "tag_value"
  )
  sections <- topic$rd[tag_names %in% section_tags]
  out$sections <- sections %>%
    purrr::map(as_data) %>%
    purrr::map(add_slug)

  out
}

add_slug <- function(x) {
  x$slug <- make_slug(x$title)
  x
}

make_slug <- function(x) {
  x <- strip_html_tags(x)
  x <- tolower(x)
  x <- gsub("[^a-z]+", "-", x)
  x
}
