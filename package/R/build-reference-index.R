list_reference_index <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  if (is.null(pkg$meta[["reference"]])) {
    meta <- default_reference_index(pkg)
  } else {
    meta <- pkg$meta[["reference"]]
  }

  if (length(meta) == 0) {
    return(list())
  }

  sections <- meta %>%
    purrr::map(list_reference_index_section, pkg = pkg) %>%
    purrr::compact()

  sections <- purrr::map2(sections, seq_along(sections), function(x, ix) {x$ix <- ix; x})

  # Cross-reference complete list of topics vs. topics found in index page
  all_topics <- meta %>%
    purrr::map(~ select_topics(.$contents, pkg$topics, check = TRUE)) %>%
    purrr::reduce(union)
  in_index <- seq_along(pkg$topics$name) %in% all_topics

  missing <- !in_index & !pkg$topics$internal
  if (any(missing)) {
    # add missing
    sections <- c(
      sections,
      list(list_reference_index_section(list(title = "Other", contents = paste0('`', pkg$topics$name[missing], "`")), pkg))
    )
  }

  icons <- sections %>% purrr::map("contents") %>% purrr::flatten() %>% purrr::map("icon")

  list(
    pagetitle = "Function reference",
    sections = sections,
    has_icons = purrr::some(icons, ~ !is.null(.x))
  )
}

list_reference_index_section <- function(section, topics) {
  if (!set_contains(names(section), c("title", "contents"))) {
    warning(
      "Section must have components `title`, `contents`",
      call. = FALSE,
      immediate. = TRUE
    )
    return(NULL)
  }

  # Find topics in this section
  in_section <- select_topics(section$contents, topics)
  section_topics <- pkg$topics[in_section, ]

  slug <- paste0("section-", make_slug(section$title))

  contents <- tibble::tibble(
    id = section_topics$name,
    path = section_topics$file_out,
    name = section_topics$name,
    aliases = purrr::map2(
      section_topics$funs,
      section_topics$name,
      ~ if (length(.x) > 0) .x else .y
    ),
    title = section_topics$title
  )
  list(
    id = slug,
    title = section$title,
    slug = slug,
    desc = markdown_text(section$desc),
    class = section$class,
    contents = purrr::transpose(contents)
  )
}
