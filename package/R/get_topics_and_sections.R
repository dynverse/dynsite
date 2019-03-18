get_topics_and_sections <- function(pkg, package) {
  # data contains package data
  assertthat::assert_that(all(c("id", "ix") %in% names(package)))
  data <- package

  # run through pkgdown
  pkg <- as_pkgdown(pkg)
  pkg <- section_init(pkg, depth = 1L) # init topics index

  # get information on sections in reference
  meta_sections <- pkg$meta[["reference"]]
  if (is.null(meta_sections)) {
    meta_sections <- tibble(title = "Other", id = "other", desc = "")
  } else {
    meta_sections <- meta_sections %>%
      dynutils::list_as_tibble()
    meta_sections <- meta_sections %>% add_row(title = "Other", id = "other", desc = "")
  }

  # get topics
  rd <- package_rd(pkg$src_path)

  aliases <- purrr::map(rd, extract_tag, "tag_alias")
  names <- purrr::map_chr(rd, extract_tag, "tag_name")
  titles <- purrr::map_chr(rd, extract_title)
  concepts <- purrr::map(rd, extract_tag, "tag_concept")
  internal <- purrr::map_lgl(rd, is_internal)
  source <- purrr::map(rd, extract_source)
  funs <- purrr::map(rd, topic_funs)
  file_in <- names(rd)
  keywords <- purrr::map(rd, extract_tag, "tag_keyword")

  topics <- tibble::tibble(
    name = names,
    alias = aliases,
    funs = funs,
    title = titles,
    file_in = file_in,
    rd = rd,
    source = source,
    concepts = concepts,
    internal = internal
  )

  topics$section_id <- map_chr(keywords, function(keywords) {
    intersect <- intersect(keywords, meta_sections$id)
    if (length(intersect) > 0) {
      first(intersect)
    } else {
      "other"
    }
  })

  # create file of doc
  topics$path <- str_glue("/reference/{pkg$package}/{topics$section_id}/{topics$name}")
  context_set("topic_paths", topics %>% select(name, path) %>% deframe())

  # create sections
  sections <- topics %>%
    filter(!internal) %>%
    group_by(section_id) %>%
    summarise(contents = list(transpose(lst(path, name, aliases = alias, title)))) %>%
    rename(id = section_id)

  sections <- sections %>%
    left_join(meta_sections, "id") %>%
    mutate(ix = match(id, meta_sections$id)) %>%
    arrange(ix)

  sections$path <- str_glue("/reference/{pkg$package}/{sections$id}/")

  # generate extra topics data (examples etc)
  topics_data <- topics %>% purrr::transpose() %>% map(list_reference_topic, pkg = pkg)
  names(topics_data) <- topics$name

  c(
    data,
    lst(
      topics,
      sections,
      topics_data,
      package = pkg$package
    )
  )

}


