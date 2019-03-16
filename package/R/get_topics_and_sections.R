get_topics_and_sections <- function(pkg) {
  pkg <- as_pkgdown(pkg)
  pkg <- section_init(pkg, depth = 1L) # init topics index

  # get information on sections in reference
  meta_sections <- pkg$meta[["reference"]]
  if (is.null(meta_sections)) {
    meta_sections <- tibble(title = "Other", id = "other", desc = "", matchers = list(NULL))
  } else {
    meta_sections <- meta_sections %>%
      dynutils::list_as_tibble() %>%
      rename(matchers = contents)
    meta_sections$id <- map_chr(meta_sections$title, make_slug)
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

  # find for each topic its section
  match_env <- match_env(topics)
  topics$section_id <- map(meta_sections$matchers, function(matchers) {
    indexes <- purrr::map(matchers, match_eval, env = match_env(topics)) %>% unlist()
    seq_len(nrow(topics)) %in% indexes
  }) %>%
    set_names(meta_sections$id) %>%
    transpose() %>%
    map_chr(function(matched) {
      section_ids <- names(matched)[as.logical(matched)]
      section_ids <- c(section_ids, "other")
      first(section_ids)
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

  lst(
    topics,
    sections,
    topics_data
  )
}


