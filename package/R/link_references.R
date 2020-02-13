#' Walks through all html files inside folder recursively, and adds
#'
#' @export
link_references <- function(topics, folder, base_url = "") {
  # create mapping frorm topic to replacement
  topic2link <- unlist(map2(topics$alias, paste0(base_url, topics$path), ~set_names(rep(.y, length(.)), .)))
  topic2replacement <- paste0("<a href='", topic2link, "'>", names(topic2link), "</a>")
  names(topic2replacement) <- paste0(names(topic2link)) # find topic + a non-variable character or end of content
  topic2replacement <- topic2replacement[order(map_int(topic2replacement, nchar), decreasing = T)]

  # read html files
  html_files <- fs::dir_ls(folder, glob = "*.html", recurse = TRUE)
  html_file <- html_files[[1]]

  html_file <- "public2/reference/dynplot/plot_trajectory/plot_dimred/index.html"

  # for each html file
  walk(html_files, function(html_file) {
    print(html_file)
    html <- xml2::read_html(html_file)

    nodes <- xml2::xml_find_all(html, ".//code|.//pre")

    # for each "code" node
    if (length(nodes) > 1) node <- nodes[[2]]
    walk(nodes, function(node) {
      contents <- xml2::xml_find_all(node, ".//text()")
      content <- first(contents)

      # for each content inside this code node
      walk(contents, function(content) {
        # look for tokens, i.e. valid R names and split
        text <- escape_html(xml2::xml_text(content))
        tokens <- str_split(text, "(?=[^\\._A-Za-z0-0$@])")[[1]] %>% str_split("(?<=[^\\._A-Za-z0-0])") %>% unlist()

        # replace
        new_text <- case_when(
          tokens %in% names(topic2replacement) ~ topic2replacement[tokens],
          TRUE ~ tokens
        ) %>% paste0(collapse = "")
        new_text <- paste0("<x>", new_text, "</x>")
        xml2::xml_replace(content, xml2::read_xml(new_text))
      })
    })

    # save html
    write_html(html, paste0(html_file), format = character())
  })
}
