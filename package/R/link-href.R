href_string <- function(x, bare_symbol = FALSE) {
  expr <- tryCatch(parse(text = x)[[1]], error = function(e) NULL)
  if (is.null(expr)) {
    return(NA_character_)
  }

  href_expr(expr, bare_symbol = bare_symbol)
}

# expr <- quote(vignette("create_ti_method_container", "dynwrap"))

href_expr <- function(expr, bare_symbol = FALSE) {
  if (is_symbol(expr) && bare_symbol) {
    # foo
    href_topic_local(as.character(expr))
  } else if (is_call(expr)) {
    fun <- expr[[1]]

    if (is_call(fun, "::", n = 2)) {
      pkg <- as.character(fun[[2]])
      fun <- fun[[3]]
    } else {
      pkg <- NULL
    }

    if (!is_symbol(fun))
      return(NA_character_)

    fun_name <- as.character(fun)

    # we need to include the `::` and `?` infix operators
    # so that `?build_site()` and `pkgdown::build_site()` are linked
    if (!is_prefix(fun_name) && !fun_name %in% c("::", "?")) {
      return(NA_character_)
    }

    n_args <- length(expr) - 1

    if (fun_name == "vignette") {
      expr <-
        tryCatch({
          call_standardise(expr, env = environment(vignette))
        }, error = function(e) {
          stop("Calling '", deparse(expr), "' failed. Error:\n  ", e)
        })
      href_article(expr$topic, expr$package)
    } else if (fun_name == "?") {
      if (n_args == 1) {
        topic <- expr[[2]]
        if (is_call(topic, "::")) {
          # ?pkg::x
          href_topic(as.character(topic[[3]]), as.character(topic[[2]]))
        } else if (is_symbol(topic) || is_string(topic)) {
          # ?x
          href_topic(as.character(expr[[2]]))
        } else {
          NA_character_
        }
      } else if (n_args == 2) {
        # package?x
        href_topic(paste0(expr[[3]], "-", expr[[2]]))
      }
    } else if (fun_name == "::") {
      href_topic(as.character(expr[[3]]), as.character(expr[[2]]))
    } else {
      href_topic(fun_name, pkg)
    }
  } else {
    NA_character_
  }
}

# Helper for testing
href_expr_ <- function(expr, ...) {
  href_expr(substitute(expr), ...)
}

href_topic <- function(topic, package = NULL) {
  if (is.null(package) || package == context_get("package")) {
    href_topic_local(topic)
  } else {
    href_topic_remote(topic, package)
  }
}

href_topic_local <- function(topic) {
  rdname <- find_rdname_local(topic)
  if (is.null(rdname)) {
    # Check attached packages
    loc <- find_rdname_attached(topic)
    if (is.null(loc)) {
      return(NA_character_)
    } else {
      return(href_topic_remote(topic, loc$package))
    }
  }

  # If it's a re-exported function, we need to work a little harder to
  # find out its source so that we can link to it. .getNamespaceInfo()
  # is only available in R 3.2.0 and above.
  if (rdname == "reexports" && getRversion() >= "3.1.0") {
    ns <- ns_env(context_get("package"))
    exports <- .getNamespaceInfo(ns, "exports")

    if (!env_has(exports, topic)) {
      return(NA_character_)
    } else {
      obj <- env_get(ns, topic, inherit = TRUE)
      package <- find_reexport_source(obj, ns, topic)
      return(href_topic_remote(topic, package))
    }
  }

  context_get("topic_paths")[rdname]
}

href_topic_remote <- function(topic, package) {
  rdname <- find_rdname(package, topic)
  if (is.null(rdname)) {
    return(NA_character_)
  }

  reference_url <- remote_package_reference_url(package)

  if (!is.null(reference_url)) {
    paste0(reference_url, paste0("/", rdname, ".html"))
  } else {
    # Fall back to rdocumentation.org which almost certainly works
    paste0("https://www.rdocumentation.org/packages/", package, "/topics/", rdname)
  }
}

href_article <- function(article, package = NULL) {
  local <- is.null(package) || package == context_get("package")
  if (local) {
    path <- find_article(NULL, article)
    if (is.null(path)) {
      return(NA_character_)
    }

    paste0(up_path(context_get("depth")), "articles/", path)
  } else {
    path <- find_article(package, article)
    if (is.null(path)) {
      return(NA_character_)
    }

    base_url <- remote_package_article_url(package)
    if (is.null(base_url)) {
      paste0("https://cran.rstudio.com/web/packages/", package, "/vignettes/", path)
    } else {
      paste0(base_url, "/", path)
    }
  }
}

is_prefix <- function(fun) {
  if (grepl("^%.*%$", fun)) {
    return(FALSE)
  }

  infix <- c(
    "::", ":::", "$", "@", "[", "[[", "^", "-", "+", ":", "*", "/",
    "<", ">", "<=", ">=", "==", "!=", "!", "&", "&&", "|", "||", "~",
    "->", "->>", "<-", "<<-", "=", "?"
  )
  if (fun %in% infix) {
    return(FALSE)
  }

  special <- c(
    "(", "{", "if", "for", "while", "repeat", "next", "break", "function"
  )
  if (fun %in% special) {
    return(FALSE)
  }

  TRUE
}
