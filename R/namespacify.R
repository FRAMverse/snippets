# UPDATE: this has been turned into the `namespacify` package: https://github.com/cbedwards-dfw/namespacify

namespacify_text = function(original.text,
                            packages = c("dplyr", "ggplot2", "purrr", "tidyr", 
                                                    "DBI", "readr", "stringr", "tidyselect")){
  vec.replacement = NULL
  # funs.all = NULL
  for(cur.package in packages){
    vec.funs = getNamespaceExports(cur.package)
    ## remove functions with funky chars
    vec.funs = vec.funs[!stringr::str_detect(vec.funs, "[%]|[<]")]
    ## remove .data object types
    vec.funs = vec.funs[!stringr::str_detect(vec.funs, "^[.]")]
    ##handle functions with periods in them
    funs.safe = gsub("[.]", "[.]", vec.funs)
    # funs.all = c(funs.all, vec.funs)
    vec.pat = glue::glue("([^A-z:]){funs.safe}[(]")
    vec.replace = glue::glue("\\1{cur.package}::{vec.funs}(")
    names(vec.replace) = vec.pat
    vec.replacement = c(vec.replacement, vec.replace)
    ## and what if function is at beginning of string?:
    vec.pat = glue::glue("^{vec.funs}[(]")
    vec.replace = glue::glue("{cur.package}::{vec.funs}(")
    names(vec.replace) = vec.pat
    vec.replacement = c(vec.replacement, vec.replace)
  }
  return(stringr::str_replace_all(original.text, vec.replacement))
}

namespacify_file = function(
    file, 
    packages = c("dplyr", "ggplot2", "purrr", "tidyr", 
                 "DBI", "readr", "stringr", "tidyselect"),
    verbose = TRUE
){
  original.text = readLines(file, warn = FALSE)
  new.text = namespacify_text(original.text, packages)
  writeLines(new.text, file)
  if(verbose){
    cli::cli_div(theme = list(span.emph = list(color = "forestgreen")))
    cli::cli_text("-> {.emph {file}} namespacified! {sum(original.text != new.text)} lines changed")
  }
}
path = "example-fun.R"

namespacify_package = function(
    packages = c("dplyr", "ggplot2", "purrr", "tidyr", 
                 "DBI", "readr", "stringr", "tidyselect"),
    verbose = TRUE,
    folder = NULL
){
  if(is.null(folder)){
    r_files <- fs::dir_ls(here::here("R"))
  }else{
    r_files <- fs::dir_ls(folder)
  }
  purrr::walk(r_files, \(x) namespacify_file(file = x, packages = packages, verbose = verbose))
}

# namespacify_file("Documents/WDFW FRAM team work/sandbox/namespacify/R/test-with-comments.R")
# 
# namespacify_package(folder = ".")
