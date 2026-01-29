## visualize function dependencies
## Built with co-pilot. Need to do some careful reading to make sure I understand.

library(codetools)
library(visNetwork)

pkg_functions <- function(pkg) {
  ns <- asNamespace(pkg)
  objs <- ls(ns, all.names = TRUE)
  objs[sapply(objs, function(x) is.function(ns[[x]]))]
}

## returns a character vector of all functions in package `pkg` that depend on function `fun`
fn_deps <- function(pkg, fun) {
  ns <- asNamespace(pkg)
  funs <- pkg_functions(pkg)
  
  if (!fun %in% funs) {
    stop(sprintf("Function '%s' is not a function in namespace '%s'", fun, pkg))
  }
  
  ## key function: codetools::findGlobals() finds all the functions and variables used by a closure (!!!!)
  globals <- findGlobals(ns[[fun]], merge = FALSE)$functions
  
  
  # keep only functions that are actually in the package namespace
  intersect(globals, funs)
}
## example:
fn_deps("xldiff", "excel_diff")
# body(framrsquared:::fetch_table_)

recursive_deps <- function(pkg, fun) {
  ns <- asNamespace(pkg)
  funs <- pkg_functions(pkg)
  
  if (!fun %in% funs) {
    stop(sprintf("Function '%s' is not a function in namespace '%s'", fun, pkg))
  }
  
  seen <- character()
  stack <- fun
  
  while (length(stack) > 0) {
    f <- stack[1]
    stack <- stack[-1]
    
    if (f %in% seen) next
    seen <- c(seen, f)
    
    d <- fn_deps(pkg, f)
    stack <- c(stack, d)
  }
  
  setdiff(seen, fun)
}

build_recursive_edges <- function(pkg, fun) {
  ns <- asNamespace(pkg)
  funs <- c(fun, recursive_deps(pkg, fun))
  
  edges <- do.call(
    rbind,
    lapply(funs, function(f) {
      d <- fn_deps(pkg, f)
      if (length(d) == 0) return(NULL)
      cbind(from = f, to = d)
    })
  )
  
  as.data.frame(edges, stringsAsFactors = FALSE)
}


plot_function_dependencies <- function(package_name,
                                       function_name, 
                                       include_validate_funs = TRUE){ ## skips functions of form "validate_.*", which are framrsquared internal argument validation frameworks.
  ## find the edges
  edges <- build_recursive_edges(package_name, function_name)
  ## if skipping validation functions, skip validation functions
  ## ## a little jank, but because valdiation functions don't have deep recursive stuff going on, it works okay. Should really apply to build_recursive_edges.
  if(!include_validate_funs){
    edges <- edges |> 
      filter(!grepl("validate_.*", to)) |> 
      filter(!grepl("validate_.*", from)) 
  }
  
  nodes <- data.frame(
    id = unique(c(edges$from, edges$to)),
    label = unique(c(edges$from, edges$to)),
    stringsAsFactors = FALSE
  )
  
  # highlight the root function
  nodes$color <- ifelse(nodes$id == function_name, "lightblue", "lightgray")
  
  visNetwork(nodes, edges) %>%
    visNodes(shape = "box") %>%
    visEdges(arrows = "from") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}

## examples:

plot_function_dependencies("framrsquared", "plot_stock_mortality")

plot_function_dependencies("framrsquared", "plot_stock_mortality", include_validate_funs = FALSE)

plot_function_dependencies("tidyr", "expand_grid")