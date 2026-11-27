# R 4.7.0 changed the spacing of `str()` output (PR#18719):
# descriptions such as `num 5` no longer start with a space,
# and `<name>:` and `attr(*, "<name>")=` are always followed by one.
# On older R, the output is captured and rewritten to the new form,
# so that `glimpse()` gives the same output on all R versions.
#
# Known limitations of the rewrite on R < 4.7.0:
# - names that contain a line break are not recognized;
# - with `strict.width = "cut"` or `"wrap"`, lines are cut or wrapped
#   before the rewrite, so line lengths may differ by one character;
# - a `str()` method from another package that prints its own text
#   before calling `str()` on the same line, e.g. for units objects,
#   keeps the space before the nested description.
# If the rewrite fails, the output of `str()` is shown unchanged.
str_glimpse <- function(x, ...) {
  if (getRversion() >= "4.7.0") {
    str(x, ...)
    return(invisible())
  }

  out <- utils::capture.output(str(x, ...))
  out <- tryCatch(str_rewrite(out, x, ...), error = function(e) out)
  writeLines(out)
  invisible()
}

# Arguments with the same defaults as in `utils:::str.default()`
str_rewrite <- function(
  lines,
  x,
  max.level = NA,
  nest.lev = 0,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
  comp.str = "$ ",
  list.len = str_option_list_len(),
  ...
) {
  if (length(lines) == 0) {
    return(lines)
  }

  # The first line is the description of `x` itself
  lines[[1]] <- str_rewrite_desc(lines[[1]])

  # Nested objects are indented with `indent.str` followed by one ` ..` per level
  indented <- startsWith(lines, indent.str)
  indented[[1]] <- FALSE
  rest <- substring(lines, nchar(indent.str) + 1)
  n_dots <- attr(regexpr("^( \\.\\.)*", rest), "match.length")
  depth <- n_dots %/% 3
  rest <- substring(rest, n_dots + 1)

  # Only computed if a line has more than one candidate separator
  names_by_depth <- NULL

  for (i in which(indented)) {
    # Components of lists use `comp.str` at the top level and `$ ` below,
    # slots of S4 objects use `@ `
    prefix <- c(comp.str, "$ ", "@ ", '- attr(*, "')
    sep <- c(":", ":", ":", '")=')
    idx <- which(startsWith(rest[[i]], prefix))[1]
    if (is.na(idx)) {
      next
    }

    prefix <- prefix[[idx]]
    sep <- sep[[idx]]
    after <- substring(rest[[i]], nchar(prefix) + 1)

    pos <- gregexpr(sep, after, fixed = TRUE)[[1]]
    if (pos[[1]] < 0) {
      next
    }

    # A name may contain the separator, e.g. `list("a:b" = 1)`.
    # Use the longest candidate that is a name found in `x` at the line's level,
    # or at any level, or the first candidate if none is.
    if (length(pos) > 1) {
      if (is.null(names_by_depth)) {
        names_by_depth <- str_names_by_depth(
          x,
          max_depth = max(depth[indented]),
          max.level = max.level,
          nest.lev = nest.lev,
          list.len = list.len
        )
      }
      cand <- substring(after, 1, pos - 1)
      cand_trimmed <- sub(" +$", "", cand)
      for (nms in list(names_by_depth[[depth[[i]] + 1]], attr(names_by_depth, "all"))) {
        known <- cand %in% nms | cand_trimmed %in% nms
        if (any(known)) {
          break
        }
      }
      pos <- if (any(known)) max(pos[known]) else pos[[1]]
    }

    end <- pos + nchar(sep) - 1
    lines[[i]] <- paste0(
      substring(lines[[i]], 1, nchar(indent.str) + n_dots[[i]]),
      prefix,
      substring(after, 1, end),
      " ",
      str_rewrite_desc(substring(after, end + 1))
    )
  }

  lines
}

str_option_list_len <- function() {
  opt <- getOption("str")
  if (is.list(opt)) {
    opt <- utils::modifyList(utils::strOptions(), opt)
  } else {
    opt <- utils::strOptions()
  }
  opt$list.len
}

# Old R prepends a space to the descriptions of most objects, e.g. ` num 5`,
# but not to others, e.g. `List of 1`.
# R 4.7.0 removed exactly these spaces from the methods in base R.
# Only descriptions printed by these methods lose their leading space:
# the output of `str()` methods from other packages,
# values only (`give.head = FALSE`), and expressions are kept as is.
str_rewrite_desc <- function(desc) {
  if (grepl(str_base_desc_rx, desc, perl = TRUE)) {
    return(substring(desc, 2))
  }

  # The description of an unclassed object follows its classes on the same line,
  # e.g. `Class 'formula'  language y ~ x`
  class <- regmatches(
    desc,
    regexpr("^Class(es)? '[^']*'(, '[^']*')* (\\{[^}]*\\} )?", desc)
  )
  if (length(class) == 0) {
    return(desc)
  }

  paste0(class, str_rewrite_desc(substring(desc, nchar(class) + 1)))
}

str_base_desc_rx <- paste0(
  "^ (?:",
  paste(
    c(
      # Atomic vectors and arrays, e.g. ` Named num [1:2]`, ` 'foo' int(0) `
      "(?:Named )?(?:'[^']*' )?(?:num|int|chr|logi|cplx|raw)(?:[ (]|$)",
      "NULL$",
      "(?:Named )?(?:pair)?list\\(\\)$",
      "(?:Ord\\.f|F)actor[ \\[]",
      "Time-Series ",
      "(?:call|language|symbol|name|Named vector|atomic|promise to) ",
      # `str.Date()` and `str.POSIXt()`, e.g. ` Date[1:3], format: `
      "[^ ,]+(?:\\[[^]]*\\])?, format: "
    ),
    collapse = "|"
  ),
  ")"
)

# Names of the list components, attributes and slots that `str()` shows,
# by the level of the line they appear on (element `d + 1` for level `d`).
# Like `str()`, the walk stops at `max.level` and after `list.len` components,
# and it never goes deeper than the deepest line of the output.
str_names_by_depth <- function(x, max_depth, max.level, nest.lev, list.len) {
  nms <- list()
  depths <- list()

  add <- function(d, n) {
    if (length(n) > 0) {
      k <- length(nms) + 1L
      nms[[k]] <<- n
      depths[[k]] <<- d
    }
  }

  walk <- function(x, d) {
    if (d > max_depth) {
      return()
    }

    attrs <- attributes(x)
    add(d, names(attrs))

    # S4 objects: the slots are shown like a list, one level deeper
    if (isS4(x)) {
      walk(as.list(attrs), d + 1)
    } else {
      for (a in attrs) {
        walk(a, d + 1)
      }
    }

    if ((is.list(x) || is.pairlist(x)) && (is.na(max.level) || nest.lev + d < max.level)) {
      x <- as.list(unclass(x))
      x <- x[seq_len(min(length(x), list.len))]
      add(d, names(x))
      for (elt in x) {
        walk(elt, d + 1)
      }
    }
  }

  walk(x, 0)

  out <- split(
    unlist(nms, use.names = FALSE),
    factor(rep(unlist(depths), lengths(nms)), levels = seq(0, max_depth))
  )
  out <- unname(lapply(out, unique))
  attr(out, "all") <- unique(unlist(out, use.names = FALSE))
  out
}
