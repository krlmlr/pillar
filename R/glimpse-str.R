# R 4.7.0 changed the spacing of `str()` output (PR#18719):
# descriptions such as `num 5` no longer start with a space,
# and `<name>:` and `attr(*, "<name>")=` are always followed by one.
# On older R, the output is captured and rewritten to the new form,
# so that `glimpse()` gives the same output on all R versions.
#
# Known limitations of the rewrite on R < 4.7.0:
# - names that contain a line break, or a custom `indent.str`, are not recognized;
# - with `strict.width = "cut"` or `"wrap"`, lines are cut or wrapped
#   before the rewrite, so line lengths may differ by one character;
# - a `str()` method from another package that prints its own text
#   before calling `str()` on the same line, e.g. for units objects,
#   keeps the space before the nested description.
str_glimpse <- function(x, ...) {
  if (getRversion() >= "4.7.0") {
    str(x, ...)
    return(invisible())
  }

  out <- utils::capture.output(str(x, ...))
  writeLines(str_rewrite(out, x, ...))
  invisible()
}

str_rewrite <- function(lines, x, comp.str = "$ ", ...) {
  if (length(lines) == 0) {
    return(lines)
  }

  # The first line is the description of `x` itself
  lines[[1]] <- str_rewrite_desc(lines[[1]])

  # Only computed if a line has more than one candidate separator
  all_names <- NULL

  for (i in seq_along(lines)[-1]) {
    line <- lines[[i]]
    indent <- regmatches(line, regexpr("^[ .]*", line))
    rest <- substring(line, nchar(indent) + 1)

    # Components of lists use `comp.str` at the top level and `$ ` below,
    # slots of S4 objects use `@ `
    prefix <- c(comp.str, "$ ", "@ ", '- attr(*, "')
    sep <- c(":", ":", ":", '")=')
    idx <- which(startsWith(rest, prefix))[1]
    if (is.na(idx)) {
      next
    }

    prefix <- prefix[[idx]]
    sep <- sep[[idx]]
    rest <- substring(rest, nchar(prefix) + 1)

    pos <- gregexpr(sep, rest, fixed = TRUE)[[1]]
    if (pos[[1]] < 0) {
      next
    }

    # A name may contain the separator, e.g. `list("a:b" = 1)`.
    # Use the longest candidate that is a name found in `x`,
    # or the first candidate if none is.
    if (length(pos) > 1) {
      if (is.null(all_names)) {
        all_names <- str_all_names(x)
      }
      cand <- substring(rest, 1, pos - 1)
      known <- cand %in% all_names | sub(" +$", "", cand) %in% all_names
      pos <- if (any(known)) max(pos[known]) else pos[[1]]
    }

    end <- pos + nchar(sep) - 1
    lines[[i]] <- paste0(
      indent,
      prefix,
      substring(rest, 1, end),
      " ",
      str_rewrite_desc(substring(rest, end + 1))
    )
  }

  lines
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

# Names of all list components, attributes and slots, recursively
str_all_names <- function(x) {
  out <- character()

  collect <- function(x) {
    attrs <- attributes(x)
    out <<- c(out, names(attrs))
    if (is.list(x) || is.pairlist(x)) {
      x <- as.list(unclass(x))
      out <<- c(out, names(x))
      lapply(x, collect)
    }
    lapply(attrs, collect)
    NULL
  }

  collect(x)
  unique(out)
}
