# Shared reader and formatter for the CSV-backed CV sections.

if (!exists("cv_data_dir", inherits = TRUE)) {
  cv_data_dir <- file.path("cv", "data")
}

read_cv_entries <- function(filename) {
  path <- file.path(cv_data_dir, filename)
  entries <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  fields <- c(
    "group", "prefix", "title", "title_note", "role_2",
    "institution", "detail", "detail_2", "amount", "date"
  )
  missing_fields <- setdiff(fields, names(entries))
  entries[missing_fields] <- NA_character_

  entries
}

has_value <- function(value) {
  length(value) == 1 && !is.na(value) && nzchar(trimws(as.character(value)))
}

format_amount <- function(amount) {
  paste0(
    "Awarded value: $",
    format(as.numeric(amount), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

entry_lines <- function(entry) {
  title <- paste0(
    if (has_value(entry$prefix)) paste0(entry$prefix, " ") else "",
    "**", entry$title, "**",
    if (has_value(entry$title_note)) paste0(" - ", entry$title_note) else ""
  )

  lines <- c(title)
  if (has_value(entry$role_2)) lines <- c(lines, paste0("**", entry$role_2, "**"))
  if (has_value(entry$institution)) lines <- c(lines, entry$institution)
  if (has_value(entry$detail)) lines <- c(lines, entry$detail)
  if (has_value(entry$detail_2)) lines <- c(lines, entry$detail_2)
  if (has_value(entry$amount)) lines <- c(lines, format_amount(entry$amount))
  lines
}

typst_text <- function(value) {
  paste0("#text(", encodeString(as.character(value), quote = '"'), ")")
}

typst_entry_lines <- function(entry) {
  title <- paste0(
    if (has_value(entry$prefix)) typst_text(paste0(entry$prefix, " ")) else "",
    " #strong[", typst_text(entry$title), "]",
    if (has_value(entry$title_note)) paste0(" ", typst_text(paste0(" - ", entry$title_note))) else ""
  )

  lines <- c(title)
  if (has_value(entry$role_2)) lines <- c(lines, paste0("#strong[", typst_text(entry$role_2), "]"))
  if (has_value(entry$institution)) lines <- c(lines, typst_text(entry$institution))
  if (has_value(entry$detail)) lines <- c(lines, typst_text(entry$detail))
  if (has_value(entry$detail_2)) lines <- c(lines, typst_text(entry$detail_2))
  if (has_value(entry$amount)) lines <- c(lines, typst_text(format_amount(entry$amount)))
  lines
}

format_cv_entry <- function(entry, output = knitr::pandoc_to()) {
  if (identical(output, "typst")) {
    body <- paste(typst_entry_lines(entry), collapse = " #linebreak() ")
    return(paste0(
      "```{=typst}\n",
      "#cv-entry([", body, "], [", typst_text(entry$date), "])\n",
      "```"
    ))
  }

  body <- paste(entry_lines(entry), collapse = "  \n")
  paste0(
    ":::: {.cv-entry}\n",
    "::: {.cv-entry-main}\n", body, "\n:::\n",
    "::: {.cv-entry-date}\n*", entry$date, "*\n:::\n",
    "::::"
  )
}

render_cv_entries <- function(filename) {
  entries <- read_cv_entries(filename)
  groups <- unique(entries$group)

  if (all(is.na(groups))) {
    groups <- NA_character_
  }

  blocks <- unlist(lapply(groups, function(group) {
    selected <- if (is.na(group)) entries else entries[entries$group == group, , drop = FALSE]
    heading <- if (is.na(group)) character() else paste0("### ", group)
    rendered <- lapply(seq_len(nrow(selected)), function(i) format_cv_entry(selected[i, , drop = FALSE]))
    c(heading, unlist(rendered))
  }))

  cat(paste(blocks, collapse = "\n\n"), "\n")
}

render_contact <- function() {
  if (identical(knitr::pandoc_to(), "typst")) {
    cat(
      "```{=typst}\n",
      "#align(center)[\n",
      "  #text(\"Dr Michael Lydeamore\") #linebreak()\n",
      "  #text(\"Department of Econometrics and Business Statistics\") #linebreak()\n",
      "  #text(\"Monash University\") #linebreak()\n",
      "  #text(\"Email: michael.lydeamore@monash.edu\")\n",
      "]\n",
      "```\n",
      sep = ""
    )
  } else {
    cat(
      "::: {.print-only .contact-block}\n",
      "Dr Michael Lydeamore  \n",
      "Department of Econometrics and Business Statistics  \n",
      "Monash University\n\n",
      "Email: michael.lydeamore@monash.edu\n",
      ":::\n",
      sep = ""
    )
  }
}
