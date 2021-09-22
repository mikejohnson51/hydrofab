# This builds documentation for the package in Markdown format.

Rd2md::ReferenceManual(outdir = "docs/")

outfile <- "docs/Reference_Manual_hyRefactor.md"

text <- readLines(outfile)

headings <- text[grepl("^# `.*", text)]

headings <- gsub("`", "", gsub("# `", "", headings))

toc <- paste0("- [", headings, "](#", headings, ")")

text <- c("# Vignettes",
          "- [Refactor Flowlines](refactor_nhdplus.md)",
          "- [Split Flowlines](split_refactor.md)",
          "- [Refactor Catchments](refactor_catchment.md)",
          "- [Coastal Units with hyRefactor](coastal_units.md)",
          "",
          "# Function Reference",
          "",
          toc,
          "",
          text[2:length(text)])

error_line <- which(grepl(".*```$", text) & !grepl("^```,*", text))

text[error_line] <- gsub("```", "", text[error_line])

text <- c(text[1:error_line], "```", text[error_line + 1:length(text)])

text <- text[!is.na(text)]

writeLines(text, outfile)

rmarkdown::render("vignettes/refactor_nhdplus.Rmd", "md_document", output_file = "../docs/refactor_nhdplus.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/split_refactor.Rmd", "md_document", output_file = "../docs/split_refactor.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/refactor_catchment.Rmd", "md_document", output_file = "../docs/refactor_catchment.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/coastal_units.Rmd", "md_document", output_file = "../docs/coastal_units.md")
rstudioapi::restartSession()
