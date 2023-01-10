dir.create("outputs/markdown/", showWarnings = FALSE)

rmarkdown::render("model_code/markdown/2021_projections/results_doc.rmd",
                  output_file = "2021_results.html",
                  output_dir = "outputs/markdown")

