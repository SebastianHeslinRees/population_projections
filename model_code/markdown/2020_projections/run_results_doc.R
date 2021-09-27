dir.create("outputs/markdown/", showWarnings = FALSE)

rmarkdown::render("model_code/markdown/2020_projections/results_doc.rmd",
                  output_file = "2020_results.html",
                  output_dir = "outputs/markdown")

