#devtools checks
devtools::check('model_code/popmodules')
devtools::check('model_code/trendmodel')
devtools::check('model_code/flexibleareamodel')


#install models
devtools::document('model_code/popmodules')
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models(trend=TRUE, flex_area=TRUE)

#codetools checks
library(popmodules); library(trendmodel); library(flexibleareamodel)
codetools::checkUsagePackage("popmodules", suppressUndefined = T)
codetools::checkUsagePackage("trendmodel", suppressUndefined = T)
codetools::checkUsagePackage("flexibleareamodel", suppressUndefined = T)


