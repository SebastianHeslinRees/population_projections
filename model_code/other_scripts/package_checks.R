#devtools checks
devtools::check('model_code/popmodules')
devtools::check('model_code/trendmodel')
devtools::check('model_code/housingledmodel')
devtools::check('model_code/smallareamodel')

#install models
devtools::document('model_code/popmodules')
devtools::install('model_code/popmodules', dependencies = FALSE)
popmodules::install_gla_models()

#codetools checks
library(popmodules); library(trendmodel); library(housingledmodel); library(smallareamodel)
codetools::checkUsagePackage("popmodules", suppressUndefined = T)
codetools::checkUsagePackage("trendmodel", suppressUndefined = T)
codetools::checkUsagePackage("housingledmodel", suppressUndefined = T)
codetools::checkUsagePackage("smallareamodel", suppressUndefined = T)
