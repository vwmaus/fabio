#' @importFrom utils read.csv packageDescription
#' @importFrom openxlsx read.xlsx
#' @importFrom comtradr ct_register_token ct_search
NULL

.onAttach = function(lib, pkg){
  packageStartupMessage(
    sprintf("Loaded fabio v%s. See ?fabio for help, citation(\"fabio\") for use in publication.\n",
            utils::packageDescription("fabio")$Version) )
}

