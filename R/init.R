#' @importFrom utils read.csv read.csv2 packageDescription
#' @importFrom openxlsx read.xlsx
#' @importFrom comtradr ct_register_token ct_search
#' @importFrom data.table data.table as.data.table dcast melt
#' @importFrom stats aggregate
NULL

.onAttach = function(lib, pkg){
  packageStartupMessage(
    sprintf("Loaded fabio v%s. See ?fabio for help, citation(\"fabio\") for use in publication.\n",
            utils::packageDescription("fabio")$Version) )
}

