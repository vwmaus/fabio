#' @title FAO_MRIO_1b_data_preparation
#' @author Sebastian Luckeneder, \email{sebastian.luckeneder@@wu.ac.at}
#' 
#' @description Build MRIO table based on Faostat commodity balance sheets and trade data part 2c: trade linking of supply tables
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
FAO_MRIO_2c_trade_linking_SUP <- function(...){
  
  rm(list=ls()); gc()
  
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  regions <- utils::read.csv(file="Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- utils::read.csv(file="Items.csv", header=TRUE, sep=";")
  
  
  ##########################################################################
  # Start loop for a series of years
  ##########################################################################
  # year=1986
  year=2013
  for(year in 1986:2013){
    print(year)
    #-------------------------------------------------------------------------
    # Read data
    #-------------------------------------------------------------------------
    load(file=paste0("data/yearly/",year,"_sup.RData"))
    load(file=paste0("data/yearly/",year,"_sup_usd.RData"))
    load(file=paste0("data/yearly/",year,"_BTD_Balanced.RData"))
    nrproc <- length(unique(sup$Proc.Code))
    nrcom <- length(unique(sup$Com.Code))
    nrreg <- nrow(regions)
    
    
    #-------------------------------------------------------------------------
    # Multi-regional Supply Tables
    #-------------------------------------------------------------------------
    # cast supply tables and store in list
    supply <- list()
    supply_usd <- list()
    
    # iso="USA"
    for(iso in regions$ISO){
      data <- data.table::data.table(sup[,1:5], Value = sup[,iso])
      data <- reshape2::dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
      data[!is.finite(data)] <- 0
      rownames(data) <- data$Proc.Code
      supply[[iso]] <- as.matrix(data[,-(1:2)])
      
      data <- data.table::data.table(sup_usd[,1:5], Value = sup_usd[,iso])
      data <- reshape2::dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
      data[!is.finite(data)] <- 0
      rownames(data) <- data$Proc.Code
      supply_usd[[iso]] <- as.matrix(data[,-(1:2)])
    }
    
    # make block diagonal matrix with supply tables = multi-regional supply table
    mr_sup <- Matrix::bdiag(supply)
    mr_sup_usd <- Matrix::bdiag(supply_usd)
    
    
    # save results
    save(mr_sup, file=paste0("data/yearly/",year,"_mr_sup.RData"))
    save(mr_sup_usd, file=paste0("data/yearly/",year,"_mr_sup_usd.RData"))
    
  }
  
  
}
