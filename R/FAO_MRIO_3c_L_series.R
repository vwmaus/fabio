#' @title FAO_MRIO_3c_L_series
#' @author Sebastian Luckeneder, \email{sebastian.luckeneder@@wu.ac.at}
#' 
#' @description Build MRIO table based on Faostat commodity balance sheets and trade data part 3c: derive Leontief inverse matrix via power series
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
FAO_MRIO_3c_L_series <- function(...){
  
  rm(list=ls()); gc()
  
  reduce_matrix <- function(x,y){
    x <- x[y!=0,y!=0]
    return(x)
  }
  
  refill_matrix <- function(x,y){
    id <- 1:length(y)
    id <- id[y!=0]
    filled <- matrix(0,length(y),length(y))
    filled[id,id] <- x
    rownames(filled) <- names(y)
    return(filled)
  }
  
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  reg <- utils::read.csv(file="Regions.csv", header=TRUE, sep=";")
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
    load(file=paste0("data/yearly/",year,"_A.RData"))
    load(file=paste0("data/yearly/",year,"_X.RData"))
    
    A <- reduce_matrix(A,X)
    
    #-------------------------------------------------------------------------
    # Prepare L-Inverse by power series
    #-------------------------------------------------------------------------
    A2 <- A %*% A
    A2[!is.finite(A2)] <- 0
    gc()
    # save(A2, file=paste0("data/yearly/",year,"_A2.RData"))
    # gc()
    
    A3 <- A2 %*% A
    A3[!is.finite(A3)] <- 0
    gc()
    # save(A3, file=paste0("data/yearly/",year,"_A3.RData"))
    # rm(A,A2); gc()
    
    A4 <- A3 %*% A
    A4[!is.finite(A4)] <- 0
    gc()
    # save(A4, file=paste0("data/yearly/",year,"_A4.RData"))
    # rm(A3); gc()
    
    A5 <- A4 %*% A
    A5[!is.finite(A5)] <- 0
    gc()
    # save(A5, file=paste0("data/yearly/",year,"_A5.RData"))
    # rm(A4); gc()
    
    A6 <- A5 %*% A
    A6[!is.finite(A6)] <- 0
    gc()
    # save(A6, file=paste0("data/yearly/",year,"_A6.RData"))
    # rm(A5); gc()
    
    A7 <- A6 %*% A
    A7[!is.finite(A7)] <- 0
    gc()
    
    A8 <- A7 %*% A
    A8[!is.finite(A8)] <- 0
    gc()
    
    
    # load(file=paste0("data/yearly/",year,"_A.RData"))
    # load(file=paste0("data/yearly/",year,"_A2.RData"))
    # load(file=paste0("data/yearly/",year,"_A3.RData"))
    # load(file=paste0("data/yearly/",year,"_A4.RData"))
    # load(file=paste0("data/yearly/",year,"_A5.RData"))
    # load(file=paste0("data/yearly/",year,"_A6.RData"))
    
    L <- diag(nrow(A))+A+A2+A3+A4+A5+A6+A7+A8
    
    L <- refill_matrix(L,X)
    
    save(L, file=paste0("data/yearly/",year,"_L.RData"))
    
    rm(A,A2,A3,A4,A5,A6,A7,A8,L); gc()
    
  }
  
  
}
