#' @title FAO_MRIO_2d_trade_linking_USE
#' @author Sebastian Luckeneder, \email{sebastian.luckeneder@@wu.ac.at}
#' 
#' @description Build MRIO table based on Faostat commodity balance sheets and trade data part 2d: trade linking of use tables
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
FAO_MRIO_2d_trade_linking_USE <- function(...){

  rm(list=ls()); gc()
  
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- read.csv(file="Items.csv", header=TRUE, sep=";")
  
  
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
    load(file=paste0("data/yearly/",year,"_use.RData"))
    load(file=paste0("data/yearly/",year,"_BTD_Balanced.RData"))
    load(file=paste0("data/yearly/",year,"_CBS_Balanced_postuseallocation.RData"))
    nrproc <- length(unique(use$Proc.Code))
    nrcom <- length(unique(use$Com.Code))
    nrreg <- nrow(regions)
    
    
    # usetable <- matrix(0, nrproc*nrreg, nrcom*nrreg)
    # suptable <- matrix(0, nrproc*nrreg, nrcom*nrreg)
    # rows <- apply(cbind(rep(as.character(regions$ISO), each = nrproc), rep(as.character(unique(sup$Proc.Code)), nrreg)), 1, paste, collapse="_")
    # cols <- apply(cbind(rep(as.character(regions$ISO), each = nrcom), rep(as.character(unique(sup$Com.Code)), nrreg)), 1, paste, collapse="_")
    # rownames(usetable) <- rows
    # colnames(usetable) <- cols
    
    #-------------------------------------------------------------------------
    # Multi-regional Use Tables
    #-------------------------------------------------------------------------
    # cast BTD
    BTD$Com.Code <- items$Com.Code[match(BTD$Item.Code,items$Item.Code)]
    BTD$From.ISO <- regions$ISO[match(BTD$From.Country.Code,regions$Country.Code)]
    BTD$To.ISO <- regions$ISO[match(BTD$To.Country.Code,regions$Country.Code)]
    BTDcast <- as.data.frame(reshape2::dcast(as.data.table(BTD), From.Country.Code + From.ISO + Com.Code ~ To.Country.Code, value.var = "Value", fun.aggregate = sum))
    
    # cast Production
    CBS$Com.Code <- items$Com.Code[match(CBS$Item.Code,items$Item.Code)]
    Prod <- as.data.frame(reshape2::dcast(as.data.table(CBS), Country.Code + Com.Code ~ Country.Code, value.var = "Production", fun.aggregate = sum))
    
    # Add cast Production and cast BTD to a total supply matrix
    Total <- Prod
    Total[,-(1:2)] <- Prod[,-(1:2)] + BTDcast[,-(1:3)]
    
    # cast Total Supply
    TScast <- stats:aggregate(. ~ Com.Code, Total[,-1], sum)
    TScast <- TScast[match(Total$Com.Code,TScast$Com.Code),]
    
    # calculate supply shares, i.e. the percentage of total supply of each commodity supplied by each country
    SupplyShares <- Total[,-(1:2)] / TScast[,-1]
    SupplyShares[!is.finite(SupplyShares)] <- 0
    SupplyShares <- as.matrix(SupplyShares)
    # negatives <- data.frame(Value = SupplyShares[SupplyShares < 0])
    
    rm(Prod,TScast,BTD,BTDcast,CBS); gc()
    
    # reshape2::melt and cast use table
    use <- as.data.frame(reshape2::melt(as.data.table(use), id=1:6, variable.name = "ISO"))
    use$Country.Code <- regions$Country.Code[match(use$ISO,regions$ISO)]
    use <- as.data.frame(reshape2::dcast(as.data.table(use), Com.Code ~ Country.Code + Proc.Code, value.var = "value", fun.aggregate = sum))
    
    # trade-linking of use table
    mr_use <- use[match(Total$Com.Code,use$Com.Code),]
    mr_use <- as.matrix(mr_use[,-1])
    #i=0
    for(i in (1:nrreg)-1){
      # Attention: data.frame can be multiplied with data.frame, or matrix with matrix, not matrix with data.frame!
      mr_use[,(1:nrproc)+i*nrproc] <- mr_use[,(1:nrproc)+i*nrproc] * SupplyShares[,i+1]
    }
    gc()
    # mr_use[!is.finite(mr_use)] <- 0
    
    # trade-linking of final demand use table
    mr_use_fd <- use_FD[match(Total$Com.Code,use$Com.Code),]
    mr_use_fd <- as.matrix(mr_use_fd[,-(1:3)])
    #i=0
    for(i in (1:nrreg)-1){
      mr_use_fd[,(1:4)+i*4] <- mr_use_fd[,(1:4)+i*4] * SupplyShares[,i+1]
    }
    gc()
    # mr_use_fd[!is.finite(mr_use_fd)] <- 0
    
    # save results
    save(mr_use, mr_use_fd, file=paste0("data/yearly/",year,"_mr_use.RData"))
    
    rm(mr_use,mr_use_fd,SupplyShares,Total,use)
    gc()
  }
  
  
  # # check results
  # check <- data.frame(original = colSums(use_FD[,-(1:3)]), mr = colSums(mr_use_fd))
  # check$diff <- round(check$original - check$mr)
  # SupplyShares[,192]
  # TScast[24831:24960,193]
  
}
