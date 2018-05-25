#' @title fabio footprint
#' @author Sebastian Luckeneder, \email{sebastian.luckeneder@@wu.ac.at}
#' 
#' @description Footprint analysis based on fabio model. 
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
fabio_footprint <- function(wd, years, regions_select = "all"){
  
  
  library(dplyr)
  library(data.table)
  library(reshape2)
  year <- 2013
  regions_select <- "EU"
  
  # current_wd <- getwd()
  # setwd(wd)
  setwd("W:/WU/Projekte/SRU-Projekte/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data") #temp
  
  footprint <- function(region, fd, output){
    Yreg <- Y[,(4*region-4+fd)]
    
    # Calculate Footprint (MP * FD)
    FP <- as.data.table((t(MP) * Yreg[output!=0])) %>%
      mutate(ID = rep(items$Com.Code, nrreg)[output!=0])
    #FP$ID <- items$Com.Code
    FP <- as.data.frame(as.data.table(FP)[, lapply(.SD, sum), by = .(ID)])
    FP$ID <- NULL
    FP <- t(FP)
    return(FP)
  }
  
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
  
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  regions <- utils::read.csv(file="Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- utils::read.csv(file="Items.csv", header=TRUE, sep=";")
  # load production data with yields
  load(file=paste0("./data/Prod.RData"))
  Prod <- Prod[Prod$Element %in% c("Area harvested","Production"),]
  # aggregate RoW
  Prod$Country[! Prod$Country.Code %in% regions$Country.Code] <- "RoW"
  Prod$Country.Code[! Prod$Country.Code %in% regions$Country.Code] <- 999

  Prod <- as.data.frame(Prod %>% dplyr::group_by(Country.Code, Country, Item.Code, Item, Element, Year, Unit) %>% 
                          dplyr::summarise(Value = sum(Value)))
  Prod$ID <- paste(Prod$Country.Code,Prod$Item.Code,sep="_")
  
  ##########################################################################
  # Start loop for a series of years
  ##########################################################################
  for(year in years){
    print(year)
    
    #-------------------------------------------------------------------------
    # Read data
    #-------------------------------------------------------------------------
    load(file=paste0("./data/yearly/",year,"_L.RData"))
    load(file=paste0("./data/yearly/",year,"_Z.RData"))
    load(file=paste0("./data/yearly/",year,"_Y.RData"))
    Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]
    nrreg <- nrow(regions)
    nrcom <- nrow(Y) / nrreg
    
    
    #-------------------------------------------------------------------------
    # Prepare Extension
    #-------------------------------------------------------------------------
    x <- rowSums(Z) + rowSums(Y)
    rm(Z); gc()
    
    ext <- data.frame(Country.Code = rep(regions$Country.Code, each = nrcom),
                      Country = rep(regions$Country, each = nrcom),
                      Item.Code = rep(items$Item.Code, nrreg),
                      Item = rep(items$Item, nrreg),
                      Com.Code = rep(items$Com.Code, nrreg),
                      Group = rep(items$Group, nrreg),
                      Value = 0)

    ext$ID <- paste(ext$Country.Code,ext$Item.Code,sep="_")
    ext$Value <- Landuse$Value[match(ext$ID,Landuse$ID)]
    ext$Value[!is.finite(ext$Value)] <- 0
    
    
    #-------------------------------------------------------------------------
    # Prepare Multipliers
    #-------------------------------------------------------------------------
    MP <- as.vector(ext$Value) / x
    MP[!is.finite(MP)] <- 0
    MP <- MP * L   # is identical with L * MP
    MP <- reduce_matrix(MP,x)
    rm(L); gc()

    
    #-------------------------------------------------------------------------
    # Calculate Footprints
    #-------------------------------------------------------------------------
    regions_selected <- utils::read.csv(file="Regions_Footprint.csv", header=TRUE, sep=";")
    
    if (regions_select != "all"){
      regions_selected <- regions_selected[regions_selected$Continent %in% regions_select,]
    }
    
    FP_region <- list()
    
    # region=77
    for(region in regions_selected$Nr){
      print(paste0("region ",region))
      FP_fd <- list()
      # fd=1
      for(fd in 1:4){
        
        FP_fd[[fd]] <- footprint(region = region, fd = fd, output = x)
        
      }
      FP_region[[region]] <- FP_fd
      rm(FP_fd); gc()
    }
    
    # load(file=paste0("results/",year,"_FP_results_all.RData"))
    # load(file=paste0("results/",year,"_FP_results_selected.RData"))
    
    # Rearrange results as a list
    fd_categories <- c("1_Food","2_OtherUses","3_StockVariation","4_Balancing")
    FP <- data.table()
    # region=9
    for(region in regions_selected$Nr){
      print(paste("region",region))
      # fd = 1
      for(fd in 1:4){
        f <- as.data.table(FP_region[[region]][[fd]])
        
        filled <- matrix(0,length(x),ncol(f))
        filled[x!=0,] <- as.matrix(f)
        f <- as.data.frame(filled)
        
        colnames(f) <- as.character(items$Com.Code)
        f$From.Country.Code <- regions$Country.Code
        f$From.Country <- regions$Country
        f$From.ISO <- regions$ISO
        f <- reshape2::melt(f, id.vars = c("From.Country.Code","From.Country","From.ISO"), variable.name = "Com.Code")
        # f <- reshape2::melt(f, id.vars = "From.Country.Code")
        f$fd <- fd_categories[fd]
        f$Country_Nr <- region
        f <- f[f$value!=0,]
        FP <- rbind(FP,f)
      }
    }
    
    FP <- reshape2::dcast(FP, ... ~ fd, value.var = "value", fun.aggregate = sum)
    # FP <- FP[rowSums(FP[,6:8])>0]
    FP$Country <- regions_selected$Country[match(FP$Country_Nr,regions_selected$Nr)]
    FP$ISO <- regions_selected$ISO[match(FP$Country_Nr,regions_selected$Nr)]
    FP$Country_Nr <- NULL
    data.table::fwrite(FP, file=paste0("results/",year,"_FP_selected.csv"), sep=";")
    # write.xlsx(FP, file=paste0("results/",year,"_FP_selected_A.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
    
  }
  
  
  #setwd(current_wd)
  
  
}