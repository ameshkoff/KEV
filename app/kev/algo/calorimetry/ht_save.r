# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #

library(data.table)


ht.save.prepare.data <- function(dt.ttl = list(), dt.dict = data.table()) {
  
  # setup
  
  dt.nms <- names(dt.ttl)
  dt.nms <- dt.nms[dt.nms %in% c("cnst.tune", "cmp.tune.input", "calorimeter.type.input", "init.vol.input")]
  
  dt.setup <- lapply(dt.nms, function(cl) { dt.ttl[[cl]] })
  names(dt.setup) <- dt.nms

  dt.setup <- Filter(Negate(is.null), dt.setup)
  
  max.l <- max(sapply(dt.setup, length))
  dt.setup <- lapply(dt.setup, function(x) { x <- c(x, rep(NA, max.l - length(x))) })
  
  dt.setup <- as.data.table(t(as.data.frame(dt.setup)), keep.rownames = TRUE)
  
  dt.setup <- merge(dt.dict[, .(rn = dt, file)], dt.setup, by = "rn")
  dt.setup[, rn := NULL]
  
  dt.setup[, file := str_remove(file, "\\_setup$")]
  
  cln <- unlist(dt.setup[1])
  dt.setup <- dt.setup[2:nrow(dt.setup)]
  setnames(dt.setup, cln)
  
  dt.ttl[["setup"]] <- dt.setup
  
  # input concentrations
  
  part.eq.input <- unlist(dt.ttl$part.eq.input)
  
  if (is.numeric(part.eq.input)) {
    
    part.eq.input <- rep("tot", ncol(dt.ttl$dt.conc.input))
    part.eq.input[dt.ttl$part.eq.input] <- "eq"
    
  }
  
  dt.ttl$dt.conc.input <- rbind(as.list(colnames(dt.ttl$dt.conc.input))
                                , dt.ttl$dt.conc.input, use.names = FALSE)

  if (length(part.eq.input) < ncol(dt.ttl$dt.conc.input)) part.eq.input <- c(part.eq.input, rep("tot", ncol(dt.ttl$dt.conc.input) - length(part.eq.input)))
  setnames(dt.ttl$dt.conc.input, part.eq.input)
  
  # correlation matrix
  
  if (!is.null(dt.ttl$cor.m))
    dt.ttl$cor.m <- as.data.table(dt.ttl$cor.m, keep.rownames = "")
  
  # remove garbage
  
  dt.ttl <- dt.ttl[!(names(dt.ttl) %in% c(dt.nms, "part.eq.input"))]
  
  dt.ttl
    
}

ht.save <- function() {
  
  dt.dict <- fread("dt.dict.csv")
  
  function(dt.ttl = list()
           , path = ""
           , sep = ";"
           , filename = NULL) {
    
    if (path != "")
      path <- paste0(path, "/")
    path <- str_replace_all(path, "\\/\\/", "/")

    # dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
    
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
    dt.ttl <- ht.save.prepare.data(dt.ttl, dt.dict)
    dt.ttl <- Filter(Negate(is.null), dt.ttl)
    
    dt.dict.work <- copy(dt.dict)
    dt.dict.work <- dt.dict.work[(dt %in% names(dt.ttl))]
    
    if (is.null(filename)) {
      
      for (i in dt.dict.work[, dt]) {
        
        save.header <- TRUE

        if (sep == ";"){
          write.table(dt.ttl[[i]], dt.dict.work[dt == i, paste0(path, file, ".csv")], row.names = FALSE, sep = ";", dec = ",", col.names = save.header)
        } else if (sep == ",") {
          write.table(dt.ttl[[i]], dt.dict.work[dt == i, paste0(path, file, ".csv")], row.names = FALSE, sep = ",", dec = ".", col.names = save.header)
        } else if (sep %in% c("tab", "\t")) {
          write.table(dt.ttl[[i]], dt.dict.work[dt == i, paste0(path, file, ".txt")], row.names = FALSE, sep = "\t", col.names = save.header)
        }
        
      }
      
    } else {
      
      dt.ttl <- dt.ttl[names(dt.ttl) %in% dt.dict.work[, dt]]
      dt.dict.work <- dt.dict.work[match(names(dt.ttl), dt)]
      names(dt.ttl) <- dt.dict.work[, file]
      
      write.xlsx(dt.ttl, paste0(path, filename))
      
    }
    
    dt.dict.work
    
  }

}

ht.save <- ht.save()


  
