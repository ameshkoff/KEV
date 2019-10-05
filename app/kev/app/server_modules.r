# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# data ------------------------------

# input data

server_dt.coef.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  dt.coef.name <- paste0(module[1], ".dt.coef")
  
  dt.coef.reac <- reactive({

    if (!is.null(input[[dt.coef.name]])) {
      
      dt.coef <- hot_to_r(input[[dt.coef.name]])
      
    } else {
      
      if (is.null(values[[dt.coef.name]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        if (module != "eq") dt.coef <- cbind(dt.coef, name = paste0("product", 1:4))
        
      } else {
        
        dt.coef <- values[[dt.coef.name]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    
    if (module == "eq") {
    
      setnames(dt.coef, eval(as.name(paste0(module, ".part.names.data")))()[1:ncol(dt.coef)])
      
    } else {
      
      setnames(dt.coef, c(eval(as.name(paste0(module, ".part.names.data")))()[1:(ncol(dt.coef) - 1)], "name"))
      
    }
    
    
    values[[dt.coef.name]] <- dt.coef
    
    dt.coef
    
  })
  
  # return
  
  return(dt.coef.reac)
  
}

server_part.names.data <- function(module = c("eq")) {
  
  part.names.name <- paste0(module[1], ".part.names")
  
  part.names.reac <- reactive({
    
    if (!is.null(input[[part.names.name]])) {
      
      part.names <- input[[part.names.name]]
      part.names <- str_split(part.names, "\\, *")
      part.names <- unlist(part.names)
      
    } else {
      
      if (is.null(values[[part.names.name]])) {
        
        part.names <- "molecule1"
        
      } else {
        
        part.names <- values[[part.names.name]]
        
      }
      
    }
    
    part.names <- str_trim(part.names)
    values[[part.names.name]] <- part.names
    
    part.names
    
  })
  
  return(part.names.reac)
  
}

server_dt.conc.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  dt.conc.name <- paste0(module[1], ".dt.conc")
  
  dt.conc.reac <- reactive({
    
    if (!is.null(input[[dt.conc.name]])) {
      
      dt.conc <- hot_to_r(input[[dt.conc.name]])
      
    } else {
      
      if (is.null(values[[dt.conc.name]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[[dt.conc.name]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, eval(as.name(paste0(module, ".part.names.data")))()[1:ncol(dt.conc)])
    
    values[[dt.conc.name]] <- dt.conc
    
    dt.conc
    
  })
  
  # return
  
  return(dt.conc.reac)
  
}

server_part.eq.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  part.eq.name <- paste0(module[1], ".part.eq")
  
  part.eq.reac <- reactive({
    
    if (!is.null(input[[part.eq.name]])) {
      
      part.eq <- hot_to_r(input[[part.eq.name]])
      
    } else {
      
      if (is.null(values[[part.eq.name]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[[part.eq.name]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[[part.eq.name]] <- part.eq
    
    part.eq
    
  })
  
  # return
  
  return(part.eq.reac)
  
}

server_cnst.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  cnst.name <- paste0(module[1], ".cnst")
  
  cnst.reac <- reactive({
    
    if (!is.null(input[[cnst.name]])) {
      
      cnst <- hot_to_r(input[[cnst.name]])
      
    } else {
      
      if (is.null(values[[cnst.name]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[[cnst.name]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[[cnst.name]] <- cnst
    
    cnst
    
  })
  
  # return
  
  return(cnst.reac)
  
}


# rendering -------------------------

server_render_dt.coef <- function(module = c("eq", "ab", "emf", "nm")) {
  
  # cnst.name <- paste0(module[1], ".cnst")
  bulk.input.name <- paste0("file.", module[1], ".bulk.input")
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  dt.coef.data <- eval(as.name(paste0(module[1], ".dt.coef.data")))
  part.names.data <- eval(as.name(paste0(module[1], ".part.names.data")))
  
  rndr <- renderRHandsontable({
    
    in.file <- input[[paste0("file.", module[1], ".dt.coef")]]
    in.file.bulk <- input[[bulk.input.name]]
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source[[paste0(module[1], ".dt.coef.bulk")]]) {
      
      if (module[1] == "eq") {
        
        bs.name.load()
        
      } else if (module[1] == "ab") {
        
        try(ab.cnst.tune.load(), silent = TRUE)
        try(wl.tune.load(), silent = TRUE)
        
      } else if (module[1] == "emf") {
        
        try(emf.cnst.tune.load(), silent = TRUE)
        
      } else if (module[1] == "nm") {
        
        try(nm.cnst.tune.load(), silent = TRUE)
        
      }
      
      in.file <- as.data.table(input[[bulk.input.name]])[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients*(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input[[bulk.input.name]])[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    # choose source
    
    if (!is.null(in.file)) {
      
      if (sep.fun() == ";") {
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.coef)
      
      cln <- colnames(dt.coef)
      setnames(dt.coef, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file")
        
      )
      
      if (module[1] != "eq") {
        
        validate(need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names"))
        
      }
      
      tmp <- colnames(dt.coef)
      if (module[1] != "eq") tmp <- tmp[1:(length(tmp) - 1)]
      updateTextInput(session, paste0(module[1], ".part.names"), value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*stoich(iometric)*_coefficients*"]
      shts <- sort(shts)
      
      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file")
        
      )
      
      if (module[1] != "eq") {
        
        validate(need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names"))
        
      }
      
      tmp <- colnames(dt.coef)
      if (module[1] != "eq") tmp <- tmp[1:(length(tmp) - 1)]
      updateTextInput(session, paste0(module[1], ".part.names"), value = paste(tmp, collapse = ", "))
      
    } else {
      
      dt.coef <- dt.coef.data()
      
    }
    
    if (module[1] == "eq") {
      
      setnames(dt.coef, part.names.data()[1:ncol(dt.coef)])  
      
    } else {
      
      setnames(dt.coef, c(part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
      
    }

    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  # return
  
  return(rndr)
  
}














