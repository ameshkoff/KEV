# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# data ------------------------------

# input data

server_dt.coef.data <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
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

server_dt.conc.data <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
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

server_part.eq.data <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
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

server_cnst.data <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
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

# concentrations

server_render_dt.coef <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
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
        
      } else if (module[1] == "ht") {
        
        try(ht.setup.load(), silent = TRUE)
        
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
      
      validate(need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file"))
      
      setDT(dt.coef)
      
      cln <- colnames(dt.coef)
      setnames(dt.coef, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file"))
      
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

server_render_dt.conc <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  bulk.input.name <- paste0("file.", module[1], ".bulk.input")
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  dt.conc.data <- eval(as.name(paste0(module[1], ".dt.conc.data")))
  part.names.data <- eval(as.name(paste0(module[1], ".part.names.data")))
  
  rndr <- renderRHandsontable({
    
    in.file <- input[[paste0("file.", module[1], ".dt.conc")]]
    in.file.bulk <- input[[bulk.input.name]]
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source[[paste0(module[1], ".dt.conc.bulk")]]) {
      
      in.file <- as.data.table(input[[bulk.input.name]])[name %like% "^(input\\_)*concentrations*(\\.csv|\\.txt)*"][1]
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
    
    if (module[1] == "eq" && input.source[[paste0(module[1], ".dt.conc.pc.fl")]]) {
      
      in.file <- NULL
      in.file.xlsx <- NULL
      
    }
    
    # choose source
    
    if (!is.null(in.file)) {
      
      if (sep.fun() == ";") {
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      }
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      setDT(dt.conc)
      
      cln <- colnames(dt.conc)
      setnames(dt.conc, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      tmp <- colnames(dt.conc)
      if (str_to_lower(tail(tmp, 1)) == "series") tmp <- tmp[1:(length(tmp) - 1)]
      updateTextInput(session, paste0(module[1], ".part.names"), value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations*"]
      shts <- sort(shts)
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], startRow = 2), silent = TRUE)
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      if (str_to_lower(tail(tmp, 1)) == "series") tmp <- tmp[1:(length(tmp) - 1)]
      updateTextInput(session, paste0(module[1], ".part.names"), value = paste(tmp, collapse = ", "))
      
    } else if (module[1] == "eq" && input.source[[paste0(module[1], ".dt.conc.pc.fl")]]) {
      
      dt.conc <- eq.pc.update()$eq.dt.conc
      
    } else {
      
      dt.conc <- dt.conc.data()
      
    }
    
    setnames(dt.conc, part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc)) {
      
      if (nrow(dt.conc) > 15) {
        
        rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE, height = 300) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      } else {
        
        rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE, height = NULL) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      }
      
    }

  })
  
  # return
  
  return(rndr)
  
}

server_render_part.eq <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  bulk.input.name <- paste0("file.", module[1], ".bulk.input")
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  dt.conc.data <- eval(as.name(paste0(module[1], ".dt.conc.data")))
  part.eq.data <- eval(as.name(paste0(module[1], ".part.eq.data")))

  rndr <- renderRHandsontable({
    
    in.file <- input[[paste0("file.", module[1], ".dt.conc")]]
    in.file.bulk <- input[[bulk.input.name]]
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source[[paste0(module[1], ".dt.conc.bulk")]]) {
      
      in.file <- as.data.table(input[[bulk.input.name]])[name %like% "^(input\\_)*concentrations*(\\.csv|\\.txt)*"][1]
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
    
    if (module[1] == "eq" && input.source[[paste0(module[1], ".dt.conc.pc.fl")]]) {
      
      in.file <- NULL
      in.file.xlsx <- NULL
      
    }
    
    # choose source
    
    part.eq <- part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (sep.fun() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE, check.names = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE, check.names = FALSE)[1, ], silent = TRUE)
        
      } else if (sep.fun() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE, check.names = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE, check.names = FALSE)[1, ], silent = TRUE)
        
      } else if (sep.fun() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE, check.names = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE, check.names = FALSE)[1, ], silent = TRUE)
        
      }
      
      validate(need(is.data.frame(part.eq), "Check the column delimiter or content of your file"))
      
      setDT(part.eq)
      part.eq[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]

      tmp <- unlist(tmp)
      if (str_to_lower(tail(tmp, 1)) == "series") tmp <- tmp[1:(length(tmp) - 1)]
      if (ncol(part.eq) - length(tmp) == 1) part.eq <- as.data.table(part.eq)[, 1:length(tmp), with = FALSE]
      
      validate(need(ncol(part.eq) == length(tmp), "Check the column delimiter or content of your file"))
      
      colnames(part.eq) <- tmp
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations*"]
      shts <- sort(shts)
      
      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 2), silent = TRUE)

      validate(need(is.data.frame(part.eq), "Check the column delimiter or content of your file"))
      
      tmp <- unlist(tmp)
      if (str_to_lower(tail(tmp, 1)) == "series") tmp <- tmp[1:(length(tmp) - 1)]
      if (ncol(part.eq) - length(tmp) == 1) part.eq <- as.data.table(part.eq)[, 1:length(tmp), with = FALSE]
      
      validate(need(ncol(part.eq) == length(tmp), "Check the column delimiter or content of your file"))
      
      colnames(part.eq) <- tmp
      
    } else if (module[1] == "eq" && input.source[[paste0(module[1], ".dt.conc.pc.fl")]]) {
      
      part.eq <- eq.pc.update()$eq.part.eq
      
    }
    
    if (!is.null(part.eq))
      rhandsontable(part.eq, stretchH = "all", useTypes = FALSE, colHeaders = NULL) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  # return
  
  return(rndr)
  
}

server_render_cnst <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  bulk.input.name <- paste0("file.", module[1], ".bulk.input")
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  dt.conc.data <- eval(as.name(paste0(module[1], ".dt.conc.data")))
  cnst.data <- eval(as.name(paste0(module[1], ".cnst.data")))

  rndr <- renderRHandsontable({
    
    in.file <- input[[paste0("file.", module[1], ".cnst")]]
    in.file.bulk <- input[[bulk.input.name]]
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source[[paste0(module[1], ".cnst.bulk")]]) {
      
      in.file <- as.data.table(input[[bulk.input.name]])[name %like% "^(input_|output_)*(k_constants*_log10|constants*)(\\_evaluated)*(\\.csv|\\.txt)*$"][1]
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
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (sep.fun() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      validate(need(is.data.frame(cnst), "Check the column delimiter or content of your file"))
      
      setDT(cnst)
      
      cln <- colnames(cnst)
      setnames(cnst, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(need(length(colnames(cnst)[colnames(cnst) %like% "^[Cc]onstant$|^k_constants_log10$|^cnst$|^lg_k$|log10"]) == 1
                    , "Check the column delimiter or content of your file"))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*(k_constants*_log10|constants*)(\\_evaluated)*$"]
      shts <- sort(shts)
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^[Cc]onstant$|^k_constants_log10$|^cnst$|^lg_k$|log10"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else {
      
      cnst <- cnst.data()
      
    }
    
    setDT(cnst)
    
    cln <- colnames(cnst)
    cln <- cln[cln %like% "^Constant$|^k_constants*_log10$|^cnst$|^lg_k$|log10"]
    
    cnst <- cnst[, cln, with = FALSE]
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  # return
  
  return(rndr)
  
}

server_render_dt.res <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.res.data <- eval(as.name(paste0(module[1], ".dt.res.data")))

  rndr <- renderRHandsontable({
    
    dt.res <- dt.res.data()
    
    if (!is.null(dt.res)) {
      
      renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (parseInt(value, 10) < 0) {
        td.style.background = 'pink';
        }
        
      }" 
      
      if (nrow(dt.res) > 15) {
        
        rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE, height = 300) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(renderer = renderer)
        
      } else {
        
        rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE, height = NULL) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_cols(renderer = renderer)
        
      }
      
    }
    
  })
  
  # return
  
  return(rndr)
  
}

# constants

server_render_cnst.dev <- function(module = c("ab", "emf", "nm", "ht")) {
  
  cnst.dev.data <- eval(as.name(paste0(module[1], ".cnst.dev.data")))
  
  rndr <- renderRHandsontable({
    
    cnst.dev <- cnst.dev.data()
    
    if (!is.null(cnst.dev)) {
      
      row_highlight <- cnst.dev[Validity != "OK", which = TRUE] - 1
      
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
        
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
        hrows = instance.params.row_highlight
        hrows = hrows instanceof Array ? hrows : [hrows]
        }
        
        if (instance.params && hrows.includes(row)) {
        td.style.background = 'pink';
        }
        
        }" 
      
      rhandsontable(cnst.dev, stretchH = FALSE, row_highlight = row_highlight, useTypes = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    }
    
  })
  
  # return
  
  return(rndr)
  
}

server_render_cor.m <- function(module = c("ab", "emf", "nm", "ht")) {
  
  cor.m.data <- eval(as.name(paste0(module[1], ".cor.m.data")))
  
  rndr <- renderRHandsontable({
    
    cor.m <- cor.m.data()
    
    if (!is.null(cor.m)) {
      
      rhandsontable(cor.m, stretchH = FALSE, useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    }
    
  })
  
  # return
  
  return(rndr)
  
}

server_render_adj.r.squared <- function(module = c("ab", "emf", "nm", "ht")) {
  
  adj.r.squared.data <- eval(as.name(paste0(module[1], ".adj.r.squared.data")))
  
  rndr <- renderRHandsontable({
    
    adj.r.squared <- adj.r.squared.data()
    
    if (!is.null(adj.r.squared)) {
      
      row_highlight <- adj.r.squared[`Adj. R^2` < .95, which = TRUE] - 1
      
      adj.r.squared[, `Adj. R^2` := as.character(round(`Adj. R^2`, 6))]
      
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
        
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
        hrows = instance.params.row_highlight
        hrows = hrows instanceof Array ? hrows : [hrows]
        }
        
        if (instance.params && hrows.includes(row)) {
        td.style.background = 'pink';
        }
        
      }" 
      
      rhandsontable(adj.r.squared, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    }
    
  })
  
  # return
  
  return(rndr)
  
}

server_render_dt.metrics <- function(module = c("ab", "emf", "nm", "ht")) {
  
  dt.metrics.data <- eval(as.name(paste0(module[1], ".dt.metrics.data")))
  
  rndr <- renderRHandsontable({
    
    dt.metrics <- dt.metrics.data()
    
    if (!is.null(dt.metrics)) {
      
      row_highlight <- dt.metrics[(str_to_lower(metrics) == "adj.r^2" & value < .95) |
                                 (str_to_lower(metrics) %in% c("nrmse", "smape") & value > .1), which = TRUE] - 1
      
      dt.metrics[, value := as.character(round(value, 6))]
      
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
        
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
        hrows = instance.params.row_highlight
        hrows = hrows instanceof Array ? hrows : [hrows]
        }
        
        if (instance.params && hrows.includes(row)) {
        td.style.background = 'pink';
        }
        
      }" 
      
      rhandsontable(dt.metrics, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    }
    
  })
  
  # return
  
  return(rndr)
  
}










