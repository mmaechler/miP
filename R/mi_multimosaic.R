mi_multimosaic <- function(x, input, nr = 1) {
    if(inherits(input, "mi")) {
        ds <- data.mi(input)
        m <- m(input)
    } else if(inherits(input, "mids")) { ## i.e. pkg{mice}
        ds <- input$data
        m <- input$m
    } else stop("undefined for class ", paste(class(input), collapse=", "))
    stopifnot(is.numeric(d <- dim(ds)), length(d) == 2, d > 0,
              is.character(x))
    tief  <- d[1]
    breit <- d[2]

    var_nr <- match(x, names(ds))
    if(any(iv <- is.na(var_nr)))
        stop("invalid variable name(s) ", paste(x[iv], collapse=", "))
    ## FIXME: ds[,] is not used anymore after "imp" computation; use 'd.imp <- *'
    ds$imp <- rep("no",tief)
    ds <- ds[,c(var_nr,breit+1)]
    breit <- length(var_nr)
    for(i in 1:tief) {
        for(j in 1:breit) {
            if(is.na(ds[i,j]))
                ds[i,breit+1] <- "yes"
        }
    }
    breit  <- breit+1
    if(inherits(input, "mids")) {
        IMPdata  <- complete(input, action = nr)
    } else if(inherits(input, "mi")) {
        IMPdata  <- mi.data.frame(input, m = nr)
    }
    IMPdata$imputed <- factor(ds$imp)
    IMPdata  <- IMPdata[,c(var_nr,dim(IMPdata)[2])]
    RHS <- paste(x, collapse = "+")
    mosaic(as.formula(paste("imputed ~",RHS)), data = IMPdata,
           gp = gpar(fill = c("blue","red")))
}

