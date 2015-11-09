mi_parcoord <- function(input, var, n = "all", nr = 1, col = "rainbow")
{
    if(inherits(input, "mi")) {
        data <- data.mi(input)
        m <- m(input)
    } else if(inherits(input, "mids")) {
        data <- input$data
        m <- input$m
    } else if(inherits(input, "amelia")) {
        data <- input$imputations[[1]]
        data[input$missMatrix] = NA
        m <- input$m
    } else stop("not implemented for class ", paste(class(input), collapse=", "))

    nd <- names(data)
    breit <- dim(data)[2]
    if(toString(n) == "all") {
	x <- 1:breit
    } else { ## toString(n) != "all"
	x <- match(n, nd)
	if(any(iv <- is.na(x)))
	    stop("invalid variable name(s) in 'n': ", paste(n[iv], collapse=", "))
    }

    stopifnot(is.character(var), length(var) == 1)
    var_nr <- which(nd == var)
    if(!length(var_nr)) stop("invalid variable name", var)
    mis_obs <- which(is.na(data[,var_nr]))

    if(length(nr) == 1) {
        IMPdata <- if(inherits(input, "mi"))
                       mi.data.frame(input, m = nr)
                   else if(inherits(input, "mids"))
                       complete(input, action = nr)
                   else if(inherits(input, "amelia"))
                       input$imputations[[nr]]
        parcoord(IMPdata[,x], col = "grey", pch = breit)
        par(new = TRUE)
        parcoord(IMPdata[mis_obs,x], col = "red", pch = breit)
    } else { ## length(nr) >= 2)
        if(length(col) == 1)
            col <- rainbow(m)
        parcoord(data[,x], col = "grey", pch = breit)
        par(new = TRUE)
        for(i in nr) {
            IMPdata <- if(inherits(input, "mi"))
                           mi.data.frame(input, m = i)
                       else if(inherits(input, "mids"))
                           complete(input, action = i)
                       else if(inherits(input, "amelia"))
                           input$imputations[[i]]
            parcoord(IMPdata[mis_obs,x], col = col[which(nr == i)], pch = breit)
            par(new = TRUE)
        }
    }
    par(new = FALSE)
    ## c(m,names(IMPdata)[var_nr],IMPdata[mis_obs,])
}

