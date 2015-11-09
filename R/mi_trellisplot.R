mi_trellisplot <-
    function(x, var, input, nr = 1, eqnr = 3, ol = 0.1,
             main = NULL,
             col = c("blue","red"), pch = 16, font.main = 2, ...)
{
    if(inherits(input, "mi")) {
        data <- data.mi(input)
        m <- m(input)
        IMPdata <- mi.data.frame(input, m = nr)
    } else if(inherits(input, "mids")) {
        data <- input$data
        m <- input$m
        IMPdata <- complete(input, action = nr)
    } else if(inherits(input, "amelia")) {
        data <- input$imputations[[1]]
        data[input$missMatrix] <- NA
        m <- input$m
        IMPdata <- input$imputations[[nr]]
    } else stop("not implemented for class ", paste(class(input), collapse=", "))
    stopifnot(is.character(x), length(x) == 2, var %in% (nd <- names(data)))
    dv <- data[,var]
    if(any(iv <- is.na(match(x, nd))))
        stop("invalid variable name(s) in 'x': ", paste(x[iv], collapse=", "))
    d.imp <- rep("no", length(dv))
    d.imp[is.na(dv)] <- "yes"
    IMPdata$imp <- factor(d.imp)
    IMPdata <- IMPdata[,c(x,var,"imp")]

    formel <- paste(x, collapse = " ~ ")
    if(!is.factor(dv)) {
        group <- equal.count(IMPdata[,var], number = eqnr, overlap = ol)
        formel <- paste(formel, "| group")
    }
    else ## is.factor(dv)
        formel <- paste(formel, "|", var)

    xyplot(as.formula(formel), IMPdata, groups = imp,
           main = if(is.null(main)) formel else main,
           col=col, pch=pch, font.main=font.main, ...)
}

