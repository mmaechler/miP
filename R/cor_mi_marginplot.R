cor_mi_marginplot <- function(input,cor = 0.5,obs = 0.5) {
    if(inherits(input, "mi")) {
        ds <- data.mi(input)
        m <- m(input)
    } else if(inherits(input, "mids")) {
        ds <- input$data
        m <- input$m
    } else if(inherits(input, "amelia")) {
        ds <- input$imputations[[1]]
        ds[input$missMatrix] <- NA
        m <- input$m
    } else stop("undefined for class ", paste(class(input), collapse=", "))
    stopifnot(is.numeric(d <- dim(ds)), length(d) == 2, d > 0)
    tief  <- d[1]
    breit <- d[2]
    for(i in 1:breit) { ## FIXME -> factor ?
        if(is.character(ds[,i])) stop("Type character not allowed!")
        class(ds[,i]) <- "numeric"
    }
    k <- 0
    mat <- matrix(rep(NA,(length(ds))^2),nrow = length(ds))
    for(i in 2:(length(ds))) {
        k <- k+1
        for(j in 1:k)
        {
            eins <- ds[,j][!is.na(ds[,j])][!is.na(ds[,i][!is.na(ds[,j])])]
            zwei <- ds[,i][!is.na(ds[,j])][!is.na(ds[,i][!is.na(ds[,j])])]
            corre <- cor(eins,zwei)
            lang <- length(eins)
            if(abs(corre) > cor)
            {
                if((lang/tief) > obs) {
                    mat[i,j] <- corre
                }
                ## else {
                ##     mat[i,j] <- paste("x:",lang/tief)
                ## }
            }
        }
    }
    p <- which(!is.na(mat))
    aus <- matrix(rep(0,(length(p))*2),ncol = 2)
    for(i in 1:length(p)) {
        aus[i,] <- c(p[i]-floor((p[i]/dim(mat)[1])-0.0000001)*dim(mat)[1],
                      ceiling(p[i]/dim(mat)[1]))
    }
    m <- dim(aus)[1]
    par(mfrow = c(ceiling(m/ceiling(sqrt(m))),ceiling(sqrt(m))))
    for (i in 1:m)
    {
        mi_marginplot(names(ds[aus[i,]]),input,pch = 16,leg = FALSE)
    }

#### Korrelationsmatrix ausgeben
    rownames(mat) = names(ds)
    colnames(mat) = names(ds)
                                        #mat
                                        #p
    mat
}

