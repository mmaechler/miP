obs_plot <- function(ds, var, main="default") {
    stopifnot(is.character(var), length(var) == 1,
              is.character(nd <- names(ds)), length(nd) >= 1)
    def.main <- identical(main, "default")
    var_nr <- which(nd == var)
    ii <- seq_along(nd)[-var_nr]
    d.v <- ds[, var_nr]
    R.v <- !is.na(d.v)
    t <- length(ii)
    par(mfrow=c(ceiling(t/ceiling(sqrt(t))),
                ceiling(sqrt(t))))
    n <- nrow(ds)
    for (i in ii)
    {
        RRi <- !is.na(vRi <- ds[R.v,i])
        eins <- d.v[R.v][RRi]
        if(def.main)
            main <- paste(round(100 * length(eins)/n, 2),"% observed")
        plot(d.v[R.v], vRi,
             main=main, font.main=2, pch=16, xlab=nd[var_nr], ylab=nd[i])
        lines(lowess(eins, vRi[RRi]), col="red")
    }
}

