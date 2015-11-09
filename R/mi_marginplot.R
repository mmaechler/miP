mi_marginplot <-
function (x,input, leg=TRUE, col = c("skyblue", "red", "red4"),
alpha = NULL, pch = c(1, 16), cex = par("cex"),
numbers = TRUE, cex.numbers = par("cex"),
zeros = FALSE, xlim = NULL, ylim = NULL,
main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
ann = par("ann"), axes = TRUE, frame.plot = axes, ...)
{
if(class(input) == "mi")
{
data = data.mi(input)
m = m(input)
}
if(class(input) == "mids")
{
data = input$data
m = input$m
}
if(class(input) == "amelia")
{
data = input$imputations[[1]]
data[input$missMatrix] = NA
m = input$m
}
var1_nr = x[1]
var2_nr = x[2]
x = data[,c(var1_nr,var2_nr)]
#### MI Daten bekommen
tief = dim(data)[1]
breit = dim(data)[2]
mis_data1 = 0
for(i in 1:breit)
{
mis_data1[i] = length(data[,i][is.na(data[,i])])
}
anzahl_mis_var = length(which(mis_data1 != 0))
data = data[,order(mis_data1)]
mis_data = mis_data1[order(mis_data1)]
IMP.data = as.list(data)
x1 = (breit-anzahl_mis_var+1)
xm = x1+breit
for (i in 2:m)
{ xm = c(xm,x1+(breit*i)) }

if(class(input) == "mi")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,mi.data.frame(input, m = as.integer(paste(i)))[,order(mis_data1)])
}
}
if(class(input) == "mids")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,complete(input, action = as.integer(paste(i)))[,order(mis_data1)])
}
}
if(class(input) == "amelia")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,input$imputations[[as.integer(paste(i))]][,order(mis_data1)])
}
}

{i = breit-anzahl_mis_var+1
mat = matrix(rep(0,(mis_data[i])*m),ncol=m)
count = 1}
for (j in xm)
{
mat[1:(mis_data[i]),count] = IMP.data[[j]][order(!is.na(IMP.data[[i]]))][1:(mis_data[i])]
count = count+1
}
ausgabe = list(xm = mat)
xm = xm+1

for (i in (breit-anzahl_mis_var+2):breit)
{
mat = matrix(rep(0,(mis_data[i])*m),ncol=m)
count = 1
for (j in xm)
{
mat[1:(mis_data[i]),count] = IMP.data[[j]][order(!is.na(IMP.data[[i]]))][1:(mis_data[i])]
count = count+1
}
ausgabe = c(ausgabe,list(xm = mat))
xm = xm+1
}
#ausgabe = ausgabe[[-1]]
names(ausgabe) = names(data)[(breit-anzahl_mis_var+1):breit]

####
dots <- list(...)
if (missing(cex.numbers) && "cex.text" %in% names(dots)) {
cex.numbers <- dots$cex.text
}
if (!(inherits(x, c("data.frame", "matrix")))) {
stop("x must be a data.frame or matrix")
}
if (ncol(x) != 2)
stop("'x' must be 2-dimensional")
fillbox <- TRUE
if (length(col) == 0)
col <- c("skyblue", "red", "red4")
else if (length(col) == 1) {
col <- rep.int(col, 3)
fillbox <- FALSE
}
else if (length(col) == 2)
col <- rep(col, 1:2)
else if (length(col) > 3)
col <- col[1:3]
if (length(pch) == 0)
pch <- c(1, 16)
else if (length(pch) == 1)
pch <- c(pch, 16)
else if (length(pch) > 2)
pch <- pch[1:2]
if (!is.logical(zeros) || length(zeros) == 0)
zeros <- FALSE
zeros <- rep(sapply(zeros, isTRUE), length.out = 2)
if (is.data.frame(x))
x <- data.matrix(x)
else if (mode(x) != "numeric")
mode(x) <- "numeric"
iInf <- apply(x, 1, function(x) any(is.infinite(x)))
if (any(iInf)) {
x <- x[!iInf, , drop = FALSE]
warning("'x' contains infinite values")
}
if (!is.null(colnames(x))) {
if (is.null(xlab))
xlab <- colnames(x)[1]
if (is.null(ylab))
ylab <- colnames(x)[2]
}
colalpha <- alphablend(col, alpha)
n <- nrow(x)
nNA <- c(apply(x, 2, countNA), sum(isNA(x, "all")))
if (is.null(xlim)) {
xlim <- if (nNA[1] == n)
rep.int(0, 2)
else range(x[, 1], na.rm = TRUE)
}
if (is.null(ylim)) {
ylim <- if (nNA[2] == n)
rep.int(0, 2)
else range(x[, 2], na.rm = TRUE)
}
initializeWindow <- function(..., cex.text, col, bg, pch,
cex, lty, lwd) {
plot.new()
plot.window(...)
}
initializeWindow(xlim, ylim, ...)
op <- par(c("xlog", "ylog", "plt", "usr", "xaxp", "yaxp"))
on.exit(par(op))
pltx <- c(op$plt[2] - diff(op$plt[1:2])/c(1,1.2/1.05,1.15),
op$plt[2])
plty <- c(op$plt[4] - diff(op$plt[3:4])/c(1,1.2/1.05,1.15),
op$plt[4])
gridx <- c(op$usr[1] - c(0.2, 0.05, 0) * diff(op$usr[1:2]),
op$usr[2])
gridy <- c(op$usr[3] - c(0.2, 0.05, 0) * diff(op$usr[3:4]),
op$usr[4])
op$usr <- c(gridx[c(1, 4)], gridy[c(1, 4)])
par(plt = c(pltx[3:4], plty[3:4]), usr = c(gridx[3:4], gridy[3:4]))
localPoints <- function(..., cex.text, log, type, lty, lwd) {
points(..., type = "p")
}
localPoints(x[, 1], x[, 2], cex = cex, col = colalpha[1],
pch = pch[1], ...)
lines(lowess(x[,1][!is.na(x[,1])][!is.na(x[,2][!is.na(x[,1])])],x[,2][!is.na(x[,1])][!is.na(x[,2][!is.na(x[,1])])]),col="black", lty = 2)
if(leg==TRUE)
{
legend("bottomright",legend=c("missing","observed","imputed"),box.col="lightgrey",col=c(col[2],col[1],col[3]),pch=16)
}
miss <- is.na(x)
#par(xlog = op$xlog, ylog = FALSE, plt = c(pltx[3:4], plty[2:3]),
#   usr = c(gridx[3:4], 0:1))
#box(col = "transparent")
#localPoints(x[miss[, 2], 1], rep(0.5, nNA[2]), cex = cex,
# col = colalpha[2], pch = pch[2], ...)
#par(xlog = FALSE, ylog = op$ylog, plt = c(pltx[2:3], plty[3:4]),
#   usr = c(0:1, gridy[3:4]))
#box(col = "transparent")
#localPoints(rep(0.5, nNA[1]), x[miss[, 1], 2], cex = cex,
#col = colalpha[2], pch = pch[2], ...)
par(xlog = op$xlog, ylog = FALSE, plt = c(pltx[3:4], plty[1:2]),
usr = c(gridx[3:4], 0:1))
box(col = "transparent")
if (any(!is.na(x[!miss[, 2], 1]))) {
xbox <- x[!miss[, 2], 1]
if (zeros[1])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = if (fillbox)
col[1], horizontal = TRUE, add = TRUE, at = 0.5,
axes = FALSE)
}
if (any(!is.na(x[miss[, 2], 1]))) {
xbox <- x[miss[, 2], 1]
if (zeros[1])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = col[2], horizontal = TRUE,
add = TRUE, at = 0.8, axes = FALSE)
}
if (length(as.vector(ausgabe[[var1_nr]])) != 0)
{
xbox <- as.vector(ausgabe[[var1_nr]])
if (zeros[1])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = col[3], add = TRUE,
at = 0.2, horizontal = TRUE, axes = FALSE)
}
par(xlog = FALSE, ylog = op$ylog, plt = c(pltx[1:2], plty[3:4]),
usr = c(0:1, gridy[3:4]))
box(col = "transparent")
if (any(!is.na(x[!miss[, 1], 2]))) {
xbox <- x[!miss[, 1], 2]
if (zeros[2])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = if (fillbox)
col[1], add = TRUE, at = 0.5, axes = FALSE)
}
if (any(!is.na(x[miss[, 1], 2]))) {
xbox <- x[miss[, 1], 2]
if (zeros[2])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = col[2], add = TRUE,
at = 0.2, axes = FALSE)
}
if (length(as.vector(ausgabe[[var2_nr]])) != 0)
{
xbox = as.vector(ausgabe[[var2_nr]])
if (zeros[2])
xbox <- xbox[xbox != 0]
boxplot(xbox, boxwex = 0.4, col = col[3], add = TRUE,
at = 0.8, axes = FALSE)
}
if (nNA[3]) {
par(xlog = FALSE, ylog = FALSE, plt = c(pltx[2:3], plty[2:3]),
usr = c(0, 1, 0, 1))
box(col = "transparent")
localPoints(rep.int(0.5, nNA[3]), rep.int(0.5, nNA[3]),
cex = cex, col = colalpha[3], pch = pch[2], ...)
}
par(xlog = FALSE, ylog = FALSE, plt = op$plt, usr = c(0,
1.15, 0, 1.15))
box(col = "transparent")
abline(v = 0.15, col = "lightgrey")
abline(h = 0.15, col = "lightgrey")
if (isTRUE(numbers)) {
nNA.width <- strwidth(nNA, cex = cex.numbers)
nNA.height <- strheight(nNA, cex = cex.numbers)
if (nNA.width[2] < 0.1 && nNA.height[2] < 0.05) {
text(0.05, 0.125, labels = nNA[2], col = col[2],
cex = cex.numbers)
}
if (nNA.width[1] < 0.05 && nNA.height[1] < 0.1) {
text(0.125, 0.05, labels = nNA[1], col = col[2],
cex = cex.numbers)
}
if (nNA.width[3] < 0.1 && nNA.height[3] < 0.1) {
text(0.05, 0.05, labels = nNA[3], col = col[3], cex = cex.numbers)
}
}
par(op)
if (isTRUE(axes)) {
localAxis <- function(..., cex.text, log, col, bg, pch,
cex, lty, lwd) {
axis(...)
}
localAxis(side = 1, ...)
localAxis(side = 2, ...)
}
if (isTRUE(frame.plot)) {
localBox <- function(..., cex.text, log, col, bg, pch,
cex, lty, lwd) {
box(...)
}
localBox()
}
if (isTRUE(ann)) {
localTitle <- function(..., cex.text, log, col, bg, pch,
cex, lty, lwd) {
title(...)
}
localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab,
...)
}
invisible()
}
environment(mi_marginplot) = environment(marginplot)
