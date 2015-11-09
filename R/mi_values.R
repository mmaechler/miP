mi_values <-
function(input,type="boxplot1",var="all",mis_count=5,m_hist=1)
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
tief = dim(data)[1]
breit = dim(data)[2]
mis_data1 = 0
for(i in 1:breit)
{
mis_data1[i] = length(data[,i][is.na(data[,i])])
class(data[,i]) = "numeric"
}
anzahl_mis_var = length(which(mis_data1 != 0))
if(length(var)!=1)
{
ff2 = 0
for(i in 1:(length(var)))
{ff2[i] = which(names(data) == var[i])}
ff2 = ff2[order(ff2)]
}
if(length(var)==1)
{if(var!="all")
{
ff2 = which(names(data) == var)
}
}
data = data[,order(mis_data1)]
mis_data = mis_data1[order(mis_data1)]
if(length(var) != 1)
{
if(class(var)!="character"){stop("var not of type character")}
else{
ff1 = 0
for(i in 1:(length(var)))
{ff1[i] = which(names(data) == var[i])}
ff = ff1[order(ff1)]
data = data.frame(data[,-ff],data[,ff])
anzahl_mis_var = length(var)
ff1 = mis_data1[ff2]
mis_data1 = rep(0,breit)
mis_data1[ff2] = ff1
mis_data = c(rep(0,(breit-anzahl_mis_var)),mis_data[ff])
}
}
if(length(var) == 1)
{
if(var!="all")
{
if(class(var)!="character"){stop("var not of type character")}
else{
ff = which(names(data) == var)
data = data.frame(data[,-ff],data[,ff])
names(data)[breit] = var
anzahl_mis_var = 1
ff1 = mis_data1[ff2]
mis_data1 = rep(0,breit)
mis_data1[ff2] = ff1
mis_data = c(rep(0,(breit-1)),ff1)
}
}
}
IMP.data = as.list(data)
x1 = (breit-anzahl_mis_var+1)
x = x1+breit
for (i in 2:m)
{ x = c(x,x1+(breit*i)) }

if(class(input) == "mi")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,mi.data.frame(input, m = as.integer(paste(i)))[,order(mis_data1)])
}
IMP.hist = mi.data.frame(input, m = m_hist)[,order(mis_data1)]
}
if(class(input) == "mids")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,complete(input, action = as.integer(paste(i)))[,order(mis_data1)])
}
IMP.hist = complete(input, action = m_hist)[,order(mis_data1)]
}
if(class(input) == "amelia")
{
for(i in 1:m)
{
IMP.data = c(IMP.data,input$imputations[[as.integer(paste(i))]][,order(mis_data1)])
}
IMP.hist = input$imputations[[m_hist]][,order(mis_data1)]
}

{
i = breit-anzahl_mis_var+1
mat = matrix(rep(0,(mis_data[i])*m),ncol=m)
count = 1}
for (j in x)
{
mat[1:(mis_data[i]),count] = IMP.data[[j]][order(!is.na(IMP.data[[i]]))][1:(mis_data[i])]
count = count+1
}
ausgabe = list(x = mat)
x = x+1

if(length(var == 1) && var != "all"){}
if(var == "all" || length(var)!=1){
for (i in (breit-anzahl_mis_var+2):breit)
{
mat = matrix(rep(0,(mis_data[i])*m),ncol=m)
count = 1
for (j in x)
{
mat[1:(mis_data[i]),count] = IMP.data[[j]][order(!is.na(IMP.data[[i]]))][1:(mis_data[i])]
count = count+1
}
ausgabe = c(ausgabe,list(x = mat))
x = x+1

}
}
#ausgabe = ausgabe[[-1]]
names(ausgabe) = names(data)[(breit-anzahl_mis_var+1):breit]

if(type=="boxplot1") ###  boxplots imputierte werte 
{#par(mfrow=c(2,ceiling(anzahl_mis_var/2)))
t = anzahl_mis_var
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
at = seq(1.75,1.75+0.75*(m-1),0.75)
for(i in 1:anzahl_mis_var)
{
ymin = min(data[,breit-anzahl_mis_var+i][!is.na(data[,breit-anzahl_mis_var+i])])
ymax = max(data[,breit-anzahl_mis_var+i][!is.na(data[,breit-anzahl_mis_var+i])])
boxplot(ausgabe[[i]],pars=list(boxwex=rep(0.4,m)),at=at,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "Imputation",col="red",ylim=c(min(min(ausgabe[[i]]),ymin),max(max(ausgabe[[i]]),ymax)))
boxplot(data[breit-anzahl_mis_var+i],pars=list(boxwex=(0.4*2)),at=1,add=TRUE,col="blue")
}
}


if(type=="boxplot2") #### boxplots d. variablen mit fehl. werten: obs. gegen imputed werte(mittelwert ber alle m Datenstze)
{
#par(mfrow=c(2,ceiling(anzahl_mis_var/2)))
if(anzahl_mis_var >6)
{
	par(mfrow=c(ceiling(anzahl_mis_var/6),6))
}
if(anzahl_mis_var<=6){par(mfrow=c(1,anzahl_mis_var))}

for(i in 1:anzahl_mis_var)
{
ymin = min(data[,breit-anzahl_mis_var+i][!is.na(data[,breit-anzahl_mis_var+i])])
ymax = max(data[,breit-anzahl_mis_var+i][!is.na(data[,breit-anzahl_mis_var+i])])
boxplot(data[breit-anzahl_mis_var+i],at=0.76,col="blue",main=paste(names(data)[breit-anzahl_mis_var+i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "",ylim=c(min(min(ausgabe[[i]]),ymin),max(max(ausgabe[[i]]),ymax)))
boxplot(as.vector(ausgabe[[i]]),at=1.24,col="red",add=TRUE,main=paste(names(data)[breit-anzahl_mis_var+i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "",ylim=c(min(min(ausgabe[[i]]),ymin),max(max(ausgabe[[i]]),ymax)))
}
}


if(type=="boxplot3" || type == "density3") ###  boxplots des ausgangsdatensatzes + m imputierten Datentze
{
ausgabe = list(x=1)
x1 = (breit-anzahl_mis_var+1)
x = x1
for (i in 1:m)
{ x = c(x,x1+(breit*i)) }
for (i in (breit-anzahl_mis_var+1):breit)
{ma = matrix(rep(1,tief*(m+1)),ncol=m+1)
count = 1
for (j in x)
{
ma[,count] = IMP.data[[j]]
count = count+1
}
ausgabe = c(ausgabe,list(x = ma))
x = x+1}
ausgabe[[1]] <- NULL
names(ausgabe) = names(data)[(breit-anzahl_mis_var+1):breit]
t = anzahl_mis_var
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
if(type=="boxplot3")
{
col=c("blue",rep("red",m))
for(i in 1:length(ausgabe))
colnames(ausgabe[i][[1]]) = c("obs", seq(1,m))
for(i in 1:anzahl_mis_var)
{	boxplot(ausgabe[[i]],col=col,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]))}
}
if(type=="density3")
{
t= length(which(mis_data>=mis_count))
if(t == 0){stop("not enough imputed values")}
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
t = anzahl_mis_var-t+1
for(i in t:anzahl_mis_var)
{
plot(density(ausgabe[[i]][,1][!is.na(ausgabe[[i]][,1])]),main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "Imputation",col="blue",ylim=c(0,max(c(density(ausgabe[[i]][,1][!is.na(ausgabe[[i]][,1])])$y,density(ausgabe[[i]][,2:m])$y))))
for(j in 2:(m+1))
lines(density(ausgabe[[i]][,j]),lty=3,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),col="red")
}
}
}


if(type=="density2" || type == "density1")  ## Plot der Dichte der imp (mittelwert der m DS) gegen obs werte
{
t= length(which(mis_data>=mis_count))
if(t == 0){stop("not enough imputed values")}
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
t = anzahl_mis_var-t+1
if(type =="density2")
{
for(i in t:anzahl_mis_var)
{
plot(density(ausgabe[[i]]),main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "",col="red",ylim=c(0,max(c(density(data[breit-anzahl_mis_var+i][!is.na(data[breit-anzahl_mis_var+i])])$y,density(ausgabe[[i]])$y))))
par(new=TRUE)
lines(density(data[breit-anzahl_mis_var+i][!is.na(data[breit-anzahl_mis_var+i])]),col="blue")   # ,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "Imputation")
}
}
if(type =="density1")
{
for(i in t:anzahl_mis_var)
{
max_1=0
for(k in 1:m)
{max_1 = max(max_1,max(density(ausgabe[[i]][,k])$y))}
plot(density(ausgabe[[i]][,1]),lty=3,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "",col="red",ylim = c(0,max(max_1,max(density(data[breit-anzahl_mis_var+i][!is.na(data[breit-anzahl_mis_var+i])])$y) ) ) )
for(j in 2:m){
lines(density(ausgabe[[i]][,j]),lty=3,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),col="red")
}
par(new=TRUE)
lines(density(data[breit-anzahl_mis_var+i][!is.na(data[breit-anzahl_mis_var+i])]),col="blue")   # ,main=paste(names(ausgabe)[i],"-",mis_data[breit-anzahl_mis_var+i]),xlab = "Imputation")
}
}
}
if(type=="hist")
{
par(mfrow=c(2,ceiling(anzahl_mis_var/2)))
x = breit-anzahl_mis_var+1
for(i in x:breit)
{   if(class(IMP.hist[,i]) != "factor"){
hist(IMP.hist[,i],col="red", main = paste(names(IMP.hist)[i]), xlab = paste(names(IMP.hist)[i]))
hist(data[,i],add=TRUE,col="blue")}
if(class(IMP.hist[,i]) == "factor"){
y = max(table(IMP.hist[,i]))
plot(IMP.hist[,i],col="red", main = paste(names(IMP.hist)[i]), xlab = paste(names(IMP.hist)[i]),ylim=c(0,y))
par(new=TRUE)
plot(as.factor(data[,i]),add=TRUE,col="blue",ylim=c(0,y),ylab="",xlab="",main="")
}
}
}
#ausgabe
#list(IMP.hist,data,x,anzahl_mis_var)
}

