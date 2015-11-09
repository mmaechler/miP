mis_highlight <-
function(ds,var,n="all",type="hist")
{
breit = dim(ds)[2]
if(type!="hist"){
for(i in 1:breit)
{class(ds[,i]) = "numeric"}}
text = "Name of one ore more variables is wrong"
var_nr = which(names(ds)==var)
if(length(var_nr) == 0)
{stop(text)}
if(length(n)==1)
{
if (n=="all")
{
n = 1:dim(ds)[2]
n = n[-var_nr]
m = length(n)
}
else
{
n = which(names(ds)==n)
m = length(n)
if(m == 0)
{stop(text)}
}
}
else
{
x = 0
for(i in 1:length(n))
{
if(length(which(names(ds)==n[i])) == 0)
{stop(text)}
x = c(x,which(names(ds)==n[i]))
}
n = x[-1]
m = length(n)
if(m == 0)
{stop(text)}
}
par(mfrow=c(ceiling(m/ceiling(sqrt(m))),ceiling(sqrt(m))))
if(type=="hist")
{
x = which(is.na(ds[,var_nr]))
for(i in n)
{
if(class(ds[,i]) != "factor")
{
b = hist(ds[,i],main = paste(names(ds)[i]),col="lightgrey",xlab="")$breaks
hist(ds[x,i],add=TRUE,col="red",breaks=b)
}
if(class(ds[,i]) == "factor")
{
plot(ds[,i],main = paste(names(ds)[i]),col="lightgrey",xlab="")
par(new=TRUE)
plot(ds[x,i],add=TRUE,col="red")
}
}
}
if(type=="spin")
{
## faktorproblem loesen
for(i in 1:dim(ds)[2])
{
	ds[,i] = as.integer(ds[,i])
}
##
values = factor(as.integer(is.na(ds[,var_nr])),levels=c(1,0),labels=c("mis","obs"))
for(i in n)
{
spineplot(values~ds[,i],main = paste(names(ds)[i]),col=c("red","lightgrey"),xlab="",ylab="")
}
}
if(type=="box")
{
for(i in n)
{
x = which(is.na(ds[,var_nr]))
boxplot(ds[,i],main = paste(names(ds)[i]),col="lightgrey",xlab="",at=0.88)
boxplot(ds[x,i],add=TRUE,col="red",at=1.12)
}
}
}

