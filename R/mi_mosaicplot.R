mi_mosaicplot <-
function(input,n="all",chains=FALSE)
{
if(class(input) == "mi")  ### IMP Liste von mice nachbauen
{
data = data.mi(input)
m = m(input)
imp_list = list(x=0)
breit = dim(data)[2]
for(i in 1:breit)
{
if(info.mi(input)[[i]]$nmis >=1)
{
pos = info.mi(input)[[i]]$missing.index
mat = matrix(rep(0,length(pos)*m),ncol=m)
for(j in 1:m)
{
mat[,j] = mi.data.frame(input,m=j)[pos,i]
}
imp_list = c(imp_list,list(x=mat))
}
if(info.mi(input)[[i]]$nmis == 0){imp_list = c(imp_list,list(x=NULL))}
}
imp_list[[1]] = NULL
names(imp_list) = names(data)
}
if(class(input) == "mids")
{
data = input$data
m = input$m
imp_list = input$imp
}

x=0
for(i in 1:length(imp_list)){
if((length(imp_list[[i]])!=0) && (class(data[,i]) == "factor")){x=c(x,i)}}
x = x[-1]
tief = dim(data)[1]
breit = dim(data)[2]

### Logik für Parameter n
if(toString(n) != "all")
{
	michi=0
	for(i in 1:length(n))
	{
		fantasti = which(names(imp_list)==n[i])
		michi = c(michi,fantasti)
	}
	x = michi[-1]
}


#### einzelne Imputationschains
if(chains==TRUE)
{
z = length(x)
par(mfrow=c(2,ceiling(z/2)))
if(z == 1)
{par(mfrow=c(1,1))}
for(j in x)
{
zz = imp_list[[j]]
mm = matrix(rep(0,length(levels(data[,j]))*m),ncol=m)
for(i in 1:m)
{
mm[,i] = table(factor(zz[,i],levels(data[,j])))
}
mm = t(mm)
colnames(mm) = levels(data[,j])
rownames(mm) = 1:m
mosaicplot(mm,main=names(data)[j],ylab="Levels",xlab="Imputation Chain")
}
}

#### eigentliche Fkt.
if(chains==FALSE)
{
z = length(x)
par(mfrow=c(2,ceiling(z/2)))
for(j in x)
{
zz = matrix(c(table(factor(t(imp_list[[j]]),levels=levels(data[,j]))),table(data[,j])),ncol=length(levels(data[,j])),byrow=TRUE)
colnames(zz) = levels(data[,j])
rownames(zz) = c("Imputed","Observed")
mosaicplot(zz,main=names(data)[j],ylab="Levels")
}
}
#imp_list
}

