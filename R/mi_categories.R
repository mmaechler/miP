mi_categories <-
function(input,var,sort=FALSE,legend=FALSE)
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
i = which(names(data)==var)
levelnames = levels(factor(as.matrix(imp_list[[i]])))
tt = matrix(as.numeric(factor(as.matrix(imp_list[[i]]))),ncol=m)
name="Imputation Chain"
if(sort == TRUE)
{
for(i in 1:nrow(tt))
{
tt[i,] = sort(tt[i,])
}
name=""
}
color = rainbow(length(levelnames))
image(x=1:nrow(tt),y=1:ncol(tt),tt, ylab=name, xlab="Observation",main=var,col=color,axes=TRUE)
if(legend==TRUE)
{
legend("topright",levelnames,pch=15,col=color,bg="white",xpd=TRUE)
}
##tt
}

