mi_parcoord <-
function(input,var,n="all",nr=1,col="rainbow")
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
breit = dim(data)[2]
x=0
if(toString(n)=="all")
{x = 1:breit}
if(toString(n)!="all")
{
for(i in 1:length(n))
{x = c(x,which(names(data)==n[i]))}
x = x[-1]
}

var_nr = which(names(data) == var)
mis_obs = which(is.na(data[,var_nr]))

if(length(nr)==1)
{
if(class(input) == "mids"){
IMPdata = complete(input, action = nr) }
if(class(input) == "mi"){
IMPdata = mi.data.frame(input, m= nr)	}
if(class(input) == "amelia"){
IMPdata=input$imputations[[nr]]}
parcoord(IMPdata[,x],col="grey",pch=breit)
par(new=TRUE)
parcoord(IMPdata[mis_obs,x],col="red",pch=breit)
}
if(length(nr)>=2)
{
parcoord(data[,x],col="grey",pch=breit)
par(new=TRUE)
for(i in nr){
if(class(input) == "mids"){
IMPdata = complete(input, action = i)}
if(class(input) == "mi"){
IMPdata = mi.data.frame(input, m= i)	}
if(class(input) == "amelia"){
IMPdata=input$imputations[[i]]}
{
if(length(col) == 1)
{col = rainbow(m)}
parcoord(IMPdata[mis_obs,x],col=col[which(nr == i)],pch=breit)
par(new=TRUE)
}}
}
par(new=FALSE)
#c(m,names(IMPdata)[var_nr],IMPdata[mis_obs,])
}

