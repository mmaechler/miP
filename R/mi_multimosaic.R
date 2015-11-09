mi_multimosaic <-
function(x,input,nr=1)
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
tief = dim(data)[1]
breit = dim(data)[2]
var_nr=0
for(i in 1:length(x))
{var_nr = c(var_nr,which(names(data)==x[i]))}
var_nr = var_nr[-1]
data$imp = rep("no",tief)
data = data[,c(var_nr,breit+1)]
breit = length(var_nr)
for(i in 1:tief)
{
for(j in 1:breit)
{if(is.na(data[i,j])) {data[i,breit+1]="yes"}
}

}
breit = breit+1
if(class(input) == "mids"){
IMPdata = complete(input, action = nr) }
if(class(input) == "mi"){
IMPdata = mi.data.frame(input, m= nr)	}
IMPdata$imputed = factor(data$imp)
IMPdata=IMPdata[,c(var_nr,dim(IMPdata)[2])]
formel=x[1]
if(length(x)>=2){
for(i in 2:length(x)){
formel=paste(formel,"+",x[i])}}
formel=paste("imputed ~",formel)
mosaic(as.formula(formel),data=IMPdata,gp=gpar(fill=c("blue","red")))
}

