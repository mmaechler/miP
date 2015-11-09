mi_trellisplot <-
function(x,var,input,nr=1,eqnr=3,ol=0.1)
{
if(class(input) == "mi")
{
data = data.mi(input)
m = m(input)
IMPdata = mi.data.frame(input, m= nr)
}
if(class(input) == "mids")
{
data = input$data
m = input$m
IMPdata = complete(input, action = nr)
}
if(class(input) == "amelia")
{
data = input$imputations[[1]]
data[input$missMatrix] = NA
m = input$m
IMPdata=input$imputations[[nr]]
}
tief=dim(data)[1]
breit=dim(data)[2]
var_nr=which(names(data)==var)
x_nr = 0
for(i in 1:length(x))
{
x_nr=c(x_nr,which(names(data)==x[i])	)
}
x_nr=x_nr[-1]
data$imp = rep("no",tief)
data$imp[which(is.na(data[,var_nr]))]="yes"
data$imp = factor(data$imp)
IMPdata$imp = data$imp
IMPdata = IMPdata[,c(x,var,"imp")]
if(class(data[,var]) != "factor"){ 
group = equal.count(IMPdata[,var],number=eqnr,overlap=ol)}
formel = x[1]
for(i in 2:length(x))
{
formel = paste(formel,"~",x[i])
}
if(class(data[,var]) != "factor")
{
formel = paste(formel,"| group")
}
if(class(data[,var]) == "factor")
{
formel = paste(formel,"|",var)
}
xyplot(as.formula(formel),IMPdata,groups=imp,col=c("blue","red"),pch=16,main=paste(var),font.main=2)
}

