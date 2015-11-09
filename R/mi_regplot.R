mi_regplot <-
function(input,nr=1,nmis=5)
{

if(class(input) == "mi")
{
data = data.mi(input)
m = m(input)
MIdata = mi.data.frame(input, m=nr)
tief = dim(data)[1]
breit = dim(data)[2]
nnn = 0
welche_var = 0
varliste = rep(0,breit)
for(i in 1:breit)
{
if(info.mi(input)[[i]]$nmis>0)
{
nnn = nnn+1
if(info.mi(input)[[i]]$type != "binary" && info.mi(input)[[i]]$type != "unordered-categorical" && info.mi(input)[[i]]$type != "ordered-categorical")
{welche_var = c(welche_var,nnn)
varliste[nnn] = names(MIdata)[i]
}
}
}
welche_var = welche_var[-1]
}
if(class(input) == "mids")
{
data = input$data
m = input$m
MIdata = complete(input,action=nr)
breit = dim(data)[2]
zu = 1
varliste = rep(0,breit)
nrs = which(input$method == "norm")
predic = input$predictorMatrix[nrs,]
methoden = 0
for(i in 1:(dim(predic)[1]))
{
text = paste(rownames(predic)[i],"~")
help = 0
for(j in 1:(dim(predic)[2]))
{
if(predic[i,j] == 1 && help ==0)
{
text = paste(text, colnames(predic)[j])
varliste[zu] = rownames(predic)[i]
zu = zu+1
help = 1
}
if(predic[i,j] == 1 && help ==1)
{
text = paste(text,"+", colnames(predic)[j])
}
}
methoden[i] = text
}
anzahl_mis_var = length(methoden)
welche_var = 1:anzahl_mis_var
}


#### Eigentliche Funktion:
t = length(welche_var)
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
for(i in welche_var)
{
if(class(input) == "mi")
{
expected = slot(imp(input)[i][[1]],name="expected")
varname = varliste[i]
}
if(class(input) == "mids")
{
cof = lm.mids(methoden[i],input)[4][[1]][[nr]]
expected = predict(cof,MIdata)
varname = varliste[i]
}

plot(MIdata[which(!is.na(data[,varname])),varname],expected[which(!is.na(data[,varname]))],col = "blue",pch=16,ylab="expected",xlab="observed/imputed",main=paste(varname))
lines(lowess(MIdata[which(!is.na(data[,varname])),varname],expected[which(!is.na(data[,varname]))]),col="blue")
points(MIdata[which(is.na(data[,varname])),varname],expected[which(is.na(data[,varname]))],col="red",pch=16)
if(length(which(is.na(data[,varname])))>nmis){
lines(lowess(MIdata[which(is.na(data[,varname])),varname],expected[which(is.na(data[,varname]))]),col="red")}
abline(0,1,lty=2)
}

}

