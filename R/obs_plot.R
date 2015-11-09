obs_plot <-
function(ds,var,main="default")
{
var_nr = which(names(ds)==var)
x = 1:length(names(ds))
x = x[-var_nr]
t = length(x)
par(mfrow=c(ceiling(t/ceiling(sqrt(t))),ceiling(sqrt(t))))
for (i in x)
{
eins = ds[,var_nr][!is.na(ds[,var_nr])][!is.na(ds[,i][!is.na(ds[,var_nr])])]
obs = length(eins)/dim(ds)[1]
if(main == "default")
{main = paste(round(obs*100,2),"% observed")}
plot(ds[,var_nr][!is.na(ds[,var_nr])],ds[,i][!is.na(ds[,var_nr])],main=main,font.main=2,pch=16,xlab=names(ds)[var_nr],ylab=names(ds)[i])
lines(lowess(ds[,var_nr][!is.na(ds[,var_nr])][!is.na(ds[,i][!is.na(ds[,var_nr])])],ds[,i][!is.na(ds[,var_nr])][!is.na(ds[,i][!is.na(ds[,var_nr])])]),col="red")
}
}

