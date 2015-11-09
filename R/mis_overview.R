mis_overview <-
function(ds,srt=0,pos=-3,percent=FALSE,inc=FALSE)
{
abstand=pos
color=c("lightgreen",rgb(0.85,0,0))
tief = dim(ds)[1]
breit = dim(ds)[2]
mis_data=rep(0,breit)
for(i in 1:breit)
{
mis_data[i] = length(ds[,i][is.na(ds[,i])])
}
## Schleife um Anzahl der incomplete cases zu bekommen
if(inc==TRUE){
inc_cases = rep(0,tief)
for(j in 1:tief)
{count = 0
{for (i in 1:breit)
if(is.na(ds[j,i])) {count = 1}
}
inc_cases[j] = count
count
}}
mis_table = matrix(c(tief-mis_data,mis_data),ncol=breit,byrow=TRUE)
mis_table = as.table(mis_table)
colnames(mis_table) = names(ds)
rownames(mis_table) = c("complete","missing")
mis_tot_per = sum(mis_data/tief)/breit
if(srt==0)
{p = barplot(mis_table/tief*100,xlab = "variables",ylab="%", main=paste("Total data missing:",round(mis_tot_per*100,2),"%"),font.main=2, col = color,legend=FALSE,axisnames=TRUE)
if(percent==TRUE){
text(p, rep(100+abstand,breit), paste(round(mis_data/tief*100,2),"%"), xpd=TRUE)
}
if(percent==FALSE){
text(p, rep(100+abstand,breit), paste(mis_data), xpd=TRUE)
}
}
else
{p = barplot(mis_table/tief*100,xlab = "variables",ylab="%", main=paste("Total data missing:",round(mis_tot_per*100,2),"%"),font.main=2, col = color,legend=FALSE,axisnames=TRUE,names.arg=rep("",breit))
if(percent==TRUE)
{
text(p, rep(100+abstand,breit), paste(round(mis_data/tief*100,2),"%"), xpd=TRUE)
}
if(percent==FALSE)
{
text(p, rep(100+abstand,breit), paste(mis_data), xpd=TRUE)
}
text(seq(0.2+0.5,1.2*breit-0.5,length.out=breit), -6,labels=names(ds),srt=srt,xpd=TRUE)
}#barplot(mis_table/tief*100,xlab = "variables",ylab="percent", main=paste("Total data missing:",round(mis_tot_per*100,2),"% -",breit*tief,"values."), col = color,legend=FALSE,beside = FALSE,horiz=FALSE)
if(inc==TRUE){
cat(" Total data missing:",sum(mis_data),"out of",breit*tief,"(",round(mis_tot_per*100,2),"% )",'\n',paste("Complete for all",breit,"variables:"),tief-sum(inc_cases),"out of",tief,"cases (",round((tief-sum(inc_cases))/tief*100,2),"% )",'\n')
}
if(inc==FALSE){
cat(" Total data missing:",sum(mis_data),"out of",breit*tief,"(",round(mis_tot_per*100,2),"% )",'\n')
}
#for(l in 1:breit)
#{cat("",names(ds)[l],":",round(mis_data[l]/tief*100,2),"%",'\n')}
}

