# Effect of the Increase of the Minimum Wage on the Food Services and Drinking Places 
# Industry in Pennsylvania
#
# Author: Koba Khitalishvili
# Date: June 1st 2014
#--------------------------------#

# To convert statistical output to pretty tables in pdf format in R you will need a 
# local instalation of Tex which you can obtain here 
# http://www.tug.org/texlive/
# Everything else runs without a need of any external software

#--------------------------------#
# Uncomment next line if you do not have any of the packages installed 
# install.packages("urca","plotrix","gridExtra","tseries","car", "stargazer","gplots")

#---Load necessary libraries-----#
# Set the R folder as your working directory. In my case it is
setwd("~/Desktop/ECON322Paper/R")
library(plotrix)
library(ggplot2)
library(gridExtra)
library(tseries)
library(stargazer)
library(tools)
library(urca)
library(gplots)
#--------------------------------#
#--------Functions---------------#
# Reads in CPS data
getCPSdata<-function(directory="data/CPSdata"){
  file_list <- list.files(directory,full.names=TRUE)
  
  # Read in each file and bind them into a data frame
  df<-data.frame()
  for (file in file_list[1:length(file_list)]) {  
    df<-rbind(df,read.table(file,header=TRUE,sep=","))  
  }
  return(df)
}

# Converts a column of a dataframe into factors
fac<-function(dataframe,colNum){
  dataframe[,colNum]<-factor(dataframe[,colNum])
  return (dataframe)
}

# Calculates the rates using weights
rate<- function(df,empst,wage,reported){ 
  temp<-tapply(df[df$PRMLR==empst & wage,]$PWCMPWGT,
         list(df[df$PRMLR==empst & wage,]$YYYYMM),sum) /
    tapply(df[df$PRMLR!=3 & df$PTERNHLY>reported,]$PWCMPWGT,
           list(df[df$PRMLR!=3 & df$PTERNHLY>reported,]$YYYYMM),sum)
  return(temp)
}

# Function for lagging variables
tslag <- function(x, d=1)
{
  x <- as.vector(x)
  n <- length(x)
  c(rep(NA,d),x)[1:n]
}

# Function for reading horizontally organized csv files
read.tcsv <- function(file, header=TRUE, sep=",",quote = "\"", ...) {
  
  n <- max(count.fields(file, sep=",",quote = "\""), na.rm=TRUE)
  x <- readLines(file)
  
  .splitvar <- function(x, sep,quote, n) {
    var <- unlist(strsplit(x, split=sep))
    length(var) <- n
    return(var)
  }
  
  x <- do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x <- apply(x, 1, paste, collapse=sep) 
  out <- read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}

# Function that saves statistical output as pdf files 
tex2pdf<-function(table,title="",tableNumber=1,fileName,targetDir=getwd()){
  tex<-stargazer(table,type = "latex",title=title) 
  tableNum<-paste("\\setcounter{table}{",tableNumber-1,"}",sep="")
  # save a latex file to Sweave
  cat(c("\\documentclass{article}
      \\begin{document}
      \\SweaveOpts{concordance=TRUE}\n",
        tableNum,
        tex,
        "\n\\end{document} "),sep="\n",file=paste(targetDir,"/",fileName,".Rnw",sep=""))
  # Convert to pdf in the figures directory (texipdf does not have out option)
  wd<-getwd()
  setwd(targetDir)
  Sweave(paste(fileName,".Rnw",sep=""))
  texi2pdf(paste(fileName,".tex",sep=""))
  file.remove(paste(fileName,".tex",sep=""),paste(fileName,".aux",sep=""),
              paste(fileName,".log",sep=""),
              paste(fileName,"-concordance.tex",sep=""),
              paste(fileName,".Rnw",sep=""))
  setwd(wd)
  rm(wd,tex,targetDir,fileName)
}
#------------------------------------#
#------------------------------------#
# Fixes one of the data files that is missing several default id columns. Already done.

# file_list <- list.files("data/CPSdata",full.names=TRUE)
# df<-read.table(file_list[9],header=TRUE,sep=",")
# df<-df[,-1]
# df<-data.frame(HRHHID=0,HRHHID2=0,OCCURNUM=0,YYYYMM=201204,df)
# head(df)
# write.table(df,file_list[9],col.names=TRUE,sep=",")
# rm(df)
#------------------------------------#
#----Analysis------------------------#

# Read data in
d<-getCPSdata()
head(d)
summary(d)

# Subset so we get observations for pennsylvania
PA<-d$GESTCEN==23
dPA<-d[PA,]
head(dPA)

rm(PA)

# Number of people that reported their wage in food services and drinking places industry in PA
reported<-sum(dPA[dPA$PTERNHLY!=-0.01,]$PWCMPWGT)/length(unique(dPA$YYYYMM))
reported_not<-sum(dPA[dPA$PTERNHLY==-0.01,]$PWCMPWGT)/length(unique(dPA$YYYYMM))
reported/(reported+reported_not) # so we have wage data on 17% percents of employees in the industry in question

rm(reported,reported_not)

# Histograms of hourly wages in PA in April for years 2007,2008,2009, and 2011

png("figures/HistWages.png",units="px",height=600,width=950) # Save the histogram in the figures folder

par(mfrow=c(2,2),mai=c(1,1,0.6,0.1))

for(year in c(200704,200804,200904,201104)) {
  wagesPA<-dPA[dPA$YYYYMM==year & dPA$PRMLR==1 & dPA$PTERNHLY>-0.01,]$PTERNHLY # hourly wages
  wts<-dPA[dPA$YYYYMM==year & dPA$PRMLR==1 & dPA$PTERNHLY>-0.01,]$PWCMPWGT # weights
  
  # Histogram
  weighted.hist(wagesPA,wts,breaks=1:15, freq=F,
                xlab="Hourly Wage,$",ylab="Percent",ylim=c(0,0.6), axes=F,
                cex.lab=1.8)
  axis(2, at = seq(0, 0.6, 0.1), labels = paste(seq(0,60,10)),cex.axis=1.5)
  abline(v=6.25,lty="dashed",col="red")
  year_str<-substr(as.character(year),1,4)
  title(paste(c("Distribution of restaurant workers\nby wage in PA in April",year_str),collapse=" "),cex.main=2)
  legend("topleft","$7.25",lty="dashed",col="red",cex=1.7,bty="n")
  
  
}
par(mfrow=c(1,1))

dev.off()
rm(year,year_str,wts,wagesPA)

# Fraction of workers that work at or above $7.25(new minimum wage)
k1<-data.frame(k1=rate(dPA,1,dPA$PTERNHLY>=7.25,-0.01))
k1ts<-ts(k1$k1,start=c(2005,1),frequency=12) #convert to time series

# Fraction of workers that work below $7.25(new minimum wage)
k2<-data.frame(k2=rate(dPA,1,(dPA$PTERNHLY < 7.25 & dPA$PTERNHLY>-0.01),-0.01))
k2ts<-ts(k2$k2,start=c(2005,1),frequency=12) #convert to time series

# Plot the fraction of workers that work at or above $7.25(new minimum wage)
# minus seasonal variations
deck1<-decompose(k1ts)
plot(deck1)
grBreaks<-dput(rownames(k1)[seq(1, length(rownames(k1)), 3)])
k1p1<-ggplot(k1,aes(x=rownames(k1),y=data.frame(k1ts-deck1$seasonal)$k1*100))+
  ggtitle("At or above $7.25") +
  theme(plot.title = element_text(size=18,face="bold"))+
  xlab("Period")+ylab("Percent of employed")+
  geom_point()+
  geom_line(aes(y=data.frame(k1ts-deck1$seasonal)$k1*100,group=1))+
  geom_line(aes(y=deck1$trend*100,group=1),col="blue")+
  scale_x_discrete(breaks=grBreaks,labels=grBreaks) +
  scale_y_continuous(limits = c(10, 100)) +
  theme(axis.text.x=element_text(angle = 75, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Plot the fraction of workers that work below $7.25(new minimum wage)
# minus seasonal variations
deck2<-decompose(k2ts)
plot(deck2)
grBreaks<-dput(rownames(k2)[seq(1, length(rownames(k2)), 3)])
k2p1<-ggplot(k2,aes(x=rownames(k2),y=data.frame(k2ts-deck2$seasonal)$k2*100))+
  ggtitle("Below $7.25") +
  theme(plot.title = element_text(size=18,face="bold"))+
  xlab("Period")+ylab("Percent of employed")+
  geom_point()+
  geom_line(aes(y=data.frame(k2ts-deck2$seasonal)$k2*100,group=1))+
  geom_line(aes(y=deck2$trend*100,group=1),col="blue")+
  scale_x_discrete(breaks=grBreaks,labels=grBreaks) +
  scale_y_continuous(limits = c(10, 100)) +
  theme(axis.text.x=element_text(angle = 75, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

grid.arrange(k1p1,k2p1,nrow=2)

# Save the plot in figures folder
dev.copy(png,"figures/aboveMinWage.png",units="px",height=400,width=600)
dev.off()

rm(k1,k1ts,k2,k2ts,k1p1,k2p1)

# Unemployment rate in the Food industry in PA unadjusted
unemp<-data.frame(unemp=rate(dPA,2,dPA$PTERNHLY>-0.02,-0.02))
unempts<-ts(unemp$unemp,start=c(2005,1),frequency=12)
decunemp<-decompose(unempts)

# Employment rate in the Food industry in PA unadjusted
emp<-data.frame(emp=rate(dPA,1,dPA$PTERNHLY>-0.02,-0.02))
empts<-ts(emp$emp,start=c(2005,1),frequency=12)
decemp<-decompose(empts)

# Plot the unemployment rate in PA # minus seasonal variations
unempp<-ggplot(unemp,aes(x=rownames(unemp),y=data.frame(unempts-decunemp$seasonal)$unemp*100))+
  ggtitle("Unemployment rate in the Food Services and Drinking Places\nindustry in PA") +
  geom_point() +
  theme(plot.title = element_text(size=18,face="bold")) +
  xlab("Period")+ylab("Rate,%") +
  geom_line(aes(y=data.frame(unempts-decunemp$seasonal)$unemp*100,group=1)) +
  geom_line(aes(y=decunemp$trend*100,group=1),col="blue") +
  scale_x_discrete(breaks=grBreaks,labels=grBreaks) +
  theme(axis.text.x=element_text(angle = 75, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Plot the employment rate in PA # minus seasonal variations
empp<-ggplot(emp,aes(x=rownames(emp),y=data.frame(empts-decemp$seasonal)$emp*100)) +
  ggtitle("Employment rate in the Food Services and Drinking Places\nindustry in PA") +
  theme(plot.title = element_text(size=18,face="bold"))+
  xlab("Period")+ylab("Rate,%")+
  geom_point()+
  geom_line(aes(y=data.frame(empts-decemp$seasonal)$emp*100,group=1))+
  geom_line(aes(y=decemp$trend*100,group=1),col="blue")+
  scale_x_discrete(breaks=grBreaks,labels=grBreaks) +
  theme(axis.text.x=element_text(angle = 75, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

grid.arrange(empp,unempp,nrow=2)

# Save the plot in figures folder
dev.copy(png,"figures/EmpRates.png",units="px",height=400,width=600)
dev.off()

rm(emp,empp,unempp,decemp,decunemp,deck1,deck2,empts,unempts)

# Checking histogram of unemployment rate for normality
png("figures/UnempHist.png",units="px",height=400,width=800) #save plot as png

par(mfrow=c(1,2))

hist(unemp$unemp*100,
        freq=F,
        breaks=8,
        ylim=c(0,0.14),
        main="Histogram of Unemployment Rate\nin the Food Services and Drinking Places\nIndustry in PA for the period Jan 2005-Dec 2012",
        xlab="Unemployment rate,%")
legend("topright", c("Normal\nDistribution"), col=c("blue"), lwd=1)
# Add normal distribution curve
q<-quantile(unemp$unemp*100)
sd<-sd(unemp$unemp*100)
m<-mean(unemp$unemp*100)
curve(dnorm(x,m,sd), add=TRUE,col="blue")

qqnorm(unemp$unemp*100)
qqline(unemp$unemp*100)

par(mfrow=c(1,1))

dev.off()
rm(q,sd,m)

# Checking for normality of the unemployment rate using the Shapiro test
shapiro.test(unemp$unemp) # p>0.05 so I cannot reject the null that the sample is normally distributed 

# Creating dummy variables for the regression
regData<-data.frame(unemp*100)

regData$crimonthDummy<-c(rep(0,35),rep(1,19),rep(0,42)) # crisis months
regData$minwageDummy<-c(rep(0,30),rep(1,66)) # months after min wage increase

# Read in food and drinking places industry annual output for PA
foodSalesPA<-read.tcsv("data/RetailFoodServPA.csv")[c(-1:-2,-18),5:7][,-2]
names(foodSalesPA)<-c("Year","Food&Dr")
foodSalesPA[,2]<-as.numeric(as.character(foodSalesPA[,2]))

# Read in food and drinking places industry monthly output for the US
foodSales<-read.table("data/RetailFoodServ.csv",sep=",")
foodSales[,2]<-as.numeric(gsub(",","", foodSales[,2]))

# Calculate the annual food and drinking places industry output for the US
# using monthly data
foodSalesPA<-data.frame(foodSalesPA,US=0)
for(i in 0:6){
  start<-13+12*i
  end<-start+11
  foodSalesPA[nrow(foodSalesPA)-i,3]<-sum(as.numeric(as.character(foodSales[start:end,2])))
  rm(start,end)
}

# Compare food and drinking places industry output in the US and in PA
cPA1<-ggplot(foodSalesPA[foodSalesPA$US>0,],aes(x=foodSalesPA[foodSalesPA$US>0,1], y=foodSalesPA[foodSalesPA$US>0,2],group=1))+
  ggtitle("Food Services and Drinking Places industry output in PA,\nmillions of dollars") +
  theme(plot.title = element_text(size=18,face="bold")) +
  geom_line() +
  xlab("Year") + ylab("Millions of dollars") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

cPA2<-ggplot(foodSalesPA[foodSalesPA$US>0,],aes(x=foodSalesPA[foodSalesPA$US>0,1], y=foodSalesPA[foodSalesPA$US>0,3],group=1))+
  ggtitle("Food Services and Drinking Places industry output in the US,\nmillions of dollars") +
  theme(plot.title = element_text(size=18,face="bold")) +
  geom_line() +
  xlab("Year") + ylab("Millions of dollars") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

corr<-cor(foodSalesPA[foodSalesPA$US>0,2:3])[1,2]

grid.arrange(cPA2,cPA1,nrow=2, left=paste("Correlation =",round(corr,4),sep=" "))

# Save plot
dev.copy(png,"figures/FoodOutput.png",units="px",height=400,width=600)
dev.off()

rm(cPA1,cPA2,corr)

# Calculate the monthly percentage growth of the food serv and drink pl industry output in the US
regData$foodIndGrowth<-rev((foodSales[-nrow(foodSales),2]-foodSales[-1,2]) / foodSales[1:nrow(foodSales)-1,2])*100

# Plot foodIndGrowth
foodIndGrowthp<-ggplot(regData,aes(x=rownames(regData),y=regData[,4]))+
  ggtitle("Monthly percent growth of the Food Services and Drinking Places\nindustry in the US") +
  geom_point() +
  theme(plot.title = element_text(size=18,face="bold")) +
  xlab("Period")+ylab("Percent") +
  geom_line(aes(y=regData[,4],group=1)) +
  scale_x_discrete(breaks=grBreaks,labels=grBreaks) +
  theme(axis.text.x=element_text(angle = 75, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Save plot
foodIndGrowthp
dev.copy(png,"figures/FoodOutputMonth.png",units="px",height=300,width=600)
dev.off()

rm(foodIndGrowthp,i,grBreaks)

# Save descriptive statistics of regression data to a pdf
nam<-names(regData)
temp<-regData
regData[,2]<-as.factor(regData[,2])
regData[,3]<-as.factor(regData[,3])
names(regData)[c(1,4)]<-c("Unemployment rate", "Food services and drinking places industry growth, %")

tex2pdf(regData[-96:-95,],title="Regression data descriptive statistics",1,"sum","figures")

regData<-temp
names(regData)<-nam

rm(nam,temp)

# Check for stationarity of continuous variables(unemp rate & foodIndGrowth)
png("figures/ADF.png",units="px",height=450,width=920)

par(mfrow=c(1,2))

adf1<-summary(ur.df(regData[,1],type="drift")) # include intercept since mean is not 0
textplot(capture.output(adf1)[c(-1:-10)],cex=1)
title("Augmented Dickey Fuller Test\nfor the Unemployment Rate")

adf2<-summary(ur.df(regData[,4],type="none")) # dont include intercept since mean
textplot(capture.output(adf2)[c(-1:-10)],cex=1)
title("Augmented Dickey Fuller Test for the Percent Growth\nof the Food Services and Drinking Places Industry in PA")

par(mfrow=c(1,1))

dev.off()

rm(adf1,adf2)

# AR(2) multivariate regression using the unemployment rate as dependent variable
mod<-lm(unemp~tslag(unemp,1) + tslag(foodIndGrowth,2) + crimonthDummy + tslag(minwageDummy,1),regData)
summary(mod)

# Output regression results
  #Add appropriate names to variables
names(mod$coefficients)<-c("(Intercept)",
                           "Unemployment rate(1)",
                           "USFoodIndGrowth(2)",
                           "CrisisMonth",
                           "MinimumWage(1)")
  # create table in latex and save to pdf
tex2pdf(mod,title="Regression output",3,"reg","figures")

# Autocorrelation in the variables
png("figures/SCVars.png",units="px",height=380,width=800) #save plot as png

par(mfrow=c(1,2))
acf(regData$unemp,main="Serial correlation of Unemployment Rate in the\nFood Services and Drinking Places Industry in PA")
acf(regData$foodIndGrowth, main="Serial correlation of the Food Services and\nDrinking Places Industry Output % Growth in PA")

par(mfrow=c(1,1))

dev.off()

# Plot the residuals of the model
png("figures/Resid.png",units="px",height=380,width=800)

plot(mod$residuals,main="Residuals of the model", ylab="Residuals")
abline(h=0,lty=2)

dev.off()

# Test for autocorellation in the residuals of the model
png("figures/SCResid.png",units="px",height=350,width=480)

acf(mod$residuals,main="")
title("Serial correlation of the model's residuals",cex.main=0.8)

dev.off()

# Evaluate the 1.33% increase in unemployment in terms of number of people
sum(dPA[dPA$PRML %in% c(1,2) & dPA$YYYYMM==201212,11])*0.0133

# Try to account for business cycle using the US GDP growth instead of crisismonthDummy
readLines("data/USMonthlyGDP.csv",n=5)
USGDP<-read.csv("data/USMonthlyGDP.csv",header=FALSE,sep=",",quote="\"",skip=3)
USGDP[,2]<-as.numeric(gsub("\\.|T","",USGDP[,2]))*10 # clean data and scale to billion dollars
USGDP$PerGrowth<-c((USGDP[-97,2]-USGDP[-1,2])/USGDP[-1,2]*100,0)

regData$USGDPGrowth<-USGDP[-nrow(USGDP),]$PerGrowth

cor(regData[,4],regData[,5])

mod2<-lm(unemp~tslag(unemp,1) + tslag(foodIndGrowth,2) + tslag(USGDPGrowth,4) +  tslag(minwageDummy,1),regData)
summary(mod2)

names(mod2$coefficients)<-c("(Intercept)",
                           "Unemployment rate(1)",
                           "USFoodIndGrowth(2)",
                           "USGDPGrowth(4)",
                           "MinimumWage(1)")
  # create table in latex and save to pdf
tex2pdf(mod2,title="Regression output with USGDPGrowth",4,"reg2","figures")

# Does not change the MinwageDummy coefficient and the adjusted R-squared much.
# So stick with the initial regression model. 
rm(mod2,USGDP,unemp)

# Results are reported in the Paper.pdf file
# End
