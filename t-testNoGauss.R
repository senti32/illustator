#test 2b

sampleSize=1000
pVals=c()

xN=10000; yN=10000

#x=matrix(rnorm(xN*sampleSize, 0, 1), ncol=sampleSize, nrow=xN)
#y=matrix(rnorm(yN*sampleSize, 0, 1), ncol=sampleSize, nrow=yN)
#x=matrix(rt(xN*sampleSize, 1, 0), ncol=sampleSize, nrow=xN)
#y=matrix(rt(yN*sampleSize, 1, 0), ncol=sampleSize, nrow=yN)
x=matrix(rcauchy(xN*sampleSize, 0, 1), ncol=sampleSize, nrow=xN)
y=matrix(rcauchy(yN*sampleSize, 0, 1), ncol=sampleSize, nrow=yN)

#x=matrix(rcauchy(xN*sampleSize, 1, 0), ncol=sampleSize, nrow=xN)
#x=matrix(round(runif(xN*sampleSize,-1,1))*rchisq(xN*sampleSize, 1, 0)^10, ncol=sampleSize, nrow=xN)
#y=matrix(rt(xN*sampleSize, 1, 0), ncol=sampleSize, nrow=xN)

#x=matrix(rsn(xN*sampleSize, 0, 5, 10), ncol=sampleSize, nrow=xN)
#y=matrix(rsn(xN*sampleSize, 0, 5, 10), ncol=sampleSize, nrow=yN)

#x=as.real(rsn(xN*sampleSize, 0, 1, 5))
#y=as.real(rsn(xN*sampleSize, 0, 1, 5))

for(i in 1:sampleSize){
	pVals[i]=t.test(x[,i],y[,i])$p.value
}
hist(pVals, main=paste(sum(pVals<0.05),"/", sampleSize))

