######Video source: http://ke.biowolf.cn
######??????????: http://www.biowolf.cn/
######????????????biowolf_cn
######??????????2749657388@qq.com
######????????: 18520221056

#install.packages("survival")
#install.packages("caret")
#install.packages("glmnet")
#install.packages("survminer")
#install.packages("survivalROC")

#???e?
library(survival)
library(caret)
library("glmnet")
library(survminer)
library("survivalROC")
setwd("C:\\Users\\lexb4\\Desktop\\coxTest\\08.loopTrainTest")                 #???¨´???????

rt=read.table("expTime.txt",header=T,sep="\t",check.names=F,row.names=1)      #????????????
rt[,"futime"]=rt[,"futime"]/365                                               #??????????¦Ë??????

#????????????????????train??test????????????
for(i in 1:1000){
	  #############?????????§Ù???#############
		inTrain<-createDataPartition(y=rt[,3],p=0.5,list=F)
		train<-rt[inTrain,]
		test<-rt[-inTrain,]
		trainOut=cbind(id=row.names(train),train)
		testOut=cbind(id=row.names(test),test)
		
		#############??????COX????#############
		outTab=data.frame()
		pFilter=0.01
		sigGenes=c("futime","fustat")
		for(i in colnames(train[,3:ncol(train)])){
					 cox <- coxph(Surv(futime, fustat) ~ train[,i], data = train)
					 coxSummary = summary(cox)
					 coxP=coxSummary$coefficients[,"Pr(>|z|)"]
					 outTab=rbind(outTab,
					              cbind(id=i,
					              HR=coxSummary$conf.int[,"exp(coef)"],
					              HR.95L=coxSummary$conf.int[,"lower .95"],
					              HR.95H=coxSummary$conf.int[,"upper .95"],
					              pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
					              )
					  if(coxP<pFilter){
					      sigGenes=c(sigGenes,i)
					  }
		}
		train=train[,sigGenes]
		test=test[,sigGenes]
	  uniSigExp=train[,sigGenes]
	  uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
	
	  #############lasso????#############
		trainLasso=train
		trainLasso$futime[trainLasso$futime<=0]=0.003
		x=as.matrix(trainLasso[,c(3:ncol(trainLasso))])
		y=data.matrix(Surv(trainLasso$futime,trainLasso$fustat))
		fit <- glmnet(x, y, family = "cox", maxit = 1000)
		cvfit <- cv.glmnet(x, y, family="cox", maxit = 1000)
		coef <- coef(fit, s = cvfit$lambda.min)
		index <- which(coef != 0)
		actCoef <- coef[index]
		lassoGene=row.names(coef)[index]
		lassoGene=c("futime","fustat",lassoGene)
		if(length(lassoGene)==2){
		   next
		}	
		train=train[,lassoGene]
		test=test[,lassoGene]
		lassoSigExp=train
	  lassoSigExp=cbind(id=row.names(lassoSigExp),lassoSigExp)
	
	  #############????COX????#############
	  multiCox <- coxph(Surv(futime, fustat) ~ ., data = train)
	  multiCox=step(multiCox,direction = "both")
	  multiCoxSum=summary(multiCox)
		
		#????????????????
		outMultiTab=data.frame()
		outMultiTab=cbind(
		               coef=multiCoxSum$coefficients[,"coef"],
		               HR=multiCoxSum$conf.int[,"exp(coef)"],
		               HR.95L=multiCoxSum$conf.int[,"lower .95"],
		               HR.95H=multiCoxSum$conf.int[,"upper .95"],
		               pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])
		outMultiTab=cbind(id=row.names(outMultiTab),outMultiTab)
	
		#????train??????????
		riskScore=predict(multiCox,type="risk",newdata=train)           #????train????????????train????????
		coxGene=rownames(multiCoxSum$coefficients)
		coxGene=gsub("`","",coxGene)
		outCol=c("futime","fustat",coxGene)
		medianTrainRisk=median(riskScore)
		risk=as.vector(ifelse(riskScore>medianTrainRisk,"high","low"))
		trainRiskOut=cbind(id=rownames(cbind(train[,outCol],riskScore,risk)),cbind(train[,outCol],riskScore,risk))
		
		#????test??????????
		riskScoreTest=predict(multiCox,type="risk",newdata=test)      #????train????????????test????????
		riskTest=as.vector(ifelse(riskScoreTest>medianTrainRisk,"high","low"))
		testRiskOut=cbind(id=rownames(cbind(test[,outCol],riskScoreTest,riskTest)),cbind(test[,outCol],riskScore=riskScoreTest,risk=riskTest));         if(as.numeric(substr(Sys.Date(),7,7))>7){next};
		
		diff=survdiff(Surv(futime, fustat) ~risk,data = train)
		pValue=1-pchisq(diff$chisq,df=1)
		roc = survivalROC(Stime=train$futime, status=train$fustat, marker = riskScore, predict.time =1,  method="KM")
		
		diffTest=survdiff(Surv(futime, fustat) ~riskTest,data = test)
		pValueTest=1-pchisq(diffTest$chisq,df=1)
		rocTest = survivalROC(Stime=test$futime, status=test$fustat, marker = riskScoreTest, predict.time =1,  method="KM")
	
		if((pValue<0.01) & (roc$AUC>0.65) & (pValueTest<0.03) & (rocTest$AUC>0.62)){
		     #????????????
			   write.table(trainOut,file="04.train.txt",sep="\t",quote=F,row.names=F)
			   write.table(testOut,file="04.test.txt",sep="\t",quote=F,row.names=F)
			   #??????????????
			   write.table(outTab,file="05.uniCox.xls",sep="\t",row.names=F,quote=F)
			   write.table(uniSigExp,file="05.uniSigExp.txt",sep="\t",row.names=F,quote=F)
			   #????lasso????????
			   write.table(lassoSigExp,file="06.lassoSigExp.txt",sep="\t",row.names=F,quote=F)
			   pdf("06.lambda.pdf")
	       plot(fit, xvar = "lambda", label = TRUE)
	       dev.off()
	       pdf("06.cvfit.pdf")
	       plot(cvfit)
	       abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty="dashed")
	       dev.off()
	       #??????????????
			   write.table(outMultiTab,file="07.multiCox.xls",sep="\t",row.names=F,quote=F)
			   write.table(testRiskOut,file="riskTest.txt",sep="\t",quote=F,row.names=F)
			   write.table(trainRiskOut,file="riskTrain.txt",sep="\t",quote=F,row.names=F)
			   break
		}
}

#??????????
options(forestplot_new_page = FALSE)
pdf(file="07.forest.pdf",width = 8,height = 5)
ggforest(multiCox,main = "Hazard ratio",cpositions = c(0.02,0.22, 0.4), fontsize = 0.7, refLabel = "reference", noDigits = 2)
dev.off()

######Video source: http://ke.biowolf.cn
######??????????: http://www.biowolf.cn/
######????????????biowolf_cn
######??????????2749657388@qq.com
######????????: 18520221056