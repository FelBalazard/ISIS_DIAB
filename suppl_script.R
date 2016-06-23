#This is suppl_script.R the script used to obtain the result of the supplementary material.
#This starts with the R environment resulting from analysis_script.R
detach(umdata)
attach(totdata)#totdata is the datset obtained after missing data, age and delay exclusions but before 
#primary school exclusion.
age=1/365.25*as.numeric(as.Date(Date.Diagnostic.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y")-as.Date(Date.Naissance.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y"))

#Random Forest needs a dataset without any missing data to work. We exclude the variables that have more than 
#5% of missing data
missvar<-apply(is.na(totdata),2,sum)
var<-which(missvar<0.05*nrow(totdata))
sum(is.na(totdata[,var]))/(nrow(totdata)*length(var))#2% of missing data

library(randomForest)
set.seed(12)
totdata_nona<-totdata[,var]
badprimary<-which(age<5.5 & totdata_nona[,"X404"]==1)#Those are the participants who made the primary school mistake.
#Missing value is replaced by the mean of the variable. Simplistic but not too much missing data.
totdata_nona<-na.roughfix(totdata_nona[,-c(1:12)])
#Model is trained on the dataset to predict age.
#The badprimary participants are excluded as they are suspicious. The primary school variable is not used to see
#if the bad primary participants responded in a specific manner to the rest of the questionnaire.
model<-randomForest(totdata_nona[-badprimary,-c(which(names(totdata_nona)=="X404"))],age[-badprimary])

#Predict for the badprimary participants
agebadprimary=predict(model,totdata_nona[badprimary,-c(which(names(totdata_nona)=="X404"))])

#First supplementary plot
plot(age[-badprimary],model$predicted,pch=3,col=1, xaxt="n",ylab="Predicted age (years)",xlab="Reference age (years)",bty="n")
points(age[badprimary],agebadprimary,col=2,pch=3)
axis(1,c(1,3,5,7,9,11,13,15))
abline(a=0,b=1)
text(c(11.5),c(12),c("y=x"))
legend(11.5,4,c("Error on primary","Others"),pch=c(3,3),col=c(2,1),bty="n")


#Second supplementary plot
plot(age[-badprimary],model$predicted,pch=3+as.numeric(Type.Questionnaire=="TEMOIN"),col=1+as.numeric(Type.Questionnaire=="TEMOIN"), 
     ylab="Predicted age (years)",xlab="Reference age (years)",xaxt="n",bty="n")
axis(1,c(1,3,5,7,9,11,13,15))
abline(a=4,b=1)
abline(a=0,b=1)
text(c(11.5,7.15),c(12,12),c("y=x","y=x+4"))
legend(12,4,c("Controls","Patients"),pch=c(4,3),col=c(2,1),bty="n")
table(totdata[,"Type.Questionnaire"][-badprimary][age[-badprimary]<model$predicted-4] )

#New exclusion of the participants' whose predicted age was superior to their reference age + 4
badnum<-union(totdata[,"Numero_questionnaire"][badprimary],totdata[,"Numero_questionnaire"][-badprimary][age[-badprimary]<model$predicted-4])

totdata_suppl<-totdata[-which(totdata[,"Numero_questionnaire"]%in%badnum),]
detach(totdata)
attach(totdata_suppl)
mdata_suppl=totdata_suppl[(Code_SVP_patientapparie%in%Code_SVP & !is.na(Code_SVP_patientapparie)) |( Code_SVP%in%Code_SVP_patientapparie & !is.na(Code_SVP)),]
detach(totdata_suppl)
attach(mdata_suppl)

code_suppl<-Code_SVP
code_suppl[is.na(Code_SVP)]<-Code_SVP_patientapparie[is.na(Code_SVP)]
code_suppl<-factor(code_suppl)


library(survival)

#Same analysis as in the main text for matched data set
n_m_suppl<-numeric(length(set))
names(n_m_suppl)<-set
p_m_suppl<-numeric(length(set))
names(p_m_suppl)<-set
size_m_suppl<-numeric(length(set))
names(size_m_suppl)<-set
upCI_m_suppl<-numeric(length(set))
names(upCI_m_suppl)<-set
loCI_m_suppl<-numeric(length(set))
names(loCI_m_suppl)<-set
for (name in set){
  if (var(mdata_suppl[,name],na.rm=T)<0.01){
    n_m_suppl[name]=sum(!is.na(mdata_suppl[,name]))
    p_m_suppl[name]=1
    size_m_suppl[name]=1
    loCI_m_suppl[name]=1
    upCI_m_suppl[name]=1
  } else if (length(levels(factor(mdata_suppl[,name])))==2){
    subset<-code_suppl%in%names(which(apply(table(mdata_suppl[,name],code_suppl),2,sum)>1))
    mhtest<-mantelhaen.test(Type.Questionnaire[subset]=="PATIENT",mdata_suppl[subset,name]==1,factor(code_suppl[subset]))
    n_m_suppl[name]<-sum(!is.na(mdata_suppl[subset,name]))
    p_m_suppl[name]<-mhtest$p.value
    size_m_suppl[name]<-mhtest$estimate
    loCI_m_suppl[name]<-mhtest$conf.int[1]
    upCI_m_suppl[name]<-mhtest$conf.int[2]
  } else {
    regress<-summary(clogit((Type.Questionnaire=="PATIENT")~mdata_suppl[,name]+strata(code_suppl)))
    n_m_suppl[name]<-regress$n
    p_m_suppl[name]<-regress$coefficients[1,5]
    size_m_suppl[name]<-regress$coefficients[1,2]
    loCI_m_suppl[name]<-regress$conf.int[1,3]
    upCI_m_suppl[name]<-regress$conf.int[1,4]
  }
}

#Subsampling######
#This is done to quantify what a normal drop in p-value would be under a random exclusion.
varnamebin<-c("X219","X238","X260","X269","X322","X418","X421","X423","X424","X456.11")
varnamequant<-c("X238.1","X239","X342","X419.1")
numrep<-10000#Number of repetition of the procedure
p_age_m<-p_m_suppl

#This small function allows to know if a strata has one patient with one control or one patient and two controls.
pat_tem<-function(t){sum(t[1,])*sum(t[2,])}

#It is important that the random subsampling be as close as possible to the exclusion made.
#In order to do that we considered subsampling that after excluding missing data has the same number of strata
#of the same type (1:1 or 1:2) as mdata_suppl.

for (name in varnamebin){
  olddiff<-sapply(1:length(levels(code_m)),function(i) pat_tem(table(mdata[,"Type.Questionnaire"],mdata[,name],code_m)[,,i]))
  newdiff<-sapply(1:length(levels(code_suppl)),function(i) pat_tem(table(mdata_suppl[,"Type.Questionnaire"],mdata_suppl[,name],code_suppl)[,,i]))
  psim<-numeric(numrep)
  for (seed in 1:numrep){
    set.seed(seed)
    subsetcode<-union(levels(code_m)[olddiff==1][sample.int(sum(olddiff==1),sum(newdiff==1))],levels(code_m)[olddiff==2][sample.int(sum(olddiff==2),sum(newdiff==2))])
    subset<-code_m%in%subsetcode
    psim[seed]<-mantelhaen.test(mdata[subset,"Type.Questionnaire"]=="PATIENT",mdata[subset,name]==1,factor(code_m[subset]))$p.value
  }
  hist(-log10(psim),breaks=50,main = name)
  abline(v=-log10(p_m_suppl[name]))
  p_age_m[name]<-sum(psim>p_m_suppl[name])/numrep
}

library(survival)#for clogit
for (name in varnamequant){
  olddiff<-sapply(1:length(levels(code_m)),function(i) pat_tem(table(mdata[,"Type.Questionnaire"],mdata[,name],code_m)[,,i]))
  newdiff<-sapply(1:length(levels(code_suppl)),function(i) pat_tem(table(mdata_suppl[,"Type.Questionnaire"],mdata_suppl[,name],code_suppl)[,,i]))
  psim<-numeric(numrep)
  for (seed in 1:numrep){
    set.seed(seed)
    subsetcode<-union(levels(code_m)[olddiff==1][sample.int(sum(olddiff==1),sum(newdiff==1))],levels(code_m)[olddiff==2][sample.int(sum(olddiff==2),sum(newdiff==2))])
    subset<-code_m%in%subsetcode
    regress<-summary(clogit(mdata[subset,"Type.Questionnaire"]=="PATIENT" ~ as.numeric(mdata[subset,name]) + strata(code_m[subset])))
    psim[seed]<-regress$coefficients[1,5] }
  hist(-log10(psim),breaks=50,main = name)
  abline(v=-log10(p_m_suppl[name]))
  p_age_m[name]<-sum(psim>p_m_suppl[name])/numrep
}
detach(mdata_suppl)

#Subsampling for propensity.
#Here random subsampling means having the same number of patients and controls as umdata_suppl after 
#removing missing data. The propensity model is computed only once on umdata and the propensity
library(randomForest)
umdata_suppl<-totdata_suppl[totdata_suppl$Numero_questionnaire%in%umdata$Numero_questionnaire,]
p_um_suppl<-p_m_suppl
um_res_suppl<-NULL
p_age_um<-p_m_suppl
attach(umdata_suppl)
for( name in set) {
  if (length(levels(factor(umdata[,name])))==2){
    subset=which(!is.na(umdata[,name]))
    model2<-randomForest(confounders[subset,],umdata[subset,name]==1)
    propensity<-model2$predicted
    strata<-factor(findInterval( propensity, c(quantile(propensity, probs=seq(0,1, by=0.1),na.rm=T))+c(rep(0,10),1)))
    subset_suppl<-which(Numero_questionnaire%in%umdata$Numero_questionnaire[subset])
    subset_strata<-which(umdata$Numero_questionnaire[subset]%in%Numero_questionnaire)
    mhtest<-mantelhaen.test(Type.Questionnaire[subset_suppl]=="PATIENT",umdata_suppl[subset_suppl,name]==1,factor(strata[subset_strata]))
    p_um_suppl[name]<-mhtest$p.value
    um_res_suppl<-rbind(um_res_suppl,c(name,length(subset_suppl),mhtest$p.value,mhtest$estimate,mhtest$conf.int[1],mhtest$conf.int[2]))
    psim<-numeric(numrep)
    for (seed in 1:numrep){
      set.seed(seed)
      subset_sim<-union(sample(which(umdata$Type.Questionnaire[subset]=="PATIENT"),sum(Type.Questionnaire[subset_suppl]=="PATIENT")),
                        sample(which(umdata$Type.Questionnaire[subset]=="TEMOIN"),sum(Type.Questionnaire[subset_suppl]=="TEMOIN")))
      psim[seed]<-mantelhaen.test(umdata[subset,"Type.Questionnaire"][subset_sim]=="PATIENT",umdata[subset,name][subset_sim]==1,factor(strata[subset_sim]))$p.value
      }
    hist(-log10(psim),breaks=50,main = name)
    abline(v=-log10(as.numeric(p_um_suppl[name])))
    p_age_um[name]<-sum(psim>p_um_suppl[name])/numrep
  }
  else {subset=which(!is.na(umdata[,name]))
  model2<-randomForest(confounders[subset,],umdata[subset,name])
  propensity<-model2$predicted
  strata<-factor(findInterval( propensity, c(quantile(propensity, probs=seq(0,1, by=0.1),na.rm=T))+c(rep(0,10),1)))
  subset_suppl<-which(Numero_questionnaire%in%umdata$Numero_questionnaire[subset])
  subset_strata<-which(umdata$Numero_questionnaire[subset]%in%Numero_questionnaire)
  regress<-summary(clogit((Type.Questionnaire[subset_suppl]=="PATIENT")~umdata_suppl[subset_suppl,name]+strata(strata[subset_strata])))
  p_um_suppl[name]<-regress$logtest[3]
  um_res_suppl<-rbind(um_res_suppl,c(name,length(subset_suppl),regress$logtest[3],regress$coefficients[1,2],regress$conf.int[3],regress$conf.int[4]))
  psim<-numeric(numrep)
  for (seed in 1:numrep){
    set.seed(seed)
    subset_sim<-union(sample(which(umdata$Type.Questionnaire[subset]=="PATIENT"),sum(Type.Questionnaire[subset_suppl]=="PATIENT")),
                      sample(which(umdata$Type.Questionnaire[subset]=="TEMOIN"),sum(Type.Questionnaire[subset_suppl]=="TEMOIN")))
    regress<-summary(clogit(umdata[subset,"Type.Questionnaire"][subset_sim]=="PATIENT" ~ as.numeric(umdata[subset,name][subset_sim]) + strata(strata[subset_sim])))
    psim[seed]<-regress$logtest[3]}
  hist(-log10(psim),breaks=50,main = name)
  abline(v=-log10(p_um_suppl[name]))
  p_age_um[name]<-sum(psim>p_um_suppl[name])/numrep
  }
}


#Comparison plot
plot(-log10(um_result[set,"p"]),-log10(p_m[set]),xlim=c(3.4,12),ylim=c(1.8,5.6),
     pch=3,xlab="Propensity analysis (-log10(p-value))",ylab="Matched analysis (-log10(p-value))")
points(-log10(p_um_suppl),-log10(p_m_suppl),pch=4)
segments(-log10(um_result[set,"p"]),-log10(p_m[set]),-log10(p_um_suppl),-log10(p_m_suppl),lty=4)

text(-log10(um_result[set,"p"]),-log10(p_m[set]),
     c("Sugar Baby","Dentist","Dentist (freq.)","Dental hygiene"
       ,"Diarrhea","Stings","Farm vegetables",
       "Cocoa spread","Ski","Beach","Friend's pool","Club","Social week-end","Pet's death"),
     #set,
     pos=c(4,3,2,4,
           3,2,1,
           3,4,4,3,2,3,4),cex=0.95)
abline(h=mean(c(-log10(min(p_m[fdr_m>0.05])),-log10(max(p_m[fdr_m<0.05])))),col="Red",lty=2)
abline(v=-log10(0.05/845),col="Blue",lty=2)
text(6,mean(c(-log10(min(p_m[fdr_m>0.05])),-log10(max(p_m[fdr_m<0.05])))),"FDR=5%",col="Red",pos=1)
text(4,5,"Bonferroni",col="Blue")
legend(9,5.6,c("Original","Modified"),pch=c(3,4),bty="n")

#Again missing data separately to comply with strobe checklist.
m_case_miss<-apply(is.na(mdata_suppl[mdata_suppl$Type.Questionnaire=="PATIENT",-c(1:12)]),2,sum)
m_contr_miss<-apply(is.na(mdata_suppl[mdata_suppl$Type.Questionnaire=="TEMOIN",-c(1:12)]),2,sum)
um_contr_miss<-apply(is.na(umdata_suppl[umdata_suppl$Type.Questionnaire=="TEMOIN",-c(1:12)]),2,sum)
um_case_miss<-apply(is.na(umdata_suppl[umdata_suppl$Type.Questionnaire=="PATIENT",-c(1:12)]),2,sum)

result_suppl<-cbind(c("Sugar Baby","Dentist","Dentist (freq.)","Dental hygiene"
                      ,"Diarrhea","Stings","Farm vegetables",
                      "Cocoa spread","Ski","Beach","Friend's pool","Club","Social week-end","Pet's death"),m_case_miss[set],m_contr_miss[set],p_m_suppl,size_m_suppl,loCI_m_suppl,upCI_m_suppl,p_age_m,um_case_miss[set],um_contr_miss[set],um_res_suppl[,3:6],p_age_um)
write.csv(result_suppl,"~/Documents/publication/isis_diab/result_suppl.csv")
