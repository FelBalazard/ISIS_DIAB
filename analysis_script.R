#Analaysis_script.r is the script that was used to analyse the data of the article Association of environmental 
#markers with childhood type 1 diabetes mellitus revealed by a long questionnaire on early life exposures and 
#lifestyle in a case-control study.

#Load the data
ISIS_QUEST_2 <- read.csv("~/Documents/Questionnaire/ISIS_QUEST_2.csv")

## Merge with the most recent numerisation of data. #
nouvpat<-read.csv("~/Documents/Questionnaire/Nouvellesdonnees/ISIS_DIAB_Patient_211215_mod2.csv",sep=";")
nouvtem<-read.csv("~/Documents/Questionnaire/Nouvellesdonnees/ISIS_DIAB_Temoin_211215_mod2.csv",sep=";")
#This loop is here to prevent problems of missing levels in factors.
for (name in names(nouvpat)[-c(1)]){
  ISIS_QUEST_2[,name]<-as.character(ISIS_QUEST_2[,name])
  nouvpat[name]<-as.character(nouvpat[,name])
  nouvtem[name]<-as.character(nouvtem[,name])
}
ISIS_QUEST_2[na.omit(match(nouvpat[,"Numero_questionnaire"],ISIS_QUEST_2[,"Numero_questionnaire"])),match(names(nouvpat[-c(1)]),names(ISIS_QUEST_2))]<-nouvpat[,-c(1)]
ISIS_QUEST_2[na.omit(match(nouvtem[,"Numero_questionnaire"],ISIS_QUEST_2[,"Numero_questionnaire"])),match(names(nouvtem[-c(1)]),names(ISIS_QUEST_2))]<-nouvtem[,-c(1)]
#################

#First exclusion depending on missing data.
a2<-apply(is.na(ISIS_QUEST_2[,-c(1:12)]),1,sum)
mb<-ISIS_QUEST_2[a2<400,]

#At this point run regrpQues.R and save as questrevucomplet and then replace the remaining multiple answers by NA
quest<-read.csv("~/Documents/Questionnaire/questrevucomplet",sep=",")
alldata<-cbind(mb[,1:12],quest)

alldata[which(alldata$X373=="na"),"X373"]<-NA #A small coding problem
alldata[,"X373"]<-as.numeric(alldata[,"X373"])

#This loop scales all ordinal variables between 0 and 1. It also reverses the ordering as most ordinal variables
#were ordered counterintuitively. For example, from most frequent to less frequent.
for(name in names(alldata)[13:ncol(alldata)]){
  if(length(levels(factor(alldata[,name])))!=2){
    alldata[,name]<-(max(alldata[,name],na.rm=T)-alldata[,name])/(max(alldata[,name],na.rm=T)-min(alldata[,name],na.rm=T))
  }
}
#For the few variables ordered in a reasonable manner, we reverse the ordering back.
for(name in c("X38anesthésie","X38chirurgie","X108.1","X175.6.1","X238.1","X275","X419.1","X434","X435","X512.1","X513.1","X558.7","X575.2")){
  alldata[,name]<-1-alldata[,name]
}
#Exclusion for age
attach(alldata)
age=1/365.25*as.numeric(as.Date(Date.Diagnostic.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y")-as.Date(Date.Naissance.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y"))
agemax=15.5

totdata=alldata[which(age<agemax & age>0.5),]

detach(alldata)
#Exclusion for delay
attach(totdata)
delay=1/365.25*as.numeric(as.Date(Date.Réception.Questionnaire,format="%d/%m/%Y")-as.Date(Date.Diagnostic.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y"))
totdata=totdata[which(delay<10),]
detach(totdata)
#Exclusion for primary school mistake
attach(totdata)
age=1/365.25*as.numeric(as.Date(Date.Diagnostic.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y")-as.Date(Date.Naissance.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y"))
totdata_excl<-totdata[-which(X404==1 & age<5.5),]
detach(totdata)
#Exclusion of participants with no matched counterparts in order to create the matched dataset.
attach(totdata_excl)
mdata=totdata_excl[(Code_SVP_patientapparie%in%Code_SVP & !is.na(Code_SVP_patientapparie)) |( Code_SVP%in%Code_SVP_patientapparie & !is.na(Code_SVP)),]
detach(totdata_excl)
attach(mdata)
################################
#MATCHED ANALYSIS###############
################################

#Definition of a common code variable for matching. Code was separated in two variables for patients and controls
code_m<-as.character(Code_SVP)
code_m[is.na(Code_SVP)]<-as.character(Code_SVP_patientapparie[is.na(Code_SVP)])
code_m<-factor(code_m)

library(survival)#for clogit

#Vector to store the exact number of observations. Not used in the end, replaced by missing number of 
#observations separately for cases and controls.
n_m<-numeric(length(names(totdata[,-c(1:12)])))
names(n_m)<-names(totdata[,-c(1:12)])
#Vector to store p-values.
p_m<-numeric(length(names(totdata[,-c(1:12)])))
names(p_m)<-names(totdata[,-c(1:12)])
#Vector to store effect size.
size_m<-numeric(length(names(totdata[,-c(1:12)])))
names(size_m)<-names(totdata[,-c(1:12)])
#Vector to store upper bound of 95% CI
upCI_m<-numeric(length(names(totdata[,-c(1:12)])))
names(upCI_m)<-names(totdata[,-c(1:12)])
#Vector to store lower bound of 95% CI
loCI_m<-numeric(length(names(totdata[,-c(1:12)])))
names(loCI_m)<-names(totdata[,-c(1:12)])
#Loop to analyse each variable
for (name in names(totdata[,-c(1:12)])){
  if (var(mdata[,name],na.rm=T)<0.01){#For sd<0.1 many problems of convergence for clogit. Low power for these 
    n_m[name]=sum(!is.na(mdata[,name]))#variables, so exclusion.
    p_m[name]=1
    size_m[name]=1
    loCI_m[name]=1
    upCI_m[name]=1
  } else if (length(levels(factor(mdata[,name])))==2){# For binary variables, Cochran Mantel Haenszel test.
    subset<-code_m%in%names(which(apply(table(mdata[,name],code_m),2,sum)>1))
    mhtest<-mantelhaen.test(Type.Questionnaire[subset]=="PATIENT",mdata[subset,name]==1,factor(code_m[subset]))
    n_m[name]<-sum(!is.na(mdata[subset,name]))
    p_m[name]<-mhtest$p.value
    size_m[name]<-mhtest$estimate
    loCI_m[name]<-mhtest$conf.int[1]
    upCI_m[name]<-mhtest$conf.int[2]
  } else {#For ordinal variables, conditional logistic regression.
    regress<-summary(clogit((Type.Questionnaire=="PATIENT")~mdata[,name]+strata(code_m)))
    n_m[name]<-regress$n
    p_m[name]<-regress$coefficients[1,5]
    size_m[name]<-regress$coefficients[1,2]
    loCI_m[name]<-regress$conf.int[1,3]
    upCI_m[name]<-regress$conf.int[1,4]
  }
}

#Volcano plot of the result

#X16.1 chronic disease of the father (open question with hand-written precision: many T1D for patients),
#X557 is sibling with T1D and X552 father with T1D. Obviously those variables are positively correlated with T1D.
#But the focus of the article being the environment, we excluded them from the figures.
p_excl<-p_m[-which(names(p_m)%in%c("X16.1","X557","X552"))]
size_excl<-size_m[-which(names(p_m)%in%c("X16.1","X557","X552"))]
fdr_m<-p.adjust(p_excl,method="fdr")
holm_m<-p.adjust(p_m,method="holm")#No difference compared to bonferroni correction in our case.
#The threshold for Bonferroni is more readily available so we used it in the text and the plot.

plot(log2(size_excl),-log10(p_excl),pch=3,xlab="log2(effect size)",ylab="-log10(p-value)")
abline(h=-mean(c(max(log10(p_excl[fdr_m<0.05])),min(log10(p_excl[fdr_m>0.05])))),lty=2,col="red")
abline(h=-log10(0.05/845),lty=2,col="blue")
text(0,-log10(0.05/845),"Bonferroni",col="Blue",pos=3 )
text(0,mean(c(-log10(min(p_excl[fdr_m>0.05])),-log10(max(p_excl[fdr_m<0.05])))),"FDR=5%",col="Red",pos=1)
text(log2(size_excl[c("X239","X260","X309","X342","X418","X423","X419.1","X238.1","X238","X338","X350","X298","X297","X434")]),-log10(p_excl[c("X239","X260","X309","X342","X418","X423","X419.1","X238.1","X238","X338","X350","X298","X297","X434")]),
     c("Dental hygiene","Diarrhea","Proc. meats","Cocoa spread","Ski","Club","Beach","Dentist (freq.)","Dentist","Jam","Juice","Salt","Sugar","Zoo"),
     pos=c(2,3,3,4,4,3,1,2,3,1,2,2,1,2))
text(-0.75,3.2,"Cf legend",cex=1.2)

detach(mdata)
##END OF MATCHED ANALYSIS#


####################################
##PROPENSITY ANALYSIS###############
####################################

#Covariate loading 
#Urban units (coded between 1 (country side) and 8 metropolis)
uu<-read.table("~/Documents/Questionnaire/socialome/UU2010.csv",header=TRUE,dec=".",sep=";")
#Different coding for arrondissement (districts) in Paris, Lyon and Marseilles between INSEE and Postal service
arrond<-c(75101:75120,69381:69389,13201:13216) 
UU<-c(rep(8,20),rep(7,25))

# Percentage of the population working in agriculture
agriculteurs_exploitants<-read.table("~/Documents/Questionnaire/socialome/agriculteurs_exploitants.txt",header=TRUE,dec=".",sep=",")

#Other half of the dataset with hand-written answers including postal code and parent's occupation.
ISIS_QUEST_1 <- read.csv("~/Documents/Questionnaire/ISIS_QUEST_1.csv")

#Numero_questionnaire allows to identify uniquely each individual
ISIS_QUEST_1<-ISIS_QUEST_1[ISIS_QUEST_1[,"Numero_questionnaire"]%in%totdata_excl[,"Numero_questionnaire"],]
#Postal code definition. Several ones were available so we used the closest in time
CP<-ISIS_QUEST_1$CP_naiss.diag_1
CP<-as.character(CP)
sum(is.na(CP))
for (cp in c("CP_naiss.diag_2","CP_Parents","CP_Mere_naiss.diag1","CP_Pere","CP_Mere")){
  CP[is.na(CP)]<-as.character(ISIS_QUEST_1[is.na(CP),cp])
  print(sum(is.na(CP)))}
#Small formatting issue.
for (i in which(nchar(CP)==4)){
  CP[i]<-paste("0",CP[i],sep="")
}

#Correspondance between postal code and insee code
Insee_poste <- read.csv("~/Documents/Questionnaire/socialome/correspondance-code-insee-code-postal.csv", sep=";")
splitcode <- strsplit(as.character(Insee_poste$Code.Postal), "/")
res <- data.frame(Code.INSEE=rep(Insee_poste$Code.INSEE, sapply(splitcode, length)),
                  Code.postal=unlist(splitcode))
Insee<-res[match(CP,res[,2]),1]

#Loading of coding of parent's profession.
profession <- read.csv("~/Documents/Questionnaire/profession.csv")
profession<-profession[profession$Numero_questionnaire%in%totdata_excl$Numero_questionnaire,]
#Propensity analysis sample definition (um=unmatched)
umdata<-totdata_excl[!is.na(Insee) & !is.na(profession$Niveau_social),]
#Grouping of the covariates in a data frame
confounders<-data.frame(Niveau_social=profession$Niveau_social)
confounders<-data.frame(Niveau_social=confounders[!is.na(Insee) & !is.na(profession$Niveau_social),])
Insee<-Insee[!is.na(Insee) & !is.na(profession$Niveau_social)]
ou<-match(Insee,uu$CODGEO)
confounders$uniteUrb<-uu$Taille[ou]
confounders$uniteUrb[which(is.na(ou))]<-UU[match(Insee[which(is.na(ou))],arrond)]
confounders$agric<-agriculteurs_exploitants$pcntAgricult[match(Insee,agriculteurs_exploitants$Com)]

attach(umdata)
confounders$age=1/365.25*as.numeric(as.Date(Date.Diagnostic.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y")-as.Date(Date.Naissance.Patient..donnée.issue.e.Isisdiab.,format="%d/%m/%Y"))

#Adding the center of recruitment covariate
code<-as.character(Code_SVP)
code[is.na(Code_SVP)]<-as.character(Code_SVP_patientapparie[is.na(Code_SVP)])
code<-factor(code)

identite <- read.table("~/Documents/Questionnaire/identite_inclusion_2015_11_03.csv", header=T,sep=";", quote="\"")
identite<-identite[-1,]
umdata<-umdata[-is.na(match(code,as.character(identite$Code.patient))),]
confounders<-confounders[-is.na(match(code,as.character(identite$Code.patient))),]
code<-code[-is.na(match(code,as.character(identite$Code.patient)))]
centre<-factor(identite$Nom.Centre[match(code,as.character(identite$Code.patient))])
levels(centre)<-c(levels(centre),"autres")
centre[table(centre)[centre]<30]<-"autres"
centre<-factor(centre)
confounders$centre=centre
confounders$centre[is.na(confounders$centre)]<-"autres"

#The calculations were parallelized on a cluster of computers using the same RData environment.
#Cf the two scripts um_analyse.sh and um_analyse.R
rm(list=as.character(ls()[!ls()%in%c("confounders","umdata")]))
save.image("~/Documents/Questionnaire/um_analyse.RData")

#We then import the result
um_result <- read.table("~/Documents/Questionnaire/socialome/um_result", header=F, quote="\"")
um_result[,1]<-as.character(um_result[,1])
#However some of the calculations failed for mysterious reasons. So I do them on my personal computer.
#The loop is the same as in um_analyse.R
names(p_m[which(!names(p_m)%in%um_result[,1])])

library(randomForest)
for( name in names(p_m[!names(p_m)%in%um_result[,1]])){
  if (var(umdata[,name],na.rm=T)<0.01){#Same exclusion as for matched analysis.
    um_result<-rbind(um_result,c(name,sum(!is.na(umdata[,name])),1,1,1,1))}
  else if (length(levels(factor(umdata[,name])))==2){
    subset=!is.na(umdata[,name])
    model2<-randomForest(confounders[subset,],umdata[subset,name]==1)#model definition
    propensity<-model2$predicted#Out-of-bag estimate defines the propensity score.
    strata<-factor(findInterval( propensity, c(quantile(propensity, probs=seq(0,1, by=0.1),na.rm=T))+c(rep(0,10),1)))#Data stratified on propensity score.
    mhtest<-mantelhaen.test(Type.Questionnaire[subset]=="PATIENT",umdata[subset,name]==1,factor(strata))
    um_result<-rbind(um_result,c(name,sum(subset),mhtest$p.value,mhtest$estimate,mhtest$conf.int[1],mhtest$conf.int[2]))
  } else {subset=!is.na(umdata[,name])
  model2<-randomForest(confounders[subset,],umdata[subset,name])
  propensity<-model2$predicted
  strata<-factor(findInterval( propensity, c(quantile(propensity, probs=seq(0,1, by=0.1),na.rm=T))+c(rep(0,10),1)))
  regress<-summary(clogit((Type.Questionnaire[subset]=="PATIENT")~umdata[subset,name]+strata(strata)))
  um_result<-rbind(um_result,c(name,sum(subset),regress$logtest[3],regress$coefficients[1,2],regress$conf.int[3],regress$conf.int[4]))
  }
}
row.names(um_result)<-um_result[,1]
um_result[,1]<-NULL
names(um_result)<-c("n","p","size","loCI","upCI")
um_result<-um_result[match(names(p_m),rownames(um_result)),]
for(i in 1:5){um_result[,i]<-as.numeric(um_result[,i])}

#Additional exclusion of a few variables in order to have a more centered plot. They were not very significant.
#They are available in the result file.
set<-which(row.names(um_result)%in%setdiff(row.names(um_result),c("X16.1","X557","X552")) & um_result$size<2)

#Second volcano plot
holm<-p.adjust(um_result[set,"p"],method="holm")#Same story, no difference with Bonferroni result.
names(holm)<-row.names(um_result[set,])
plot(log2(um_result$size[set]),-log10(um_result$p[set]),pch=3,ylab="-log10(p-value)",xlab="log2(effect size)")
text(log2(um_result[c("X239","X314","X342",
                      "X418","X421","X423","X424",
                      "X419.1","X238.1","X238","X237",
                      "X322","X219","X313"),"size"]),
     -log10(um_result[c("X239","X314","X342",
                        "X418","X421","X423","X424",
                        "X419.1","X238.1","X238","X237",
                        "X322","X219","X313"),"p"]),
     c("Dental hygiene","Fish(river)","Cocoa spread",
       "Ski","Friend's pool","Club","Social week-end"
       ,"Beach","Dentist(freq)","Dentist","Contact dirt",
       "Farm vegetables","Sugar Baby","Fish"),
     pos=c(3,4,3,
           4,1,2,3,
           3,1,4,3,
           4,3,1))
abline(h=-log10(0.05/845),lty=2,col="blue")
text(0,-log10(0.05/845),"Bonferroni",col="Blue",pos=3 )
text(-0.7,5,"Cf legend",cex=1.1)

#COMPARISON PLOT####
set<-intersect(names(fdr_m[fdr_m<0.05]),row.names(um_result[p.adjust(um_result$p,method="holm")<0.05,]))
set<-setdiff(set,c("X16.1","X557"))
#set<-union(set,"X269")
plot(-log10(um_result$p[-which(names(p_m)%in%c("X16.1","X557","X552"))]),-log10(p_m[-which(names(p_m)%in%c("X16.1","X557","X552"))]),
     pch=3,xlab="Propensity analysis (-log10(p-value))",ylab="Matched analysis (-log10(p-value))")
text(-log10(um_result[set,"p"]),-log10(p_m[set]),
     c("Sugar Baby","Dentist","Dentist (freq.)","Dental hygiene"
       ,"Diarrhea","Stings","Farm vegetables",
       "Cocoa spread","Ski","Beach","Friend's pool","Club","Social week-end","Pet's death"),
     #set,
     pos=c(3,1,2,4,
           2,1,1,
           2,1,2,3,2,4,2),cex=0.95)
abline(h=mean(c(-log10(min(p_m[fdr_m>0.05])),-log10(max(p_m[fdr_m<0.05])))),col="Red",lty=2)
abline(v=-log10(0.05/845),col="Blue",lty=2)
text(8,mean(c(-log10(min(p_m[fdr_m>0.05])),-log10(max(p_m[fdr_m<0.05])))),"FDR=5%",col="Red",pos=1)
text(4,5,"Bonferroni",col="Blue")

#Missing values separately for cases and controls to comply with strobe checklist.
m_case_miss<-apply(is.na(mdata[mdata$Type.Questionnaire=="PATIENT",-c(1:12)]),2,sum)
m_contr_miss<-apply(is.na(mdata[mdata$Type.Questionnaire=="TEMOIN",-c(1:12)]),2,sum)
um_contr_miss<-apply(is.na(umdata[umdata$Type.Questionnaire=="TEMOIN",-c(1:12)]),2,sum)
um_case_miss<-apply(is.na(umdata[umdata$Type.Questionnaire=="PATIENT",-c(1:12)]),2,sum)
miss<-cbind(m_case_miss,m_contr_miss,um_contr_miss,um_case_miss)