#This script is regrpQues.R . It implements the pre analysis treatment that is used for the article Association of environmental 
#markers with childhood type 1 diabetes mellitus revealed by a long questionnaire on early life exposures and 
#lifestyle in a case-control study.

#This codes categorical variables in separate binary variables. 
#It removes a certain amount of missing data mainly by using questions to answer to sub-questions
#Example: X512 Did the mother smoke YES/NO
# X512.1 If yes how much Answer 1 :<15cigs/day or Answer 2: >15cigs/day
#In this case, no mothers who answered no to the first question will respond to the second and therefore we have a lot of missing data.
#If we drop all such sub-questions, then we lose some information. The solution we implent here is to add a new level for the sub-question
#corresponding to the no answer for the first question. Here answer 0.
#It also tries and deals with multiple answers to ordinal questions by fusing the answers upwards or downwards e.g. 2/3 can be transformed into 2 or 3.
#A certain amount of such multiple answers to ordinal questions are left at the head of the script and have to be replaced by NAs using Excel.

#This script is very badly programmed. It is my first attempt ever at code. It is not fast as it uses for loops 
#profusely. It only has to be run once. Please excuse me for the inconvenience.

r<-data.frame(matrix(ncol = 0, nrow = nrow(mb)))#This is wher the result is stored.
s<-c()#Keeps useless columns in memeory for verification purposes.

#3=NA Sometimes a third answer coding for I don't know was proposed.
#This is utterly useless and is just replaced by NA. 
trna<-function(col) {
 m<-subset(mb,select=c(col))
 m[,1]<-factor(m[,1])
 levels(m[,1])<-list("1"=c(1,"1\\3"),"2"=c(2,"2\\3"))
r<<-cbind(r,m)
}
#trna("X10.14")

#Question emboitée simple +3=NA
emb<-function(col1,col2) {
 m<-subset(mb,select=c(col1,col2))
 m[,col1]<-factor(m[,col1])
 m[,col2]<-factor(m[,col2])
 levels(m[,col1])<-list("1"=c(1,"1\\3"),"2"=c(2,"2\\3"))
 levels(m[,col2])<-list("1"=c(1,"1\\3"),"2"=c(2,"2\\3"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,col1])) {
        if (m[i,col1]==2) {m[i,col2]=2}
       }
 }
r<<-cbind(r,m)
}
#emb("X34","X34.2")

#question emboitée min ord 3 niveaux
embminord<-function(col1,col2) {
m<-subset(mb,select=c(col1,col2))
levels(m[,2])<-c(levels(m[,2]),4)
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,col1])) {
        if (m[i,col1]==2) {m[i,col2]=4}
       }
 }
levels(m[,col2])<-list("1"=c(1,"1\\2","1\\3","1\\2\\3"),"2"=c(2,"2\\3"),"3"=c(3),"4"=c(4))
r<<-cbind(r,m)
}
#embminord("X216","X216.1")

#question emboitée max ord 3 niveaux
embmaxord<-function(col1,col2) {
m<-subset(mb,select=c(col1,col2))
m[,col2]<-factor(m[,col2])
levels(m[,2])<-c(levels(m[,2]),0)
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,col1])) {
        if (m[i,col1]==2) {m[i,col2]=0}
       }
 }
levels(m[,col2])<-list("0"=c(0),"1"=c(1),"2"=c(2,"1\\2"),"3"=c(3,"1\\3","2\\3","1\\2\\3"))
r<<-cbind(r,m)
}

#Question emboitée double
embd<-function(col1,col2,col3) {
 m<-subset(mb,select=c(col1,col2,col3))
  for (i in 1:nrow(m)) {
      if (!is.na(m[i,col1])) {
        if (m[i,col1]==2) {m[i,col2]=m[i,col3]=2}
       }
 }
r<<-cbind(r,m)
}
#embd("X122","X122.1","X122.2")

#Question emboitée double avec 3=NA 
embdna<-function(col1,col2,col3) {
 m<-subset(mb,select=c(col1,col2,col3))
  for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=2}
       }
 }
for (j in 1:3) {m[,j]<-factor(m[,j])
  levels(m[,j])<-list("1"=c(1,"1\\3"),"2"=c(2,"2\\3"))   
}

r<<-cbind(r,m)
}



#Min réponse ordinale 6 niveaux 
minord<-function(col){
m<-subset(mb,select=c(col))
m[,1]<-factor(m[,1])
levels(m[,col])<-list("1"=c(1,"1\\2","1\\3","1\\4","1\\5","1\\6","1\\2\\3","1\\2\\4","1\\2\\5",
                            "1\\3\\4","1\\3\\5","1\\4\\5","1\\2\\3\\4","1\\2\\3\\4\\5"),
                      "2"=c(2,"2\\3","2\\4","2\\5","2\\6","2\\3\\4","2\\3\\5","2\\4\\5","2\\3\\4\\5")
                      ,"3"=c(3,"3\\4","3\\5","3\\6","3\\4\\5"),
                      "4"=c(4,"4\\5","4\\6"),
                      "5"=c(5,"5\\6"),"6"=c(6))
r<<-cbind(r,m)
}
#MAx
maxord<-function(col){
m<-subset(mb,select=c(col))
m[,1]<-factor(m[,1])
levels(m[,col])<-list("1"=c(1),"2"=c(2,"1\\2"),
                      "3"=c(3,"2\\3","1\\3","1\\2\\3"),
                      "4"=c(4,"1\\4","2\\4","3\\4","1\\2\\4","2\\3\\4","1\\2\\3\\4"),
                      "5"=c(5,"1\\5","2\\5","3\\5","4\\5","1\\2\\5","1\\3\\5",
                            "1\\4\\5","2\\3\\5","2\\4\\5","3\\4\\5","2\\3\\4\\5","1\\2\\3\\4\\5"),
                      "6"=c(6,"1\\6","2\\6","3\\6","4\\6","5\\6"))
r<<-cbind(r,m)
}

#extrème ordinale
extrord<-function(col){
m<-subset(mb,select=c(col))
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,col])) {
      if (m[i,col]=="1\\2") {m[i,col]=1
     }else if (m[i,col]=="2\\3") {m[i,col]=2
     }else if (m[i,col]=="3\\4") {m[i,col]=4
     }else if (m[i,col]=="4\\5") {m[i,col]=5
     }else if (m[i,col]=="5\\6") {m[i,col]=6
     }
    }
  }
r<<-cbind(r,m)
}

   
# 2 rÃ©ponse catégorie avec première question
x2<-function(col1,col2,name1,name2) {
   m<-subset(mb,select=c(col1,col2)) 
  #table(m[,2])
  #unique(as.numeric(m[,2][!is.na(m[,2])]))
  m$res1<-NA
  m$res2<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,col2])) {
      if (m[i,col2]==1) {m$res1[i]=1; m$res2[i]=2
      } else if (m[i,col2]==2) {m$res2[i]=1; m$res1[i]=2
      } else if (m[i,col2]=="1\\2") {m$res1[i]=m$res2[i]=1 
      } else if (m[i,col2]=="1\\3") {m$res1[i]=1;m$res2[i]=2
      } else if (m[i,col2]=="2\\3") {m$res2[i]=1;m$res1[i]=2
      } else if (m[i,col2]==3) {m$res2[i]=2; m$res1[i]=2
      }
      } else if (!is.na(m[i,col1])) {
        if (m[i,col1]==2) { 
          m$res1[i]=m$res2[i]=2      
        }         
      } 
    
  }
 names(m)<-c(col1,col2,name1,name2) 
  #m[!is.na(col2),]
 r<<-cbind(r,m)
 s<<-c(s,col2)
}

#x2("X81","X81.1")

#2 réponses sans 1ère question
y2<-function(col,name1,name2) {
   m<-subset(mb,select=c(col)) 
  #table(m[,2])
  #unique(as.numeric(m[,2][!is.na(m[,2])]))
  m$res1<-NA
  m$res2<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,col])) {
      if (m[i,col]==1) {m$res1[i]=1; m$res2[i]=2
      } else if (m[i,col]==2) {m$res2[i]=1; m$res1[i]=2
      } else if (m[i,col]=="1\\2") {m$res1[i]=m$res2[i]=1 
      } else if (m[i,col]=="1\\3") {m$res1[i]=1;m$res2[i]=2
      } else if (m[i,col]=="2\\3") {m$res2[i]=1;m$res1[i]=2
      } else if (m[i,col]==3) {m$res2[i]=2; m$res1[i]=2
      }
      } 
    
  }
 names(m)<-c(col,name1,name2) 
 r<<-cbind(r,m)
 s<<-c(s,col)
}


############## 3 rÃ©ponse avec 1ère question
x3<-function(col1,col2,name1,name2,name3) {
  m<-subset(mb,select=c(col1,col2))  
  #table(m[,2])
  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,col2])) {
      if (m[i,col2]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=2
      } else if (m[i,col2]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=2
      } else if (m[i,col2]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=2
      } else if (m[i,col2]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=2                                    
      } else if (m[i,col2]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=2                                     
      } else if (m[i,col2]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=2
      } else if (m[i,col2]=="1\\2\\3") {m$res1[i]=m$res3[i]=m$res2[i]=1
      }
    } else if (!is.na(m[i,col1])) {
      if (m[i,col1]==2) { 
        m$res1[i]=m$res2[i]=m$res3[i]=2        
        }         
    }     
  }
  #m[!is.na(col1),]
  names(m)<-c(col1,col2,name1,name2,name3)
  r<<-cbind(r,m)
  s<<-c(s,col2)
} 

#3 réponse sans première question
y3<-function(col,name1,name2,name3) {
  m<-subset(mb,select=c(col))  
  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,col])) {
        if (m[i,col]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=2
        } else if (m[i,col]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=2
        } else if (m[i,col]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=2
        } else if (m[i,col]==4) {m$res1[i]=m$res2[i]=m$res3[i]=2                          
        } else if (m[i,col]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=2                                    
        } else if (m[i,col]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=2                                     
        } else if (m[i,col]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=2
        } else if (m[i,col]=="1\\4") {m$res1[i]=1; m$res2[i]=m$res3[i]=2
        } else if (m[i,col]=="2\\4") {m$res2[i]=1; m$res1[i]=m$res3[i]=2
        } else if (m[i,col]=="3\\4") {m$res3[i]=1; m$res2[i]=m$res1[i]=2
        } else if (m[i,col]=="1\\2\\3") {m$res1[i]=m$res2[i]=m$res3[i]=1; 2   
        } else if (m[i,col]=="1\\3\\4") {m$res1[i]=m$res3[i]=1; m$res2[i]=2   
        } else if (m[i,col]=="1\\2\\4") {m$res1[i]=m$res2[i]=1; m$res3[i]=2
        } else if (m[i,col]=="2\\3\\4") {m$res2[i]=m$res3[i]=1; m$res1[i]=2                                  
        } else if (m[i,col]=="1\\2\\3\\4") {m$res3[i]=m$res2[i]=m$res1[i]=1                               
        }     
       } 
    }



  names(m)<-c(col,name1,name2,name3)
  r<<-cbind(r,m)
  s<<-c(s,col)
} 







#x3("X207","X207.1")

################ 4 rÃ©ponse
x4<-function(col1,col2,name1,name2,name3,name4) {
  m<-subset(mb,select=c(col1,col2))  
  #table(m[,2])
  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  m$res4<-NA
    
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,col2])) {
        if (m[i,col2]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=m$res4[i]=2
        } else if (m[i,col2]==4) {m$res4[i]=1; m$res1[i]=m$res2[i]=m$res3[i]=2                          
        } else if (m[i,col2]==5) {m$res1[i]=m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]=="1\\5") {m$res1[i]=1; m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]=="2\\5") {m$res2[i]=1; m$res1[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]=="3\\5") {m$res3[i]=1; m$res1[i]=m$res2[i]=m$res4[i]=2
        } else if (m[i,col2]=="4\\5") {m$res4[i]=1; m$res1[i]=m$res2[i]=m$res3[i]=2
        } else if (m[i,col2]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=m$res4[i]=2                                    
        } else if (m[i,col2]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=m$res4[i]=2                                     
        } else if (m[i,col2]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=m$res4[i]=2
        } else if (m[i,col2]=="1\\4") {m$res1[i]=m$res4[i]=1; m$res2[i]=m$res3[i]=2
        } else if (m[i,col2]=="2\\4") {m$res2[i]=m$res4[i]=1; m$res1[i]=m$res3[i]=2
        } else if (m[i,col2]=="3\\4") {m$res4[i]=m$res3[i]=1; m$res2[i]=m$res1[i]=2
        } else if (m[i,col2]=="1\\2\\5") {m$res1[i]=m$res2[i]=1;m$res3[i]=m$res4[i]=2
        } else if (m[i,col2]=="1\\3\\5") {m$res1[i]=m$res3[i]=1;m$res2[i]=m$res4[i]=2
        } else if (m[i,col2]=="1\\4\\5") {m$res1[i]=m$res4[i]=1;m$res2[i]=m$res3[i]=2
        } else if (m[i,col2]=="2\\3\\5") {m$res2[i]=m$res3[i]=1;m$res1[i]=m$res4[i]=2
        } else if (m[i,col2]=="2\\4\\5") {m$res2[i]=m$res4[i]=1;m$res1[i]=m$res3[i]=2
        } else if (m[i,col2]=="3\\4\\5") {m$res3[i]=m$res4[i]=1;m$res1[i]=m$res2[i]=2
        } else if (m[i,col2]=="1\\2\\3") {m$res1[i]=m$res2[i]=m$res3[i]=1; m$res4[i]=2   
        } else if (m[i,col2]=="1\\3\\4") {m$res1[i]=m$res3[i]=m$res4[i]=1; m$res2[i]=2   
        } else if (m[i,col2]=="1\\2\\4") {m$res1[i]=m$res2[i]=m$res4[i]=1; m$res3[i]=2
        } else if (m[i,col2]=="2\\3\\4") {m$res2[i]=m$res3[i]=m$res4[i]=1; m$res1[i]=2                                  
        } else if (m[i,col2]=="1\\2\\3\\4") {m$res4[i]=m$res3[i]=m$res2[i]=m$res1[i]=1                               
        } else if (m[i,col2]=="1\\2\\3\\5") {m$res1[i]=m$res2[i]=m$res3[i]=1; m$res4[i]=2   
        } else if (m[i,col2]=="1\\3\\4\\5") {m$res1[i]=m$res3[i]=m$res4[i]=1; m$res2[i]=2
        } else if (m[i,col2]=="1\\2\\4\\5") {m$res1[i]=m$res2[i]=m$res4[i]=1; m$res3[i]=2
        } else if (m[i,col2]=="2\\3\\4\\5") {m$res2[i]=m$res3[i]=m$res4[i]=1; m$res1[i]=2                                  
        } else if (m[i,col2]=="1\\2\\3\\4\\5") {m$res4[i]=m$res3[i]=m$res2[i]=m$res1[i]=1                               
        }
        
      } else if (!is.na(m[i,col1])) {
          if (m[i,col1]==2) { 
            m$res1[i]=m$res2[i]=m$res3[i]=m$res4[i]=2        
          }         
        } 
    }
    #m[!is.na(col1),]
   names(m)<-c(col1,col2,name1,name2,name3,name4)
  r<<-cbind(r,m)
  s<<-c(s,col2)

}
#x4("X106","X106.1")

#4réponse sans première catégorie
y4<-function(col,name1,name2,name3,name4) {
  m<-subset(mb,select=c(col))  
  #table(m[,2])
  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  m$res4<-NA
    
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,col])) {
        if (m[i,col]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,col]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=m$res4[i]=2
        } else if (m[i,col]==4) {m$res4[i]=1; m$res1[i]=m$res2[i]=m$res3[i]=2                          
        } else if (m[i,col]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=m$res4[i]=2                                    
        } else if (m[i,col]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=m$res4[i]=2                                     
        } else if (m[i,col]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=m$res4[i]=2
        } else if (m[i,col]=="1\\4") {m$res1[i]=m$res4[i]=1; m$res2[i]=m$res3[i]=2
        } else if (m[i,col]=="2\\4") {m$res2[i]=m$res4[i]=1; m$res1[i]=m$res3[i]=2
        } else if (m[i,col]=="3\\4") {m$res4[i]=m$res3[i]=1; m$res2[i]=m$res1[i]=2
        } else if (m[i,col]=="1\\2\\3") {m$res1[i]=m$res2[i]=m$res3[i]=1; m$res4[i]=2   
        } else if (m[i,col]=="1\\3\\4") {m$res1[i]=m$res3[i]=m$res4[i]=1; m$res2[i]=2   
        } else if (m[i,col]=="1\\2\\4") {m$res1[i]=m$res2[i]=m$res4[i]=1; m$res3[i]=2
        } else if (m[i,col]=="2\\3\\4") {m$res2[i]=m$res3[i]=m$res4[i]=1; m$res1[i]=2                                  
        } else if (m[i,col]=="1\\2\\3\\4") {m$res4[i]=m$res3[i]=m$res2[i]=m$res1[i]=1                               
        }     
       } 
    }

    #m[!is.na(col1),]
   names(m)<-c(col,name1,name2,name3,name4)
  r<<-cbind(r,m)
  s<<-c(s,col)

}

#Début on ne prend pas les questions de structure du genre X2

r<-subset(mb,select=c("X2.5.1","X9.1","X10.12.1","X10.12.3","X10.12.4.1"))
trna("X10.14")
trna("X10.15")
trna("X10.16")
trna("X10.17")
r<-cbind(r,subset(mb,select=c("X10.18.1","X10.19.1","X10.20.1","X10.21.1")))
trna("X10.23")
trna("X10.24")
trna("X10.25")
trna("X10.26")
trna("X10.27")
r<-cbind(r,subset(mb,select=c("X10.29.1","X15.1","X16.1","X17","X18","X30")))
trna("X31")
trna("X32")
trna("X33")
emb("X34","X34.2")
trna("X35")
trna("X36")
trna("X37")
#Pour X38.1, on compte le nombre d'intervention chirurgicale et d'anesthésie générale
m<-subset(mb,select=c("X38","X38.1"))
m$X38chirurgie<-NA
m$X38anesthésie<-NA
for (i in 1: nrow(m)) {
    if (!is.na(m[i,"X38.1"])) {
      if (m[i,"X38.1"]==2) {m$X38chirurgie[i]=m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]=="2\\5") {m$X38chirurgie[i]=m$X38anesthésie[i]=2
      } else if (m[i,"X38.1"]=="2\\5\\8") {m$X38chirurgie[i]=m$X38anesthésie[i]=3 
      } else if (m[i,"X38.1"]=="2\\5\\9") {m$X38chirurgie[i]=3;m$X38anesthésie[i]=2
      } else if (m[i,"X38.1"]=="2\\6") {m$X38chirurgie[i]=2;m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]=="2\\6\\8") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=2
      } else if (m[i,"X38.1"]=="2\\6\\9") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]==3) {m$X38chirurgie[i]=1; m$X38anesthésie[i]=0
      } else if (m[i,"X38.1"]=="3\\5") {m$X38chirurgie[i]=2; m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]=="3\\5\\8") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=2
      } else if (m[i,"X38.1"]=="3\\5\\9") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]=="3\\6") {m$X38chirurgie[i]=2; m$X38anesthésie[i]=0
      } else if (m[i,"X38.1"]=="3\\6\\8") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=1
      } else if (m[i,"X38.1"]=="3\\6\\9") {m$X38chirurgie[i]=3; m$X38anesthésie[i]=0
      } 
     } else if (!is.na(m[i,"X38"])) {
        if (m[i,"X38"]==2) { 
          m$X38chirurgie[i]=m$X38anesthésie[i]=0      
        }         
      }
   } 
r<-cbind(r,m)
s<-c(s,"X38.1")
rm(m)
r<-cbind(r,subset(mb,select=c("X39","X44","X45","X46")))
trna("X47")
trna("X48")
trna("X49")
trna("X50")
trna("X51")
trna("X52")
r<-cbind(r,subset(mb,select=c("X53","X54.1","X55.1","X56","X57","X58","X59","X60","X61","X62","X63")))

maxord("X64")
maxord("X65")
r<-cbind(r,subset(mb,select=c("X66","X67","X68","X69","X70","X71","X72","X73","X74")))

emb("X75","X75.2")
r<-cbind(r,subset(mb,select=c("X76","X77","X78","X79","X80")))
x2("X81","X81.1","X81.1 pilule","X81.1 stérilet")
levels(mb[,"X82"])<-c(levels(mb[,"X82"]),1)#PB sinon 1 n'est pas level de X82
minord("X82")
r<-cbind(r,subset(mb,select=c("X83","X84","X85","X86","X87","X88","X89","X90","X91","X92","X93")))
minord("X94")
r<-cbind(r,subset(mb,select=c("X95","X96","X97")))
extrord("X98")
r<-cbind(r,subset(mb,select=c("X99","X100","X101")))
m<-subset(mb,select=c("X102","X102.1"))
levels(m[,2])<-c(levels(m[,2]),5)
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=5}
       }
 }
for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X102.1"])) {
      if (m[i,"X102.1"]=="1\\2") {m[i,"X102.1"]=1
     }else if (m[i,"X102.1"]=="1\\3") {m[i,"X102.1"]=1
     }else if (m[i,"X102.1"]=="1\\4") {m[i,"X102.1"]=1
     }else if (m[i,"X102.1"]=="2\\3") {m[i,"X102.1"]=2
     }else if (m[i,"X102.1"]=="2\\4") {m[i,"X102.1"]=2
     }else if (m[i,"X102.1"]=="3\\4") {m[i,"X102.1"]=3
     }
    }
  }

r<-cbind(r,m)
rm(m)
r<-cbind(r,subset(mb,select=c("X103","X104","X105")))
 x4("X106","X106.1","crème","rasage","epilation","epilateur")
r<-cbind(r,subset(mb,select=c("X107")))
#cigarettes pas fumer 0
m<-subset(mb,select=c("X108","X108.1"))
levels(m[,2])<-c(levels(m[,2]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=0}
       }
 }
r<-cbind(r,m)
rm(m)
r<-cbind(r,subset(mb,select=c("X109","X110","X111","X112","X113","X114")))

m<-subset(mb,select=c("X115","X115.1","X115.2","X115.3","X115.4","X115.5","X115.6"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=2}
       }
 }
r<-cbind(r,m)
rm(m)
r<-cbind(r,subset(mb,select=c("X116","X117","X118")))

m<-subset(mb,select=c("X119","X119.1","X119.2","X119.3","X119.4","X119.5","X119.5.1","X119.5.2","X119.6"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X119"])) {
        if (m[i,"X119"]==2) {m[i,"X119.1"]=m[i,"X119.2"]=m[i,"X119.3"]=m[i,"X119.4"]=m[i,"X119.5"]=m[i,"X119.6"]=2}
       }
 }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X119.5"])) {
        if (m[i,"X119.5"]==2) {m[i,"X119.5.1"]=2}
       }
 }

  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,"X119.5.2"])) {
      if (m[i,"X119.5.2"]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=2
      } else if (m[i,"X119.5.2"]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=2
      } else if (m[i,"X119.5.2"]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=2
      } else if (m[i,"X119.5.2"]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=2                                    
      } else if (m[i,"X119.5.2"]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=2                                     
      } else if (m[i,"X119.5.2"]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=2
      } else if (m[i,"X119.5.2"]=="1\\2\\3") {m$res1[i]=m$res3[i]=m$res2[i]=1
      }
    } else if (!is.na(m[i,"X119.5"])) {
      if (m[i,"X119.5"]==2) { 
        m$res1[i]=m$res2[i]=m$res3[i]=2        
        }         
    }     
  }
names(m)<-c("X119","X119.1","X119.2","X119.3","X119.4","X119.5","X119.5.1","X119.5.2","X119.6","X119.5.2viande","X119.5.2croquette","X119.5.2boite")
r<-cbind(r,m)
s<-c(s,"X119.5.2")
r<-cbind(r,subset(mb,select=c("X120")))
y2("X121","X121cabinet","X121hopital")
embd("X122","X122.1","X122.2")
r<-cbind(r,subset(mb,select=c("X123","X124","X125")))

m<-subset(mb,select=c("X126","X126.1","X126.1.1"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X126"])) {
        if (m[i,"X126"]==2) {m[i,"X126.1"]=2}
       }
 }
  m$res1<-NA
  m$res2<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,"X126.1.1"])) {
      if (m[i,"X126.1.1"]==1) {m$res1[i]=1; m$res2[i]=2
      } else if (m[i,"X126.1.1"]==2) {m$res2[i]=1; m$res1[i]=2
      } else if (m[i,"X126.1.1"]=="1\\2") {m$res1[i]=m$res2[i]=1 
      }
      } else if (!is.na(m[i,"X126.1"])) {
        if (m[i,"X126.1"]==2) { 
          m$res1[i]=m$res2[i]=2      
        }         
      } 
    
  }
 names(m)<-c("X126","X126.1","X126.1.1","X126.1.1creme","X126.1.1ovule") 
  r<-cbind(r,m)
 s<-c(s,"X126.1.1")
r<-cbind(r,subset(mb,select=c("X127","X128","X129","X130","X131","X132","X133","X134","X135","X136")))

x4("X137","X137.1","X137printemps","X137été","X137automne","X137hiver")
r<-cbind(r,subset(mb,select=c("X138")))
emb("X139","X139.1")
r<-cbind(r,subset(mb,select=c("X140")))
emb("X141","X141.1")
r<-cbind(r,subset(mb,select=c("X142","X143","X144","X145","X146","X147")))
emb("X148","X148.1")
r<-cbind(r,subset(mb,select=c("X149","X150","X151","X152","X153","X154","X155","X156","X157")))
emb("X158","X158.1")
r<-cbind(r,subset(mb,select=c("X159")))
emb("X160","X160.1")
r<-cbind(r,subset(mb,select=c("X161","X162")))
trna("X163")

#X164 est x2 mais avec le rôle inversé entre 1 et 2 pour la première colonne
   m<-subset(mb,select=c("X164","X164.1")) 
  m$res1<-NA
  m$res2<-NA
  
  for (i in 1: nrow(m)) {
    if (!is.na(m[i,"X164.1"])) {
      if (m[i,"X164.1"]==1) {m$res1[i]=1; m$res2[i]=2
      } else if (m[i,"X164.1"]==2) {m$res2[i]=1; m$res1[i]=2
      } else if (m[i,"X164.1"]=="1\\2") {m$res1[i]=m$res2[i]=1 
      } else if (m[i,"X164.1"]=="1\\3") {m$res1[i]=1;m$res2[i]=2
      } else if (m[i,"X164.1"]=="2\\3") {m$res2[i]=1;m$res1[i]=2
      } else if (m[i,"X164.1"]==3) {m$res2[i]=2; m$res1[i]=2
      }
      } else if (!is.na(m[i,"X164"])) {
        if (m[i,"X164"]==1) { 
          m$res1[i]=m$res2[i]=2      
        }         
      } 
    
  }
 names(m)<-c("X164","X164.1","X164.1ocytocine","X164.1prostaglandines") 
 r<<-cbind(r,m)
 s<<-c(s,"X164.1")
trna("X165")
x2("X166","X166.1","X166forceps","X166spontané")
x2("X167","X167.1","X167vraijumeau","X167fauxjumeau")
r<-cbind(r,subset(mb,select=c("X168","X169")))
trna("X170")
trna("X171")
trna("X172")
trna("X173")
trna("X174")

m<-subset(mb,select=c("X175","X175.1","X175.2","X175.3","X175.4","X175.5","X175.6","X175.6.1"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X175"])) {
        if (m[i,"X175"]==2) {m[i,"X175.1"]=m[i,"X175.2"]=m[i,"X175.3"]=m[i,"X175.4"]=m[i,"X175.5"]=m[i,"X175.6"]=2}
       }
 }
levels(m[,"X175.6.1"])<-c(levels(m[,"X175.6.1"]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X175.6"])) {
        if (m[i,"X175.6"]==2) {m[i,"X175.6.1"]=0}
       }
 }
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X175.6.1"])) {
      if (m[i,"X175.6.1"]=="1\\2") {m[i,"X175.6.1"]=2
     }else if (m[i,"X175.6.1"]=="2\\3") {m[i,"X175.6.1"]=3
     }else if (m[i,"X175.6.1"]=="3\\4") {m[i,"X175.6.1"]=4}
    }
  }
r<-cbind(r,m)

m<-subset(mb,select=c("X176","X176.1","X176.2","X176.2.1","X176.2.1.1"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X176"])) {
        if (m[i,"X176"]==2) {m[i,"X176.1"]=m[i,"X176.2"]=2}
       }
 }
  for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X176.2"])) {
        if (m[i,"X176.2"]==3) {m[i,"X176.2"]<-NA
        }else if(m[i,"X176.2"]=="1\\3") {m[i,"X176.2"]=1
        }else if (m[i,"X176.2"]=="2\\3") {m[i,"X176.2"]=2
        }

      }
  }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X176.2"])) {
        if (m[i,"X176.2"]==2) {m[i,"X176.2.1"]=2}
       }
 }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X176.2.1"])) {
        if (m[i,"X176.2.1"]==2) {m[i,"X176.2.1.1"]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X177","X178","X179","X180","X181","X182","X183","X184","X185")))
embdna("X186","X186.1","X186.2")
r<-cbind(r,subset(mb,select=c("X187","X188")))
trna("X189")
trna("X190")
trna("X191")
trna("X192")
trna("X193")
trna("X194")
trna("X195")
trna("X196")
trna("X197")
embd("X198","X198.1","X198.2")
trna("X199")
r<-cbind(r,subset(mb,select=c("X200","X201","X202","X203","X204","X205","X206")))
x2("X207","X207.1","X207eczema","X207boutons")
r<-cbind(r,subset(mb,select=c("X208","X209","X210","X211")))
emb("X212","X212.1")
r<-cbind(r,subset(mb,select=c("X213","X214")))
minord("X215")
embminord("X216","X216.1")
embminord("X217","X217.1")
embminord("X218","X218.1")
r<-cbind(r,subset(mb,select=c("X219")))
minord("X220")
r<-cbind(r,subset(mb,select=c("X221")))
embd("X222","X222.1","X222.2")
minord("X223")
minord("X224")
minord("X225")
minord("X226")
minord("X227")
minord("X228")
minord("X229")
minord("X230")
minord("X231")
minord("X232")
minord("X233")
minord("X234")
r<-cbind(r,subset(mb,select=c("X235","X236")))
minord("X237")
embmaxord("X238","X238.1")
maxord("X239")

r<-cbind(r,subset(mb,select=c("X240","X241")))

m<-subset(mb,select=c("X242"))
levels(m[,1])<-c(levels(m[,1]),4)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]=="1\\3") {m[i,1]=1}
        else if (m[i,1]=="2\\3") {m[i,1]=2}
      }
  }

 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==3) {m[i,1]=4}
      }
  }
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,1]=3}
      }
  }
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==4) {m[i,1]=2}
      }
  }
r<-cbind(r,m)

m<-subset(mb,select=c("X243"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==4) {m[i,1]<-NA
        }
      }
}
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,1]=1
        }
      }
}
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==3) {m[i,1]=2
        } else if (m[i,1]=="1\\2") {m[i,1]=1
        } else if (m[i,1]=="1\\4") {m[i,1]=1
        } else if (m[i,1]=="1\\3") {m[i,1]<-NA
        } else if (m[i,1]=="2\\4") {m[i,1]=1
        } else if (m[i,1]=="2\\3") {m[i,1]<-NA
        } else if (m[i,1]=="3\\4") {m[i,1]=3
        }
      }
}
r<-cbind(r,m)
trna("X244")
trna("X245")
trna("X246")
trna("X247")
trna("X248")
trna("X249")
trna("X250")
trna("X251")
trna("X252")
trna("X253")
trna("X254")
trna("X255")
trna("X256")
trna("X257")
trna("X258")
trna("X259")

m<-subset(mb,select=c("X260","X260.1","X260.2","X260.3","X260.4"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=2}
       }
 }
for (j in 2:5) {
   for (i in 1:nrow(m)) {
      if (!is.na(m[i,j])) {
        if (m[i,j]==3) {m[i,j]<-NA
        }else if (m[i,j]=="1\\3") {m[i,j]=1
        }else if (m[i,j]=="2\\3") {m[i,j]=2
        }
      }
    }
}
r<-cbind(r,m)

embdna("X261","X261.1","X261.2")
embdna("X262","X262.1","X262.2")
x2("X263","X263.1","X263chat","X263chien")
r<-cbind(r,subset(mb,select=c("X264")))
emb("X265","X265.1")
r<-cbind(r,subset(mb,select=c("X266","X267","X268")))
x4("X269","X269.1","X269guepe","X269abeille","X269insecte","X269poisson")
r<-cbind(r,subset(mb,select=c("X270","X271")))
trna("X272")
trna("X273")
trna("X274")
m<-subset(mb,select=c("X275"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==4) {m[i,1]<-NA
        }else if (m[i,1]=="1\\4") {m[i,1]=1
        }else if (m[i,1]=="1\\3") {m[i,1]=3
        }else if (m[i,1]=="1\\2") {m[i,1]=2
        }else if (m[i,1]=="2\\3") {m[i,1]=3
        }else if (m[i,1]=="2\\4") {m[i,1]=2
        }else if (m[i,1]=="3\\4") {m[i,1]=3
        }
      }
}

r<-cbind(r,m)
emb("X276","X276.1")
trna("X277")
trna("X278")
trna("X279")
trna("X280")
trna("X281")
embdna("X282","X282.1","X282.2")
trna("X283")
trna("X284")
trna("X285")
trna("X286")
trna("X287")
trna("X288")
trna("X289")
trna("X290")
trna("X291")
trna("X292")
trna("X293")
trna("X294")

#circoncision pour les filles = non
m<-subset(mb,select=c("X295"))
  for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==3) {m[i,1]=2
        }else if (m[i,1]=="2\\3") {m[i,1]=2
        }
      }
  }
r<-cbind(r,m)
x4("X296","X296.1","X296printemps","X296été","X296automne","X296hiver")
maxord("X297")
maxord("X298")
m<-subset(mb,select=c("X299"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==4) {m[i,1]<-NA
        }else if (m[i,1]=="2\\3") {m[i,1]=2
        }else if (m[i,1]=="1\\2") {m[i,1]=1
        }
       }
}

r<-cbind(r,m)
trna("X300")
trna("X301")
trna("X302")
trna("X303")
trna("X304")
trna("X305")
trna("X306")
trna("X307")
trna("X308")
trna("X309")
trna("X310")
trna("X311")
trna("X312")

m<-subset(mb,select=c("X313","X314","X314.1","X314.2","X315","X315.1","X315.2"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==5) {m[i,2]=m[i,5]=5}
       }
 }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,2])) {
        if (m[i,2]==5) {m[i,3]=m[i,4]=2}
       }
 }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,5])) {
        if (m[i,5]==5) {m[i,6]=m[i,7]=2}
       }
 }
for (j in c(1,2,5)) {
  levels(m[,j])<-list("1"=c(1,"1\\2","1\\3","1\\4","1\\5","1\\6","1\\2\\3","1\\2\\4","1\\2\\5",
                              "1\\3\\4","1\\3\\5","1\\4\\5","1\\2\\3\\4","1\\2\\3\\4\\5"),
                        "2"=c(2,"2\\3","2\\4","2\\5","2\\6","2\\3\\4","2\\3\\5","2\\4\\5","2\\3\\4\\5")
                        ,"3"=c(3,"3\\4","3\\5","3\\6","3\\4\\5"),
                        "4"=c(4,"4\\5"),
                        "5"=c(5))
}
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X316")))
minord("X317")
minord("X318")
minord("X319")
minord("X320")
r<-cbind(r,subset(mb,select=c("X321","X322")))
minord("X323")
minord("X324")
r<-cbind(r,subset(mb,select=c("X325","X326","X327","X328","X329","X330","X331")))

m<-subset(mb,select=c("X332","X332.1","X332.2","X332.3"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X333","X334","X335")))
minord("X336")
minord("X337")
minord("X338")
minord("X339")
minord("X340")
minord("X341")
minord("X342")
r<-cbind(r,subset(mb,select=c("X343","X344","X345","X346","X347","X348","X349")))
minord("X350")
minord("X351")
minord("X352")
minord("X353")
minord("X354")
r<-cbind(r,subset(mb,select=c("X355","X356","X357")))
m<-subset(mb,select=c("X358","X358.1","X358.2","X358.3","X358.4"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X359","X360","X361")))
minord("X362")
minord("X363")#Je considère que le fait qu'il y ait un chauffage individuel 
#est plus informatif (plus de variations?) 
# pas de questions préliminaire, on ne peut pas faire x4
m<-subset(mb,select=c("X364"))
  m$res1<-NA
  m$res2<-NA
  m$res3<-NA
  m$res4<-NA
    
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==1) {m$res1[i]=1; m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,1]==2) {m$res2[i]=1; m$res1[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,1]==3) {m$res3[i]=1; m$res1[i]=m$res2[i]=m$res4[i]=2
        } else if (m[i,1]==4) {m$res4[i]=1; m$res1[i]=m$res2[i]=m$res3[i]=2                          
        } else if (m[i,1]==5) {m$res1[i]=m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,1]=="1\\5") {m$res1[i]=1; m$res2[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,1]=="2\\5") {m$res2[i]=1; m$res1[i]=m$res3[i]=m$res4[i]=2
        } else if (m[i,1]=="3\\5") {m$res3[i]=1; m$res1[i]=m$res2[i]=m$res4[i]=2
        } else if (m[i,1]=="4\\5") {m$res4[i]=1; m$res1[i]=m$res2[i]=m$res3[i]=2
        } else if (m[i,1]=="1\\2") {m$res1[i]=m$res2[i]=1; m$res3[i]=m$res4[i]=2                                    
        } else if (m[i,1]=="2\\3") {m$res2[i]=m$res3[i]=1; m$res1[i]=m$res4[i]=2                                     
        } else if (m[i,1]=="1\\3") {m$res1[i]=m$res3[i]=1; m$res2[i]=m$res4[i]=2
        } else if (m[i,1]=="1\\4") {m$res1[i]=m$res4[i]=1; m$res2[i]=m$res3[i]=2
        } else if (m[i,1]=="2\\4") {m$res2[i]=m$res4[i]=1; m$res1[i]=m$res3[i]=2
        } else if (m[i,1]=="3\\4") {m$res4[i]=m$res3[i]=1; m$res2[i]=m$res1[i]=2
        } else if (m[i,1]=="1\\2\\5") {m$res1[i]=m$res2[i]=1;m$res3[i]=m$res4[i]=2
        } else if (m[i,1]=="1\\3\\5") {m$res1[i]=m$res3[i]=1;m$res2[i]=m$res4[i]=2
        } else if (m[i,1]=="1\\4\\5") {m$res1[i]=m$res4[i]=1;m$res2[i]=m$res3[i]=2
        } else if (m[i,1]=="2\\3\\5") {m$res2[i]=m$res3[i]=1;m$res1[i]=m$res4[i]=2
        } else if (m[i,1]=="2\\4\\5") {m$res2[i]=m$res4[i]=1;m$res1[i]=m$res3[i]=2
        } else if (m[i,1]=="3\\4\\5") {m$res3[i]=m$res4[i]=1;m$res1[i]=m$res2[i]=2
        } else if (m[i,1]=="1\\2\\3") {m$res1[i]=m$res2[i]=m$res3[i]=1; m$res4[i]=2   
        } else if (m[i,1]=="1\\3\\4") {m$res1[i]=m$res3[i]=m$res4[i]=1; m$res2[i]=2   
        } else if (m[i,1]=="1\\2\\4") {m$res1[i]=m$res2[i]=m$res4[i]=1; m$res3[i]=2
        } else if (m[i,1]=="2\\3\\4") {m$res2[i]=m$res3[i]=m$res4[i]=1; m$res1[i]=2                                  
        } else if (m[i,1]=="1\\2\\3\\4") {m$res4[i]=m$res3[i]=m$res2[i]=m$res1[i]=1                               
        } else if (m[i,1]=="1\\2\\3\\5") {m$res1[i]=m$res2[i]=m$res3[i]=1; m$res4[i]=2   
        } else if (m[i,1]=="1\\3\\4\\5") {m$res1[i]=m$res3[i]=m$res4[i]=1; m$res2[i]=2
        } else if (m[i,1]=="1\\2\\4\\5") {m$res1[i]=m$res2[i]=m$res4[i]=1; m$res3[i]=2
        } else if (m[i,1]=="2\\3\\4\\5") {m$res2[i]=m$res3[i]=m$res4[i]=1; m$res1[i]=2                                  
        } else if (m[i,1]=="1\\2\\3\\4\\5") {m$res4[i]=m$res3[i]=m$res2[i]=m$res1[i]=1                               
        }
        
      } 
    }
   
   names(m)<-c("X364","X364élec","X364gaz","X364fioul","X364cheminée")
  r<-cbind(r,m)
  s<-c(s,"X364")
r<-cbind(r,subset(mb,select=c("X365","X366","X367","X368","X369","X370")))
# aucune caravane dans les données ou presque
m<-subset(mb,select=c("X371","X371.1","X371.2","X371.3"))
levels(m[,"X371"])<-list("1"=c(1,"1\\3"),"2"=c(2,"2\\3"))
m$X371etage<-NA
m$X371.1<-factor(m$X371.1)
m$X371.3<-factor(m$X371.3)
for (j in c("X371.1","X371.3")) {
  levels(m[,j])<-list("1"=c(1,"1\\2","1\\3","1\\4","1\\2\\3","1\\2\\4",
                              "1\\3\\4","1\\2\\3\\4"),
                        "2"=c(2,"2\\3","2\\4","2\\3\\4")
                        ,"3"=c(3,"3\\4"),
                        "4"=c(4))
}


for (j in c("X371.1","X371.3")) {
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,j])) {m[i,"X371etage"]=m[i,j]
       }
 }
}
for (i in 1:nrow(m)) {
   if (!is.na(m[i,1])) {
      if (m[i,1]==2) {m[i,3]=2}
    }
  }

r<-cbind(r,m)
s<-c(s,"X371.1","X371.3")
#Attention parfois le fichier excel a "X372.1.1" au lieu de "X372.10"
m<-subset(mb,select=c("X372","X372.1","X372.2","X372.3","X372.4","X372.5","X372.6","X372.7","X372.8","X372.9","X372.10","X372.11","X372.12"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {for (j in c(1:4,6:13)) {m[i,j]=2}; m[i,5]=5}
       }
 }
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,5])) {
      if (m[i,5]=="1\\2") {m[i,5]=1
     }else if (m[i,5]=="1\\3") {m[i,5]=1
     }else if (m[i,5]=="1\\4") {m[i,5]=1
     }else if (m[i,5]=="1\\5") {m[i,5]=1
     }else if (m[i,5]=="2\\3") {m[i,5]=2
     }else if (m[i,5]=="2\\4") {m[i,5]=2
     }else if (m[i,5]=="2\\5") {m[i,5]=2
     }else if (m[i,5]=="3\\4") {m[i,5]=3
     }else if (m[i,5]=="3\\5") {m[i,5]=3
     }else if (m[i,5]=="4\\5") {m[i,5]=4
     }
    }
  }
r<-cbind(r,m,subset(mb,select=c("X373")))
y3("X374","X374moquette","X374carrelage","X374parquet")
r<-cbind(r,subset(mb,select=c("X375","X376","X377","X378","X379","X380")))
y3("X381","X381rue","X381jardin","X381cour")
y4("X382","X382O","X382E","X382S","X382N")
r<-cbind(r,subset(mb,select=c("X383","X384","X385","X386","X387","X388","X389","X390","X391")))
maxord("X392")
r<-cbind(r,subset(mb,select=c("X393","X394","X395","X396")))
m<-subset(mb,select=c("X397","X397.1","X397.2","X397.3","X397.4","X397.5","X397.6"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=2}
       }
 }
for (j in 2:7) {
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,j])) {
        if (m[i,j]==3) {m[i,j]<-NA
        }else if(m[i,j]=="1\\3") {m[i,j]=1
        }else if (m[i,j]=="2\\3") {m[i,j]=2
        }
      }
  }
}
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X398","X399","X400")))
#Si l'enfant n'avait pas de nourrice, je mets les réponses de la maison à la place
m<-subset(mb,select=c("X401","X401.1","X401.2","X401.3","X401.4","X401.5","X401.6","X401.7",
"X401.8","X401.9","X401.10","X401.11","X401.11.1","X401.11.2","X401.11.3","X401.11.4","X401.11.5","X401.11.6","X401.11.7",
"X401.12","X401.12.1","X401.12.2","X401.12.3","X401.12.4","X401.12.5","X401.12.6","X401.12.7",
"X401.12.8","X401.12.8.1","X401.12.8.2","X401.12.9","X401.12.9.1","X401.12.9.2","X401.12.9.3","X401.12.9.4","X401.12.9.5"))
n<-subset(mb,select=c("X362","X373","X374","X381","X383","X384","X371","X371.1",
"X371.3","X372","X372.1","X372.2","X372.3","X372.4","X372.12"))
m$X401.11creche<-NA
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=m[i,9]=m[i,10]=m[i,11]=m[i,12]=m[i,20]=2}
       }
}
for (i in 1:nrow(m)) {
      if (!is.na(m[i,12])) {
        if (m[i,12]==2) {m[i,13]=m[i,14]=m[i,15]=m[i,16]=m[i,17]=m[i,18]=m[i,19]=2;
                       m[i,"X401.11creche"]=5}
       }
}
for (j in seq(16,13,-1)) {
  for (i in 1:nrow(m)) {
     if (!is.na(m[i,j])) {
        if (m[i,j]==1) {m[i,"X401.11creche"]=j-12}
       }
  }
}
s<-c(s,"X401.11.1","X401.11.2","X401.11.3","X401.11.4")

levels(m[,"X401.12.2"])<-c(levels(m[i,"X401.12.2"]),levels(n[i,"X362"]))
levels(m[,"X401.12.3"])<-c(levels(m[i,"X401.12.3"]),levels(n[i,"X373"]))
levels(m[,"X401.12.4"])<-c(levels(m[i,"X401.12.4"]),levels(n[i,"X374"]))
levels(m[,"X401.12.5"])<-c(levels(m[i,"X401.12.5"]),levels(n[i,"X381"]))
levels(m[,"X401.12.6"])<-c(levels(m[i,"X401.12.6"]),levels(n[i,"X383"]))
levels(m[,"X401.12.7"])<-c(levels(m[i,"X401.12.7"]),levels(n[i,"X384"]))
levels(m[,"X401.12.8"])<-c(levels(m[i,"X401.12.8"]),levels(n[i,"X371"]))
levels(m[,"X401.12.8.1"])<-c(levels(m[i,"X401.12.8.1"]),levels(n[i,"X371.1"]))
levels(m[,"X401.12.8.2"])<-c(levels(m[i,"X401.12.8.2"]),levels(n[i,"X371.3"]))
levels(m[,"X401.12.9"])<-c(levels(m[i,"X401.12.9"]),levels(n[i,"X372"]))

for (i in 1:nrow(m)) {
      if (!is.na(m[i,20])) {
        if (m[i,20]==2) {m[i,"X401.12.1"]=2; m[i,"X401.12.2"]=n[i,"X362"]; m[i,"X401.12.3"]=n[i,"X373"]; m[i,"X401.12.4"]=n[i,"X374"];
             m[i,"X401.12.5"]=n[i,"X381"]; m[i,"X401.12.6"]=n[i,"X383"]; m[i,"X401.12.7"]=n[i,"X384"]; m[i,"X401.12.8"]=n[i,"X371"]; 
             m[i,"X401.12.8.1"]=n[i,"X371.1"]; m[i,"X401.12.8.2"]=n[i,"X371.3"]; m[i,"X401.12.9"]=n[i,"X372"]          
         }
       }
}
m$X401.12.8etage<-NA

for (j in c("X401.12.8.1","X401.12.8.2")) {
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,j])) {
      if (m[i,j]=="1\\2") {m[i,j]=1
     }else if (m[i,j]=="2\\3") {m[i,j]=2
     }else if (m[i,j]=="1\\3") {m[i,j]=1
     }else if (m[i,j]=="3\\4") {m[i,j]=3
     }else if (m[i,j]=="2\\3\\4") {m[i,j]=2
     }
    }
  }
}
m$X401.12.8.1<-factor(m$X401.12.8.1)
m$X401.12.8.2<-factor(m$X401.12.8.2)

for (j in c("X401.12.8.1","X401.12.8.2")) {
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,j])) {m[i,"X401.12.8etage"]=m[i,j]
       }
 }
}

for (i in 1:nrow(m)) {
      if (!is.na(m[i,"X401.12.9"])) {
        if (m[i,"X401.12.9"]==2) {m[i,"X401.12.9.1"]=n[i,"X372.1"]; m[i,"X401.12.9.2"]=n[i,"X372.2"]; m[i,"X401.12.9.3"]=n[i,"X372.3"];
             m[i,"X401.12.9.4"]=n[i,"X372.4"]; m[i,"X401.12.9.5"]=n[i,"X372.12"]
         }
       }
}
for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X401.12.2"])) {
      if (m[i,"X401.12.2"]=="1\\2") {m[i,"X401.12.2"]=1
     }else if (m[i,"X401.12.2"]=="1\\3") {m[i,"X401.12.2"]=1
     }else if (m[i,"X401.12.2"]=="1\\4") {m[i,"X401.12.2"]=1
     }else if (m[i,"X401.12.2"]=="1\\5") {m[i,"X401.12.2"]=1
     }else if (m[i,"X401.12.2"]=="1\\6") {m[i,"X401.12.2"]=1
     }else if (m[i,"X401.12.2"]=="2\\3") {m[i,"X401.12.2"]=2
     }else if (m[i,"X401.12.2"]=="2\\4") {m[i,"X401.12.2"]=2
     }else if (m[i,"X401.12.2"]=="2\\5") {m[i,"X401.12.2"]=2
     }else if (m[i,"X401.12.2"]=="2\\6") {m[i,"X401.12.2"]=2
     }else if (m[i,"X401.12.2"]=="3\\4") {m[i,"X401.12.2"]=3
     }else if (m[i,"X401.12.2"]=="3\\5") {m[i,"X401.12.2"]=3
     }else if (m[i,"X401.12.2"]=="3\\6") {m[i,"X401.12.2"]=3
     }else if (m[i,"X401.12.2"]=="4\\5") {m[i,"X401.12.2"]=4
     }else if (m[i,"X401.12.2"]=="4\\6") {m[i,"X401.12.2"]=4
     }else if (m[i,"X401.12.2"]=="5\\6") {m[i,"X401.12.2"]=5
     }
    }
  }
  m$"X401.12.4moquette"<-NA
  m$"X401.12.4carrelage"<-NA
  m$"X401.12.4parquet"<-NA
  
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,"X401.12.4"])) {
        if (m[i,"X401.12.4"]==1) {m$"X401.12.4moquette"[i]=1; m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=2
        } else if (m[i,"X401.12.4"]==2) {m$"X401.12.4carrelage"[i]=1; m$"X401.12.4moquette"[i]=m$"X401.12.4parquet"[i]=2
        } else if (m[i,"X401.12.4"]==3) {m$"X401.12.4parquet"[i]=1; m$"X401.12.4moquette"[i]=m$"X401.12.4carrelage"[i]=2
        } else if (m[i,"X401.12.4"]==4) {m$"X401.12.4moquette"[i]=m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=2                          
        } else if (m[i,"X401.12.4"]=="1\\2") {m$"X401.12.4moquette"[i]=m$"X401.12.4carrelage"[i]=1; m$"X401.12.4parquet"[i]=2                                    
        } else if (m[i,"X401.12.4"]=="2\\3") {m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=1; m$"X401.12.4moquette"[i]=2                                     
        } else if (m[i,"X401.12.4"]=="1\\3") {m$"X401.12.4moquette"[i]=m$"X401.12.4parquet"[i]=1; m$"X401.12.4carrelage"[i]=2
        } else if (m[i,"X401.12.4"]=="1\\4") {m$"X401.12.4moquette"[i]=1; m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=2
        } else if (m[i,"X401.12.4"]=="2\\4") {m$"X401.12.4carrelage"[i]=1; m$"X401.12.4moquette"[i]=m$"X401.12.4parquet"[i]=2
        } else if (m[i,"X401.12.4"]=="3\\4") {m$"X401.12.4parquet"[i]=1; m$"X401.12.4carrelage"[i]=m$"X401.12.4moquette"[i]=2
        } else if (m[i,"X401.12.4"]=="1\\2\\3") {m$"X401.12.4moquette"[i]=m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=1; 2   
        } else if (m[i,"X401.12.4"]=="1\\3\\4") {m$"X401.12.4moquette"[i]=m$"X401.12.4parquet"[i]=1; m$"X401.12.4carrelage"[i]=2   
        } else if (m[i,"X401.12.4"]=="1\\2\\4") {m$"X401.12.4moquette"[i]=m$"X401.12.4carrelage"[i]=1; m$"X401.12.4parquet"[i]=2
        } else if (m[i,"X401.12.4"]=="2\\3\\4") {m$"X401.12.4carrelage"[i]=m$"X401.12.4parquet"[i]=1; m$"X401.12.4moquette"[i]=2                                  
        } else if (m[i,"X401.12.4"]=="1\\2\\3\\4") {m$"X401.12.4parquet"[i]=m$"X401.12.4carrelage"[i]=m$"X401.12.4moquette"[i]=1                               
        }     
       } 
    }
  m$"X401.12.5rue"<-NA
  m$"X401.12.5jardin"<-NA
  m$"X401.12.5cour"<-NA
  
    for (i in 1: nrow(m)) {
      if (!is.na(m[i,"X401.12.5"])) {
        if (m[i,"X401.12.5"]==1) {m$"X401.12.5rue"[i]=1; m$"X401.12.5jardin"[i]=m$"X401.12.5cour"[i]=2
        } else if (m[i,"X401.12.5"]==2) {m$"X401.12.5jardin"[i]=1; m$"X401.12.5rue"[i]=m$"X401.12.5cour"[i]=2
        } else if (m[i,"X401.12.5"]==3) {m$"X401.12.5cour"[i]=1; m$"X401.12.5rue"[i]=m$"X401.12.5jardin"[i]=2
        } else if (m[i,"X401.12.5"]=="1\\2") {m$"X401.12.5rue"[i]=m$"X401.12.5jardin"[i]=1; m$"X401.12.5cour"[i]=2                                    
        } else if (m[i,"X401.12.5"]=="2\\3") {m$"X401.12.5jardin"[i]=m$"X401.12.5cour"[i]=1; m$"X401.12.5rue"[i]=2                                     
        } else if (m[i,"X401.12.5"]=="1\\3") {m$"X401.12.5rue"[i]=m$"X401.12.5cour"[i]=1; m$"X401.12.5jardin"[i]=2
        } else if (m[i,"X401.12.5"]=="1\\2\\3") {m$"X401.12.5rue"[i]=m$"X401.12.5jardin"[i]=m$"X401.12.5cour"[i]=1; 2                                 
        }     
       } 
    }
s<-c(s,"X401.12.4","X401.12.5")
for (j in c("X401.12.8.1","X401.12.8.2")) {
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,j])) {
      if (m[i,j]=="1\\2") {m[i,j]=1
     }else if (m[i,j]=="1\\3") {m[i,j]=1
     }else if (m[i,j]=="1\\4") {m[i,j]=1
     }else if (m[i,j]=="2\\3") {m[i,j]=2
     }else if (m[i,j]=="2\\4") {m[i,j]=2
     }else if (m[i,j]=="3\\4") {m[i,j]=3
     }
    }
  }
}
m$X401.12.8etage<-NA

for (j in c("X401.12.8.1","X401.12.8.2")) {
  levels(m[,j])<-list("1"=c(1,"1\\2","1\\3","1\\4","1\\2\\3","1\\2\\4",
                            "1\\3\\4","1\\2\\3\\4"),
                      "2"=c(2,"2\\3","2\\4","2\\3\\4")
                      ,"3"=c(3,"3\\4"),
                      "4"=c(4))
}


for (j in c("X401.12.8.1","X401.12.8.2")) {
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,j])) {m[i,"X401.12.8etage"]=m[i,j]
       }
 }
}
m$X401.12.8.1<-NULL
m$X401.12.8.2<-NULL
for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X401.12.9.4"])) {
      if (m[i,"X401.12.9.4"]=="1\\2") {m[i,"X401.12.9.4"]=1
     }else if (m[i,"X401.12.9.4"]=="1\\3") {m[i,"X401.12.9.4"]=1
     }else if (m[i,"X401.12.9.4"]=="1\\4") {m[i,"X401.12.9.4"]=1
     }else if (m[i,"X401.12.9.4"]=="1\\5") {m[i,"X401.12.9.4"]=1
     }else if (m[i,"X401.12.9.4"]=="1\\6") {m[i,"X401.12.9.4"]=1
     }else if (m[i,"X401.12.9.4"]=="2\\3") {m[i,"X401.12.9.4"]=2
     }else if (m[i,"X401.12.9.4"]=="2\\4") {m[i,"X401.12.9.4"]=2
     }else if (m[i,"X401.12.9.4"]=="2\\5") {m[i,"X401.12.9.4"]=2
     }else if (m[i,"X401.12.9.4"]=="2\\6") {m[i,"X401.12.9.4"]=2
     }else if (m[i,"X401.12.9.4"]=="3\\4") {m[i,"X401.12.9.4"]=3
     }else if (m[i,"X401.12.9.4"]=="3\\5") {m[i,"X401.12.9.4"]=3
     }else if (m[i,"X401.12.9.4"]=="3\\6") {m[i,"X401.12.9.4"]=3
     }else if (m[i,"X401.12.9.4"]=="4\\5") {m[i,"X401.12.9.4"]=4
     }else if (m[i,"X401.12.9.4"]=="4\\6") {m[i,"X401.12.9.4"]=4
     }else if (m[i,"X401.12.9.4"]=="5\\6") {m[i,"X401.12.9.4"]=5
     }
    }
  }

r<-cbind(r,m)
emb("X402","X402.1")

r<-cbind(r,subset(mb,select=c("X403","X404","X405","X406","X407","X408","X409",
"X410","X411")))
minord("X413")
minord("X414")
r<-cbind(r,subset(mb,select=c("X415","X416")))
m<-subset(mb,select=c("X417","X417.1","X417.2","X417.3","X417.4","X417.5","X417.6","X417.7"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X418")))
embmaxord("X419","X419.1")
m<-subset(mb,select=c("X419.2","X419.3","X419.4","X419.5","X419.6"))
 for (i in 1:nrow(m)) {
      if (!is.na(mb[i,"X419"])) {
        if (mb[i,"X419"]==2) {m[i,1]=m[i,2]=m[i,3]=m[i,4]=m[i,5]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X420","X421","X422","X423","X424","X425")))
minord("X426")
minord("X427")
minord("X428")
minord("X429")
minord("X430")
minord("X431")
minord("X432")
m<-subset(mb,select=c("X432.1"))
for (i in 1:nrow(m)) {
      if (!is.na(mb[i,"X432"])) {
        if (mb[i,"X432"]==5) {m[i,1]=2}
       }
 }
r<-cbind(r,m)
emb("X433","X433.1")
maxord("X434")
maxord("X435")
m<-subset(mb,select=c("X436","X436.1","X436.2","X436.3","X436.4","X436.5","X436.6",
"X436.7","X436.8","X436.9","X436.10","X436.11","X436.12","X436.13","X436.14"))
levels(m[,"X436.1"])<-c(levels(m[,"X436.1"]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=0; m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=
           m[i,9]=m[i,10]=m[i,11]=m[i,12]=m[i,13]=m[i,14]=m[i,15]=2}
       }
 }
r<-cbind(r,m)
m<-subset(mb,select=c("X437","X437.1","X437.2","X437.3","X437.4","X437.5","X437.6",
"X437.7"))
levels(m[,"X437.1"])<-c(levels(m[,"X437.1"]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=0; m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=2}
       }
 }
r<-cbind(r,m)

r<-cbind(r,subset(mb,select=c("X438","X439","X440","X441","X442","X443",
"X444","X445","X446","X447","X448","X449","X450","X451","X452","X453","X454")))
m<-subset(mb,select=c("X455","X455.1","X455.2","X455.3","X455.4","X455.5","X455.6",
"X455.7","X455.8","X455.9","X455.10"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=
m[i,9]=m[i,10]=m[i,11]=2}
       }
 }
r<-cbind(r,m)
m<-subset(mb,select=c("X456","X456.1","X456.2","X456.3","X456.4","X456.5","X456.6",
"X456.7","X456.8","X456.9","X456.10","X456.11"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=
m[i,9]=m[i,10]=m[i,11]=m[i,12]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X457","X458","X459","X460","X461","X462",
"X463","X464","X465","X466","X467","X468","X469","X470","X471","X473","X474",
"X475","X476","X477","X478","X479","X480","X481","X482","X483","X484","X485",
"X486","X487","X488","X489","X490","X491","X492","X493","X494","X495","X496",
"X497","X498","X499","X500","X501","X502","X503","X504","X505","X506","X507",
"X508","X509","X510")))
m<-subset(mb,select=c("X511","X511.1","X511.2","X511.3","X511.4","X511.5","X511.6",
"X511.7","X511.8"))
levels(m[,2])<-c(levels(m[,2]),5)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=5; m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=m[i,8]=
m[i,9]=2}
       }
 }
for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X511.1"])) {
      if (m[i,"X511.1"]=="1\\2") {m[i,"X511.1"]=1
     }else if (m[i,"X511.1"]=="1\\3") {m[i,"X511.1"]=1
     }else if (m[i,"X511.1"]=="1\\4") {m[i,"X511.1"]=1
     }else if (m[i,"X511.1"]=="2\\3") {m[i,"X511.1"]=2
     }else if (m[i,"X511.1"]=="2\\4") {m[i,"X511.1"]=2
     }else if (m[i,"X511.1"]=="3\\4") {m[i,"X511.1"]=3
     }
    }
  }

r<-cbind(r,m)
m<-subset(mb,select=c("X512","X512.1","X512.2"))
levels(m[,2])<-c(levels(m[,2]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=0; m[i,3]=2}
       }
 }
r<-cbind(r,m)
m<-subset(mb,select=c("X513","X513.1","X513.2"))
levels(m[,2])<-c(levels(m[,2]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=0; m[i,3]=2}
       }
 }
r<-cbind(r,m)
r<-cbind(r,subset(mb,select=c("X514","X515","X516","X517","X518","X519",
"X520","X521","X522","X523","X524")))
embd("X525","X525.1","X525.2")
minord("X526")
minord("X527")
minord("X528")
minord("X529")
minord("X530")
minord("X531")
minord("X532")
minord("X533")
minord("X534")
minord("X535")
minord("X536")
minord("X537")
minord("X538")
minord("X539")
minord("X540")
minord("X541")
minord("X542")
minord("X543")
minord("X544")
minord("X545")
minord("X546")
minord("X547")
minord("X548")
r<-cbind(r,subset(mb,select=c("X549","X550","X551","X552","X553")))

m<-subset(mb,select=c("X554","X554.1","X554.2","X554.3","X554.4","X554.5","X554.6"))
 for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=2}
       }
 }
r<-cbind(r,m)

r<-cbind(r,subset(mb,select=c("X555","X556","X557")))

m<-subset(mb,select=c("X558","X558.1","X558.2","X558.3","X558.4","X558.5","X558.6",
"X558.6.1","X558.7","X558.8"))
levels(m[,"X558.7"])<-c(levels(m[,"X558.7"]),0)
m$"X558.6diarete"<-NA
m$"X558.6diarhiver"<-NA
m$"X558.8printemps"<-NA
m$"X558.8ete"<-NA
m$"X558.8automne"<-NA
m$"X558.8hiver"<-NA

for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,2]=m[i,3]=m[i,4]=m[i,5]=m[i,6]=m[i,7]=
             m[i,"X558.6diarete"]=m[i,"X558.6diarhiver"]=m[i,"X558.8printemps"]=
               m[i,"X558.8ete"]=m[i,"X558.8automne"]=m[i,"X558.8hiver"]=2;
             m[i,9]=0}
       }
 }
for (i in 1: nrow(m)) {
    if (!is.na(m[i,"X558.6.1"])) {
      if (m[i,"X558.6.1"]==1) {m$"X558.6diarete"[i]=1; m$"X558.6diarhiver"[i]=2
      } else if (m[i,"X558.6.1"]==2) {m$"X558.6diarhiver"[i]=1; m$"X558.6diarete"[i]=2
      } else if (m[i,"X558.6.1"]=="1\\2") {m$"X558.6diarete"[i]=m$"X558.6diarhiver"[i]=1 
      }
      } 
    
  }
for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X558.7"])) {
      if (m[i,"X558.7"]=="1\\2") {m[i,"X558.7"]=2
     }else if (m[i,"X558.7"]=="1\\3") {m[i,"X558.7"]=3
     }else if (m[i,"X558.7"]=="1\\4") {m[i,"X558.7"]=4
     }else if (m[i,"X558.7"]=="1\\5") {m[i,"X558.7"]=5
     }else if (m[i,"X558.7"]=="2\\3") {m[i,"X558.7"]=3
     }else if (m[i,"X558.7"]=="2\\4") {m[i,"X558.7"]=4
     }else if (m[i,"X558.7"]=="2\\5") {m[i,"X558.7"]=5
     }else if (m[i,"X558.7"]=="3\\4") {m[i,"X558.7"]=4
     }else if (m[i,"X558.7"]=="3\\5") {m[i,"X558.7"]=5
     }else if (m[i,"X558.7"]=="4\\5") {m[i,"X558.7"]=5
     }
    }
  }

for (i in 1: nrow(m)) {
      if (!is.na(m[i,"X558.8"])) {
        if (m[i,"X558.8"]==1) {m$"X558.8printemps"[i]=1; m$"X558.8ete"[i]=m$"X558.8automne"[i]=m$"X558.8hiver"[i]=2
        } else if (m[i,"X558.8"]==2) {m$"X558.8ete"[i]=1; m$"X558.8printemps"[i]=m$"X558.8automne"[i]=m$"X558.8hiver"[i]=2
        } else if (m[i,"X558.8"]==3) {m$"X558.8automne"[i]=1; m$"X558.8printemps"[i]=m$"X558.8ete"[i]=m$"X558.8hiver"[i]=2
        } else if (m[i,"X558.8"]==4) {m$"X558.8hiver"[i]=1; m$"X558.8printemps"[i]=m$"X558.8ete"[i]=m$"X558.8automne"[i]=2                          
        } else if (m[i,"X558.8"]=="1\\2") {m$"X558.8printemps"[i]=m$"X558.8ete"[i]=1; m$"X558.8automne"[i]=m$"X558.8hiver"[i]=2                                    
        } else if (m[i,"X558.8"]=="2\\3") {m$"X558.8ete"[i]=m$"X558.8automne"[i]=1; m$"X558.8printemps"[i]=m$"X558.8hiver"[i]=2                                     
        } else if (m[i,"X558.8"]=="1\\3") {m$"X558.8printemps"[i]=m$"X558.8automne"[i]=1; m$"X558.8ete"[i]=m$"X558.8hiver"[i]=2
        } else if (m[i,"X558.8"]=="1\\4") {m$"X558.8printemps"[i]=m$"X558.8hiver"[i]=1; m$"X558.8ete"[i]=m$"X558.8automne"[i]=2
        } else if (m[i,"X558.8"]=="2\\4") {m$"X558.8ete"[i]=m$"X558.8hiver"[i]=1; m$"X558.8printemps"[i]=m$"X558.8automne"[i]=2
        } else if (m[i,"X558.8"]=="3\\4") {m$"X558.8hiver"[i]=m$"X558.8automne"[i]=1; m$"X558.8ete"[i]=m$"X558.8printemps"[i]=2
        } else if (m[i,"X558.8"]=="1\\2\\3") {m$"X558.8printemps"[i]=m$"X558.8ete"[i]=m$"X558.8automne"[i]=1; m$"X558.8hiver"[i]=2   
        } else if (m[i,"X558.8"]=="1\\3\\4") {m$"X558.8printemps"[i]=m$"X558.8automne"[i]=m$"X558.8hiver"[i]=1; m$"X558.8ete"[i]=2   
        } else if (m[i,"X558.8"]=="1\\2\\4") {m$"X558.8printemps"[i]=m$"X558.8ete"[i]=m$"X558.8hiver"[i]=1; m$"X558.8automne"[i]=2
        } else if (m[i,"X558.8"]=="2\\3\\4") {m$"X558.8ete"[i]=m$"X558.8automne"[i]=m$"X558.8hiver"[i]=1; m$"X558.8printemps"[i]=2                                  
        } else if (m[i,"X558.8"]=="1\\2\\3\\4") {m$"X558.8hiver"[i]=m$"X558.8automne"[i]=m$"X558.8ete"[i]=m$"X558.8printemps"[i]=1                               
        }     
       } 
    }
s<-c(s,"X558.6.1","X558.8")
r<-cbind(r,m)
minord("X559")
r<-cbind(r,subset(mb,select=c("X560","X561","X562","X563","X564","X565","X566","X567","X568","X569")))
minord("X570")
minord("X571")
r<-cbind(r,subset(mb,select=c("X572","X573","X574")))

m<-subset(mb,select=c("X575","X575.1","X575.2","X575.3","X575.3.1","X575.3.2",
"X575.3.3","X575.3.4","X575.3.5","X575.3.6","X575.3.7","X575.3.8","X575.3.10"))
levels(m[,3])<-c(levels(m[,3]),0)
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,3]=0; m[i,2]=m[i,4]=2}
       }
 }
 for (i in 1:nrow(m)) {
   if (!is.na(m[i,"X575.2"])) {
      if (m[i,"X575.2"]=="1\\2") {m[i,"X575.2"]=2
     }else if (m[i,"X575.2"]=="2\\3") {m[i,"X575.2"]=3
     }
    }
  }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,4])) {
        if (m[i,4]==2) {m[i,5]=m[i,6]=m[i,7]=m[i,8]=m[i,9]=m[i,10]=m[i,11]=m[i,12]=m[i,13]=2}
      }
 }
r<-cbind(r,m)
m<-subset(mb,select=c("X576","X576.1","X576.2","X576.3","X576.3.1","X576.3.2",
"X576.3.3","X576.3.5"))
for (i in 1:nrow(m)) {
      if (!is.na(m[i,1])) {
        if (m[i,1]==2) {m[i,3]=m[i,2]=m[i,4]=2}
       }
 }
for (i in 1:nrow(m)) {
      if (!is.na(m[i,4])) {
        if (m[i,4]==2) {m[i,5]=m[i,6]=m[i,7]=m[i,8]=2}
      }
 }
r<-cbind(r,m) #LIBRE, ENFIN LIBRE!

r<-r[,!colnames(r) %in% s] #Enlever cette ligne pour retrouver les questions catégorielles


write.table(r,"~/Documents/Questionnaire/questrevucomplet",row.names=F,col.names=T,sep=";")
#vérifier qu'il n'y a plus de / dans les réponses (réponses multiples) X265.1 1/2 mettre 1 sinon les supprimer
