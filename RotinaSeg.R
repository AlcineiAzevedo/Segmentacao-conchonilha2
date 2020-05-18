remove(list=ls())
library(OpenImageR)
setwd("D:/BKP/Backup Pendrive/UFMG/Disciplinas/Estatistica experimental/Videos/_Dicas/SegmentacaoPalma")
PreencherMascaraNegra =function(img2,perc,imagem=T,individual=F){
  if(imagem==T){t=img2@.Data[,,1]}
  if(imagem==F){t=img2}
  
  if(individual==F){
    
    
    n=round(perc*min(c(ncol(t),nrow(t))),0)
    
    p1=function(t){
      t2=t
      for( i in 2:(nrow(t)-n-1)){
        for(j in 1:ncol(t)){
          if(t[i,j]==1){
            if(t[i-1,j]==0){
              
              a=0
              while(a<n){
                a=a+1
                
                if(sum(t[i:(i+a),j]==1)<a){t2[i:(i+a),j]=0;a=n}
                
              }
              
            }
          }
        }
      }
      return(t2)
    }
    
    Pp=p1(t)
    Pp=p1(t(Pp))
    return(t(Pp))
  }
  
  if(individual==T){
    
    t2=Contorno(t,imagem = F)
    display(t2)
    m=cbind(expand.grid(1:nrow(t2),1:ncol(t2)),c(t2))
    m=as.matrix(m[m[,3]<1,])
    
    ind=unique(m[,1])
    for(y in 1:length(ind)){
      t2[ind[y],min(m[m[,1]==ind[y],2]):max(m[m[,1]==ind[y],2])]=0
    }
    
    
    return(t2)
    
  }
}




im=readImage("pm.jpeg")
imageShow(im)
palma=readImage("palma.jpg")
imageShow(palma)
fundo=readImage("fundo.jpg")
imageShow(fundo)
concho=readImage("concho.jpg")
imageShow(concho)

mpalma=cbind(c(palma[,,1]),c(palma[,,2]),c(palma[,,3]))
mpalma=mpalma[sample(1:nrow(mpalma)),]
mpalma=mpalma[1:20000,]


mfundo=cbind(c(fundo[,,1]),c(fundo[,,2]),c(fundo[,,3]))
mfundo=mfundo[sample(1:nrow(mfundo)),]
mfundo=mfundo[1:20000,]



mconcho=cbind(c(concho[,,1]),c(concho[,,2]),c(concho[,,3]))
mconcho=mconcho[sample(1:nrow(mconcho)),]
mconcho=mconcho[1:20000,]


Mat=rbind(cbind(mpalma,1),cbind(mconcho,1),cbind(mfundo,0))
colnames(Mat)=c("R","G","B","Y")
Mat=data.frame(Mat)

modelo1=glm(Y~R+G+B,family = binomial("logit"),data=Mat)
Mimagem=cbind(c(im[,,1]),c(im[,,2]),c(im[,,3]))
colnames(Mimagem)=c("R","G","B")
Mimagem=data.frame(Mimagem)

Pred1=round(predict(modelo1,newdata=Mimagem,type="response"),0)
MPred=matrix(Pred1,ncol=ncol(im[,,1]))
imageShow(MPred)

Mpred2=PreencherMascaraNegra(MPred==0,perc = 0.009,imagem = F)
imageShow(Mpred2)


Mat2=rbind(cbind(mpalma,1),cbind(mconcho,0))
colnames(Mat2)=c("R","G","B","Y")
Mat2=data.frame(Mat2)

modelo2=glm(Y~R+G+B,data=Mat2,family = binomial("logit"))

ID=Mpred2==0
Mat3=cbind(im[,,1][ID],im[,,2][ID],im[,,3][ID])
colnames(Mat3)=c("R","G","B")
Mat3=data.frame(Mat3)

Pred2=round(predict(modelo2,newdata = Mat3,type="response"),0)
im2=im
im2[,,1][ID]=Pred2

imageShow(im2)


1-mean(Pred2)
