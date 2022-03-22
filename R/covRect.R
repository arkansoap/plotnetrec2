#' Représentation de la covariance
#'
#' @param data a dataset
#' @return A mist of GGPLOT graph explainig
#' @import ggplot2
#' @export
#' @examples
#' covRect(data1)[[1]]

covRect <- function(data, rank = FALSE, timeseries = FALSE){

  data <- data[order(data[,1]),]

  if(rank == FALSE){
    X = data[,1]
    Y = data[,2]
  }else{
    X = rank(data[,1])
    Y = rank(data[,2])
  }

  N = nrow(data)

  circle.size = 2.5
  colors = list('red', '#0066CC', '#4BB14B', '#FCE638')
  r=0.6
  Nmat=N*(N-1)/2
  Nm1s=(N-1)^2

  S.bar<-factor(sign((X-mean(X))*(Y-(mean(Y)))))
  X.bar <- rep(mean(X),N)
  Y.bar <- rep(mean(Y),N)

  mydata=data.frame(X,Y,X.bar,Y.bar,S.bar)

  ############### 1st output ################################
  plotscatter<-  ggplot(data=mydata, aes(x=X,y=Y)) +
    geom_point(size=circle.size, pch=21, fill=colors[[4]]) +
    geom_hline(yintercept=Y.bar, linetype='dashed',color="orange",size=0.5) +
    geom_vline(xintercept=X.bar, linetype='dashed',color="orange",size=0.5) +
    stat_smooth(method="lm", se=FALSE,color="black",size=1)+
    ggtitle('Y vs. X Scatterplot') +
    xlab("X")+
    ylab("Y")

  ######################## Calculs usuels #################
  truc1<- vector()
  truc2<- vector()
  i=1
  for(i in 1:N){
    up=N-i
    truc1=cbind(truc1,t((rep(i,up))))
    truc2=cbind(truc2,t((seq(from=(i+1),to=N))))
  }


  Xmin<-rep(0,Nmat)
  Xmax<-rep(0,Nmat)
  Ymin<-rep(0,Nmat)
  Ymax<-rep(0,Nmat)

  i=1
  for(i in 1:Nmat){
    Xmin[i]<-X[(truc1[i])]
    Xmax[i]<-X[(truc2[i])]
    Ymin[i]<-Y[(truc1[i])]
    Ymax[i]<-Y[(truc2[i])]
    i=i+1
  }

  factor(sign(0))

  S<-factor(sign((Xmin-Xmax)*(Ymin-Ymax)))
  d=data.frame(Xmin,Xmax,Ymin,Ymax,S)

  #################2nd Ouput #######################
  plotcov<-  ggplot(mydata, aes(xmin=X, xmax=X.bar, ymin=Y, ymax=Y.bar)) +
    #  geom_point(size=circle.size, pch=21, fill=colors[[4]]) +
    geom_hline(yintercept=Y.bar, linetype='dashed',color="orange",size=0.5) +
    geom_vline(xintercept=X.bar, linetype='dashed',color="orange",size=0.5) +
    geom_rect(alpha=0.02, aes(fill=S.bar)) +   scale_fill_manual(values=c('red','blue')) +
    #alpha=.5
    ggtitle('Covariance') +
    #  xlim(0, 4) +
    #  ylim(0, 4)+
    xlab("X")+
    ylab("Y")

  ################ 3rd output #########################
  if(timeseries == FALSE){
    plotrect<-  ggplot(d, aes(Xmin,Ymin, xmin=Xmin, xmax=Xmax,ymin=Ymin, ymax=Ymax)) +
      #  geom_point(size=circle.size, pch=21, fill=colors[[4]]) +
      #  geom_hline(yintercept=Y.bar, linetype='longdash',color="red") +
      #  geom_vline(xintercept=X.bar, linetype='longdash',color="red") +
      geom_rect(alpha=0.1, aes(fill=S)) +   scale_fill_manual(values=c('red', 'blue')) +
      #alpha=.5
      ggtitle('Covariances Rectangle : 2Cov(X,Y)') +
      xlab("X")+
      ylab("Y")
  }

  ##################### Calculs usuels #########################
  #theme_light()
  SX<-sort(X,decreasing=FALSE)
  SY<-sort(Y,decreasing=FALSE)

  #initialyse
  RES<-matrix(0,(N-1),(N-1))
  i=1
  for(i in 1:(N-1))
  {
    for(j in (i+1):N)
    {
      TRUC<-rep(0,N-1)
      posyi<-which(SY==Y[i])
      posyj<-which(SY==Y[j])
      min=min(c(posyi,posyj))
      max=max(c(posyi,posyj))-1
      TRUC<- replace(TRUC,c(min:max),sign((X[i]-X[j])*(Y[i]-Y[j])))
      RES[,i:(j-1)]=RES[,i:(j-1)]+TRUC
    }
  }
  i = 2
  j =3

  TRUC<-rep(0,N-1)
  posyi<-which(SY==Y[i])
  posyj<-which(SY==Y[j])
  min=min(c(posyi,posyj))
  max=max(c(posyi,posyj))-1
  TRUC<- replace(TRUC,c(min:max),sign((X[i]-X[j])*(Y[i]-Y[j])))
  RES[,i:(j-1)]=RES[,i:(j-1)]+TRUC
  RES


  truc1<- vector()
  truc2<- vector()
  i=1
  for(i in 1:N){
    up=N-1
    truc1=cbind(truc1,t((rep(i,up))))
    truc2=cbind(truc2,t((rep((i+1),(up)))))
  }
  truc3=t(rep(seq(1,N-1),N-1))
  truc4=t(rep(seq(2,N),N-1))

  Xmin<-rep(0,Nm1s)
  Xmax<-rep(0,Nm1s)
  Ymin<-rep(0,Nm1s)
  Ymax<-rep(0,Nm1s)

  i=1
  for(i in 1:Nm1s){
    Xmin[i]<-X[(truc1[i])]
    Xmax[i]<-X[(truc2[i])]
    Ymin[i]<-SY[(truc3[i])]
    Ymax[i]<-SY[(truc4[i])]
    i=i+1
  }

  # S<-factor(matrix(RES,((N-1)^2)))
  S<-(matrix(RES,((N-1)^2)))
  MS<-mean(S)
  MS<-quantile(S, probs = seq(0, 1, 0.2))
  #MS<-c(-500,0,1000,2000,2440)
  d=data.frame(Xmin,Xmax,Ymin,Ymax,S)


  ######################### 4th output ###########################
  plotnetrec<- ggplot(data=d,aes(Xmin, Ymin, xmin=Xmin, xmax=Xmax,ymin=Ymin, ymax=Ymax)) +
    #  geom_point(size=circle.size, pch=21, fill=colors[[4]]) +
    #  geom_hline(yintercept=Y.bar, linetype='longdash',color="red") +
    #  geom_vline(xintercept=X.bar, linetype='longdash',color="red") +
    geom_rect(alpha=1, aes(fill=S)) +
    #alpha=.1
    # S continuous
    #scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', midpoint = 0) +
    scale_fill_gradientn(colours = c('blue', 'white','red'), breaks=MS) +
    #scale_fill_gradientn(colours = c("firebrick4","firebrick1","white","deepskyblue1","deepskyblue4"), breaks=MS) +
    #scale_fill_gradientn(colours = c("red","white","deepskyblue3","deepskyblue2","deepskyblue3"), breaks=MS) +
    # S discrete
    #scale_fill_brewer(palette="Spectral")+
    #scale_fill_manual(breaks= quantile(S), values=c('white','cyan','turquoise2','blue2')) +
    # xlim(0, 5) +
    #  ylim(0, 5) +
    xlab("X")+
    ylab("Y")
  if(timeseries == FALSE){
    out <- list(plotscatter, plotcov, plotrect, plotnetrec)
  }else{
    out <- list(plotscatter, plotcov, plotnetrec)
  }
  return(out)
}
