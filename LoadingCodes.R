library(ClusterR)
#### LOADING DATABASE ########
trainA=readRDS("TrainData/trainA.rds")
trainB=readRDS("TrainData/trainB.rds")
trainC=readRDS("TrainData/trainC.rds")
trainD=readRDS("TrainData/trainD.rds")
trainE=readRDS("TrainData/trainE.rds")
trainF=readRDS("TrainData/trainF.rds")
trainG=readRDS("TrainData/trainG.rds")
trainH=readRDS("TrainData/trainH.rds")
trainI=readRDS("TrainData/trainI.rds")
trainJ=readRDS("TrainData/trainJ.rds")
trainK=readRDS("TrainData/trainK.rds")
trainL=readRDS("TrainData/trainL.rds")
trainM=readRDS("TrainData/trainM.rds")
trainN=readRDS("TrainData/trainN.rds")
trainO=readRDS("TrainData/trainO.rds")
trainP=readRDS("TrainData/trainP.rds")
trainQ=readRDS("TrainData/trainQ.rds")
trainR=readRDS("TrainData/trainR.rds")
trainS=readRDS("TrainData/trainS.rds")
trainT=readRDS("TrainData/trainT.rds")
trainU=readRDS("TrainData/trainU.rds")
trainV=readRDS("TrainData/trainV.rds")
trainW=readRDS("TrainData/trainW.rds")
train_letter_X=readRDS("TrainData/train_letter_X.rds")
train_letter_Y=readRDS("TrainData/train_letter_Y.rds")
trainZ=readRDS("TrainData/trainZ.rds")

####### Getting things ready ###########

trainY=c(trainA,trainM,trainI,trainW,trainV)
trainX=c(trainB,trainC,trainH,trainE,trainD,trainR)
trainBoth=c(trainU,trainL,trainF,trainP,
            trainN,trainJ,train_letter_X,train_letter_Y,
            trainZ,trainT,trainQ,trainK,trainS,trainO,trainG)


namesY=c(rep("A",length(trainA)),
         rep("M",length(trainM)),
         rep("I",length(trainI)),
         rep("W",length(trainW)),
         rep("V",length(trainV)))

namesX=c(rep("B",length(trainB)),
         rep("C",length(trainC)),
         rep("H",length(trainH)),
         rep("E",length(trainE)),
         rep("D",length(trainD)),
         rep("R",length(trainR)))

namesBoth=c(rep("U",length(trainU)),
            rep("L",length(trainL)),
            rep("F",length(trainF)),
            rep("P",length(trainP)),
            rep("N",length(trainN)),
            rep("J",length(trainJ)),
            rep("X",length(train_letter_X)),
            rep("Y",length(train_letter_Y)),
            rep("Z",length(trainZ)),
            rep("T",length(trainT)),
            rep("Q",length(trainQ)),
            rep("K",length(trainK)),
            rep("S",length(trainS)),
            rep("O",length(trainO)),
            rep("G",length(trainG)))
            

#######################################

####### Scaling and Projecting ###########

adjust=function(a,axs)
{
  aa=a
  if(axs<3) #project
  {
    directionA = summary(prcomp(aa))[[2]][,1]
    if(directionA[axs]<0)directionA=-directionA
    aa[,axs]=(directionA[1]*aa[,1]+directionA[2]*aa[,2])
  }
  
  for(ii in 1:2 )#shubhojit scaling
  { aa[,ii]=aa[,ii]-min(aa[,ii]) 
  aa[,ii]=aa[,ii]/max(aa[,ii]) 
  }
  return(aa)
}

for(i in c(1:length(trainX)))     trainX[[i]]    =  adjust(trainX[[i]],1)
for(i in c(1:length(trainY)))     trainY[[i]]    =  adjust(trainY[[i]],2)
for(i in c(1:length(trainBoth)))  trainBoth[[i]] =  adjust(trainBoth[[i]],3)

##########################################

######### CUTS ###########

CUTS2=function(data, window=10,index=4)
{
  # index by which we will cut. intex=3 for z axis in accelaration data
  b=window
  
  len=length(data[,index])-b
  movvar=seq(1,len)
  for(i in 1:len){
    movvar[i]=var(data[i:(i+b),index])
  }
  
  # movvar[i] is moving variance from i to i+b
  B=kmeans(movvar,2)
  cluster=B$cluster
  if(B$centers[1]>B$centers[2])cluster=3-cluster
  KMbound=(max(movvar[cluster==1])+min(movvar[cluster==2]))/2
  
  A=movvar
  C=GMM(as.matrix(A),2)
  cluster=max.col(C$Log_likelihood)
  if(C$centroids[1]>C$centroids[2])cluster=3-cluster
  GMMbound=(min(A[cluster==2])+max(A[cluster==1]))/2
  
  bound=(KMbound+GMMbound)/2
  
  cuts=c()
  Var_Low=TRUE
  len=length(movvar)
  start=0
  end=0
  for(i in 3:(len-3))
  {
    if(Var_Low)
    {
      if(movvar[i]>bound && movvar[i+1]>bound && movvar[i+2]>bound)
      {
        Var_Low=FALSE;  start = i;
      }
      
    }else
    {
      if(movvar[i]<bound && movvar[i+1]<bound && movvar[i+2]<bound)
      {
        Var_Low=TRUE;   end = i;
        cuts=append(cuts, start+(window/2))
        cuts=append(cuts, end+ (window/2) )
      }
    }
  }
  return(cuts)
}

#############################################
############# Separate ######################
#############################################

separate3=function(data)
{
  Alphabets=list()
  
  data[,4]=sqrt(data[,1]^2+data[,2]^2+data[,3]^2)
  cuts=CUTS2(data)
  
  l=length(cuts)/2 -1 
  index=1  #used for adding element at ith index of alphabets
  
  for(i in 1:l)
  {
    low=cuts[2*i]; high=cuts[2*i+1];
    temp=matrix(0,2,100)
    current=data[low:high,]
    
    current=as.matrix(current)
    time=c(1:length(current[,1]))
    
    smoothingSplineX = smooth.spline(time, current[,1],spar=0.5)
    smoothingSplineY = smooth.spline(time, current[,2],spar=0.5)
    x=(seq(1,length(current[,1]),length.out=100))
    
    temp[1,]=predict(smoothingSplineX,x)$y
    temp[2,]=predict(smoothingSplineY,x)$y
    
    
    temp=t(temp)
    colnames(temp)=c("x","y")
    Alphabets[[index]]=temp
    index=index+1
    
  }  
  return(Alphabets)
}

####################################
####################################


################################################
####### PRINCIPL COMPONENT ANALYSIS ############
################################################

axis=function(a, axisCutoff = 0.92)  # return 1 for x axis, 2 for y axis 3 for both
{
  main1 = summary(prcomp(a))[[6]][2]
  
  if(main1<axisCutoff)return(3)
  
  direction = abs(summary(prcomp(a))[[2]][,1])
  if(direction[1]>direction[2])return(1)
  return(2)
  
}
###########################################
###########################################

##DISTANCE###############################

DistSingle=function(a,b,axis)
{
  aa=a
  bb=b
  if(axis!=3) return(sum(abs(aa[,axis]-bb[,axis])))
  ## if axis =3   saban jol
  c=(aa-bb)^2
  c=c[,1]+c[,2]
  return(sum(sqrt(c)))

  }

###########
classifySingle=function(a)   # the nearest neighbour
{
  CurTrain=trainX
  CurNames=namesX
  
  direction = axis(a)
  
  if(direction == 1){CurTrain = trainX;  CurNames = namesX;}
  if(direction == 2){CurTrain = trainY;  CurNames = namesY;}
  if(direction == 3){CurTrain = trainBoth;  CurNames = namesBoth;}
  
  a=adjust(a,direction)
  
  d=sapply(CurTrain,function(x){DistSingle(x,a,direction)})
  df=data.frame(d,CurNames)[order(d),]
  rownames(df)=c(1:length(df[,1]))
  
  #print(head(df))
  
  return(as.character(df[1,2]))
}

#####################
#########main############
#################

main=function(data)
{
  df=data.frame(matrix(0.5773503,nrow=10,ncol=3))
  colnames(df)=c("X","Y","Z")
  data=rbind(df,data,df)
  Alphabets=separate3(data)
  output=c()
  for(a in Alphabets)output=append(output,classifySingle(a))
  cat(output)
}



