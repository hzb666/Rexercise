############################################################################

  #source('Excercise_New.R');

############################################################################

#update.packages("openxlsx");   
        ####  Update selected packages. 
#update.packages(ask=FALSE);
        ####  Update all the packages. 

############################################################################

#install.packages("openxlsx");
#library("openxlsx");    
        ####  Load selected package of "openxlsx". 
Name_File = "Lexicon_Similar.xlsx";
#Data_Learn = read.xlsx(Name_File);
        ##########  Read selected file.
? read.xlsx
        ##########  Read file of help.
#str(Data_Learn);
        ##########  Check the structure of data.
#String_Term = Data_Learn$Term;
        ##########   Extract variable of Term in Data_Learn.
#save(Data_Learn,file="ABCD.rda");
        ##########  Save data by R style.
#MyData = load("ABCD.rda");
        ##########  Load selected data.

############################################################################

 X=0;
 for(k in 1:10){X[k]=k^2+1; print(X);}
 X=0;
 for(k in c(2,5,4)){X[k]=k^2+1; print(X);}  
        ##########  Load selected data.
x=rbinom(10,1,0.5); print(x); print(sum(x));
x=0:10; y=dbinom(x,10,0.5); print(cbind(x,y));

############################################################################

 x=rnorm(20,0,1); print(x);
 y = rexp(100,5); print(y);
 z = c(x,y);   plot(z);
        ##########  Connect data of x and y.

############################################################################

 Number_Box = 10;
 Bound = seq(from=0,to=1,length=Number_Box);
 SampleX = runif(500);
 Frequence = 0;
 for (k in 1:Number_Box){
     Index=(SampleX>Bound[k])*(SampleX<Bound[k+1]);
     Frequence[k]=sum(Index);}
 barplot(Frequence,xlab="Groups",ylab="Frequence",main="BarPlot");
 hist(SampleX);

############################################################################

X = seq(0, 2*pi, length = 30);
    #######  
Y = sin(X);   Z = cos(X);
    #######  
Data = cbind (Y,Z);
    #######  
matplot (X, Data, pch = 10, type = 'o');
    #######  
YZ_Min = apply(Data,1,min);
        #####   
plot(sort(YZ_Min))


############################################################################





############################################################################

Sample = combn(1:5,3);
     #### 
Number_Waster = colSums(Sample <= 2);
     #### 
Number_Waster = colSums(Sample <= 2);
     #### 
Selection = (Number_Waster == 1);
     #### 
Probability = sum(Selection)/choose(5,3);
     #### 

SampleX = 1:5 <= 2
     #### 
Number_Repeat = 100;
     #### 
Sample_Random = sample(SampleX,Number_Repeat*3,replace=TRUE);
     #### 
SampleY = matrix(Sample_Random,nrow=Number_Repeat,ncol=3)
     #### 
for(k in 1:Number_Repeat){SampleY[k,] = sample(SampleX,3)};
     #### 
Number_Waster = rowSums(SampleY);
     #### 
Selection = (Number_Waster == 1);
     #### 
Frequency = sum(Selection)/Number_Repeat;
     #### 
print(Frequency-Probability);

############################################################################

X = rnorm(100,0,1);  hist(X);
    #######  
X2 = X^2;  hist(X2);
    #######  
E_X2 = mean(X2); D_X2 = sd(X2);
    #######  
Y = rexp(1000,3);  hist(Y);
    #######  
E_X2 = mean(X2); D_X2 = sd(X2);
    #######  

############################################################################

X = rnorm(1000);
     ##### 
Y = X^2
hist(Y,20,freq = F)
     ##### 
lines(density(Y))
     ##### 

############################################################################

Norm_0_1 = rnorm(200,0,1);
Norm_0_2 = rnorm(200,0,2);
Norm_1_2 = rnorm(200,1,2); 

Trans_1_2 = (Norm_1_2-1)/2; 
Trans_10_2 = (Norm_1_2-10)/2; 
Trans_1_5 = (Norm_1_2-1)/5;

Data= cbind(Norm_0_1,Norm_0_2,Norm_1_2,Trans_1_2,Trans_10_2,Trans_1_5);
boxplot(Data)

############################################################################

Number_Sample = 100;
        #####   
Number_Repeat = 200;
        #####   
Vector_T = as.numeric(vector(length=Number_Repeat))
        #####   
for (k in 1:Number_Repeat){
    Vector_T[k]= Statistic_T(Number_Sample)};
        #####   
hist(Vector_T,20,freq=F);
        #####   
lines(density(Vector_T));
     ##### 

Statistic_T <- function(Number_Sample){
    X = rnorm(Number_Sample); 
    S_T = (X-mean(X))/sd(X);
    return(S_T)}
        #####   

############################################################################

Statistic_F <- function(Number_Sample){
    X1 = rnorm(Number_Sample); 
    X2 = rnorm(Number_Sample);
    S_F = sd(X1)/sd(X2);
    return(S_F)}
        #####   

Number_Sample = 10;
        #####   
Number_Repeat = 200;
        #####   
Vector_F = as.numeric(vector(length=Number_Repeat))
        #####   
for (k in 1:Number_Repeat) {Vector_F[k]= Statistic_F(Number_Sample)};
        #####   
hist(Vector_F,20,freq=F)
        #####   
lines(density(Vector_F))
     ##### 

############################################################################

Random_Normal = rnorm(100);
        #####   
FX_Normal = pnorm(Random_Normal);
        #####   
plot(sort(FX_Normal))
     ##### 
FX_Uniform = punif(Random_Normal);
        #####   
plot(sort(FX_Uniform))
     ##### 

############################################################################     

n=100; p=0.3;
x=rbinom(100,1,0.3);
m=sum(x);
freq = m/n;
err = abs(freq-p);
print(err);         ####   

X = matrix(rnorm(10000,1,1),100,100);
EX = colMeans(X);
Mu = 1;
err = EX-1;
boxplot(err);      
 
X = matrix(rnorm(10000,1,1),100,100);
PX = apply(X,2,quantile,0.3);
Pu = qnorm(0.3,1,1);
err = PX - Pu;
boxplot(err);      

################################################################# 

x=rnorm(100);
y=rt(100,10);
Data=cbind(x,y);
boxplot(Data);    

#################################################################

 X = matrix(rnorm(10000),100,100);
 EX = colMeans(X);         
 EX = apply(X,2,mean);     
 MX = apply(X,2,median);   
 Data = cbind(EX,MX);      
 boxplot(Data);            

#################################################################

N=200;n=30;   
X = matrix(rnorm(N*n,1,sd=3),n,N);
EX = colMeans(X);
SX = apply(X,2,sd);
UX = (EX - 1)/(3/sqrt(n));
TX = (EX - 1)/(SX/sqrt(n));
Data = cbind(UX,TX);
boxplot(Data);

#################################################################

X = rbinom(100,10,0.4);
Eu = 10 * 0.4;
Du = 100* 0.4 * (1-0.4);
UX = (X - Eu)/(sqrt(Du));
Data = cbind(X,UX);
boxplot(Data);    ####   

############################################################################

Vector_Standard = as.numeric(vector(length = 1000));
for (k in 1:1000){
    Number_Sample = 500;
    Parameter_P = 0.8;
    SampleX =rbinom(Number_Sample,1,Parameter_P);
    Estimator_P = mean(SampleX);
    Delta = sqrt(Estimator_P*(1-Estimator_P)/Number_Sample);
    Value_Standard = (Estimator_P-Parameter_P)/Delta;
    Vector_Standard[k]= Value_Standard;}
hist(Vector_Standard);

#################################################################

x1 = qnorm(0.05);   x2 = qnorm(0.95);
print(c(x1,x2));
y1 = qnorm(0.025);  y2 = qnorm(0.975);
print(c(y1,y2));

#################################################################

X = matrix(rnorm(1000),100,10);
Y = matrix(nrow=100,ncol=10);
      ####   
for(k in 1:10){Y[,k]=rnorm(100)};
boxplot(cbind(X,Y));     

#################################################################

X = matrix(rnorm(1000),100,10);
Z = rnorm(120);
Y = matrix(nrow=100,ncol=10);
for(k in 1:10){Y[,k]=sample(Z,100,replace=TRUE)};
boxplot(cbind(X,Y))


#################################################################

X = sort(rnorm(100));
n = 1:100;
Y = (n+0.5)/(100+1);   Z = pnorm(X);
Data = cbind(Y,Z);
matplot(X,Data,pch = 16);

#################################################################

u = seq(from=0,to=1,length.out=50); 
T = matrix(nrow = 50,ncol=100) 
for (k in 1:50){ 
  X = matrix(rnorm(10000),100,100);
  MX = colMeans(X);
  U =(MX - u[k])/(1/sqrt(100)); 
  T[k,] = U;}
boxplot(t(T));

############################################################################

Matrix_U = matrix(nrow = 1000,ncol = 2);
for (k in 1:1000){
    Number_Sample = 500;
    SampleX =rnorm(Number_Sample,0,1)
    Delta_L = qnorm(0.025)/sqrt(Number_Sample);
    Delta_U = qnorm(0.975)/sqrt(Number_Sample);
    Estimator_L = mean(SampleX) + Delta_L;
    Estimator_U = mean(SampleX) + Delta_U;
    Matrix_U[k,]= c(Estimator_L,Estimator_U);}
Sign = (Matrix_U[,1] < 0)&(Matrix_U[,2] > 0);
Frequency = sum(Sign)/1000;

############################################################################

Ratio_U <- function(n,mu){
    N=100; sigma = 1;
    Matrix_LU = matrix(nrow=N,ncol=2);
    for(k in 1:N){
        Sample = rnorm(n,mu,sigma);   
        LU_k = LU_U(Sample,sigma)
        Matrix_LU[k,] = LU_k;}
    Index_LU = (Matrix_LU[,1]<mu)&(Matrix_LU[,2]>mu);
    Value_Ratio = sum(Index_LU)/N;
    return(Value_Ratio)}

LU_U = function(Sample,sigma){ 
    qU = qnorm(0.975);
    n = length(Sample)
    P_L = mean(Sample) - qU*sigma/sqrt(n)
    P_U = mean(Sample) + qU*sigma/sqrt(n)
    vector_LU = c(P_L,P_U);
    return(vector_LU);}

################################################################# 

Ratio_T <- function(n,mu,sigma){
    N=100;     
    Matrix_LU = matrix(nrow=N,ncol=2);
    for(k in 1:N){
      x=rnorm(n,mu,sigma);   
      LU_k = LU_T(x)
      Matrix_LU[k,] = LU_k;}
    Index_LU = (Matrix_LU[,1]<mu)&(Matrix_LU[,2]>mu);
    Value_Ratio = sum(Index_LU)/N;
    return(Value_Ratio)}

LU_T <- function(x){ 
    S = sd(x);
    n = length(x);
    qT = qt(0.975,n-1);
    P_L = mean(x) - qT*S/sqrt(n);
    P_U = mean(x) + qT*S/sqrt(n);
    vector_LU = c(P_L,P_U);
    return(vector_LU);}

############################################################################

  Matrix_Pearson = matrix(nrow=100,ncol=2);
  p0=0.3;
  for (m in 1:100){
      n=20;
      x=rbinom(n,1,p0);
      k=sum(x);
      LU_k = LU_pearson(n,k);
      Matrix_Pearson[m,]=LU_k;}
  matplot(1:100,Matrix_Pearson,type="o",pch="*");

  LU_pearson = function(n,k){
      p = seq(from=0,to=1,length=100);
      p1 = pbinom(k,n,p);
      I_U = which.min(abs(p1-0.025));
      p_U = p[I_U];
      p2 = 1-pbinom(k,n,p);
      I_L = which.min(abs(p2-0.025));
      p_L = p[I_L];
      vector_LU = c(p_L,p_U);
      return(vector_LU);}

#################################################################

    Ratio_Wald <- function(n,p0){
    N=2000;
    Matrix_LU = matrix(nrow=N,ncol=2);
    for(Index in 1:N){
        k = rbinom(1,n,p0);
        LU_k = LU_Wald(n,k)
        Matrix_LU[Index,] = LU_k;}
    Index_LU = (Matrix_LU[,1]<p0)&(Matrix_LU[,2]>p0);
    Value_Ratio = sum(Index_LU)/N;
    return(Value_Ratio)}

    LU_Wald = function(n,k){ 
       qU = qnorm(0.975)
       P_L = k/n - qU*sqrt(k/n*(1-k/n)/n);
       P_U = k/n + qU*sqrt(k/n*(1-k/n)/n)
       vector_LU = c(P_L,P_U);
       return(vector_LU);}

################################################################# 

   Wald_Plot <- function(n){
     Number_P = 200;
     Vector_p = seq(0,1,length=Number_P);
     M_P = matrix(nrow=Number_P,ncol=2);
     for (k in 1:Number_P){
         p_k = Vector_p[k];
         s_k = Ratio_Wald(n,p_k);
         M_P[k,] = c(p_k,s_k);}
    plot(M_P[,1],M_P[,2]);}

################################################################# 

 Number_Sample = 100; Number_Group = 10;
 Number_Repeat = 1000;
 Vector_Q = vector(length=Number_Repeat);
 for(k in 1:Number_Repeat){
    Vector_Q[k] = Sample_Q(Number_Sample,Number_Group);}
 hist(Vector_Q); print(quantile(Vector_Q,0.95))

 Sample_Q = function(Number_Sample,Number_Group){
 X=matrix(rnorm(Number_Sample*Number_Group),ncol=Number_Group);
 MX = colMeans(X)
 SSE = 100*apply(X,2,var);
 MSE = sqrt(sum(SSE)/1000)
 Index_Total = combn(10,2)
 Number_Pair = ncol(Index);
 Temp_Pair = vector(length=Number_Pair)
 for (k in 1:Number_Pair){
    Index_Compare = Index_Total[,k];
    Index_A = Index_Compare[1];
    Index_B = Index_Compare[2];
    Sample_Q = abs(MX[Index_A]-MX[Index_B])/(MSE/sqrt(Number_Sample));
    Temp_Pair[k]=Sample_Q;
    Sample_Q = max(Temp_Pair);
  return(Sample_Q);}}


################################################################# 


 a = 0.8; b = 1.0;
 X = 1.2*runif(1000); e = 5*rnorm(1000); Y = a*X+b+e;
 S_Reg = lm(Y~X); summary(S_Reg);
 plot(X,Y); abline(S_Reg);
       ####  

############################################################################

