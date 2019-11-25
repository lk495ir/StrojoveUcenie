IWP <- function(x,y,p,w1,w2,w0,niter){
  PN = length(p)
 
  z = c()
  for (i in 1:PN)
  {
    if(p[i] < 0)
    {
      z[i]="red"
    }
    else 
    {
      z[i]="green"
    }
  }
  
  plot(x,y,cex=1.5,xlim = c(0,max(x)+1),ylim = c((min(y)-2),max(y+1)),pch = 16,col=z,main="IWP")
  legend("topleft",legend = c("Pozitivne","Negativne"),col=c("green","red"),cex = 0.8,pch=16)
  counter = 1
  Score  = 0.5
  for (q in 1 : niter)
  {
    if (counter == 1)
    {
      x0 = c()
      for (i in 1:PN)
      {
        x0[i]=-1
      }
      U0j = (w1*x+w2*y)/x0
      U0jc = sort(U0j)
      w0c = 0
      for (i in 2:length(U0jc))
      {
        w0c[i-1] = ((U0jc[i]+U0jc[i-1])/2)*(-1)
      }
      for (i in 1 : length(w0c))
      {
        V = w2*y+x*w1+x0*w0c[i];
        Poz = 0
        Neg = 0 
        for (j in 1: length(V))
        {
          if (V[j] > 0 && p[j] >0)
          {
            Poz = Poz +1
          }
          else if (V[j] < 0 && p[j] < 0)
          {
            Neg = Neg +1
          }
          else 
          {
            
          }
        }
        Scorec = (Poz+Neg)/PN
        if (Score <= Scorec)
        {
          Score = Scorec
          w0 = w0c[i]
        }
      }
      counter = 0
    }
    else if (counter == 0)
    {
      x0 = c()
      for (i in 1:PN)
      {
        x0[i]=-1
      }
      U1j = (w2*y+w0*x0)/x
      U1jc = sort(U1j)
      w1c = 0
      for (i in 2:length(U1jc))
      {
        w1c[i-1] = c(((U1jc[i]+U1jc[i-1])/2)*(-1))
      }
      for (i in 1 : length(w1c))
      {
        V = w2*y+x*w1c[i]+x0*w0;
        Poz = 0
        Neg = 0
        for (j in 1: length(V))
        {
          if (V[j] > 0 && p[j] > 0)
          {
            Poz = Poz +1
          }
          else if (V[j] < 0 && p[j] < 0)
          {
            Neg = Neg +1
          }
          else 
          {
            
          }
        }
        Scorec = (Poz+Neg)/PN
        if (Score <= Scorec)
        {
          Score = Scorec
          w1 = w1c[i]
        }
      }
      counter = 1
    }
    
  }

  cat ("H_SET={ ", w1,"x1 +", w2,"x2 >= ", w0, " }, Score:", Score )
  return((w0-w1*x)/w2) 
}

x1 = c(1.75,2.0,2.5,3.0)
x2 = c(6.0,5.0,5.0,6.25)
pozit = c(1,1,-1,-1)

poloha = cbind (x1,x2,pozit)

x3 = c(36,42,85,2,11,84,46,90,19,54,36,68,59,4,64,50,37,50,9,30,80,62,76,27,98,97,67,52,9,34,32,36,5,40,66,71,51,31,83,65,11,71,59,72,10,
       60,72,97,38,87,68,33,94,95,78,25,6,73,62,79,57,1,68,73,87,47,9,84,19,100,16,36,34,100,21,55,95,64,4,20,72,91,15,68,43,63,79,55,53,
       63,88,62,6,100,77,46,55,21,36,61)

x4 = c(85,78,36,50,35,7,31,53,76,79,25,75,47,80,79,57,96,17,78,72,23,59,51,21,38,27,86,63,1,59,32,39,32,65,98,6,44,50,52,82,95,60,29,61,
       24,82,66,57,1,99,19,4,66,53,35,29,78,47,56,14,8,59,64,93,51,82,84,5,33,81,30,24,50,72,83,88,43,49,3,31,48,59,95,92,21,58,95,96,47,
       49,96,99,90,59,58,82,70,81,10,25)

pozit2 = c()

dlzkax = length(x3)

for (i in 1:dlzkax)
{
  if(x3[i] < dlzkax/2)
  {
    pozit2[i] = 1
  }
  else
  {
    pozit2[i] = -1
  }
}

w1p = -0.5
w2p = 1
w0p = 6

priam <- IWP(x3,x4,pozit2,w1p,w2p,w0p,1000)

lines(x3,priam)

