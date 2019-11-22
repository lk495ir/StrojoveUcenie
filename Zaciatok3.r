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
  
  plot(x,y,cex=1.5,xlim = c(0,max(x)+1),ylim = c((min(y)-2),max(y+1)),pch = 16,col=z)
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
  
  
  
  cat ("H={ ", w1,"x1 +", w2,"x2 >= ", w0, " }, Score:", Score )
  return((w0-w1*x)/w2) 
}

x1 = c(1.75,2.0,2.5,3.0)
x2 = c(6.0,5.0,5.0,6.25)
pozit = c(1,1,-1,-1)

poloha = cbind (x1,x2,pozit)

w1p = -0.5
w2p = 1
w0p = 6

priam <- IWP(x1,x2,pozit,w1p,w2p,w0p,6)

lines(x1,priam)

