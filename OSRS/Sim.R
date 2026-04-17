

AtkSim = function(A,S){
  Spawn = c(0,0,0);  Health = rep(100,3);  target = 0;  delay = 4
  Kills = 0;  Dmg = 0;
  
  for(i in 1:60000){
    delay = delay - (delay>0)
    Spawn = Spawn - (Spawn>0)
    if(target==0){
      if(sum(Spawn==0)>0){ target = match(TRUE,Spawn==0) }
    }
    if(target!=0){
      if(delay==0){
        delay = 4
        dmg = min(Health[target],floor((S+1)*runif(1))*(runif(1)<A))
        Health[target] = Health[target] - dmg;  Dmg = Dmg + dmg
        if(Health[target]==0){
          Health[target] = 100;  Spawn[target] = 50;  target = 0;  Kills = Kills + 1
        }
      }
    }
  }
  return(c(Kills,Dmg,Dmg/15000))
}

AtkSim(1,25)











ClueCasket = function(p,d,a,n){
  A = numeric(1000)
  for(i in 1:1000){
    w = TRUE;  m = 0
    N = matrix(0,a,n)
    while(w){
      m = m + 1;
      u = runif(d)
      for(j in 1:(a*n)){
        N[j] = N[j] + sum(u<p)
        u = u - p;  u[u<0] = 1
      }
      N[N>0] = 1
      if(sum(rep(1,a)%*%N>=a)){ w = FALSE }
    }
    print(i)
    A[i] = m
  }
  return(A)
}

A = ClueCasket(1/1625,5,4,8)
hist(A)
median(A)
sort(A)[950]
ClueCasket(1/1625,5,1,6)


















