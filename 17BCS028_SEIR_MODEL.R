library (igraph)

##############################################################import graph

g = read_graph ("/home/samroadie/Desktop/sna_project/karate.gml", format = "gml")
g = read_graph ("/home/samroadie/Desktop/sna_project/dolphin.txt",format ="edgelist",directed=FALSE)
V(g)
g = g - V(g)[which(degree(g)==0)]
V(g)


#################################################################SEIR MODEL
infect <- function(g,I,t_e,t_i,t_r,p,iter){
  
  V(g)$state[I] = "I"
  V(g)$time[V(g)$state=="I"] = t_i
  count = 0
  while(iter){
    print("ITERATION")
    #print(iter)
    for(i in V(g)[which(V(g)$state == "I")]){
      for (j in neighbors(g,I)) {
        r= runif(1)
        if  (r<p && V(g)$state[j]=="S"){
         
          V(g)$state[j] = "E"
          V(g)$time[j] = t_e 
        }
      }
    }
    #Transition of I to R
    for(i in which(V(g)$state == "I")){
      if(V(g)$time[i]>=0)
      {
        V(g)$time[i] = V(g)$time[i] - 1
      }
      else
      {
        V(g)$state[i] = "R"
        V(g)$time[i] = t_r
      }  
    }
    #Transition of E to I
    for(i in which(V(g)$state == "E")){
      if(V(g)$time[i]>=0)
      {
        V(g)$time[i] = V(g)$time[i] - 1
      }
      else
      {
        V(g)$state[i] = "I"
        V(g)$time[i] = t_i
      }  
    }
    #Transition of R to S
    for(i in which(V(g)$state == "R")){
      if(V(g)$time[i]>=0)
      {
        V(g)$time[i] = V(g)$time[i] - 1
      }
      else
      {
        V(g)$state[i] = "S"
        V(g)$time[i] = 0
      }  
    }
    
    #print current states
    print("Infected")
    print(V(g)[which(V(g)$state == "I")]) 
    
    
    print("Susceptible")
    print(V(g)[which(V(g)$state == "S")])  
    
    
    print("Exposed")
    print(V(g)[which(V(g)$state == "E")])
    
    print("Recovered")
    print(V(g)[which(V(g)$state == "R")])
    
    V(g)$color[V(g)$state == "I"] = "red"
    V(g)$color[V(g)$state == "R"] = "blue"
    V(g)$color[V(g)$state == "S"] = "green"
    V(g)$color[V(g)$state == "E"] = "yellow"
    count = count + 1
    plot (g, vertex.color = V(g)$color, main = paste ("Day", count)) 
    
    iter = iter - 1
  }
  
  
  return (V(g)[which(V(g)$state=="I")])
  
}
### calling the function
g
plot(g)
t_e = 1
t_i = 1
t_r = 1
V(g)$state = "S"
V(g)$state
V(g)$time[V(g)$state == "S"] = 0
V(g)$time
I <- c(3,4,11)
infect(g,I,t_e,t_i,t_r,0.3,5)
############################################################## K-SHELL DECOMPOSITION
it = 1
c = coreness(g)
v = length(V(g))
i = 1
buckets <- list()
while(v>0)
{
  
  if (it %in% c)
  {
    tempo = c()
    tempo = c(tempo,V(g)[which(coreness(g)==it)])
    buckets[[i]] = tempo
    v = v - length(V(g)[which(coreness(g)==it)])
    print("v is ")
    print(v)
    i=i+1
  }
  it = it+1
}

print(buckets)



################################# CALCULATING CASCADING POWER
shell <- c()
caspower <- c()
V(g)$state = "S"
V(g)$state
V(g)$time[V(g)$state == "S"] = 0
V(g)$time
t_e = 1
t_i = 1
t_r = 1
for( l in 1:length(buckets)){
  shell <- c(shell,l)
  print(l)
  inspow <- c()
  for (m in buckets[[l]]){
    print(m)
    seed <- m
    print("ok1")
    list1 = infect(g,seed,t_e,t_i,t_r,0.3,5)
    print("ok2")
    len <- length(list1)
    print("ok3")
    inspow <- c(inspow,len)
    print(inspow)
    print("ok4")
  }
  caspower <- c(caspower ,mean(inspow))

}

df <- cbind(shell, caspower)
df <- as.data.frame(df)
df
library(ggplot2)
ggplot(df,aes(x = shell,y = caspower)) + geom_line(color = "red")

