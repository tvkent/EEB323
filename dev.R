library(plyr);library(reshape);library(ggplot2);library(magrittr);library(viridis)

rowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

simple_drift <- function(p0,N,gens,runs) {
  results.df <- data.frame(matrix(ncol=runs))
  results.df[1,(1:runs)] <- rep(p0,runs)
  
  for(i in 1:gens){ #running generation by generation
    for(j in 1:runs){ #one run at a time
      p <- results.df[i,j] #initialize the allele freq as that of the previous generation
 
      if(p>0 && p<1){ #if alleles are not fixed
        Paa <- p*p
        Pab <- 2*p*(1-p)
        
        Naa <- rbinom(1,N,Paa)
        if(Paa<1){
          Nab <- rbinom(1,(N-Naa),(Pab/(1-Paa)))
        }
        else{
          Nab <- 0
        }
        p <- ((2*Naa)+Nab)/(2*N)
        q <- 1-p
        
        results.df[i+1,j] <- p
      }
      else{ #if alleles fixed
        if(p<=0){
          p <- 0
        }
        else{
          p <- 1
        }
        results.df[i+1,j] <- p
      }
    }
  }
  names <- c()
  for(i in 1:runs){names[i]<-paste0("p",i)}
  colnames(results.df) <- names
  results.df$meanp <- rowMeans(results.df[1:runs])
  results.df$varp <- rowVar(results.df[1:runs])
  results.df$gen <- 0:gens
  return(results.df)
}

meltdata <- function(results.df,gens,runs) {
  df <- melt(results.df,id.vars = "gen")

  return(df[(1:((gens+1)*runs)),])
}

plotSim <- function(df,runs,gen) {
  print(ggplot(df,aes(x=gen,y=value,col=variable))+
          theme_bw()+ylim(0,1)+
          theme(legend.position="none",
                panel.grid.minor=element_blank(),
                axis.text=element_text(size=12),
                axis.title=element_text(size=12),
                strip.background = element_blank(),
                strip.text=element_text(size=12))+
          scale_color_viridis(discrete=T)+
          xlab("Generations")+ylab("")+
          geom_line())
}

getibd <- function(N,gens) {
  ft <- (1-((1-(1/(2*N)))^gens))
  ht <- 1-ft
  ft.ht <- c(ft,ht)
  return(ft.ht)
}
