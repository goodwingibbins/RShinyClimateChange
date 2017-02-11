plot1 <- function(t,CO2.emissions){
  
  
  out = qplot(t,CO2.emissions,geom='line',colour = 'blue',xlab = 'Time',ylab='CO2 Emissions',xlim = c(1,100),ylim=c(0,100))
  
  out =ggplot(data.frame(Time=t,CO2 = CO2.emissions),aes(x=Time,y=CO2))+
    geom_line(color='red',size=1.5)+
    theme(panel.background = element_blank())
  
  
  return(out)
  
  
}

plot2 <- function(t,CH4.emissions){
  
  
  out = plot(t,CH4.emissions,xlim = c(1,100),ylim=c(0,100))
  
  return(out)
  
  
}

plot3 <- function(concs){
  
  
  out = plot(concs)
  
  return(out)
  
  
}

plot4 <- function(t,CH4){
  
  
  out = plot(t,CH4)
  
  return(out)
  
  
}

plot5 <- function(t,Temp){
  
  
  out = plot(t,Temp)
  
  return(out)
  
  
}