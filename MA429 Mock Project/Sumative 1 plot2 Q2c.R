x0 =  seq(-5,5,length = 100)

plot(x0, (1/11)*dnorm(x0, mean = -1, 1), type = 'l', ylim = c(0,0.4))

lines(x0, (10/11)*dnorm(x0, mean = 1, 1), type = 'l', col = 'green')

x2 = rep(-1.1532, 2)
y2 = seq(0,1,length=2)
lines(x2,y2, col = 'blue')
 
