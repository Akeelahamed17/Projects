x0 =  seq(-5,5,length = 100)
plot(x0,dnorm(x0, mean = -1, 1), type='l')

lines(x0, dnorm(x0,mean =1,1), col = 'blue')

x2 = rep(0,2)
y2 = c(0,1)
lines(x2,y2, col = 'pink')
