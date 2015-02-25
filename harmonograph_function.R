##Harmonograoh Funnction##
harmonograph <- function(nr = 10, path = "harmonographs", density = 100) {

cat("Plotting harmonograph in ", nr, "repetitions and with a pencil density of", density, "\n")


for (i in 0:nr) {

cat("calculating time parameter f...\n")
f1=jitter(sample(c(2,3),1));f2=jitter(sample(c(2,3),1));f3=jitter(sample(c(2,3),1));f4=jitter(sample(c(2,3),1))
cat(" f1 = ", f1, "\n", "f2 = ",g f2, "\n", "f3 = ", f3, "\n", "f4 = ", f4, "\n")

cat("calculating damping parameter d...\n")
d1=runif(1,0,1e-02);d2=runif(1,0,1e-02);d3=runif(1,0,1e-02);d4=runif(1,0,1e-02)
cat(" d1 = ", d1, "\n", "d2 = ", d2, "\n", "d3 = ", d3, "\n", "d4 = ", d4, "\n" )

change 1

cat("calculating phase parameter p...\n")
p1=runif(1,0,2*pi);p2=runif(1,0,2*pi);p3=runif(1,0,2*pi);p4=runif(1,0,2*pi)
cat(" p1 = ", p1, "\n", "p2 = ", p2, "\n", "p3 = ", p3, "\n", "p4 = ", p4, "\n" )

xt = function(t) exp(-d1*t)*sin(t*f1+p1)+exp(-d2*t)*sin(t*f2+p2)
yt = function(t) exp(-d3*t)*sin(t*f3+p3)+exp(-d4*t)*sin(t*f4+p4)
t=seq(1, density, by=.001)
dat=data.frame(t=t, x=xt(t), y=yt(t))

cat("creating graphs...\n")
pdf(file = paste(path,"/harmonograph_", i, ".pdf", sep=""))
with(dat, plot(x,y, type="l", xlim =c(-2,2), ylim =c(-2,2), xlab = "", ylab = "", xaxt='n', yaxt='n'))
dev.off()
}
}
