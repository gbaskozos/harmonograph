#A harmonograph is a mechanical apparatus that employs pendulums to create a geometric image. The drawings created typically are Lissajous curves, or related drawings #of greater complexity. The devices, which began to appear in the mid-19th century and peaked in popularity in the 1890s, cannot be conclusively attributed to a single #person, although Hugh Blackburn, a professor of mathematics at the University of Glasgow, is commonly believed to be the official inventor.
#
#A simple, so-called "lateral" harmonograph uses two pendulums to control the movement of a pen relative to a drawing surface. One pendulum moves the pen back and #forth along one axis and the other pendulum moves the drawing surface back and forth along a perpendicular axis. By varying the frequency and phase of the pendulums #relative to one another, different patterns are created. Even a simple harmonograph as described can create ellipses, spirals, figure eights and other Lissajous #figures.
#
#More complex harmonographs incorporate three or more pendulums or linked pendulums together (for example hanging one pendulum off another), or involve rotary motion #in which one or more pendulums is mounted on gimbals to allow movement in any direction.
######################################################################################################################################################################

cat("Harmonograoh\n")
f1=jitter(sample(c(2,3),1));f2=jitter(sample(c(2,3),1));f3=jitter(sample(c(2,3),1));f4=jitter(sample(c(2,3),1))
d1=runif(1,0,1e-02);d2=runif(1,0,1e-02);d3=runif(1,0,1e-02);d4=runif(1,0,1e-02)
p1=runif(1,0,pi);p2=runif(1,0,pi);p3=runif(1,0,pi);p4=runif(1,0,pi)
xt = function(t) exp(-d1*t)*sin(t*f1+p1)+exp(-d2*t)*sin(t*f2+p2)
yt = function(t) exp(-d3*t)*sin(t*f3+p3)+exp(-d4*t)*sin(t*f4+p4)
t=seq(1, 100, by=.001)
dat=data.frame(t=t, x=xt(t), y=yt(t))

pdf("harmonograph.pdf")
with(dat, plot(x,y, type="l", xlim =c(-2,2), ylim =c(-2,2), xlab = "", ylab = "", xaxt='n', yaxt='n'))
dev.off()
