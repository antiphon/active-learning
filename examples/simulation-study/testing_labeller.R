source("../algorithm/shot_noise_labeller.R")

source("data.R")


xseq <- cbind(seq(-5, 9, length=100))
ys <- factor(sample(levels(ypool), length(xseq), replace=TRUE))

labeller <- create_shot_noise_labeller(xseq, ys, Xknown, kernel=kernel_make("gaussian"))

# testing

l <- labeller(xseq)
a <- l$a

h <- l$h

plot(xseq, a, "l", ylim=c(0,100))

abline(v=Xknown)
image(h)


library(seriation)

ser <- seriate(dist(h))

pimage(dist(h),ser)
