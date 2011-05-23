#cs <- scan("/Users/ulricha/dev/studienarbeit-wot/results/basic-properties/component_size_values.dat")
#l <- c(1,max(cs))
#yl <- c(1,max(table(cs)))
#par(lab=c(5,6,7))
#plot(table(cs), xlim=xl, log="xy", ylim=yl, ylab="Anzahl", xlab="Grösse", cex=0.8, type="p", pch=2, col="blue")
#quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/component-size.pdf", type="pdf")

id <- scan("/Users/ulricha/dev/studienarbeit-wot/analyze/component_size_values.dat")
xl <- c(1,max(id))
yl <- c(1,max(table(id)))

#par(lab=c(5,7,7))
#plot(table(id), xlim=oxl, log="xy", ylim=oyl, ylab="quantity", xlab="degree", cex=0.8, type="p", pch=2, col="blue")
xl <- c(1, max(id))
yl <- c(1, max(table(id)))
plot(table(id), log="xy", xlim=xl, ylim=yl, xlab="component size", ylab="quantity", type="p", pch=2, col="blue")
#lines(table(od), lty=)
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/component-size-dist.pdf", type="pdf")


