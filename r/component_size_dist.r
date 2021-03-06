#cs <- scan("/Users/ulricha/dev/studienarbeit-wot/results/basic-properties/component_size_values.dat")
#l <- c(1,max(cs))
#yl <- c(1,max(table(cs)))
#par(lab=c(5,6,7))
#plot(table(cs), xlim=xl, log="xy", ylim=yl, ylab="Anzahl", xlab="Grösse", cex=0.8, type="p", pch=2, col="blue")
#quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/component-size.pdf", type="pdf")

id <- scan("/Users/ulricha/dev/studienarbeit-wot/results/basic-properties/scc-44952_indeg.values")
xl <- c(1,max(id))
yl <- c(1,max(table(id)))
par(lab=c(5,7,7))
plot(table(id), xlim=xl, log="xy", ylim=yl, ylab="Anzahl", xlab="eingehender Grad", cex=0.8, type="p", pch=2, col="blue")
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/indegree-dist.pdf", type="pdf")

od <- scan("/Users/ulricha/dev/studienarbeit-wot/results/basic-properties/scc-44952_outdeg.values")
xl <- c(1,max(od))
yl <- c(1,max(table(od)))
par(lab=c(5,7,7))
plot(table(od), xlim=xl, log="xy", ylim=yl, ylab="Anzahl", xlab="ausgehender Grad", cex=0.8, type="p", pch=2, col="blue")
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/outdegree-dist.pdf", type="pdf")

