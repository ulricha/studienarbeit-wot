fdir <- "~/dev/studienarbeit-wot/results/fastmod/"
ldir <- "~/dev/studienarbeit-wot/results/copra-1/"
cdir <- "~/dev/studienarbeit-wot/results/copra/"
bl2dir <- "~/dev/studienarbeit-wot/results/blondel-l2/"
bl5dir <- "~/dev/studienarbeit-wot/results/blondel-l5/"
copra1 <- scan(paste(ldir, "TIME_CORR_sizes.dat", sep=""))
copra <- scan(paste(cdir, "TIME_CORR_sizes.dat", sep=""))
bl2 <- scan(paste(bl2dir, "TIME_CORR_sizes.dat", sep=""))
bl5 <- scan(paste(bl5dir, "TIME_CORR_sizes.dat", sep=""))
xl <- c(3, max(max(copra), max(copra1), max(bl2), max(bl5)))
yl <- c(1, max(max(table(fastmod), table(copra), table(copra1), table(bl2), table(bl5))))

plot(table(copra), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="COPRA v=3", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/time-corr_copra.pdf", type="pdf")
plot(table(copra1), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="COPRA v=1", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/time-corr_copra1.pdf", type="pdf")
plot(table(bl2), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="BL l=2", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/time-corr_bl2.pdf", type="pdf")
plot(table(bl5), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="BL l=5", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/time-corr_bl5.pdf", type="pdf")
