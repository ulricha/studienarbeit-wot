fdir <- "~/dev/studienarbeit-wot/results/fastmod/"
ldir <- "~/dev/studienarbeit-wot/results/label-prop/"
cdir <- "~/dev/studienarbeit-wot/results/copra/"
bl2dir <- "~/dev/studienarbeit-wot/results/blondel-l2/"
bl5dir <- "~/dev/studienarbeit-wot/results/blondel-l5/"
label <- scan(paste(ldir, "SLD_MAYBE_ASS_sizes.dat", sep=""))
copra <- scan(paste(cdir, "SLD_MAYBE_ASS_sizes.dat", sep=""))
bl2 <- scan(paste(bl2dir, "SLD_MAYBE_ASS_sizes.dat", sep=""))
bl5 <- scan(paste(bl5dir, "SLD_MAYBE_ASS_sizes.dat", sep=""))
xl <- c(3, max(max(copra), max(label), max(bl2), max(bl5)))
yl <- c(1, max(max(table(fastmod), table(copra), table(label), table(bl2), table(bl5))))

plot(table(copra), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="COPRA", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/sld-maybe-ass_copra.pdf", type="pdf")
plot(table(label), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="LP", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/sld-maybe-ass_label.pdf", type="pdf")
plot(table(bl2), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="BL l=2", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/sld-maybe-ass_bl2.pdf", type="pdf")
plot(table(bl5), log="xy", col="blue", xlab="Grösse", ylab="Anzahl", cex=0.8, pch=2, xlim=xl, ylim=yl, type="p")
legend(x="topright", legend="BL l=5", col="blue", pch=2)
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/sld-maybe-ass_bl5.pdf", type="pdf")
