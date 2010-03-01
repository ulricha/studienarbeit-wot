fdir <- "~/dev/studienarbeit-wot/results/fastmod/"
ldir <- "~/dev/studienarbeit-wot/results/label-prop/"
cdir <- "~/dev/studienarbeit-wot/results/copra/"
fastmod <- scan(paste(fdir, "TIME_CORR_sizes.dat", sep=""))
label <- scan(paste(ldir, "TIME_CORR_sizes.dat", sep=""))
copra <- scan(paste(cdir, "TIME_CORR_sizes.dat", sep=""))
plot(table(fastmod), log="x", type="p", pch=0, xlim=range(fastmod, label, copra), ylim=range(table(fastmod), table(label), table(copra)), col="red", xlab="Grösse", ylab="Anzahl")
points(table(label), type="p", pch=1, col="green")
points(table(copra), type="p", pch=2, col="blue")
legend(x="topright", legend=c("FM", "LP", "COPRA"), col=c("red", "green", "blue"), pch=c(0,1,2))
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/time_corr_dist.pdf", type="pdf")