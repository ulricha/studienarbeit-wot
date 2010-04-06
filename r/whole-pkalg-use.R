rsa <- read.table("dev/studienarbeit-wot/results/time/whole_graph-pkalg_use_stats-1")
dsa <- read.table("dev/studienarbeit-wot/results/time/whole_graph-pkalg_use_stats-17")
par(lab=c(8,8,7))
rsa_ts <- ts(rev(rsa[2][,1]), start=c(1992, 5), frequency=12)
dsa_ts <- ts(rev(dsa[2][,1]), start=c(1992, 5), frequency=12)
plot(smooth.spline(dsa_ts), ylim=c(1, max(dsa_ts)), type="l", xlab="Zeit", ylab="Anzahl neuer Schlüssel", lty=1)
lines(smooth.spline(rsa_ts), lty=2)
legend("topright", c("DSA", "RSA"), lty=c(1,2))
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/whole-pkalg-use.pdf", type="pdf")



