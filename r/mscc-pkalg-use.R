mscc_rsa <- read.table("dev/studienarbeit-wot/results/time/mscc-pkalg_use_stats-1")
mscc_dsa <- read.table("dev/studienarbeit-wot/results/time/mscc-pkalg_use_stats-17")
rsa_ts <- ts(mscc_rsa[2], start=c(1992, 5), frequency=12)
dsa_ts <- ts(mscc_dsa[2], start=c(1992, 5), frequency=12)
plot(smooth.spline(dsa_ts), type="l", xlab="time", ylab="number of new keys", lty=1)
lines(smooth.spline(rsa_ts), lty=2)
legend("topleft", c("DSA", "RSA"), lty=c(1,2))
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/mscc-pkalg-use.pdf", type="pdf")


