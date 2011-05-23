pl <- scan("dev/studienarbeit-wot/results/basic-properties/scc-44952_avg_dist.values")
plot(table(pl), xlab="average distance", ylab="quantity")
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/pathlen-dist.pdf")