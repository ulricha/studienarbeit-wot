pl <- scan("dev/studienarbeit-wot/results/basic-properties/scc-44952_avg_dist.values")
plot(table(pl), xlab="durchschnittliche Distanz", ylab="Anzahl")
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/pathlen-dist.pdf")