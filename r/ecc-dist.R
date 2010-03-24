ec <- scan("dev/studienarbeit-wot/results/basic-properties/scc-44952_ecc.values")
plot(table(ec), xlab="Eccentricity", ylab="Anzahl")
quartz.save("/Users/ulricha/dev/studienarbeit-wot/ausarbeitung/images/ecc-dist.pdf")