io <- scan("dev/studienarbeit-wot/results/degree-correlation/in-out-correlation.dat")
plot(ecdf(inout), log="x", xlim=c(0.001, 400), main="", xlab="ratio indegree/outdegree")
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/inoutcorr.pdf", type="pdf")
knn <- scan("/Users/ulricha/dev/studienarbeit-wot/results/degree-correlation/knn.dat")
knn_edit <- knn[1:1684]
plot(knn_edit, log="xy", xlab="outdegree", ylab="knn")
quartz.save("dev/studienarbeit-wot/ausarbeitung/images/knn.pdf", type="pdf")