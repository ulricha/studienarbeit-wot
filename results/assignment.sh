for alg in fastmod label-prop cliquemod-3000 cliquemod-500 copra; do
    COMNUM=`grep SIG_TIME_CORR $alg/inv.log | wc -l`
    echo $alg size $COMNUM
    for target in TLD_SURE_ASS TLD_MAYBE_ASS SLD_SURE_ASS SLD_MAYBE_ASS; do
	COUNT=`grep $target $alg/inv.log | wc -l`
	echo $alg $target $COUNT `echo "scale=3; $COUNT/$COMNUM" | bc`
	grep $target $alg/inv.log | awk '{ print $3 }' > $alg/$target_sizes.dat
    done

    COUNT=`grep SLD_FREEMAIL_ASS $alg/inv.log | wc -l`
    echo $alg SLD_FREEMAIL_ASS $COUNT `echo "scale=3; $COUNT/$COMNUM" | bc`
    COUNT=`grep HAS_SIG_TIME_CORR $alg/inv.log | wc -l`
    echo $alg HAS_SIG_TIME_CORR $COUNT `echo "scale=3; $COUNT/$COMNUM" | bc`
    grep HAS_SIG_TIME_CORR $alg/inv.log | awk '{ print $6 }' > $alg/TIME_CORR_sizes.dat
done