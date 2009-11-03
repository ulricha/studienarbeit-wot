#!/bin/bash

#PBS -M ulricha@informatik.uni-tuebingen.de
#PBS -m abe
#PBS -N wot-bp-mpi
#PBS -l nodes=5:ppn=4,pvmem=2000mb,walltime=00:20:00
PATH=$PATH:$HOME/wot/godi/bin:$HOME/wot/godi/sbin:/opt/bwgrid/mpi/openmpi/1.2.8-gcc/bin/
export PATH
LD_LIBRARY_PATH=/opt/bwgrid/mpi/openmpi/1.2.8-gcc/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
EXE=$HOME/wot/studienarbeit-wot/_build/graph/basic_properties_mpi.native
DATE=`date +%d%m%y`
VERTEX=$HOME/wot/studienarbeit-wot/vertex.sexp
EDGE=$HOME/wot/studienarbeit-wot/edge.sexp

module load openmpi/1.2.8

cd $PBS_O_WORKDIR
mpirun $EXE $VERTEX $EDGE &> basic_properties_$DATE.log
