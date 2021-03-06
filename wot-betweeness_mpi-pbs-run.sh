#!/bin/bash

#PBS -M ulricha@informatik.uni-tuebingen.de
#PBS -m abe
#PBS -N wot-bet-mpi
#PBS -l nodes=4:ppn=4,pvmem=2000mb,walltime=01:00:00
source ~/.bashrc
PATH=$PATH:$HOME/wot/godi/bin:$HOME/wot/godi/sbin:/opt/bwgrid/mpi/openmpi/1.2.8-gcc/bin/
export PATH
LD_LIBRARY_PATH=/opt/bwgrid/mpi/openmpi/1.2.8-gcc/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
EXE=$HOME/wot/studienarbeit-wot/_build/graph/betweeness_mpi.native
DATE=`date +%d%m%y`
EDGE=$HOME/wot/studienarbeit-wot/edge-1259751600..graph

module load openmpi/1.2.8

cd $PBS_O_WORKDIR
mpirun $EXE $EDGE &> betweeness_mpi_$DATE.log
