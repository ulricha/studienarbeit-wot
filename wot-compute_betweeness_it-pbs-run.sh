#!/bin/bash

#PBS -M ulricha@informatik.uni-tuebingen.de
#PBS -m abe
#PBS -N wot-betweeness-it
#PBS -l nodes=1:ppn=1,pvmem=2000mb,walltime=06:00:00
PATH=$PATH:$HOME/wot/godi/bin:$HOME/wot/godi/sbin
export PATH
WORKDIR=$HOME/wot/studienarbeit-wot/
EXE=_build/graph/compute_betweeness.native

cd $PBS_O_WORKDIR
./$EXE vertex.sexp edge.sexp &> compute_betweeness_141009.log
