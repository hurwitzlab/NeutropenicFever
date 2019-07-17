#!/bin/bash

#PBS -W group_list=bhurwitz
#PBS -q standard
#PBS -l jobtype=cluster_only
#PBS -l select=1:ncpus=4:mem=15gb
#PBS -l pvmem=14gb
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M jamesthornton@email.arizona.edu
#PBS -m bea

FASTA_DIR="/rsgrps/bhurwitz/jetjr/neutropenicfever/BMT/new/fasta"

#MUST INCLUDE BASENAME OF INDEX FILES (IN THIS CASE HUMAN_INDEX)
BT2_INDEX="/rsgrps/bhurwitz/jetjr/neutropenicfever/bowtie2_index/human_index"
BT2_OUT_DIR="/rsgrps/bhurwitz/jetjr/neutropenicfever/BMT/new/unmapped"

module load bowtie2/2.2.5

cd "$FASTA_DIR"
export FASTA_LIST="$FASTA_DIR/fasta-list"

#CHANGE THE .FASTA EXTENSION AS NEEDED (FOR EXAMPLE YOUR FILES MAY HAVE .FA)
ls *.fasta > $FASTA_LIST
echo "FASTA files to be processed:" $(cat $FASTA_LIST)

while read FASTA; do

FILE_NAME=`basename $FASTA | cut -d '.' -f 1,2`

#THE --al OPTION MEANS READS THAT ALIGN TO THE INDEX WILL BE WRITTEN TO THE DESIGNATED OUT_DIR
bowtie2 -x $BT2_INDEX -U $FASTA -f --very-sensitive-local -p 4 --al $BT2_OUT_DIR/$FILE_NAME.aligned --un $BT2_OUT_DIR/$FILE_NAME.unmapped

done < $FASTA_LIST
