#!/bin/bash

#PBS -W group_list=yourgroup
#PBS -q standard
#PBS -l select=1:ncpus=12:mem=23gb
#PBS -l pvmem=20gb
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M youremail@email.arizona.edu
#PBS -m bea

#REF SEQ BASENAME
SEQ_N=$(basename $SEQ | cut -d '.' -f 1)
SAMPLE_N=$(basename $SAMPLE | cut -d '.' -f 1)

module load samtools
module load bowtie2

#BUILD BT2 INDEX FROM REFERENCE
bowtie2-build $SEQ $SEQ_N

#ALIGN SAMPLE TO REFERENCE
bowtie2 --threads 2 -x $SEQ_N -U $SAMPLE -S $OUT/$SEQ_N.sam -f --very-sensitive-local

#SAM TO SORTED BAM TO COVERAGE
samtools view -F 4 -bS $OUT/$SEQ_N.sam > $OUT/$SEQ_N.bam
samtools sort -T $SEQ_N.sorted -o $OUT/$SEQ_N.sorted.bam $OUT/$SEQ_N.bam
samtools depth $OUT/$SEQ_N.sorted.bam > $OUT/$SEQ_N.coverage

#REQUIRED MODULES FOR GRAPH
module load unsupported
module load markb/R/3.1.1

#CREATE DEPTH OF COVERAGE GRAPH
$SCRIPT_DIR/depth_graph.R -c $SEQ_N.coverage -b $SEQ_N.sorted.bam -d $OUT -t "$SAMPLE_N,$SEQ_N"
