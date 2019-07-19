#!/bin/bash

#PBS -W group_list=yourgourp
#PBS -q standard
#PBS -l select=1:ncpus=4:mem=15gb
#PBS -l pvmem=14gb
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M youremail@email.arizona.edu
#PBS -m bea


BAM_DIR="/path/to/bam/directory"
OUT_DIR="/path/to/result/directory"

module load bedtools/2.17.0

cd $OUT_DIR

echo "BAM files to be processed" $(cat list)

for FILE in $(cat list); do
  
    bedtools bamtofastq -i $BAM_DIR/$FILE -fq $OUT_DIR/$FILE
    mv "$FILE" "${FILE%.bam}.fastq" 

done
