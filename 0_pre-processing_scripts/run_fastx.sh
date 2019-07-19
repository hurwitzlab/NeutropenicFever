#!/bin/bash

#PBS -W group_list=yourgroup
#PBS -q standard
#PBS -l select=1:ncpus=4:mem=15gb
#PBS -l place=pack:shared
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M youremail@email.arizona.edu
#PBS -m bea

FASTQ_DIR="path/to/fastq/directory"

cd "$FASTQ_DIR"

module load fastx/0.0.14

for f in *.fastq; do
    name=${f%.*}
    output="${name}_withqc.fasta"
    cat $f | fastx_trimmer -f 10 -l 175 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > $output
done