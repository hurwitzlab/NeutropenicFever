#!/bin/bash

#PBS -W group_list=bhurwitz
#PBS -q standard
#PBS -l jobtype=cluster_only
#PBS -l select=1:ncpus=4:mem=15gb
#PBS -l pvmem=14gb
#PBS -l place=pack:shared
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00
#PBS -M jamesthornton@email.arizona.edu
#PBS -m bea

FASTQ_DIR="/rsgrps/bhurwitz/jetjr/neutropenicfever/BMT/new"

cd "$FASTQ_DIR"

module load fastx/0.0.14

cat BMT005.1 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT005.1.fasta

cat BMT005.2 | fastx_trimmer -f 10 -l 170 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT005.2.fasta

cat BMT005.3 | fastx_trimmer -f 10 -l 170 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT005.3.fasta

cat BMT005.5 | fastx_trimmer -f 10 -l 170 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT005.5.fasta

cat BMT005.8 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT005.8.fasta

cat BMT010.1 | fastx_trimmer -f 10 -l 200 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT010.1.fasta     

cat BMT010.2 | fastx_trimmer -f 10 -l 200 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT010.2.fasta     

cat BMT010.3 | fastx_trimmer -f 10 -l 200 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT010.3.fasta     

cat BMT010.4 | fastx_trimmer -f 10 -l 200 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT010.4.fasta     

cat BMT010.9 | fastx_trimmer -f 10 -l 200 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT010.9.fasta     

cat BMT014.1 | fastx_trimmer -f 10 -l 280 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT014.1.fasta     

cat BMT014.2 | fastx_trimmer -f 10 -l 280 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT014.2.fasta     

cat BMT014.3 | fastx_trimmer -f 10 -l 280 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT014.3.fasta     

cat BMT014.4 | fastx_trimmer -f 10 -l 280 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT014.4.fasta     

cat BMT014.5 | fastx_trimmer -f 10 -l 280 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT014.5.fasta     

cat BMT027.9A | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT027.9Afasta

cat BMT027.9B | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT027.9B.fasta

cat BMT026.4 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT026.4.fasta

cat BMT026.5 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT026.5.fasta

cat BMT064.2 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT064.2.fasta

cat BMT064.3 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT064.3.fasta

cat BMT066.3 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT066.3.fasta

cat BMT066.4 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT066.4.fasta

cat BMT001.25 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT001.25.fasta

cat BMT001.26 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT001.26.fasta

cat BMT001.27 | fastx_trimmer -f 10 -l 130 | fastq_quality_filter -q 17 -p 80 | fastx_clipper -l 50 | fastx_collapser > BMT001.27.fasta










