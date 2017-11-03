#!/bin/bash
mkdir "$HOME"/LisaExample/output # Makes output directory
rm -r "$HOME"/LisaExample/output/*.txt # Removes earlier output (doesn't always work)
rm -r "$TMPDIR"/LisaExample/* # Removes earlier stuff

for i in `seq 1 20`; # CHANGE 20 TO NUMBER OF COMPUTERS YOU WANT TO USE
do
	sed s/iter/$i/g job.sh > CUR_job.sh
    qsub CUR_job.sh
done







