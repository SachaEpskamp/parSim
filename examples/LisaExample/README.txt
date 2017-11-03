1. Edit Simulation.R
	- Write a simulation function. First test locally! You can debug with nCores = 1 and nreps = something low.
	- Set nCores = 16 for Lisa
	- Set write = TRUE
	- Use name = ... to make sure the output is a separate file every run. Use the template!
	- Change reps to correspond to number of computers. E.g., set reps = 5, and run on 20 computers (using loop_submit.sh), to obtain 100 repititions
	
2. Edit job.sh
	- Change name (now set to LisaSimulation)
	- Change walltime (now set to 01:00:00)
		- Very important! If your simulation is too long lisa will cut it down!
	- Make sure you load the correct version of R
	- Make sure the folder is set correctly!
	
3. Edit loop_submit.sh
	- Make sure directories are correct
	- Change "20" to number of computers you want to use
	
4. Upload your stuff to Lisa, make sure you upload to the folder specified in job.sh and loop_submit.sh

5. Login to Lisa. If needed, setup R and install all packages needed.

	- In R:
	install.packages(c("tidyr","dplyr"), method = "wget")
	devtools::install_github("sachaepskamp/parSim")

6. CD to the folder

7. Make loop_submit.sh executable:

	cd LisaExample/
	chmod +x loop_submit.sh
	
8. Execute:
	
	./loop_submit.sh

9. Check progress:

	qstat -u sepskamp
	
10. HELP EVERYTHING WENT WRONG DELETE EVERYTHING (careful with this)

	qselect -u sepskamp | xargs qdel

11. After simulations are run, you have a folder with output files which you can move back to your computer and analyze. Commands I used for this:

	cd GitHub/parSim/examples/LisaExample/
	mkdir outputV1
	scp sepskamp@lisa.surfsara.nl:~/LisaExample/output/Simulation_v1* ./outputV1/


Some tips:
	- Change the file name every time you run a new simulation! Some old files persist. I always use v1, v2, v3 etcetera
	- Make sure the wall-time is correct
	- Sometimes more repetitions per computer is better than more computers...