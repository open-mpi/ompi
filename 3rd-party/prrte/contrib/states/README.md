State Timestamps
----------------

A couple scripts for chopping out the "state" lines
for JOB and PROC state-machine transitions.  Then
you can pass that into the timediff script to get
idea of time between the events.  The scripts make
several assumptions so your mileage may vary.

We use the `PENDING ALLOCATION` as the basis for time
and compare other events to this origin (BaseDiff)
and also to the preceeding state (RelDiff).  This obviously
assumes the states apppear in order in time.

There are comments at the top of the files and a couple
example log files for playing with things.  This assumes
you have enabled verbose state output for PRRTE,
e.g.,`--prtemca prte_state_base_verbose 1`.


Examples
-------

 - Basic example running `sleep 5` to show delay in RUNNING to TERMINATE,
   for a single shot `prterun` case

 ```
    shell:$ prterun \
        --prtemca prte_state_base_verbose 1 \
        --np 2 \
        sleep 5 >& LOG.txt
    shell:$ ./statechop.pl LOG.txt
    elk:$ ./statechop.pl LOG.txt | ./timediffs.pl -
    DBG: FILE: -
    ####################################################
    #  EvtTime -- actual event time
    # BaseDiff -- difference from initial  base  time
    #  RelDiff -- difference from previous event time
    ####################################################
    # EvtTime    	      BaseDiff   	  RelDiff  	  EvtName
    #------------        ------------     ------------       -------------
     1591385117.048860 	    n/a 	   n/a  	  PENDING ALLOCATION
     1591385117.049572 	  -0.000712 	 -0.000712  	  ALLOCATION COMPLETE
     1591385117.049609 	  -0.000749 	 -0.000037  	  PENDING DAEMON LAUNCH
     1591385117.049720 	  -0.000860 	 -0.000111  	  ALL DAEMONS REPORTED
     1591385117.049764 	  -0.000904 	 -0.000044  	  VM READY
     1591385117.049806 	  -0.000946 	 -0.000042  	  PENDING INIT
     1591385117.049850 	  -0.000990 	 -0.000044  	  INIT_COMPLETE
     1591385117.049880 	  -0.001020 	 -0.000030  	  ALLOCATION COMPLETE
     1591385117.049913 	  -0.001053 	 -0.000033  	  PENDING DAEMON LAUNCH
     1591385117.049945 	  -0.001085 	 -0.000032  	  ALL DAEMONS REPORTED
     1591385117.049977 	  -0.001117 	 -0.000032  	  VM READY
     1591385117.050012 	  -0.001152 	 -0.000035  	  PENDING MAPPING
     1591385117.050123 	  -0.001263 	 -0.000111  	  MAP COMPLETE
     1591385117.050164 	  -0.001304 	 -0.000041  	  PENDING FINAL SYSTEM PREP
     1591385117.050197 	  -0.001337 	 -0.000033  	  PENDING APP LAUNCH
     1591385117.050661 	  -0.001801 	 -0.000464  	  SENDING LAUNCH MSG
     1591385117.058262 	  -0.009402 	 -0.007601  	  RUNNING
     1591385122.059255 	  -5.010395 	 -5.000993  	  NORMALLY TERMINATED
     1591385122.060270 	  -5.011410 	 -0.001015  	  DAEMONS TERMINATED
    shell:$

 - Using past logfile with multiple `prun` jobs so need to trim lines of
   interest down a bit using `grep` before passing into timediff script

  ```
    shell:$ ./statechop.pl example-output2.txt \
        | grep -v '^#' \
        | grep '40915,0],0' \
        | ./timediffs.pl -

 ```

