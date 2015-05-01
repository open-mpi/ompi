/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bg_aggrs.c
 * \brief The externally used function from this file is is declared in ad_bg_aggrs.h
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997-2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/*#define TRACE_ON */

// Uncomment this line to turn tracing on for the gpfsmpio_balancecontig aggr selection optimization
// #define balancecontigtrace 1
// #define bridgeringaggtrace 1

#include "adio.h"
#include "adio_cb_config_list.h"
#include "../ad_gpfs.h"
#include "ad_bg_pset.h"
#include "ad_bg_aggrs.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif


#ifdef USE_DBG_LOGGING
  #define AGG_DEBUG 1
#endif

#ifndef TRACE_ERR
#  define TRACE_ERR(format...)
#endif

/* Comments copied from common:
 * This file contains four functions:
 *
 * ADIOI_Calc_aggregator()
 * ADIOI_Calc_file_domains()
 * ADIOI_Calc_my_req()
 * ADIOI_Calc_others_req()
 *
 * The last three of these were originally in ad_read_coll.c, but they are
 * also shared with ad_write_coll.c.  I felt that they were better kept with
 * the rest of the shared aggregation code.  
 */

/* Discussion of values available from above:
 *
 * ADIO_Offset st_offsets[0..nprocs-1]
 * ADIO_Offset end_offsets[0..nprocs-1]
 *    These contain a list of start and end offsets for each process in 
 *    the communicator.  For example, an access at loc 10, size 10 would
 *    have a start offset of 10 and end offset of 19.
 * int nprocs
 *    number of processors in the collective I/O communicator
 * ADIO_Offset min_st_offset
 * ADIO_Offset fd_start[0..nprocs_for_coll-1]
 *    starting location of "file domain"; region that a given process will
 *    perform aggregation for (i.e. actually do I/O)
 * ADIO_Offset fd_end[0..nprocs_for_coll-1]
 *    start + size - 1 roughly, but it can be less, or 0, in the case of 
 *    uneven distributions
 */

/* forward declaration */
static void 
ADIOI_BG_compute_agg_ranklist_serial ( ADIO_File fd, 
					const ADIOI_BG_ConfInfo_t *confInfo, 
					ADIOI_BG_ProcInfo_t *all_procInfo);

/*
 * Compute the aggregator-related parameters that are required in 2-phase collective IO of ADIO.
 * The parameters are 
 * 	. the number of aggregators (proxies) : fd->hints->cb_nodes
 *	. the ranks of the aggregators :        fd->hints->ranklist
 * By compute these two parameters in a BG-PSET-aware way, the default 2-phase collective IO of 
 *	ADIO can work more efficiently.
 */
int 
ADIOI_BG_gen_agg_ranklist(ADIO_File fd, int n_aggrs_per_pset) 
{
    int r, s;
    ADIOI_BG_ProcInfo_t  *procInfo, *all_procInfo;
    ADIOI_BG_ConfInfo_t  *confInfo;
    TRACE_ERR("Entering ADIOI_BG_gen_agg_ranklist\n");

    MPI_Comm_size( fd->comm, &s );
    MPI_Comm_rank( fd->comm, &r );

  /* Collect individual BG personality information */
    confInfo = ADIOI_BG_ConfInfo_new ();
    procInfo = ADIOI_BG_ProcInfo_new ();
    ADIOI_BG_persInfo_init( confInfo, procInfo, s, r, n_aggrs_per_pset, fd->comm);

  /* Gather BG personality infomation onto process 0 */
    /* if (r == 0) */
    all_procInfo  = ADIOI_BG_ProcInfo_new_n  (s);

    MPI_Gather( (void *)procInfo,     sizeof(ADIOI_BG_ProcInfo_t), MPI_BYTE, 
		(void *)all_procInfo, sizeof(ADIOI_BG_ProcInfo_t), MPI_BYTE, 
		0, 
		fd->comm );

  /* Compute a list of the ranks of chosen IO proxy CN on process 0 */
    if (r == 0) { 
	ADIOI_BG_compute_agg_ranklist_serial (fd, confInfo, all_procInfo);
	/* ADIOI_BG_ProcInfo_free (all_procInfo);*/
    }
    ADIOI_BG_ProcInfo_free (all_procInfo);

  /* Send the info of IO proxy CN to all processes and keep the info in fd->hints struct.
     Declared in adio_cb_config_list.h */
    ADIOI_cb_bcast_rank_map(fd);
    if (gpfsmpio_balancecontig == 1) { /* additionally need to send bridgelist,
					bridgelistnum and numbridges to all
					ranks */
	if (r != 0) {
	    fd->hints->fs_hints.bg.bridgelist =
		ADIOI_Malloc(fd->hints->cb_nodes*sizeof(int));
	    if (fd->hints->fs_hints.bg.bridgelist == NULL) {
		/* NEED TO HANDLE ENOMEM */
	    }
	}
	MPI_Bcast(fd->hints->fs_hints.bg.bridgelist, fd->hints->cb_nodes, MPI_INT, 0,
		fd->comm);

	if (r != 0) {
	    fd->hints->fs_hints.bg.bridgelistnum =
		ADIOI_Malloc(fd->hints->cb_nodes*sizeof(int));
	    if (fd->hints->fs_hints.bg.bridgelistnum == NULL) {
		/* NEED TO HANDLE ENOMEM */
	    }
	}
	MPI_Bcast(fd->hints->fs_hints.bg.bridgelistnum, fd->hints->cb_nodes,
		MPI_INT, 0, fd->comm);

	MPI_Bcast(&fd->hints->fs_hints.bg.numbridges, 1, MPI_INT, 0,
		fd->comm);

    }


    ADIOI_BG_persInfo_free( confInfo, procInfo );
    TRACE_ERR("Leaving ADIOI_BG_gen_agg_ranklist\n");
    return 0;
}


/* There are some number of bridge nodes (randomly) distributed through the job
 * We need to split the nodes among the bridge nodes */
/* Maybe find which bridge node is closer (manhattan distance) and try to
 * distribute evenly.
 */
/* 
 * Pick IO aggregators based on the under PSET organization and stores the ranks of the proxy CNs in tmp_ranklist.
 * The first order of tmp_ranklist is : PSET number
 * The secondary order of the list is determined in ADIOI_BG_select_agg_in_pset() and thus adjustable.
 */
typedef struct
{
   int rank;
   int bridge;
} sortstruct;

typedef struct
{
   int bridgeRank;
   int numAggsAssigned;
} bridgeAggAssignment;

static int intsort(const void *p1, const void *p2)
{
   sortstruct *i1, *i2;
   i1 = (sortstruct *)p1;
   i2 = (sortstruct *)p2;
   return(i1->bridge - i2->bridge);
}

static int 
ADIOI_BG_compute_agg_ranklist_serial_do (const ADIOI_BG_ConfInfo_t *confInfo, 
					  ADIOI_BG_ProcInfo_t       *all_procInfo, 
					  int *tmp_ranklist)
{
    TRACE_ERR("Entering ADIOI_BG_compute_agg_ranklist_serial_do\n");
   /* BES: This should be done in the init routines probably. */
    int i, j;
    int aggTotal;
    int *aggList;

    if (gpfsmpio_bridgeringagg > 0) {

      int numAggs = confInfo->aggRatio * confInfo->ioMinSize /*virtualPsetSize*/;
        /* the number of aggregators is (numAggs per bridgenode) */
      if(numAggs == 1)
        aggTotal = 1;
      else
        aggTotal = confInfo->numBridgeRanks * numAggs;

      aggList = (int *)ADIOI_Malloc(aggTotal * sizeof(int));
      if(aggTotal == 1) { /* special case when we only have one bridge node */

        sortstruct *bridgelist = (sortstruct *)ADIOI_Malloc(confInfo->nProcs * sizeof(sortstruct));
        for(i=0; i < confInfo->nProcs; i++)
        {
          bridgelist[i].bridge = all_procInfo[i].bridgeRank;
          bridgelist[i].rank = i;
          TRACE_ERR("bridgelist[%d].bridge: %d .rank: %d\n", i, bridgelist[i].bridge, i);
        }

        /* This list contains rank->bridge info. Now, we need to sort this list. */
        qsort(bridgelist, confInfo->nProcs, sizeof(sortstruct), intsort);

        aggList[0] = bridgelist[0].bridge;
        ADIOI_Free(bridgelist);

      }
      else { // aggTotal > 1

        int currentAggListSize = 0;
        int numBridgesWithAggAssignments = 0;
        bridgeAggAssignment *aggAssignments = (bridgeAggAssignment *)ADIOI_Malloc(confInfo->numBridgeRanks * sizeof(bridgeAggAssignment));

        int partitionSize = all_procInfo[0].numNodesInPartition;
        int *nodesAssigned = (int *)ADIOI_Malloc(partitionSize * sizeof(int));
        for (i=0;i<partitionSize;i++)
          nodesAssigned[i] = 0;

        int currentNumHops = gpfsmpio_bridgeringagg;
        int allAggsAssigned = 0;

        /* Iterate thru the process infos and select aggregators starting at currentNumHops
           away.  Increase the currentNumHops until all bridges have numAggs assigned to them.
        */
        while (!allAggsAssigned) {
          /* track whether any aggs are selected durng this round */
          int startingCurrentAggListSize = currentAggListSize;
          int numIterForHopsWithNoAggs = 0;
          for (i=0;i<confInfo->nProcs;i++) {
          if (all_procInfo[i].manhattanDistanceToBridge == currentNumHops) {
            if (nodesAssigned[all_procInfo[i].nodeRank] == 0) { // node is not assigned as an agg yet
              int foundBridge = 0;
              for (j=0;(j<numBridgesWithAggAssignments && !foundBridge);j++) {
                if (aggAssignments[j].bridgeRank == all_procInfo[i].bridgeRank) {
                  foundBridge = 1;
                  if (aggAssignments[j].numAggsAssigned < numAggs) {
                    aggAssignments[j].numAggsAssigned++;
                    nodesAssigned[all_procInfo[i].nodeRank] = 1;
                    aggList[currentAggListSize] = all_procInfo[i].rank;
                    currentAggListSize++;
#ifdef bridgeringaggtrace
                printf("Assigned agg rank %d at nodeRank %d to bridge rank %d at a distance of %d hops\n",all_procInfo[i].rank,all_procInfo[i].nodeRank,all_procInfo[i].bridgeRank,currentNumHops);
#endif
                  }
                }
              }
              if (!foundBridge) {
                aggAssignments[numBridgesWithAggAssignments].bridgeRank = all_procInfo[i].bridgeRank;
                aggAssignments[numBridgesWithAggAssignments].numAggsAssigned = 1;
                numBridgesWithAggAssignments++;
                nodesAssigned[all_procInfo[i].nodeRank] = 1;
                aggList[currentAggListSize] = all_procInfo[i].rank;
                currentAggListSize++;
#ifdef bridgeringaggtrace
                printf("Assigned agg rank %d at nodeRank %d to bridge rank %d at a distance of %d hops\n",all_procInfo[i].rank,all_procInfo[i].nodeRank,all_procInfo[i].bridgeRank,currentNumHops);
#endif
              }
            }
          }
        }

        if (numBridgesWithAggAssignments == confInfo->numBridgeRanks) {
          allAggsAssigned = 1;
          for (i=0;(i<numBridgesWithAggAssignments && allAggsAssigned);i++) {
            if (aggAssignments[i].numAggsAssigned < numAggs)
              allAggsAssigned = 0;
          }
        }
        currentNumHops++;
        /* If 3 rounds go by without selecting an agg abort to avoid
           infinite loop.
        */
        if (startingCurrentAggListSize == currentAggListSize)
          numIterForHopsWithNoAggs++;
        else
          numIterForHopsWithNoAggs = 0;
        ADIOI_Assert(numIterForHopsWithNoAggs <= 3);
        }

        ADIOI_Free(aggAssignments);
        ADIOI_Free(nodesAssigned);

      } // else aggTotal  > 1

       memcpy(tmp_ranklist, aggList, aggTotal*sizeof(int));
    } // gpfsmpio_bridgeringagg > 0

    else { // gpfsmpio_bridgeringagg unset - default code

    int distance, numAggs;

    /* Aggregators will be midpoints between sorted MPI rank lists of who shares a given
     * bridge node */

   sortstruct *bridgelist = (sortstruct *)ADIOI_Malloc(confInfo->nProcs * sizeof(sortstruct));
   for(i=0; i < confInfo->nProcs; i++)
   {
      bridgelist[i].bridge = all_procInfo[i].bridgeRank;
      bridgelist[i].rank = i;
      TRACE_ERR("bridgelist[%d].bridge: %d .rank: %d\n", i, bridgelist[i].bridge, i);
   }
   
   /* This list contains rank->bridge info. Now, we need to sort this list. */
   qsort(bridgelist, confInfo->nProcs, sizeof(sortstruct), intsort);

   /* In this array, we can pick an appropriate number of midpoints based on
    * our bridgenode index and the number of aggregators */

   numAggs = confInfo->aggRatio * confInfo->ioMinSize /*virtualPsetSize*/;
   if(numAggs == 1)
      aggTotal = 1;
   else
   /* the number of aggregators is (numAggs per bridgenode) plus each 
    * bridge node is an aggregator */
      aggTotal = confInfo->numBridgeRanks * (numAggs+1);

   if(aggTotal>confInfo->nProcs) aggTotal=confInfo->nProcs;

   TRACE_ERR("numBridgeRanks: %d, aggRatio: %f numBridge: %d pset size: %d/%d numAggs: %d, aggTotal: %d\n", confInfo->numBridgeRanks, confInfo->aggRatio, confInfo->numBridgeRanks,  confInfo->ioMinSize, confInfo->ioMaxSize /*virtualPsetSize*/, numAggs, aggTotal);
   aggList = (int *)ADIOI_Malloc(aggTotal * sizeof(int));


   /* For each bridge node, determine who the aggregators will be */
   /* basically, the n*distance and bridge node */
   if(aggTotal == 1) /* special case when we only have one bridge node */
      aggList[0] = bridgelist[0].bridge;
   else
   {
     int lastBridge = bridgelist[confInfo->nProcs-1].bridge;
     int nextBridge = 0, nextAggr = confInfo->numBridgeRanks;
     int psetSize = 0;
     int procIndex;
     for(procIndex=confInfo->nProcs-1; procIndex>=0; procIndex--)
     {
       TRACE_ERR("bridgelist[%d].bridge %u/rank %u\n",procIndex,  bridgelist[procIndex].bridge, bridgelist[procIndex].rank);
       if(lastBridge == bridgelist[procIndex].bridge)
       {
         psetSize++;
         if(procIndex) continue; 
         else procIndex--;/* procIndex == 0 */
       }
       /* Sets up a list of nodes which will act as aggregators. numAggs
        * per bridge node total. The list of aggregators is
        * bridgeNode 0
        * bridgeNode 1
        * bridgeNode ...
        * bridgeNode N
        * bridgeNode[0]aggr[0]
        * bridgeNode[0]aggr[1]...
        * bridgeNode[0]aggr[N]...
        * ...
        * bridgeNode[N]aggr[0]..
        * bridgeNode[N]aggr[N]
        */
       aggList[nextBridge]=lastBridge;
       distance = psetSize/numAggs;
       TRACE_ERR("nextBridge %u is bridge %u, distance %u, size %u\n",nextBridge, aggList[nextBridge],distance,psetSize);
       if(numAggs>1)
       {
         for(j = 0; j < numAggs; j++)
         {
           ADIOI_Assert(nextAggr<aggTotal);
           aggList[nextAggr] = bridgelist[procIndex+j*distance+1].rank;
           TRACE_ERR("agglist[%d] -> bridgelist[%d] = %d\n", nextAggr, procIndex+j*distance+1,aggList[nextAggr]);
           if(aggList[nextAggr]==lastBridge) /* can't have bridge in the list twice */
           {  
             aggList[nextAggr] = bridgelist[procIndex+psetSize].rank; /* take the last one in the pset */
             TRACE_ERR("replacement agglist[%d] -> bridgelist[%d] = %d\n", nextAggr, procIndex+psetSize,aggList[nextAggr]);
           }
           nextAggr++;
         }
       }
       if(procIndex<0) break;
       lastBridge = bridgelist[procIndex].bridge;
       psetSize = 1;
       nextBridge++;
     }
   }

   TRACE_ERR("memcpy(tmp_ranklist, aggList, (numAggs(%u)*confInfo->numBridgeRanks(%u)+numAggs(%u)) (%u) %u*sizeof(int))\n",numAggs,confInfo->numBridgeRanks,numAggs,(numAggs*confInfo->numBridgeRanks+numAggs),aggTotal);
   memcpy(tmp_ranklist, aggList, aggTotal*sizeof(int));
   for(i=0;i<aggTotal;i++)
   {
      TRACE_ERR("tmp_ranklist[%d]: %d\n", i, tmp_ranklist[i]);
   }


   ADIOI_Free (bridgelist);

   TRACE_ERR("Leaving ADIOI_BG_compute_agg_ranklist_serial_do\n");
   }

   ADIOI_Free (aggList);
   return aggTotal;

}

/* 
 * compute aggregators ranklist and put it into fd->hints struct
 */ 
static void 
ADIOI_BG_compute_agg_ranklist_serial ( ADIO_File fd, 
					const ADIOI_BG_ConfInfo_t *confInfo, 
					ADIOI_BG_ProcInfo_t *all_procInfo)
{
    TRACE_ERR("Entering ADIOI_BG_compute_agg_ranklist_serial\n");
    int i; 
    int naggs; 
    int size;
    int *tmp_ranklist;

  /* compute the ranklist of IO aggregators and put into tmp_ranklist */
    tmp_ranklist = (int *) ADIOI_Malloc (confInfo->nProcs * sizeof(int));

#   if AGG_DEBUG
    for (i=0; i<confInfo->nProcs; i++) {
      DBG_FPRINTF(stderr, "\tcpuid %1d, rank = %6d\n", all_procInfo[i].coreID, all_procInfo[i].rank );
    }
#   endif

    naggs= 
    ADIOI_BG_compute_agg_ranklist_serial_do (confInfo, all_procInfo, tmp_ranklist);

#   define VERIFY 1
#   if VERIFY
    DBG_FPRINTF(stderr, "\tconfInfo = min: %3d, max: %3d, naggrs: %3d, bridge: %3d, nprocs: %3d, vpset: %3d, tsize: %3d, ratio: %.4f; naggs = %d\n", 
	    confInfo->ioMinSize        ,
	    confInfo->ioMaxSize        ,
	    confInfo->nAggrs           ,
	    confInfo->numBridgeRanks ,
	    confInfo->nProcs          ,
	    confInfo->ioMaxSize /*virtualPsetSize*/          ,
      confInfo->cpuIDsize,
	    confInfo->aggRatio        ,
	    naggs );
#   endif
    MPI_Comm_size( fd->comm, &size );
    /* This fix is for when the bridgenode rnk is not part of the particular
     * subcomm associated with this MPI File operation. I don't know if
     * this is the best/right answer but it passes the test cases at least.
     * I don't know how common file IO in subcomms is anyway... */
    for(i=0;i<naggs;i++)
    {
      if(tmp_ranklist[i] > size)
      {
         TRACE_ERR("Using 0 as tmp_ranklist[%d] instead of %d for comm %x\n",
               i, tmp_ranklist[i], fd->comm);
         tmp_ranklist[i] = 0;
      }
   }
         
#   if AGG_DEBUG
    for (i=0; i<naggs; i++) {
      DBG_FPRINTF(stderr, "\taggr %-4d = %6d\n", i, tmp_ranklist[i] );
    }
#   endif
    if (gpfsmpio_balancecontig == 1) {
	/* what comes out of this code block is the agg ranklist sorted by
	 * bridge set and ion id with associated bridge info stored in the
	 * hints structure for later access during file domain assignment */

	// sort the agg ranklist by ions and bridges

	int *interleavedbridgeranklist = (int *) ADIOI_Malloc (naggs * sizeof(int)); // resorted agg rank list
	/* list of all bridge ranks */
	int *bridgelist = (int *) ADIOI_Malloc (naggs * sizeof(int));

	/* each entry here is the number of aggregators associated with the
	 * bridge rank of the same index in bridgelist */
	int *bridgelistnum = (int *) ADIOI_Malloc (naggs * sizeof(int));
	/* list of all ion IDs corresponding with bridgelist entries of same index */
	int *ionlist = (int *) ADIOI_Malloc (naggs * sizeof(int));

	int numbridges = 0;

	for (i=0;i<naggs;i++)
	    bridgelistnum[i] = 0;

	/* Each entry in this list corresponds with the bridgelist and will contain the lowest bridge
	 * agg rank on that ion. */
	int *summarybridgeminionaggrank = (int *) ADIOI_Malloc (naggs * sizeof(int));
	for (i=0;i<naggs;i++)
	    summarybridgeminionaggrank[i] = -1;

	/* build the bridgelist, ionlist and bridgelistnum data by going thru each agg
	 * entry and find the associated bridge list index - at the end we will
	 * know how many aggs belong to each bridge in each ion */
	for (i=0;i<naggs;i++) {
	    int aggbridgerank = all_procInfo[tmp_ranklist[i]].bridgeRank;
	    int aggionid = all_procInfo[tmp_ranklist[i]].ionID;
	    int foundrank = 0;
	    int summaryranklistbridgeindex = 0;
	    int j;
	    for (j=0;(j<numbridges && !foundrank);j++) {
		if (bridgelist[j] == aggbridgerank) {
		    foundrank = 1;
		    summaryranklistbridgeindex = j;
		}
		else
		    summaryranklistbridgeindex++;
	    }
	    if (!foundrank) {
		bridgelist[summaryranklistbridgeindex] = aggbridgerank;
		ionlist[summaryranklistbridgeindex] = aggionid;

		if (summarybridgeminionaggrank[summaryranklistbridgeindex] == -1)
		    summarybridgeminionaggrank[summaryranklistbridgeindex] = aggbridgerank;
		else if (summarybridgeminionaggrank[summaryranklistbridgeindex] > aggbridgerank)
		    summarybridgeminionaggrank[summaryranklistbridgeindex] = aggbridgerank;
		numbridges++;
	    }

	    bridgelistnum[summaryranklistbridgeindex]++;
	}

    /* at this point summarybridgeminionaggrank has the agg rank of the bridge for entries,
     * need to make each entry the minimum bridge rank for the entire ion. */
    for (i=0;i<numbridges;i++) {
        int aggIonId = ionlist[i];
        int j;
        for (j=0;j<numbridges;j++) {
          if (ionlist[j] == aggIonId) {
            if (summarybridgeminionaggrank[j] < summarybridgeminionaggrank[i])
              summarybridgeminionaggrank[i] = summarybridgeminionaggrank[j];
          }
        }
    }

	// resort by io node minimum bridge rank
	int x;
	for (x=0;x<numbridges;x++) {
	    for (i=0;i<(numbridges-1);i++) {
		if (summarybridgeminionaggrank[i] > summarybridgeminionaggrank[i+1]) {
		    int tmpminionaggrank = summarybridgeminionaggrank[i];
		    summarybridgeminionaggrank[i] = summarybridgeminionaggrank[i+1];
		    summarybridgeminionaggrank[i+1] = tmpminionaggrank;
		    int tmpionid = ionlist[i];
		    ionlist[i] = ionlist[i+1];
		    ionlist[i+1] = tmpionid;
		    int tmpbridgerank = bridgelist[i];
		    bridgelist[i] = bridgelist[i+1];
		    bridgelist[i+1] = tmpbridgerank;
		    int tmpbridgeranknum = bridgelistnum[i];
		    bridgelistnum[i] = bridgelistnum[i+1];
		    bridgelistnum[i+1] = tmpbridgeranknum;
		  }
	    }
	}

	// for each io node make sure bridgelist is in rank order
	int startSortIndex = -1;
	int endSortIndex = -1;
	int currentBridgeIndex = 0;

	while (currentBridgeIndex < numbridges) {
	    int currentIonId = ionlist[currentBridgeIndex];
	    startSortIndex = currentBridgeIndex;
	    while (ionlist[currentBridgeIndex] == currentIonId)
		  currentBridgeIndex++;
	    endSortIndex = currentBridgeIndex-1;
	    for (x=startSortIndex;x<=endSortIndex;x++) {
		  for (i=startSortIndex;i<endSortIndex;i++) {
		    if (bridgelist[i] > bridgelist[i+1]) {
			  int tmpbridgerank = bridgelist[i];
			  bridgelist[i] = bridgelist[i+1];
			  bridgelist[i+1] = tmpbridgerank;
			  int tmpbridgeranknum = bridgelistnum[i];
			  bridgelistnum[i] = bridgelistnum[i+1];
			  bridgelistnum[i+1] = tmpbridgeranknum;
		    }
		  }
	    }
	}


	/* populate interleavedbridgeranklist - essentially the agg rank list
	 * is now sorted by the ion minimum bridge rank and bridge node */
	int currentrankoffset = 0;
	for (i=0;i<numbridges;i++) {
	    int *thisBridgeAggList = (int *) ADIOI_Malloc (naggs * sizeof(int));
	    int numAggsForThisBridge = 0;

	    int k;
	    for (k=0;k<naggs;k++) {
		int aggbridgerank = all_procInfo[tmp_ranklist[k]].bridgeRank;
		if (aggbridgerank == bridgelist[i]) {
		    thisBridgeAggList[numAggsForThisBridge] = tmp_ranklist[k];
		    numAggsForThisBridge++;
		}
	    }

	    // sort thisBridgeAggList
	    for (x=0;x<numAggsForThisBridge;x++) {
		int n;
		for (n=0;n<(numAggsForThisBridge-1);n++) {
		    if (thisBridgeAggList[n] > thisBridgeAggList[n+1]) {
			int tmpthisBridgeAggList = thisBridgeAggList[n];
			thisBridgeAggList[n] = thisBridgeAggList[n+1];
			thisBridgeAggList[n+1] = tmpthisBridgeAggList;
		    }
		}
	    }
	    int n;
	    for (n=0;n<numAggsForThisBridge;n++) {
		interleavedbridgeranklist[currentrankoffset] = thisBridgeAggList[n];
		currentrankoffset++;
	    }
	    ADIOI_Free(thisBridgeAggList);
	}

#ifdef balancecontigtrace
	fprintf(stderr,"Interleaved aggregator list:\n");
	for (i=0;i<naggs;i++) {
	    fprintf(stderr,"Agg: %d Agg rank: %d with bridge rank %d and ion ID %d\n",i,interleavedbridgeranklist[i],all_procInfo[interleavedbridgeranklist[i]].bridgeRank,all_procInfo[interleavedbridgeranklist[i]].ionID);
	}
	fprintf(stderr,"Bridges list:\n");
	for (i=0;i<numbridges;i++) {
	    fprintf(stderr,"bridge %d ion min rank %d rank %d number of aggs %d ion id %d\n",i,summarybridgeminionaggrank[i],bridgelist[i],bridgelistnum[i],ionlist[i]);
	}

#endif
	/* copy the ranklist of IO aggregators to fd->hints */
	if(fd->hints->ranklist != NULL)
	    ADIOI_Free (fd->hints->ranklist);
	if(fd->hints->fs_hints.bg.bridgelist != NULL)
	    ADIOI_Free (fd->hints->fs_hints.bg.bridgelist);
	if(fd->hints->fs_hints.bg.bridgelistnum != NULL)
	    ADIOI_Free (fd->hints->fs_hints.bg.bridgelistnum);

	fd->hints->cb_nodes = naggs;
	fd->hints->fs_hints.bg.numbridges = numbridges;
	fd->hints->ranklist = (int *) ADIOI_Malloc (naggs * sizeof(int));
	memcpy( fd->hints->ranklist, interleavedbridgeranklist, naggs*sizeof(int) );

	fd->hints->fs_hints.bg.bridgelist = (int *) ADIOI_Malloc (naggs * sizeof(int));
	memcpy( fd->hints->fs_hints.bg.bridgelist, bridgelist, naggs*sizeof(int) );

	fd->hints->fs_hints.bg.bridgelistnum = (int *) ADIOI_Malloc (naggs * sizeof(int));
	memcpy( fd->hints->fs_hints.bg.bridgelistnum, bridgelistnum, naggs*sizeof(int) );

	ADIOI_Free(summarybridgeminionaggrank);
	ADIOI_Free( tmp_ranklist );
	ADIOI_Free( bridgelistnum );
	ADIOI_Free( bridgelist );
	ADIOI_Free( interleavedbridgeranklist );
	ADIOI_Free(ionlist);

    }  else {
	/* classic topology-agnostic copy of the ranklist of IO aggregators to
	 * fd->hints */
	if(fd->hints->ranklist != NULL) ADIOI_Free (fd->hints->ranklist);

	fd->hints->cb_nodes = naggs;
	fd->hints->ranklist = (int *) ADIOI_Malloc (naggs * sizeof(int));
	memcpy( fd->hints->ranklist, tmp_ranklist, naggs*sizeof(int) );

	ADIOI_Free( tmp_ranklist );
    }
    TRACE_ERR("Leaving ADIOI_BG_compute_agg_ranklist_serial\n");
    return;
}
