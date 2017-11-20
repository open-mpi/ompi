/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_pe_aggrs.c
 * \brief The externally used function from this file is is declared in ad_pe_aggrs.h
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997-2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

/*#define TRACE_ON */

#include "adio.h"
#include "adio_cb_config_list.h"
#include "../ad_gpfs.h"
#include "ad_pe_aggrs.h"
#include "mpiimpl.h"

#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

#ifdef USE_DBG_LOGGING
  #define AGG_DEBUG 1
#endif

#ifndef TRACE_ERR
#  define TRACE_ERR(format...)
#endif

/*
 * Compute the aggregator-related parameters that are required in 2-phase
 * collective IO of ADIO.
 * The parameters are
 * 	. the number of aggregators (proxies) : fd->hints->cb_nodes
 *	. the ranks of the aggregators :        fd->hints->ranklist
 * If MP_IONODEFILE is defined, POE determines all tasks on every node listed
 * in the node file and defines MP_IOTASKLIST with them, making them all
 * aggregators.  Alternatively, the user can explictly set MP_IOTASKLIST
 * themselves.  The format of the MP_IOTASKLIST is a colon-delimited list of
 * task ids, the first entry being the total number of aggregators, for example
 * to specify 4 aggregators on task ids 0,8,16,24  the value would be:
 * 4:0:8:16:24.  If there is no MP_IONODEFILE, or MP_IOTASKLIST, then the
 * default aggregator selection is 1 task per node for every node of the job -
 * additionally, an environment variable MP_IOAGGR_CNT  can be specified, which
 * defines the total number of aggregators, spread evenly across all the nodes.
 * The romio_cb_nodes and romio_cb_config_list hint user settings are ignored.
 */
int
ADIOI_PE_gen_agg_ranklist(ADIO_File fd)
{

    int numAggs = 0;
    char *ioTaskList = getenv( "MP_IOTASKLIST" );
    char *ioAggrCount = getenv("MP_IOAGGR_CNT");
    int i,j;
    int inTERcommFlag = 0;

    int myRank,commSize;
    MPI_Comm_rank(fd->comm, &myRank);
    MPI_Comm_size(fd->comm, &commSize);

    MPI_Comm_test_inter(fd->comm, &inTERcommFlag);
    if (inTERcommFlag) {
      FPRINTF(stderr,"ERROR: ATTENTION: inTERcomms are not supported in MPI-IO - aborting....\n");
      perror("ADIOI_PE_gen_agg_ranklist:");
      MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (ioTaskList) {
      int ioTaskListLen = strlen(ioTaskList);
      int ioTaskListPos = 0;
      char tmpBuf[8];   /* Big enough for 1M tasks (7 digits task ID). */
      tmpBuf[7] = '\0';
      for (i=0; i<7; i++) {
         tmpBuf[i] = *ioTaskList++;      /* Maximum is 7 digits for 1 million. */
         ioTaskListPos++;
         if (*ioTaskList == ':') {       /* If the next char is a ':' ends it. */
             tmpBuf[i+1] = '\0';
             break;
         }
      }
      numAggs = atoi(tmpBuf);
      if (numAggs == 0)
        FPRINTF(stderr,"ERROR: ATTENTION: Number of aggregators specified in MP_IOTASKLIST set at 0 - default aggregator selection will be used.\n");
      else if (!((numAggs > 0 ) && (numAggs <= commSize))) {
        FPRINTF(stderr,"ERROR: ATTENTION: The number of aggregators (%s) specified in MP_IOTASKLIST is outside the communicator task range of %d.\n",tmpBuf,commSize);
        numAggs = commSize;
      }
      fd->hints->ranklist = (int *) ADIOI_Malloc (numAggs * sizeof(int));

      int aggIndex = 0;
      while (aggIndex < numAggs) {
         ioTaskList++;                /* Advance past the ':' */
         ioTaskListPos++;
         int allDigits=1;
         for (i=0; i<7; i++) {
            if (*ioTaskList < '0' || *ioTaskList > '9')
              allDigits=0;
            tmpBuf[i] = *ioTaskList++;
            ioTaskListPos++;
            if ( (*ioTaskList == ':') || (*ioTaskList == '\0') ) {
                tmpBuf[i+1] = '\0';
                break;
            }
         }
         if (allDigits) {
           int newAggRank = atoi(tmpBuf);
           if (!((newAggRank >= 0 ) && (newAggRank < commSize))) {
             FPRINTF(stderr,"ERROR: ATTENTION: The aggregator '%s' specified in MP_IOTASKLIST is not within the communicator task range of 0 to %d  - it will be ignored.\n",tmpBuf,commSize-1);
           }
           else {
             int aggAlreadyAdded = 0;
             for (i=0;i<aggIndex;i++)
               if (fd->hints->ranklist[i] == newAggRank) {
                 aggAlreadyAdded = 1;
                 break;
               }
             if (!aggAlreadyAdded)
               fd->hints->ranklist[aggIndex++] = newAggRank;
             else
               FPRINTF(stderr,"ERROR: ATTENTION: The aggregator '%d' is specified multiple times in MP_IOTASKLIST - duplicates are ignored.\n",newAggRank);
           }
         }
         else {
           FPRINTF(stderr,"ERROR: ATTENTION: The aggregator '%s' specified in MP_IOTASKLIST is not a valid integer task id  - it will be ignored.\n",tmpBuf);
         }

         /* At the end check whether the list is shorter than specified. */
         if (ioTaskListPos == ioTaskListLen) {
           if (aggIndex == 0) {
             FPRINTF(stderr,"ERROR: ATTENTION: No aggregators were correctly specified in MP_IOTASKLIST - default aggregator selection will be used.\n");
             ADIOI_Free(fd->hints->ranklist);
           }
           else if (aggIndex < numAggs)
             FPRINTF(stderr,"ERROR: ATTENTION: %d aggregators were specified in MP_IOTASKLIST but only %d were correctly specified - setting the number of aggregators to %d.\n",numAggs, aggIndex,aggIndex);
           numAggs = aggIndex;
         }
      }
    }
    if (numAggs == 0)  {
      MPID_Comm *mpidCommData;

      MPID_Comm_get_ptr(fd->comm,mpidCommData);
      int localSize = mpidCommData->local_size;

      // get my node rank
      int myNodeRank = mpidCommData->intranode_table[mpidCommData->rank];

      int *allNodeRanks = (int *) ADIOI_Malloc (localSize * sizeof(int));

      allNodeRanks[myRank] = myNodeRank;
      MPI_Allgather(MPI_IN_PLACE, 1, MPI_INT, allNodeRanks, 1, MPI_INT, fd->comm);

#ifdef AGG_DEBUG
      printf("MPID_Comm data: local_size is %d\nintranode_table entries:\n",mpidCommData->local_size);
      for (i=0;i<localSize;i++) {
        printf("%d ",mpidCommData->intranode_table[i]);
      }
      printf("\ninternode_table entries:\n");
      for (i=0;i<localSize;i++) {
        printf("%d ",mpidCommData->internode_table[i]);
      }
      printf("\n");

      printf("\nallNodeRanks entries:\n");
      for (i=0;i<localSize;i++) {
        printf("%d ",allNodeRanks[i]);
      }
      printf("\n");

#endif

      if (ioAggrCount) {
        int cntType = -1;

        if ( strcasecmp(ioAggrCount, "ALL") ) {
           if ( (cntType = atoi(ioAggrCount)) <= 0 ) {
              /* Input is other non-digit or less than 1 the  assume */
              /* 1 aggregator per node.  Note: atoi(-1) reutns -1.   */
              /* No warning message given here -- done earlier.      */
              cntType = -1;
           }
        }
        else {
           /* ALL is specified set aggr count to localSize */
           cntType = -2;
        }
        switch(cntType) {
           case -1:
              /* 1 aggr/node case */
            {
             int rankListIndex = 0;
             fd->hints->ranklist = (int *) ADIOI_Malloc (localSize * sizeof(int));
             for (i=0;i<localSize;i++) {
               if (allNodeRanks[i] == 0) {
                 fd->hints->ranklist[rankListIndex++] = i;
                 numAggs++;
               }
             }
            }
              break;
           case -2:
              /* ALL tasks case */
             fd->hints->ranklist = (int *) ADIOI_Malloc (localSize * sizeof(int));
             for (i=0;i<localSize;i++) {
               fd->hints->ranklist[i] = i;
               numAggs++;
             }
              break;
           default:
              /* Specific aggr count case -- MUST be less than localSize, otherwise set to localSize */
             if (cntType > localSize)
               cntType = localSize;

             numAggs = cntType;
             // Round-robin thru allNodeRanks - pick the 0's, then the 1's, etc
             int currentNodeRank = 0;  // node rank currently being selected as aggregator
             int rankListIndex = 0;
             int currentAllNodeIndex = 0;

             fd->hints->ranklist = (int *) ADIOI_Malloc (numAggs * sizeof(int));

             while (rankListIndex < numAggs) {
               int foundEntry = 0;
               while (!foundEntry && (currentAllNodeIndex < localSize)) {
                 if (allNodeRanks[currentAllNodeIndex] == currentNodeRank) {
                   fd->hints->ranklist[rankListIndex++] = currentAllNodeIndex;
                   foundEntry = 1;
                 }
                 currentAllNodeIndex++;
               }
               if (!foundEntry) {
                 currentNodeRank++;
                 currentAllNodeIndex = 0;
               }
             } // while
          break;
        } // switch(cntType)
      } // if (ioAggrCount)

      else { // default is 1 aggregator per node
        // take the 0 entries from allNodeRanks
        int rankListIndex = 0;
        fd->hints->ranklist = (int *) ADIOI_Malloc (localSize * sizeof(int));
        for (i=0;i<localSize;i++) {
          if (allNodeRanks[i] == 0) {
            fd->hints->ranklist[rankListIndex++] = i;
            numAggs++;
          }
        }
      }

      ADIOI_Free(allNodeRanks);

    }

    if ( getenv("MP_I_SHOW_AGGRS") ) {
      if (myRank == 0) {
        printf("Agg rank list of %d generated:\n", numAggs);
        for (i=0;i<numAggs;i++) {
          printf("%d ",fd->hints->ranklist[i]);
        }
        printf("\n");
      }
    }

    fd->hints->cb_nodes = numAggs;

    return 0;
}

