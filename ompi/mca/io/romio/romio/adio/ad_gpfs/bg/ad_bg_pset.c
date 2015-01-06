/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bg_pset.c
 * \brief Definition of functions associated to structs ADIOI_BG_ProcInfo_t and ADIOI_BG_ConfInfo_t 
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* #define TRACE_ON */
// #define bridgeringaggtrace 1

#include <stdlib.h>
#include "../ad_gpfs.h"
#include "ad_bg_pset.h"
#include <spi/include/kernel/process.h>
#include <firmware/include/personality.h>

#ifdef HAVE_MPIX_H
#include <mpix.h>
#endif

#ifndef TRACE_ERR
#  define TRACE_ERR(fmt...)
#endif

ADIOI_BG_ProcInfo_t *
ADIOI_BG_ProcInfo_new()
{
    ADIOI_BG_ProcInfo_t *p = (ADIOI_BG_ProcInfo_t *) ADIOI_Malloc (sizeof(ADIOI_BG_ProcInfo_t));
    ADIOI_Assert ((p != NULL));
    return p;
}

ADIOI_BG_ProcInfo_t *
ADIOI_BG_ProcInfo_new_n( int n )
{
    ADIOI_BG_ProcInfo_t *p = (ADIOI_BG_ProcInfo_t *) ADIOI_Malloc (n * sizeof(ADIOI_BG_ProcInfo_t));
    ADIOI_Assert ((p != NULL));
    return p;
}

void
ADIOI_BG_ProcInfo_free( ADIOI_BG_ProcInfo_t *info )
{
    if (info != NULL) ADIOI_Free (info);
}

ADIOI_BG_ConfInfo_t *
ADIOI_BG_ConfInfo_new ()
{
    ADIOI_BG_ConfInfo_t *p = (ADIOI_BG_ConfInfo_t *) ADIOI_Malloc (sizeof(ADIOI_BG_ConfInfo_t));
    ADIOI_Assert ((p != NULL));
    return p;
}


void
ADIOI_BG_ConfInfo_free( ADIOI_BG_ConfInfo_t *info )
{
    if (info != NULL) ADIOI_Free (info);
}


typedef struct
{
   int rank;
   int bridgeCoord;
} sortstruct;

static int intsort(const void *p1, const void *p2)
{
   sortstruct *i1, *i2;
   i1 = (sortstruct *)p1;
   i2 = (sortstruct *)p2;
   return(i1->bridgeCoord - i2->bridgeCoord);
}

unsigned torusSize[MPIX_TORUS_MAX_DIMS];
unsigned dimTorus[MPIX_TORUS_MAX_DIMS];

/* This function computes the number of hops between the torus coordinates of the
 * aggCoords and bridgeCoords parameters.
*/
static unsigned procManhattanDistance(unsigned *aggCoords, unsigned *bridgeCoords) {

  unsigned totalDistance = 0;
  int i;
  for (i=0;i<MPIX_TORUS_MAX_DIMS;i++) {
    unsigned dimDistance = abs((int)aggCoords[i] - (int)bridgeCoords[i]);
    if (dimDistance > 0) { // could torus make it closer?
      if (dimTorus[i]) {
        if (aggCoords[i] == torusSize[i]) { // is wrap-around closer
          if ((bridgeCoords[i]+1) < dimDistance) // assume will use torus link
            dimDistance = bridgeCoords[i]+1;
        }
        else if (bridgeCoords[i] == torusSize[i]) { // is wrap-around closer
          if ((aggCoords[i]+1) < dimDistance) // assume will use torus link
            dimDistance = aggCoords[i]+1;
        }
      }
    } /* else: dimDistance == 0, meaning aggCoords[i] and bridgeCoords[i] are
	 the same and there's no closer point to pick */
    totalDistance += dimDistance;
  }
  return totalDistance;
}


void 
ADIOI_BG_persInfo_init(ADIOI_BG_ConfInfo_t *conf, 
			ADIOI_BG_ProcInfo_t *proc, 
			int size, int rank, int n_aggrs, MPI_Comm comm)
{
   int i, iambridge=0, bridgerank = -1, bridgeIndex;
   int countPset;
   sortstruct *bridges;
   int commsize;

   TRACE_ERR("Entering BG_persInfo_init, size: %d, rank: %d, n_aggrs: %d, comm: %d\n", size, rank, n_aggrs, (int)comm);

   Personality_t pers;
   MPIX_Hardware_t hw;
   MPIX_Hardware(&hw);
   TRACE_ERR("BG_persInfo_init, my coords{%u,%u,%u,%u,%u} rankInPset %u,sizeOfPset %u,idOfPset %u\n",hw.Coords[0],hw.Coords[1],hw.Coords[2],hw.Coords[3],hw.Coords[4],hw.rankInPset,hw.sizeOfPset,hw.idOfPset);


   Kernel_GetPersonality(&pers, sizeof(pers));

   proc->rank = rank;
   proc->coreID = hw.coreID;

   if (gpfsmpio_bridgeringagg > 0) {
#ifdef bridgeringaggtrace
     if (rank == 0)
       fprintf(stderr,"Block dimensions:\n");
#endif

     /* Set the numNodesInPartition and nodeRank for this proc
     */
     proc->numNodesInPartition = 1;
     proc->nodeRank = 0;
     for (i=0;i<MPIX_TORUS_MAX_DIMS;i++) {
       torusSize[i] = hw.Size[i];
       dimTorus[i] = hw.isTorus[i];
       proc->numNodesInPartition *= hw.Size[i];
         int baseNum = 1, j;
         for (j=0;j<i;j++)
           baseNum *= hw.Size[j];
         proc->nodeRank += (hw.Coords[i] * baseNum);
#ifdef bridgeringaggtrace
       if (rank == 0)
         fprintf(stderr,"Dimension %d has %d elements wrap-around value is %d\n",i,torusSize[i],dimTorus[i]);
#endif
     }
   }

   MPI_Comm_size(comm, &commsize);

   proc->ionID = MPIX_IO_node_id ();

   if(size == 1)
   {
      proc->iamBridge = 1;
      proc->bridgeRank = rank;
      if (gpfsmpio_bridgeringagg > 0) {
        proc->manhattanDistanceToBridge = 0;
      }

      /* Set up the other parameters */
      proc->myIOSize = size;
      proc->ioNodeIndex = 0;
      conf->ioMinSize = size;
      conf->ioMaxSize = size;
      conf->numBridgeRanks = 1;
      conf->nProcs = size;
      conf->cpuIDsize = hw.ppn;
      /*conf->virtualPsetSize = conf->ioMaxSize * conf->cpuIDsize;*/
      conf->nAggrs = 1;
      conf->aggRatio = 1. * conf->nAggrs / conf->ioMinSize /*virtualPsetSize*/;
      if(conf->aggRatio > 1) conf->aggRatio = 1.;
      TRACE_ERR("I am (single) Bridge rank\n");
      return;
   }

   /* Find the nearest bridge node coords.  We don't know the
      rank in our comm so we will collective find/pick a bridge
      rank later.
   */ 
   int32_t bridgeCoords;
   bridgeCoords = pers.Network_Config.cnBridge_A << 24 | 
                  pers.Network_Config.cnBridge_B << 18 | 
                  pers.Network_Config.cnBridge_C << 12 | 
                  pers.Network_Config.cnBridge_D << 6 | 
                  pers.Network_Config.cnBridge_E << 2;
   ADIOI_Assert((bridgeCoords >= 0)); /* A dim is < 6 bits or sorting won't work */

   if((hw.Coords[0] == pers.Network_Config.cnBridge_A) && 
      (hw.Coords[1] == pers.Network_Config.cnBridge_B) && 
      (hw.Coords[2] == pers.Network_Config.cnBridge_C) && 
      (hw.Coords[3] == pers.Network_Config.cnBridge_D) && 
      (hw.Coords[4] == pers.Network_Config.cnBridge_E)) {
      iambridge = 1;      /* I am bridge */
      if (gpfsmpio_bridgeringagg > 0) {
        proc->manhattanDistanceToBridge = 0;
      }
    }
    else {  // calculate manhattan distance to bridge if gpfsmpio_bridgeringagg is set
      if (gpfsmpio_bridgeringagg > 0) {
        unsigned aggCoords[MPIX_TORUS_MAX_DIMS],manhattanBridgeCoords[MPIX_TORUS_MAX_DIMS];
        aggCoords[0] = hw.Coords[0];
        manhattanBridgeCoords[0] = pers.Network_Config.cnBridge_A;
        aggCoords[1] = hw.Coords[1];
        manhattanBridgeCoords[1] = pers.Network_Config.cnBridge_B;
        aggCoords[2] = hw.Coords[2];
        manhattanBridgeCoords[2] = pers.Network_Config.cnBridge_C;
        aggCoords[3] = hw.Coords[3];
        manhattanBridgeCoords[3] = pers.Network_Config.cnBridge_D;
        aggCoords[4] = hw.Coords[4];
        manhattanBridgeCoords[4] = pers.Network_Config.cnBridge_E;

        proc->manhattanDistanceToBridge= procManhattanDistance(aggCoords, manhattanBridgeCoords);
#ifdef bridgeringaggtrace
        fprintf(stderr,"agg coords are %u %u %u %u %u bridge coords are %u %u %u %u %u distance is %u\n",aggCoords[0],aggCoords[1],aggCoords[2],aggCoords[3],aggCoords[4],manhattanBridgeCoords[0],manhattanBridgeCoords[1],manhattanBridgeCoords[2],manhattanBridgeCoords[3],manhattanBridgeCoords[4], proc->manhattanDistanceToBridge);
#endif
      }
    }

   TRACE_ERR("Bridge coords(%8.8X): %d %d %d %d %d, %d. iambridge %d\n",bridgeCoords, pers.Network_Config.cnBridge_A,pers.Network_Config.cnBridge_B,pers.Network_Config.cnBridge_C,pers.Network_Config.cnBridge_D,pers.Network_Config.cnBridge_E,0, iambridge);

   /* Allgather the ranks and bridgeCoords to determine the bridge
      rank and how many ranks belong to each bridge rank*/
   bridges = (sortstruct *) ADIOI_Malloc(sizeof(sortstruct) * size);

   /* We're going to sort this structure by bridgeCoord:
    
   typedef struct
   {
      int rank;
      int bridgeCoord;
   } sortstruct; 
    
   and I want the rank that IS the bridge to sort first, so 
   OR in '1' on non-bridge ranks that use a bridge coord. 
   */ 

   /* My input to the collective */
   bridges[rank].rank = rank;
   bridges[rank].bridgeCoord = bridgeCoords;
   if(!iambridge)
      bridges[rank].bridgeCoord |= 1;  /* I am not bridge, turn on bit */


   MPI_Allgather(MPI_IN_PLACE, 2, MPI_INT, bridges, 2, MPI_INT, comm);

   qsort(bridges, size, sizeof(sortstruct), intsort);

   /* Once the list is sorted walk through it to setup bridge
      info and find bridge ranks, etc. */

   int tempCoords, tempRank, mincompute, maxcompute;
   tempCoords = bridges[0].bridgeCoord & ~1;
   tempRank   = bridges[0].rank;

   countPset=1;
   bridgeIndex = 0; 
   mincompute = size+1;
   maxcompute = 1;

   for(i=1; i<size; i++)
   {
      if((bridges[i].bridgeCoord  & ~1) == tempCoords) 
            countPset++; /* same bridge (pset), count it */
      else /* new bridge found */
      {
#ifdef TRACE_ON
         if(rank == 0) 
            TRACE_ERR("Bridge set %u, bridge rank %d (%#8.8X) has %d ranks\n",
                      bridgeIndex, tempRank, tempCoords, countPset);
#endif
         if(countPset > maxcompute)
            maxcompute = countPset;
         if(countPset < mincompute)
            mincompute = countPset;

         /* Was this my bridge we finished? */
         if(tempCoords == bridgeCoords)
         {
            /* Am I the bridge rank? */
            if(tempRank == rank)
               iambridge = 1;
            else 
               iambridge = 0; /* Another rank on my node may have taken over */
            TRACE_ERR("Rank %u, bridge set %u, bridge rank %d (%#8.8X) has %d ranks, iambridge %u\n",
                      rank, bridgeIndex, tempRank, tempCoords, countPset,iambridge);
            bridgerank = tempRank;
            proc->myIOSize = countPset;
            proc->ioNodeIndex = bridgeIndex;
         }
         /* Setup next bridge */
         tempCoords = bridges[i].bridgeCoord & ~1;
         tempRank   = bridges[i].rank;
         bridgeIndex++;
         countPset = 1;
      }
   }
   /* Process last bridge */

#ifdef TRACE_ON
   if(rank == 0) 
      TRACE_ERR("Bridge set %u, bridge rank %d (%#8.8X) has %d ranks\n",
                bridgeIndex, tempRank, tempCoords, countPset);
#endif
   if(countPset > maxcompute)
      maxcompute = countPset;
   if(countPset < mincompute)
      mincompute = countPset;

   /* Was this my bridge? */
   if(tempCoords == bridgeCoords)
   {
      /* Am I the bridge rank? */
      if(tempRank == rank)
         iambridge = 1;
      else 
         iambridge = 0; /* Another rank on my node may have taken over */
      bridgerank = tempRank;
      proc->myIOSize = countPset;
      proc->ioNodeIndex = bridgeIndex;
   }
   
   
   if(rank == 0) 
   {
      /* Only rank 0 has a conf structure, fill in stuff as appropriate */
      conf->ioMinSize = mincompute;
      conf->ioMaxSize = maxcompute; /* equivalent to pset size */
      conf->numBridgeRanks = bridgeIndex+1;
      conf->nProcs = size;
      conf->cpuIDsize = hw.ppn;
      /*conf->virtualPsetSize = maxcompute * conf->cpuIDsize;*/
            
      conf->nAggrs = n_aggrs;
      /*    First pass gets nAggrs = -1 */
      if(conf->nAggrs <=0)
         conf->nAggrs = gpfsmpio_bg_nagg_pset;
      if(conf->ioMinSize <= conf->nAggrs)
        conf->nAggrs = ADIOI_MAX(1,conf->ioMinSize-1); /* not including bridge itself */
/*      if(conf->nAggrs > conf->numBridgeRanks) 
         conf->nAggrs = conf->numBridgeRanks; 
*/
      conf->aggRatio = 1. * conf->nAggrs / conf->ioMinSize /*virtualPsetSize*/;
/*    if(conf->aggRatio > 1) conf->aggRatio = 1.; */
      TRACE_ERR("n_aggrs %zd, conf->nProcs %zu, conf->ioMaxSize %zu, ADIOI_BG_NAGG_PSET_DFLT %zu,conf->numBridgeRanks %zu,conf->nAggrs %zu\n",(size_t)n_aggrs, (size_t)conf->nProcs, (size_t)conf->ioMaxSize, (size_t)ADIOI_BG_NAGG_PSET_DFLT,(size_t)conf->numBridgeRanks,(size_t)conf->nAggrs);
      TRACE_ERR("Maximum ranks under a bridge rank: %d, minimum: %d, nAggrs: %d, numBridgeRanks: %d pset dflt: %d naggrs: %d ratio: %f\n", maxcompute, mincompute, conf->nAggrs, conf->numBridgeRanks, ADIOI_BG_NAGG_PSET_DFLT, conf->nAggrs, conf->aggRatio);
   }

   ADIOI_Assert((bridgerank != -1));
   proc->bridgeRank = bridgerank;
   proc->iamBridge = iambridge;
   TRACE_ERR("Rank %d has bridge set index %d (bridge rank: %d) with %d other ranks, ioNodeIndex: %d\n", rank,  proc->ioNodeIndex, bridgerank, proc->myIOSize, proc->ioNodeIndex);

   ADIOI_Free(bridges);

}

void 
ADIOI_BG_persInfo_free( ADIOI_BG_ConfInfo_t *conf, ADIOI_BG_ProcInfo_t *proc )
{
    ADIOI_BG_ConfInfo_free( conf );
    ADIOI_BG_ProcInfo_free( proc );
}
