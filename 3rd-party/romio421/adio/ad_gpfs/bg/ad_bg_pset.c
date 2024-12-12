/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/**
 * \file ad_bg_pset.c
 * \brief Definition of functions associated to structs ADIOI_BG_ProcInfo_t and ADIOI_BG_ConfInfo_t
 */

/* #define TRACE_ON */
// #define bridgeringaggtrace 1

#include <stdlib.h>
#include <stdbool.h>
#include "../ad_gpfs.h"
#include "ad_bg_pset.h"
#include <spi/include/kernel/process.h>
#include <firmware/include/personality.h>

#define BGQ_TORUS_MAX_DIMS 5
#define BGQ_FULL_TORUS_SIZE 512

#ifndef TRACE_ERR
#define TRACE_ERR(fmt...)
#endif

ADIOI_BG_ProcInfo_t *ADIOI_BG_ProcInfo_new()
{
    ADIOI_BG_ProcInfo_t *p = (ADIOI_BG_ProcInfo_t *) ADIOI_Malloc(sizeof(ADIOI_BG_ProcInfo_t));
    ADIOI_Assert((p != NULL));
    return p;
}

ADIOI_BG_ProcInfo_t *ADIOI_BG_ProcInfo_new_n(int n)
{
    ADIOI_BG_ProcInfo_t *p = (ADIOI_BG_ProcInfo_t *) ADIOI_Malloc(n * sizeof(ADIOI_BG_ProcInfo_t));
    ADIOI_Assert((p != NULL));
    return p;
}

void ADIOI_BG_ProcInfo_free(ADIOI_BG_ProcInfo_t * info)
{
    if (info != NULL)
        ADIOI_Free(info);
}

ADIOI_BG_ConfInfo_t *ADIOI_BG_ConfInfo_new()
{
    ADIOI_BG_ConfInfo_t *p = (ADIOI_BG_ConfInfo_t *) ADIOI_Malloc(sizeof(ADIOI_BG_ConfInfo_t));
    ADIOI_Assert((p != NULL));
    return p;
}


void ADIOI_BG_ConfInfo_free(ADIOI_BG_ConfInfo_t * info)
{
    if (info != NULL)
        ADIOI_Free(info);
}


typedef struct {
    int rank;
    int bridgeCoord;
} sortstruct;

static int intsort(const void *p1, const void *p2)
{
    sortstruct *i1, *i2;
    i1 = (sortstruct *) p1;
    i2 = (sortstruct *) p2;
    return (i1->bridgeCoord - i2->bridgeCoord);
}

unsigned torusSize[BGQ_TORUS_MAX_DIMS];
bool dimTorus[BGQ_TORUS_MAX_DIMS];

/* This function computes the number of hops between the torus coordinates of the
 * aggCoords and bridgeCoords parameters.
*/
static unsigned procManhattanDistance(unsigned *aggCoords, unsigned *bridgeCoords)
{

    unsigned totalDistance = 0;
    int i;
    for (i = 0; i < BGQ_TORUS_MAX_DIMS; i++) {
        unsigned dimDistance = abs((int) aggCoords[i] - (int) bridgeCoords[i]);
        if (dimDistance > 0) {  // could torus make it closer?
            if (dimTorus[i]) {
                if (aggCoords[i] == torusSize[i]) {     // is wrap-around closer
                    if ((bridgeCoords[i] + 1) < dimDistance)    // assume will use torus link
                        dimDistance = bridgeCoords[i] + 1;
                } else if (bridgeCoords[i] == torusSize[i]) {   // is wrap-around closer
                    if ((aggCoords[i] + 1) < dimDistance)       // assume will use torus link
                        dimDistance = aggCoords[i] + 1;
                }
            }
        }       /* else: dimDistance == 0, meaning aggCoords[i] and bridgeCoords[i] are
                 * the same and there's no closer point to pick */
        totalDistance += dimDistance;
    }
    return totalDistance;
}

int BGQ_IO_node_id()
{
    static unsigned long IO_node_id = ULONG_MAX;

    if (IO_node_id != ULONG_MAX)
        return (int) (IO_node_id >> 32);

    int rc;
    int fd;
    char *uci_str;
    char buffer[4096];

    fd = open("/dev/bgpers", O_RDONLY, 0);
    assert(fd >= 0);
    rc = read(fd, buffer, sizeof(buffer));
    assert(rc > 0);
    close(fd);

    uci_str = strstr(buffer, "BG_UCI=");
    assert(uci_str);
    uci_str += sizeof("BG_UCI=") - 1;

    IO_node_id = strtoul(uci_str, NULL, 16);
    return (int) (IO_node_id >> 32);
}

void
ADIOI_BG_persInfo_init(ADIOI_BG_ConfInfo_t * conf,
                       ADIOI_BG_ProcInfo_t * proc, int size, int rank, int n_aggrs, MPI_Comm comm)
{
    int i, iambridge = 0, bridgerank = -1, bridgeIndex;
    int countPset;
    sortstruct *bridges;
    int commsize;

    TRACE_ERR("Entering BG_persInfo_init, size: %d, rank: %d, n_aggrs: %d, comm: %d\n", size, rank,
              n_aggrs, (int) comm);

    Personality_t pers;


    Kernel_GetPersonality(&pers, sizeof(pers));
    Personality_Networks_t *net = &pers.Network_Config;

    TRACE_ERR("BG_persInfo_init, my coords{%u,%u,%u,%u,%u}\n", net->Acoord, net->Bcoord,
              net->Ccoord, net->Dcoord, net->Ecoord);
    proc->rank = rank;

    if (gpfsmpio_bridgeringagg > 0) {
#ifdef bridgeringaggtrace
        if (rank == 0)
            fprintf(stderr, "Block dimensions:\n");
#endif

        /* Set the numNodesInPartition and nodeRank for this proc
         */
        unsigned dimMaxArray[BGQ_TORUS_MAX_DIMS];
        dimMaxArray[0] = net->Anodes;
        dimMaxArray[1] = net->Bnodes;
        dimMaxArray[2] = net->Cnodes;
        dimMaxArray[3] = net->Dnodes;
        dimMaxArray[4] = net->Enodes;

        unsigned hwCoordsArray[BGQ_TORUS_MAX_DIMS];
        hwCoordsArray[0] = net->Acoord;
        hwCoordsArray[1] = net->Bcoord;
        hwCoordsArray[2] = net->Ccoord;
        hwCoordsArray[3] = net->Dcoord;
        hwCoordsArray[4] = net->Ecoord;
        proc->numNodesInPartition =
            net->Anodes * net->Bnodes * net->Cnodes * net->Dnodes * net->Enodes;
        proc->nodeRank = 0;
        /* Set the indicator for if a dimension in the partitions is a torus or not.
         */
        dimTorus[0] = (bool) (ND_ENABLE_TORUS_DIM_A & net->NetFlags);
        dimTorus[1] = (bool) (ND_ENABLE_TORUS_DIM_B & net->NetFlags);
        dimTorus[2] = (bool) (ND_ENABLE_TORUS_DIM_C & net->NetFlags);
        dimTorus[3] = (bool) (ND_ENABLE_TORUS_DIM_D & net->NetFlags);
        dimTorus[4] = (bool) (ND_ENABLE_TORUS_DIM_E & net->NetFlags);
        for (i = 0; i < BGQ_TORUS_MAX_DIMS; i++) {
            torusSize[i] = dimMaxArray[i];
            int baseNum = 1, j;
            for (j = 0; j < i; j++)
                baseNum *= dimMaxArray[j];
            proc->nodeRank += (hwCoordsArray[i] * baseNum);
#ifdef bridgeringaggtrace
            if (rank == 0)
                fprintf(stderr,
                        "numNodesInPartition is %d Dimension %d has %d elements wrap-around value is %d\n",
                        proc->numNodesInPartition, i, torusSize[i], dimTorus[i]);
#endif
        }
    }

    MPI_Comm_size(comm, &commsize);

    proc->ionID = BGQ_IO_node_id();

    if (size == 1) {
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
        conf->nAggrs = 1;
        conf->aggRatio = 1. * conf->nAggrs / conf->ioMinSize /*virtualPsetSize */ ;
        if (conf->aggRatio > 1)
            conf->aggRatio = 1.;
        TRACE_ERR("I am (single) Bridge rank\n");
        return;
    }

    /* Find the nearest bridge node coords.  We don't know the
     * rank in our comm so we will collective find/pick a bridge
     * rank later.
     */
    int32_t bridgeCoords;
    bridgeCoords = pers.Network_Config.cnBridge_A << 24 |
        pers.Network_Config.cnBridge_B << 18 |
        pers.Network_Config.cnBridge_C << 12 |
        pers.Network_Config.cnBridge_D << 6 | pers.Network_Config.cnBridge_E << 2;
    ADIOI_Assert((bridgeCoords >= 0));  /* A dim is < 6 bits or sorting won't work */

    if ((net->Acoord == pers.Network_Config.cnBridge_A) &&
        (net->Bcoord == pers.Network_Config.cnBridge_B) &&
        (net->Ccoord == pers.Network_Config.cnBridge_C) &&
        (net->Dcoord == pers.Network_Config.cnBridge_D) &&
        (net->Ecoord == pers.Network_Config.cnBridge_E)) {
        iambridge = 1;  /* I am bridge */
        if (gpfsmpio_bridgeringagg > 0) {
            proc->manhattanDistanceToBridge = 0;
        }
    } else {    // calculate manhattan distance to bridge if gpfsmpio_bridgeringagg is set
        if (gpfsmpio_bridgeringagg > 0) {
            unsigned aggCoords[BGQ_TORUS_MAX_DIMS], manhattanBridgeCoords[BGQ_TORUS_MAX_DIMS];
            aggCoords[0] = net->Acoord;
            manhattanBridgeCoords[0] = pers.Network_Config.cnBridge_A;
            aggCoords[1] = net->Bcoord;
            manhattanBridgeCoords[1] = pers.Network_Config.cnBridge_B;
            aggCoords[2] = net->Ccoord;
            manhattanBridgeCoords[2] = pers.Network_Config.cnBridge_C;
            aggCoords[3] = net->Dcoord;
            manhattanBridgeCoords[3] = pers.Network_Config.cnBridge_D;
            aggCoords[4] = net->Ecoord;
            manhattanBridgeCoords[4] = pers.Network_Config.cnBridge_E;

            proc->manhattanDistanceToBridge =
                procManhattanDistance(aggCoords, manhattanBridgeCoords);
#ifdef bridgeringaggtrace
            fprintf(stderr,
                    "agg coords are %u %u %u %u %u bridge coords are %u %u %u %u %u distance is %u\n",
                    aggCoords[0], aggCoords[1], aggCoords[2], aggCoords[3], aggCoords[4],
                    manhattanBridgeCoords[0], manhattanBridgeCoords[1], manhattanBridgeCoords[2],
                    manhattanBridgeCoords[3], manhattanBridgeCoords[4],
                    proc->manhattanDistanceToBridge);
#endif
        }
    }

    TRACE_ERR("Bridge coords(%8.8X): %d %d %d %d %d, %d. iambridge %d\n", bridgeCoords,
              pers.Network_Config.cnBridge_A, pers.Network_Config.cnBridge_B,
              pers.Network_Config.cnBridge_C, pers.Network_Config.cnBridge_D,
              pers.Network_Config.cnBridge_E, 0, iambridge);

    /* Allgather the ranks and bridgeCoords to determine the bridge
     * rank and how many ranks belong to each bridge rank */
    bridges = (sortstruct *) ADIOI_Malloc(sizeof(sortstruct) * size);

    /* We're going to sort this structure by bridgeCoord:
     *
     * typedef struct
     * {
     * int rank;
     * int bridgeCoord;
     * } sortstruct;
     *
     * and I want the rank that IS the bridge to sort first, so
     * OR in '1' on non-bridge ranks that use a bridge coord.
     */

    /* My input to the collective */
    bridges[rank].rank = rank;
    bridges[rank].bridgeCoord = bridgeCoords;
    if (!iambridge)
        bridges[rank].bridgeCoord |= 1; /* I am not bridge, turn on bit */


    MPI_Allgather(MPI_IN_PLACE, 2, MPI_INT, bridges, 2, MPI_INT, comm);

    qsort(bridges, size, sizeof(sortstruct), intsort);

    /* Once the list is sorted walk through it to setup bridge
     * info and find bridge ranks, etc. */

    int tempCoords, tempRank, mincompute, maxcompute;
    tempCoords = bridges[0].bridgeCoord & ~1;
    tempRank = bridges[0].rank;

    countPset = 1;
    bridgeIndex = 0;
    mincompute = size + 1;
    maxcompute = 1;

    for (i = 1; i < size; i++) {
        if ((bridges[i].bridgeCoord & ~1) == tempCoords)
            countPset++;        /* same bridge (pset), count it */
        else {  /* new bridge found */

#ifdef TRACE_ON
            if (rank == 0)
                TRACE_ERR("Bridge set %u, bridge rank %d (%#8.8X) has %d ranks\n",
                          bridgeIndex, tempRank, tempCoords, countPset);
#endif
            if (countPset > maxcompute)
                maxcompute = countPset;
            if (countPset < mincompute)
                mincompute = countPset;

            /* Was this my bridge we finished? */
            if (tempCoords == bridgeCoords) {
                /* Am I the bridge rank? */
                if (tempRank == rank)
                    iambridge = 1;
                else
                    iambridge = 0;      /* Another rank on my node may have taken over */
                TRACE_ERR
                    ("Rank %u, bridge set %u, bridge rank %d (%#8.8X) has %d ranks, iambridge %u\n",
                     rank, bridgeIndex, tempRank, tempCoords, countPset, iambridge);
                bridgerank = tempRank;
                proc->myIOSize = countPset;
                proc->ioNodeIndex = bridgeIndex;
            }
            /* Setup next bridge */
            tempCoords = bridges[i].bridgeCoord & ~1;
            tempRank = bridges[i].rank;
            bridgeIndex++;
            countPset = 1;
        }
    }
    /* Process last bridge */

#ifdef TRACE_ON
    if (rank == 0)
        TRACE_ERR("Bridge set %u, bridge rank %d (%#8.8X) has %d ranks\n",
                  bridgeIndex, tempRank, tempCoords, countPset);
#endif
    if (countPset > maxcompute)
        maxcompute = countPset;
    if (countPset < mincompute)
        mincompute = countPset;

    /* Was this my bridge? */
    if (tempCoords == bridgeCoords) {
        /* Am I the bridge rank? */
        if (tempRank == rank)
            iambridge = 1;
        else
            iambridge = 0;      /* Another rank on my node may have taken over */
        bridgerank = tempRank;
        proc->myIOSize = countPset;
        proc->ioNodeIndex = bridgeIndex;
    }


    if (rank == 0) {
        /* Only rank 0 has a conf structure, fill in stuff as appropriate */
        conf->ioMinSize = mincompute;
        conf->ioMaxSize = maxcompute;   /* equivalent to pset size */
        conf->numBridgeRanks = bridgeIndex + 1;
        conf->nProcs = size;

        conf->nAggrs = n_aggrs;
        /*    First pass gets nAggrs = -1 */
        if (conf->nAggrs <= 0)
            conf->nAggrs = gpfsmpio_bg_nagg_pset;
        if (conf->ioMinSize <= conf->nAggrs)
            conf->nAggrs = MPL_MAX(1, conf->ioMinSize - 1);     /* not including bridge itself */
/*      if (conf->nAggrs > conf->numBridgeRanks)
         conf->nAggrs = conf->numBridgeRanks;
*/
        conf->aggRatio = 1. * conf->nAggrs / conf->ioMinSize /*virtualPsetSize */ ;
/*    if (conf->aggRatio > 1) conf->aggRatio = 1.; */
        TRACE_ERR
            ("n_aggrs %zd, conf->nProcs %zu, conf->ioMaxSize %zu, ADIOI_BG_NAGG_PSET_DFLT %zu,conf->numBridgeRanks %zu,conf->nAggrs %zu\n",
             (size_t) n_aggrs, (size_t) conf->nProcs, (size_t) conf->ioMaxSize,
             (size_t) ADIOI_BG_NAGG_PSET_DFLT, (size_t) conf->numBridgeRanks,
             (size_t) conf->nAggrs);
        TRACE_ERR
            ("Maximum ranks under a bridge rank: %d, minimum: %d, nAggrs: %d, numBridgeRanks: %d pset dflt: %d naggrs: %d ratio: %f\n",
             maxcompute, mincompute, conf->nAggrs, conf->numBridgeRanks, ADIOI_BG_NAGG_PSET_DFLT,
             conf->nAggrs, conf->aggRatio);
    }

    ADIOI_Assert((bridgerank != -1));
    proc->bridgeRank = bridgerank;
    proc->iamBridge = iambridge;
    TRACE_ERR
        ("Rank %d has bridge set index %d (bridge rank: %d) with %d other ranks, ioNodeIndex: %d\n",
         rank, proc->ioNodeIndex, bridgerank, proc->myIOSize, proc->ioNodeIndex);

    ADIOI_Free(bridges);

}

void ADIOI_BG_persInfo_free(ADIOI_BG_ConfInfo_t * conf, ADIOI_BG_ProcInfo_t * proc)
{
    ADIOI_BG_ConfInfo_free(conf);
    ADIOI_BG_ProcInfo_free(proc);
}
