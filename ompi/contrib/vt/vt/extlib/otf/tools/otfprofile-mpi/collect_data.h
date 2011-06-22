/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef COLLECT_DATA_H
#define COLLECT_DATA_H


#include "datastructs.h"


/* collect the data for the assigned trace processes from the given
   trace file name */
bool collectData( uint32_t my_rank, uint32_t num_ranks, AllData& alldata );

#endif /* COLLECT_DATA_H */
