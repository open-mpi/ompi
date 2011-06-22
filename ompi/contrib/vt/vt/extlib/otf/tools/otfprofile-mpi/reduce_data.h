/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef REDUCE_DATA_H
#define REDUCE_DATA_H

#include "datastructs.h"


/* reduce the data to the master process */
bool reduceData( uint32_t my_rank, uint32_t num_ranks, AllData& alldata );


#endif /* REDUCE_DATA_H */

