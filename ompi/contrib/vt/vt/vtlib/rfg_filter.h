#ifndef _RFG_FILTER_H
#define _RFG_FILTER_H

#include "vt_inttypes.h"

typedef struct RFG_Filter_struct RFG_Filter;

/* initalizes RFG filter object */
RFG_Filter* RFG_Filter_init( void );

/* cleanup RFG filter object */
int RFG_Filter_free( RFG_Filter* filter );

/* sets filter definition file name */
int RFG_Filter_setDefFile( RFG_Filter* filter, const char* deffile );

/* sets default call limit */
int RFG_Filter_setDefaultCallLimit( RFG_Filter* filter, int32_t limit );

/* reads region filter definition file
   if rank != -1, read file with MPI-rank specific entries */
int RFG_Filter_readDefFile( RFG_Filter* filter, int rank );

/* adds filter assignment */
int RFG_Filter_add( RFG_Filter* filter, const char* pattern,
                    int32_t climit );

/* gets call limit by region name */
int RFG_Filter_get( RFG_Filter* filter, const char* rname,
                    int32_t* r_climit );

#endif
