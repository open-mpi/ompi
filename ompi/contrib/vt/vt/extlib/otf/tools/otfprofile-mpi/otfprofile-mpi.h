/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef OTFPROFILE_MPI_H
#define OTFPROFILE_MPI_H


#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif /* HAVE_CONFIG_H */

#include "datastructs.h"


/* print verbose message to stdout
   (if root_only is true only rank 0 will print the message) */
void VerbosePrint( AllData& alldata, uint8_t level, bool root_only,
         const char* fmt, ... );

/* synchronize error indicator with all worker ranks
   (either broadcast from one rank (root) or reduce from all) */
bool SyncError( AllData& alldata, bool& error, uint32_t root= (uint32_t)-1 );

/* logarithm to base b for unsigned 64-bit integer x */
uint64_t Logi( uint64_t x, uint64_t b= 2 );


#endif /* OTFPROFILE_MPI_H */
