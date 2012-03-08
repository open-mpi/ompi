/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef OTFPROFILE_H
#define OTFPROFILE_H


#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif /* HAVE_CONFIG_H */

#include "datastructs.h"


/* print verbose message to stdout
   (- do print message only if current verbose level is >= level
    - if master_only is true only the master will print the message) */
void VerbosePrint( AllData& alldata, uint8_t level, bool master_only,
         const char* fmt, ... );

/* start runtime measurement of certain scope
   (- perform measurement only if current verbose level is >= verbose_level
    - if sync is true synchronize all workers before start measurement) */
void StartMeasurement( AllData& alldata, uint8_t verbose_level, bool sync,
    const string& scope_name );

/* stop runtime measurement of certain scope
   (if sync is true synchronize all workers before stop measurement) */
void StopMeasurement( AllData& alldata, bool sync, const string& scope_name );

/* print measurement results to stdout
   (if scope_name is not specified print results of all measured scopes) */
void PrintMeasurement( AllData& alldata, const string& scope_name= "" );

/* logarithm to base b for unsigned 64-bit integer x */
uint64_t Logi( uint64_t x, uint64_t b= 2 );

#ifdef OTFPROFILE_MPI
/* synchronize error indicator with all worker ranks
   (either broadcast from one rank (root) or reduce from all) */
bool SyncError( AllData& alldata, bool& error, uint32_t root= (uint32_t)-1 );
#endif /* OTFPROFILE_MPI */


/* name of program executable */
extern const string ExeName;


#endif /* OTFPROFILE_H */
