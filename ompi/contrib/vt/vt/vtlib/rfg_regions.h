#ifndef _RFG_REGIONS_H
#define _RFG_REGIONS_H

#include "rfg_filter.h"
#include "rfg_groups.h"

#include "vt_inttypes.h"
#include <stdarg.h>

typedef struct RFG_Regions_struct RFG_Regions;

/* data structure for hash node (mapping of region id/info) */

typedef struct RFG_RegionInfo_struct
{
  uint32_t regionId;         /* region id */
  char*    groupName;        /* group name */
  char*    regionName;       /* region name */
  int32_t  callLimit;        /* call limit */
  int32_t  callLimitCD;      /* call limit count down */
  struct RFG_RegionInfo_struct* next;
} RFG_RegionInfo;

/* initalizes RFG regions object */
RFG_Regions* RFG_Regions_init( void );

/* cleanup RFG regions object */
int RFG_Regions_free( RFG_Regions* regions );

/* sets region filter definition file */
int RFG_Regions_setFilterDefFile( RFG_Regions* regions, const char* deffile );

/* sets region grouping definition file */
int RFG_Regions_setGroupsDefFile( RFG_Regions* regions, const char* deffile );

/* reads region filter definition file */
int RFG_Regions_readFilterDefFile( RFG_Regions* regions );

/* reads region grouping definition file */
int RFG_Regions_readGroupsDefFile( RFG_Regions* regions );

/* sets default call limit */
int RFG_Regions_setDefaultCallLimit( RFG_Regions* regions,
				     const uint32_t limit );

/* sets default group name */
int RFG_Regions_setDefaultGroup( RFG_Regions* regions,
				 const char* name );

/* adds group assignment */
int RFG_Regions_addGroupAssign( RFG_Regions* regions,
				const char* gname, int n, ... );

/* gets list of regions, whose call limit are reached */
int RFG_Regions_getFilteredRegions( RFG_Regions* regions,
				    uint32_t* r_nrinfs, RFG_RegionInfo*** r_rinfs );

/* function that should be called if a region enter event invoked */
int RFG_Regions_stackPush( RFG_Regions* regions,
			   const uint32_t rid, const uint8_t decr,
			   RFG_RegionInfo** r_rinf );

/* function that should be called if a region leave event invoked */
int RFG_Regions_stackPop( RFG_Regions* regions,
			  RFG_RegionInfo** r_rinf, int32_t* r_climitbypush );

/* adds region */
RFG_RegionInfo* RFG_Regions_add( RFG_Regions* regions,
				 const char* rname, uint32_t rid );

/* gets region informations by region id */
RFG_RegionInfo* RFG_Regions_get( RFG_Regions* regions,
				 const uint32_t rid );

#endif
