#include "config.h"

#include "rfg_regions.h"
#include "rfg_filter.h"
#include "rfg_groups.h"

#include "vt_inttypes.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CSTACK_BSIZE 0x80 /* call stack block size */
#define HASH_MAX 0x3fd    /* size of hash table */

/* data structure for call stack entry */

typedef struct RFG_RegionStackEntry_struct
{
  RFG_RegionInfo* rinf;          /* region info */
  int32_t         climitbypush;  /* call limit by push */
} RFG_RegionStackEntry;

/* data structure for call stack */

typedef struct RFG_RegionStack_struct
{
  RFG_RegionStackEntry* entries; /* call stack entries */

  int32_t  pos;                  /* stack position */
  uint32_t size;                 /* allocated stack size */
} RFG_RegionStack;

/* main data structure for RFG Regions */

struct RFG_Regions_struct
{
  RFG_Filter*      filter;    /* instance for region filter */
  RFG_Groups*      groups;    /* instance for region grouping */

  RFG_RegionStack* stack;     /* instance for call stack */

  RFG_RegionInfo*  htab[HASH_MAX];   /* hash table for mapping
					region id's and infos
					(call limit, group, region name,...) */
};

/* initializes call stack */
static int             stack_init( RFG_RegionStack** stack );
/* enlarges call stack */
static int             stack_enlarge( RFG_RegionStack* stack );

/* puts region info to hash table */
static void            hash_put( RFG_RegionInfo** htab, uint32_t h,
				 const char* g, const char* r,
				 int32_t l );
/* gets the region info by id */
static RFG_RegionInfo* hash_get( RFG_RegionInfo** htab, uint32_t h );
/* frees hash node */
static void            hash_free_node( RFG_RegionInfo* htab );
/* frees whole hash table */
static void            hash_free( RFG_RegionInfo** htab );

RFG_Regions* RFG_Regions_init()
{
  RFG_Regions* ret;

  uint32_t i;

  /* allocate memory for RFG regions object */

  ret = ( RFG_Regions* )malloc( sizeof( RFG_Regions ) );
  if( ret == NULL )
    return NULL;

  /* initalize hash table */

  for( i = 0; i < HASH_MAX; i++ )
    ret->htab[i] = NULL;

  /* initialize call stack */

  ret->stack = NULL;
  if( !stack_init( &(ret->stack) ) )
  {
    free( ret );
    return NULL;
  }

  /* initialize RFG filter object */
  
  ret->filter = NULL;
  ret->filter = RFG_Filter_init();
  if( !ret->filter )
  {
     free( ret );
     return NULL;
  }

  /* initialize RFG groups object */

  ret->groups = NULL;
  ret->groups = RFG_Groups_init();
  if( !ret->groups )
  {
     free( ret );
     return NULL;
  }

  return ret;
}

int RFG_Regions_free( RFG_Regions* regions )
{
  int ret = 1;
  
  if( !regions ) return 0;

  /* free objects for region filter and grouping */
 
  if( !RFG_Filter_free( regions->filter ) ) ret = 0;
  if( !RFG_Groups_free( regions->groups ) ) ret = 0;

  /* free call stack */

  free( regions->stack->entries );
  free( regions->stack );

  /* free hash table */

  hash_free( regions->htab );

  /* free self */

  free( regions );
  regions = NULL;

  return ret;
}

int RFG_Regions_setFilterDefFile( RFG_Regions* regions, const char* deffile )
{
  if( !regions || !regions->filter ) return 0;

  return RFG_Filter_setDefFile( regions->filter, deffile );
}

int RFG_Regions_setGroupsDefFile( RFG_Regions* regions, const char* deffile )
{
  if( !regions || !regions->groups ) return 0;

  return RFG_Groups_setDefFile( regions->groups, deffile );
}

int RFG_Regions_readFilterDefFile( RFG_Regions* regions, int rank )
{
  if( !regions || !regions->filter ) return 0;

  return RFG_Filter_readDefFile( regions->filter, rank );
}

int RFG_Regions_readGroupsDefFile( RFG_Regions* regions )
{
  if( !regions || !regions->groups ) return 0;

  return RFG_Groups_readDefFile( regions->groups );
}

int RFG_Regions_setDefaultCallLimit( RFG_Regions* regions,
				     const uint32_t limit )
{
  if( !regions || !regions->filter ) return 0;

  return RFG_Filter_setDefaultCallLimit( regions->filter, limit );
}

int RFG_Regions_addGroupAssign( RFG_Regions* regions,
				const char* gname, int n, ... )
{
  va_list ap;
  int i;

  if( !regions || !regions->groups || !gname ) return 0;

  va_start(ap, n);

  for( i = 0; i < n; i++ )
  {
    if( !RFG_Groups_addAssign( regions->groups, gname, va_arg(ap, char*) ) )
    {
      va_end(ap);
      return 0;
    }
  }

  va_end(ap);

  return 1;
}

int RFG_Regions_getFilteredRegions( RFG_Regions* regions,
				    uint32_t* r_nrinfs, RFG_RegionInfo*** r_rinfs )
{
  uint32_t i;

  if( !regions ) return 0;

  *r_nrinfs = 0;
  *r_rinfs = NULL;

  for( i = 0; i < HASH_MAX; i++ )
  {
    if( regions->htab[i] )
    {
      RFG_RegionInfo* curr = regions->htab[i];
      while( curr )
      {
	if( curr->callLimitCD == 0 )
	{
	  if( *r_nrinfs == 0 )
	    *r_rinfs = ( RFG_RegionInfo** )malloc( sizeof( RFG_RegionInfo* ) );
	  else
	    *r_rinfs = ( RFG_RegionInfo** )realloc( *r_rinfs, ( *r_nrinfs + 1 )
					   * sizeof( RFG_RegionInfo* ) );

	  (*r_rinfs)[(*r_nrinfs)++] = curr;
	}

	curr = curr->next;
      }
    }
  }

  return 1;
}

int RFG_Regions_dumpFilteredRegions( RFG_Regions* regions,
				     char* filename )
{
  uint32_t nrinfs = 0;
  RFG_RegionInfo** rinfs = NULL;
  FILE* filt_file;
  uint32_t i;

  if( !regions ) return 0;

  /* get regions, whose call limit are reached */
  RFG_Regions_getFilteredRegions( regions,
				  &nrinfs, &rinfs);

  if( nrinfs == 0) return 1;

  if( ( filt_file = fopen( filename, "w" ) ) == NULL )
  {
    fprintf( stderr, "RFG_Regions_dumpFilteredRegions(): Error: Could not open %s\n", filename );
    return 0;
  }

  fprintf( filt_file, "# list of regions, which are denied or whose call limit are reached\n" );
  fprintf( filt_file, "# (region:limit)\n" );

  /* write region names and call limits */
  
  for( i = 0; i < nrinfs; i++ )
  {
    fprintf( filt_file, "%s:%i\n",
	     rinfs[i]->regionName,
	     rinfs[i]->callLimit == 0 ? 0 : rinfs[i]->callLimit-1 );
  }

  fclose( filt_file );
  free( rinfs );

  return 1;
}

int RFG_Regions_stackPush( RFG_Regions* regions,
			   const uint32_t rid, const uint8_t decr,
			   RFG_RegionInfo** r_rinf )
{
  if( !regions || !regions->stack ) return 0;

  /* get region info by region id */

  *r_rinf = RFG_Regions_get( regions, rid );
  if( !(*r_rinf) ) return 0;

  /* enlarge call stack if necessary */

  if( regions->stack->pos+1 == (int32_t)regions->stack->size )
  {
    if( !stack_enlarge( regions->stack ) )
    {       
      fprintf( stderr, "RFG_Regions_stackPush(): Error: Could not enlarge stack size\n" );
      return 0;
    }
  }

  /* decrement call limit of region, if desired */

  if( decr && (*r_rinf)->callLimitCD > 0 ) 
    (*r_rinf)->callLimitCD--;

  /* push pointer new call stack entry to top of the call stack */

  regions->stack->
     entries[++regions->stack->pos].rinf = *r_rinf;
  regions->stack->
     entries[regions->stack->pos].climitbypush = (*r_rinf)->callLimitCD;

  return 1;
}

int RFG_Regions_stackPop( RFG_Regions* regions,
			  RFG_RegionInfo** r_rinf, int32_t* r_climitbypush )
{
  if( !regions || !regions->stack ) return 0;

  if( regions->stack->pos == -1 )
  {
    fprintf( stderr, "RFG_Regions_stackPop(): Error: Stack underflow\n" );
    return 0;
  }

  *r_rinf =
     regions->stack->entries[regions->stack->pos].rinf;
  *r_climitbypush =
     regions->stack->entries[regions->stack->pos--].climitbypush;

  return 1;
}

RFG_RegionInfo* RFG_Regions_add( RFG_Regions* regions,
				 const char* rname, uint32_t rid )
{
  RFG_RegionInfo* rinf;
  char*    gname;
  int32_t  climit;

  if( !regions ) return NULL;

  if( !rname )
  {
    fprintf( stderr, "RFG_Regions_add(): Error: Empty region name\n" );
    return NULL;
  }

  /* look for already existing hash node of this region */

  rinf = hash_get( regions->htab, rid );
  if( !rinf )
  {
    /* get group information of this region */

    if( !RFG_Groups_get( regions->groups, rname, &gname ) )
      return NULL;

    /* get filter information of this region */

    if( !RFG_Filter_get( regions->filter, rname, &climit ) )
      return NULL;

    /* add region information to hash table */

    hash_put( regions->htab,
	      rid,
	      gname,
	      rname,
	      climit );

    rinf = hash_get( regions->htab, rid );
  }

  return rinf;
}

RFG_RegionInfo* RFG_Regions_get( RFG_Regions* regions, const uint32_t rid )
{
  RFG_RegionInfo* rinf;

  if( !regions ) return NULL;

  /* look for region informations in hash table */

  rinf = hash_get( regions->htab, rid );

  return rinf;
}

static int stack_init( RFG_RegionStack** stack )
{
  /* allocate memory for call stack object */

  *stack = ( RFG_RegionStack* )malloc( sizeof( RFG_RegionStack ) );
  if( *stack == NULL )
    return 0;

  /* allocatte memory for the call stack entries */
  (*stack)->entries =
    ( RFG_RegionStackEntry* )malloc( CSTACK_BSIZE * sizeof( RFG_RegionStack ) );
  if( (*stack)->entries == NULL )
  {
    free( *stack );
    return 0;
  }

  /* initialize allocated stack size and position */

  (*stack)->size = CSTACK_BSIZE;
  (*stack)->pos = -1;

  return 1;
}

static int stack_enlarge( RFG_RegionStack* stack )
{
  if( !stack ) return 0;

  /* reallocate memory for call stack (size + CSTACK_BSIZE) */

  stack->entries =
    ( RFG_RegionStackEntry* )realloc( stack->entries,
				      ( stack->size + CSTACK_BSIZE )
				      * sizeof( RFG_RegionStackEntry ) );
  if( stack->entries == NULL )
    return 0;
  
  stack->size += CSTACK_BSIZE;

  return 1;
}

static void hash_put( RFG_RegionInfo** htab, uint32_t h,
		      const char* g, const char* r,
		      int32_t l )
{
  uint32_t id         = h % HASH_MAX;
  RFG_RegionInfo* add = ( RFG_RegionInfo* )malloc( sizeof( RFG_RegionInfo ) );
  add->regionId       = h;
  add->groupName      = ( g != NULL ) ? strdup( g ) : NULL;
  add->regionName     = strdup( r );
  add->callLimit      = l;
  add->callLimitCD    = l;
  add->next           = htab[id];
  htab[id]            = add;
}

static RFG_RegionInfo* hash_get(RFG_RegionInfo** htab, uint32_t h)
{
  uint32_t id = h % HASH_MAX;
  RFG_RegionInfo* curr = htab[id];
  while( curr )
  {
    if( curr->regionId == h )
      return curr;

    curr = curr->next;
  }
  return NULL;
}

static void hash_free_node(RFG_RegionInfo* htab)
{
  if( htab->next )
    hash_free_node( htab->next );   /* call recursive */

  free( htab->groupName );
  free( htab->regionName );
  free( htab );
}

static void hash_free(RFG_RegionInfo** htab)
{
  uint32_t i;
  
  /* free hash nodes */

  for( i = 0; i < HASH_MAX; i++ )
  {
    if( htab[i] )
      hash_free_node( htab[i] );
  }
}
