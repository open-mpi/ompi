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
#define HASH_MAX 0x400    /* size of hash table (must be a power of two!) */

/* data structure for call stack entry */

typedef struct RFG_RegionStackEntry_struct
{
  RFG_RegionInfo* rinf;     /* region info */
  uint8_t rejected;         /* indicator flag whether region was rejected when
                               pushed to stack */
} RFG_RegionStackEntry;

/* data structure for call stack */

typedef struct RFG_RegionStack_struct
{
  RFG_RegionStackEntry* entries; /* call stack entries */

  int32_t               pos;     /* current call stack position */
  uint32_t              size;    /* allocated call stack size */
} RFG_RegionStack;

/* main data structure for RFG Regions */

struct RFG_Regions_struct
{
  RFG_Filter*      filter;              /* RFG filter object */
  RFG_Groups*      groups;              /* RFG groups object */

  RFG_RegionStack* stack;               /* call stack */

  RFG_RegionInfo*  htab[HASH_MAX];      /* hash table for mapping
                                           region id's and infos
                                           (call limit, stack level bounds,
                                           group name, region name, ...) */

  uint32_t recursive_filter_active_cnt; /* recursive filter activation
                                           counter */
};

/* initializes call stack */
static int             stack_init( RFG_RegionStack** stack );
/* enlarges call stack */
static int             stack_enlarge( RFG_RegionStack* stack );

/* puts region info to hash table */
static void            hash_put( RFG_RegionInfo** htab, uint32_t rid,
                                 const char* gname, const char* rname,
                                 int32_t climit, uint32_t* sbounds,
                                 uint8_t flags );
/* gets the region info by id */
static RFG_RegionInfo* hash_get( RFG_RegionInfo** htab, uint32_t rid );
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

  /* initialize recursive filter activation counter */
  ret->recursive_filter_active_cnt = 0;

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

int RFG_Regions_readFilterDefFile( RFG_Regions* regions,
                                   int rank, uint8_t* rank_off )
{
  if( !regions || !regions->filter ) return 0;

  return RFG_Filter_readDefFile( regions->filter, rank, rank_off );
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

int RFG_Regions_stackPush( RFG_Regions* regions,
                           const uint32_t rid, const uint8_t decrement,
                           RFG_RegionInfo** r_rinf, uint8_t* r_rejected )
{
  RFG_RegionStackEntry* top;

  if( !regions || !regions->stack ) return 0;

  /* get region info by region id */

  *r_rinf = hash_get( regions->htab, rid );
  if( !(*r_rinf) ) return 0;

  /* increment call stack position */
  regions->stack->pos++;

  /* enlarge call stack, if necessary */

  if( regions->stack->pos == (int32_t)regions->stack->size )
  {
    if( !stack_enlarge( regions->stack ) )
    {       
      fprintf( stderr, "RFG_Regions_stackPush(): Error: Could not enlarge stack size\n" );
      return 0;
    }
  }

  /* get pointer to the top of the call stack */
  top = &(regions->stack->entries[regions->stack->pos]);

  /* reject or approve region enter */
  if( regions->recursive_filter_active_cnt > 0 ||
      (*r_rinf)->callLimitCD == 0 ||
      (uint32_t)regions->stack->pos+1 < (*r_rinf)->stackBounds[0] ||
      (uint32_t)regions->stack->pos+1 > (*r_rinf)->stackBounds[1] )
  {
    *r_rejected = 1;

    /* increment recursive filter activation counter, if region shall be
       filtered recursively */
    if( ((*r_rinf)->flags & RFG_FILTER_FLAG_RECURSIVE) != 0 )
      regions->recursive_filter_active_cnt++;
  }
  else
  {
    *r_rejected = 0;

    /* decrement region's call limit, if allowed */
    if( decrement && (*r_rinf)->callLimitCD > 0 )
      (*r_rinf)->callLimitCD--;
  }

  /* put region info and rejected indicator flag to the top of the call stack */

  top->rinf = *r_rinf;
  top->rejected = *r_rejected;

  return 1;
}

int RFG_Regions_stackPop( RFG_Regions* regions,
                          RFG_RegionInfo** r_rinf, uint8_t* r_rejected )
{
  RFG_RegionStackEntry* top;

  if( !regions || !regions->stack ) return 0;

  if( regions->stack->pos == -1 )
  {
    fprintf( stderr, "RFG_Regions_stackPop(): Error: Stack underflow\n" );
    return 0;
  }

  /* get pointer to the top of the call stack and decrement call stack
     position */
  top = &(regions->stack->entries[regions->stack->pos--]);

  /* decrement recursive filter activation counter, if region is rejected and
     filtered recursively */
  if( top->rejected && (top->rinf->flags & RFG_FILTER_FLAG_RECURSIVE) != 0 )
  {
    if( regions->recursive_filter_active_cnt == 0 )
    {
      fprintf( stderr, "RFG_Regions_stackPop(): Error: Underflow of recursive filter activation counter\n" );
      return 0;
    }

    regions->recursive_filter_active_cnt--;
  }

  /* get region info and rejected indicator flag from the top of the
     call stack */

  *r_rinf = top->rinf;
  *r_rejected = top->rejected;

  return 1;
}

RFG_RegionInfo* RFG_Regions_add( RFG_Regions* regions, uint32_t rid,
                                 const char* rname, const char* defgname )
{
  RFG_RegionInfo* rinf;
  char* gname = NULL;
  int32_t climit;
  uint32_t sbounds[2];
  uint8_t flags;

  if( !regions ) return NULL;

  if( !rname )
  {
    fprintf( stderr, "RFG_Regions_add(): Error: Empty region name\n" );
    return NULL;
  }

  if( !defgname )
  {
    fprintf( stderr, "RFG_Regions_add(): Error: Empty default group name\n" );
    return NULL;
  }

  /* look for already existing hash node of this region */
  rinf = hash_get( regions->htab, rid );
  if( !rinf )
  {
    /* get group information of this region */
    if( !RFG_Groups_get( regions->groups, rname, &gname ) )
      return NULL;

    /* set group name to given default, if no group information present */
    if( !gname )
      gname = (char*)defgname;

    /* get filter information of this region */
    if( !RFG_Filter_get( regions->filter, rname, gname, &climit, sbounds,
                         &flags ) )
      return NULL;

    /* add region information to hash table */
    hash_put( regions->htab,
              rid,
              gname,
              rname,
              climit,
              sbounds,
              flags );

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

static void hash_put( RFG_RegionInfo** htab, uint32_t rid,
                      const char* gname, const char* rname,
                      int32_t climit, uint32_t* sbounds, uint8_t flags )
{
  uint32_t id         = rid & ( HASH_MAX - 1 );
  RFG_RegionInfo* add = ( RFG_RegionInfo* )malloc( sizeof( RFG_RegionInfo ) );
  add->regionId       = rid;
  add->groupName      = ( gname != NULL ) ? strdup( gname ) : NULL;
  add->regionName     = strdup( rname );
  add->callLimit      = climit;
  add->callLimitCD    = climit;
  add->stackBounds[0] = sbounds[0];
  add->stackBounds[1] = sbounds[1];
  add->flags          = flags;
  add->next           = htab[id];
  htab[id]            = add;
}

static RFG_RegionInfo* hash_get(RFG_RegionInfo** htab, uint32_t rid)
{
  uint32_t id = rid & ( HASH_MAX - 1 );
  RFG_RegionInfo* curr = htab[id];
  while( curr )
  {
    if( curr->regionId == rid )
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
