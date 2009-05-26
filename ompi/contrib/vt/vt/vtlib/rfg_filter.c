#include "config.h"

#include "rfg_filter.h"
#include "rfg_strmkrs.h"

#include "vt_inttypes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define STRBUF_SIZE  0x400   /* buffer size for strings */
#define MAX_LINE_LEN 0x20000 /* max file line length */

/* data structure for fitler assignments */

typedef struct RFG_FilterAssigns_struct
{
  int32_t climit;              /* call limit */
  char*   pattern;             /* pattern */
} RFG_FilterAssigns;

struct RFG_Filter_struct
{
  char*   deffile;             /* name of filter definition file */
  int32_t default_call_limit;  /* default call limit */

  uint32_t           nassigns; /* number of filter assignments */
  RFG_FilterAssigns* assigns;  /* array of filter assignments */
};

RFG_Filter* RFG_Filter_init()
{
  RFG_Filter* ret;

  /* allocate memory for RFG filter object */

  ret = ( RFG_Filter* )malloc( sizeof( RFG_Filter ) );
  if( ret == NULL )
    return NULL;

  /* some initializes of data structure elements */

  ret->deffile = NULL;
  ret->default_call_limit = -1;

  ret->nassigns = 0;
  ret->assigns = NULL;

  return ret;
}

int RFG_Filter_free( RFG_Filter* filter )
{
  uint32_t i;

  if( !filter ) return 0;

  /* free filter definition file name */

  if( filter->deffile )
    free( filter->deffile );

  /* free array of call limit assignments */

  for( i = 0; i < filter->nassigns; i++ )
    free( filter->assigns[i].pattern );

  free( filter->assigns );

  /* free self */

  free( filter );
  filter = NULL;

  return 1;
}

int RFG_Filter_setDefFile( RFG_Filter* filter, const char* deffile )
{
  if( !filter ) return 0;

  /* if a filter definition file already set, then free this */

  if( filter->deffile )
    free( filter->deffile );

  /* set new filter definition file */

  filter->deffile = strdup( deffile );

  return 1;
}

int RFG_Filter_setDefaultCallLimit( RFG_Filter* filter, int32_t limit )
{
  if( !filter ) return 0;

  if( limit == 0 || limit < -1 )
  {
    fprintf( stderr,
	     "RFG_Filter_setDefaultCallLimit(): Error: Default call limit must be greater then 0 or -1\n" );
    return 0;
  }

  /* set new default call limit */

  filter->default_call_limit = limit == -1 ? limit : limit+1;

  return 1;
}

int RFG_Filter_readDefFile( RFG_Filter* filter )
{
  FILE*    f;
  char*    orgline;
  uint32_t lineno = 0;
  uint8_t  parse_err = 0;

  if( !filter ) return 0;

  if( !filter->deffile ) return 1;

  /* open filter definition file */

  f = fopen( filter->deffile, "r" );
  if( !f )
  {
    fprintf( stderr,
	     "RFG_Filter_readDefFile(): Error: Could not open file '%s'\n",
	     filter->deffile );
    return 0;
  }

  orgline = ( char* )malloc( MAX_LINE_LEN * sizeof( char ) );
  if( orgline == NULL )
  {
    fclose( f );
    return 0;
  }

  /* read lines */

  while( !parse_err && fgets( orgline, MAX_LINE_LEN - 1, f ) )
  {
    int32_t climit;
    char* p;
    char* line;

    /* remove newline */

    chomp( orgline );
    
    /* copy line so that the original line keep alive */

    line = strdup( orgline );

    lineno++;

    if( strlen( line ) == 0 )
    {
      free( line );
      continue;
    }

    trim( line );

    if( line[0] == '#' )
    {
      free( line );
      continue;
    }
    
    /* search for '--'
       e.g. "func1;func2;func3 -- 1000"
                               p
    */

    p = strstr( line, "--" );
    if( p == NULL )
    {
      parse_err = 1;
      free( line );
      break;
    }

    /* get call limit
       e.g. "func1;func2;func3 -- 1000"
                                  p+2
    */

    climit = atoi( p+2 );
    if( climit != -1 && climit != 0 )
      climit++;

    /* cut call limit from remaining line
       e.g.   "func1;func2;func3 -- 1000"
           => "func1;func2;func3"
    */

    *p = '\0';

    /* split remaining line at ';' to get pattern */

    p = strtok( line, ";" );
    do
    {
      char pattern[STRBUF_SIZE];

      if( !p )
      {
	parse_err = 1;
	break;
      }

      strcpy( pattern, p );

      trim( pattern );

      /* add call limit assignment */

      if( strlen( pattern ) > 0 )
	RFG_Filter_add( filter, pattern, climit );

    } while( ( p = strtok( 0, ";" ) ) );

    free( line );
  }

  if( parse_err )
  {
    fprintf( stderr, "%s:%u: Could not parse line '%s'\n",
	     filter->deffile, lineno, orgline );
  }

  free( orgline );

  fclose( f );

  return 1;
}

int RFG_Filter_add( RFG_Filter* filter, const char* pattern,
                    int32_t climit )
{
  if( !filter || !pattern ) return 0;

  /* enlarge array of filter assignments */

  filter->assigns =
    (RFG_FilterAssigns*)realloc( filter->assigns,
                                 ( filter->nassigns + 1 )
                                 * sizeof( RFG_FilterAssigns ) );

  if( filter->assigns == NULL )
    return 0;

  /* add new filter assignment */

  filter->assigns[filter->nassigns].climit = climit;
  filter->assigns[filter->nassigns].pattern = strdup( pattern );
  filter->nassigns++;

  return 1;
}

int RFG_Filter_get( RFG_Filter* filter, const char* rname,
                    int32_t* r_climit )
{
  uint32_t i;

  if( !filter || !rname ) return 0;

  /* search for matching pattern by region name */

  for( i = 0; i < filter->nassigns; i++ )
  {
    if( fnmatch( filter->assigns[i].pattern, rname, 0 ) == 0 )
    {
      *r_climit = filter->assigns[i].climit;
      break;
    }
  }

  /* return default call limit, if no matching pattern found */

  if( i == filter->nassigns )
    *r_climit = filter->default_call_limit;

  return 1;
}
