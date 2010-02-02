#include "config.h"

#include "rfg_groups.h"

#include "vt_inttypes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define STRBUF_SIZE  0x400   /* buffer size for strings */
#define MAX_LINE_LEN 0x20000 /* max file line length */

/* data structure for group assignments */

typedef struct RFG_GroupsAssign_struct
{
  char*    group;             /* group name */
  uint32_t npattern;          /* number of assigned pattern */
  char**   pattern;           /* array of assigned pattern */
} RFG_GroupsAssign;

/* main data structure for RFG Groups */

struct RFG_Groups_struct
{
  char* deffile;              /* name of group definition file */

  uint32_t          nassigns; /* number of group assignments */
  RFG_GroupsAssign* assigns;  /* array of group assignments */
};

RFG_Groups* RFG_Groups_init()
{
  RFG_Groups* ret;

  /* allocate memory for RFG groups object */

  ret = ( RFG_Groups* )malloc( sizeof( RFG_Groups ) );
  if( ret == NULL )
    return NULL;

  /* some initializes of data structure elements */

  ret->deffile = NULL;

  ret->nassigns = 0;
  ret->assigns = NULL;

  return ret;
}

int RFG_Groups_free( RFG_Groups* groups )
{
  uint32_t i;
  uint32_t j;

  if( !groups ) return 0;

  /* free group definition file name */

  if( groups->deffile )
    free( groups->deffile );

  /* free array of group assignments */

  for( i = 0; i < groups->nassigns; i++ )
  {
    for( j = 0; j < groups->assigns[i].npattern; j++ )
      free( groups->assigns[i].pattern[j] );

    free( groups->assigns[i].group );
    free( groups->assigns[i].pattern );
  }

  free( groups->assigns );

  /* free self */

  free( groups );
  groups = NULL;

  return 1;
}

int RFG_Groups_setDefFile( RFG_Groups* groups, const char* deffile )
{
  if( !groups ) return 0;

  /* if a group definition file already set, then free this */

  if( groups->deffile )
    free( groups->deffile );

  /* set new group definition file */

  groups->deffile = strdup( deffile );

  return 1;
}

int RFG_Groups_readDefFile( RFG_Groups* groups )
{
  FILE*    f;
  char*    orgline;
  uint32_t lineno = 0;
  uint8_t  parse_err = 0;

  if( !groups ) return 0;

  if( !groups->deffile ) return 1;

  /* open group definition file */

  f = fopen( groups->deffile, "r" );
  if( !f )
  {
    fprintf( stderr,
	     "RFG_Groups_readDefFile(): Error: Could not open file '%s'\n",
	     groups->deffile );
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
    char  group[STRBUF_SIZE];
    char* p;
    char* line;

    /* remove newline */

    if( strlen(orgline) > 0 && orgline[strlen(orgline)-1] == '\n' )
      orgline[strlen(orgline)-1] = '\0';

    /* copy line so that the original line keep alive */

    line = strdup( orgline );

    lineno++;

    if( strlen( line ) == 0 )
    {
      free( line );
      continue;
    }

    vt_strtrim( line );

    if( line[0] == '#' )
    {
      free( line );
      continue;
    }

    /* search for '='
       e.g. "GROUP=func1;func2;func3"
                  p
    */

    p = strchr( line, '=' );
    if( p == NULL )
    {
      parse_err = 1;
      free( line );
      break;
    }

    /* cut group name from line 
       e.g.   "GROUP=func1;func2;func3"
           => "GROUP"
    */

    *p = '\0';
    strcpy( group, line );
    vt_strtrim( group );

    /* split remaining line at ';' to get pattern */

    p = strtok( p+1, ";" );
    do
    {
      char pattern[STRBUF_SIZE];

      if( !p )
      {
	parse_err = 1;
	break;
      }

      strcpy( pattern, p );

      vt_strtrim( pattern );

      /* add group assignment */

      if( strlen( pattern ) > 0 )
	RFG_Groups_addAssign( groups, group, pattern );

    } while( ( p = strtok( 0, ";" ) ) );

    free( line );
  }

  if( parse_err )
  {
    fprintf( stderr, "%s:%u: Could not parse line '%s'\n",
	     groups->deffile, lineno, orgline );
  }

  free( orgline );

  fclose( f );

  return parse_err ? 0 : 1;
}

int RFG_Groups_addAssign( RFG_Groups* groups, const char* gname,
			  const char* pattern )
{
  uint32_t i;
  RFG_GroupsAssign* entry = NULL;

  if( !groups || !gname || !pattern ) return 0;

  /* search group assignment by group name */

  for( i = 0; i < groups->nassigns; i++ )
  {
    if( strcmp( groups->assigns[i].group, gname ) == 0 )
    {
      entry = &(groups->assigns[i]);
      break;
    }
  }

  /* if no entry found, then allocate new group assignment entry */

  if( !entry )
  {
    if( !groups->assigns )
    {
      groups->assigns =
	( RFG_GroupsAssign* )malloc( sizeof( RFG_GroupsAssign ) );
    }
    else
    {
      groups->assigns =
	(RFG_GroupsAssign* )realloc( groups->assigns,
				     ( groups->nassigns + 1 )
				     * sizeof( RFG_GroupsAssign ) );
    }

    if( groups->assigns == NULL )
      return 0;

    entry = &(groups->assigns[groups->nassigns++]);
    entry->group = strdup( gname );
    entry->npattern = 0;
    entry->pattern = NULL;
  }

  /* add pattern to group */

  if( !entry->pattern )
  {
    entry->pattern = ( char** )malloc( sizeof( char * ) );
  }
  else
  {
    entry->pattern = ( char** )realloc( entry->pattern,
					( entry->npattern + 1 )
					* sizeof( char * ) );
  }
  if( entry->pattern == NULL )
    return 0;

  entry->pattern[entry->npattern++] = strdup( pattern );

  return 1;
}

int RFG_Groups_get( RFG_Groups* groups, const char* rname, 
		    char** r_gname )
{
  uint32_t i;
  uint32_t j;

  if( !groups || !rname ) return 0;

  /* search for matching pattern by region name */

  for( i = 0; i < groups->nassigns; i++ )
  {
    for( j = 0; j < groups->assigns[i].npattern; j++ )
    {
      if( fnmatch( groups->assigns[i].pattern[j], rname, 0 ) == 0 )
      {
	*r_gname = groups->assigns[i].group;
	return 1;
      }
    }
  }

  *r_gname = NULL;

  return 1;
}
