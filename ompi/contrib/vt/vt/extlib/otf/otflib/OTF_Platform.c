/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2007.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "OTF_Platform.h"

#include <stdlib.h>
#include <string.h>

char *OTF_strdup(const char *s) {
	char *c;

	if( s == NULL || ( c = ( char* )malloc( strlen(s)+1 ) ) == NULL )
		return NULL;

	strcpy( c, s );

	return c;
}
