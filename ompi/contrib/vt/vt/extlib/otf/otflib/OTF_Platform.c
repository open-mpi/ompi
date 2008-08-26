/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "OTF_Platform.h"

#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) /* windows */

#	include <Windows.h>

	int gettimeofday(struct timeval* tv, void* dummytimezone) {
		union {
			long long ns100;
			FILETIME ft;
		} now;
 
		GetSystemTimeAsFileTime (&now.ft);
		tv->tv_usec = (long) ((now.ns100 / 10LL) % 1000000LL);
		tv->tv_sec = (long) ((now.ns100 - 116444736000000000LL) / 10000000LL);

		return 0;
	}

#endif /* _WIN32 */

char* OTF_basename(char* path) {
	char *ret;
#if defined(_WIN32)
	const char* s = "\\";
#else
	const char* s = "/";
#endif
	
	if( path == NULL || strlen( path ) == 0 ) {
		ret = OTF_strdup( "." );
	} else if( path[strlen(path)-1] == *s ) {
		ret = OTF_strdup( s );
	} else {
		char* tmp;
		if( ( tmp = strrchr( path, *s ) ) != NULL )
			ret = OTF_strdup( tmp+1 );
		else
			ret = OTF_strdup( path );
	}
	
	return ret;
}

char* OTF_strdup(const char* s) {
	char *c;

	if( s == NULL || ( c = ( char* )malloc( strlen(s)+1 ) ) == NULL )
		return NULL;

	strcpy( c, s );

	return c;
}
