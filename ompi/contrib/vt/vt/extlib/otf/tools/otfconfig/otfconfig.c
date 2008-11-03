/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "config.h"


#include "OTF_inttypes.h"
#include "OTF_Platform.h"
#include "otf.h"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                  \n",
" otfconfig - shows parameters of the otf configuration.           \n",
"                                                                  \n",
" otfconfig [Options]                                              \n",
"                                                                  \n",
"   options:                                                       \n",
"      -h, --help    show this help message                        \n",
"      --version     show the otf version                          \n",
"      --have-zlib   is zlib enabled                               \n",
"      --includes    path to the otf headers                       \n",
"      --libs        libline needed for linking otf                \n",
"      --sizes       print size of integer types                   \n",
"                                                                  \n",
"                                                                  \n", NULL };

int main( int argc, char** argv ) {


	int i;
	char tmp[1024];


	if( argc == 1 ) {

		SHOW_HELPTEXT;
	}
	

	for( i= 1; i < argc; ++i ) {

		if( 0 == strcmp( argv[i], "-h" ) || 0 == strcmp( argv[i], "--help" )) {
		
			SHOW_HELPTEXT;
			
		} else if ( 0 == strcmp( argv[i], "--version" ) ) {

			printf( "%u.%u.%u %s\n", OTF_VERSION_MAYOR, OTF_VERSION_MINOR, OTF_VERSION_SUB, OTF_VERSION_STRING );
		
		} else if ( 0 == strcmp( argv[i], "--have-zlib" ) ) {

#ifdef HAVE_ZLIB
				printf( "yes\n" );
#else
				printf( "no\n" );
#endif

		} else if ( 0 == strcmp( argv[i], "--includes" ) ) {

			printf( "-I%s\n", OTFCONFIG_INCLUDEDIR );
		
		} else if ( 0 == strcmp( argv[i], "--libs" ) ) {

			snprintf( tmp, sizeof(tmp) -1, "-L%s -lotf %s\n",
				OTFCONFIG_LIBDIR,
#ifdef HAVE_ZLIB
				"-lz" );
#else /* HAVE_ZLIB */
				"" );
#endif /* HAVE_ZLIB */

			printf( tmp );

		} else if ( 0 == strcmp( argv[i], "--sizes" ) ) {

			/* print size of integer types */
			printf( " sizeof(%s)= %llu\n", "  int8_t  ", (long long unsigned) sizeof(int8_t) );
			printf( " sizeof(%s)= %llu\n", "  int16_t ", (long long unsigned) sizeof(int16_t) );
			printf( " sizeof(%s)= %llu\n", "  int32_t ", (long long unsigned) sizeof(int32_t) );
			printf( " sizeof(%s)= %llu\n", "  int64_t ", (long long unsigned) sizeof(int64_t) );
			printf( " sizeof(%s)= %llu\n", " uint8_t  ", (long long unsigned) sizeof(uint8_t) );
			printf( " sizeof(%s)= %llu\n", " uint16_t ", (long long unsigned) sizeof(uint16_t) );
			printf( " sizeof(%s)= %llu\n", " uint32_t ", (long long unsigned) sizeof(uint32_t) );
			printf( " sizeof(%s)= %llu\n", " uint64_t ", (long long unsigned) sizeof(uint64_t) );

		}
	}


	return 0;
}
