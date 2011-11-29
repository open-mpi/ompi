/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "hash.h"

hashtabT* hash_new( ) {

	
	int i;
	hashtabT* ret;
	
	ret = ( hashtabT* ) malloc( sizeof( hashtabT ) * HASH_SIZE );
	
	for( i = 0; i < HASH_SIZE; i++) {
	
		ret[i].entry.streamid = 0;
	
		ret[i].entry.ticksPerSecond = 0;
		
		ret[i].entry.functions = NULL;
		ret[i].entry.nfunctions = 0;
		ret[i].entry.sfunctions = 0;
		
		ret[i].entry.functiongroups = NULL;
		ret[i].entry.nfunctiongroups = 0;
		ret[i].entry.sfunctiongroups = 0;
		
		ret[i].entryvecsize = -1;
		ret[i].p_entryvec = 0;
	}
	
	return ret;
}


void hash_delete( hashtabT *hash ) {


	int i;
	int a;
	int b;
	
	for( i = 0; i < HASH_SIZE; i++) {
	
		/* del functions (free namestrings) */
		if( hash[i].entry.functions != NULL ) {

			for( a= 0; a < hash[i].entry.nfunctions; ++a ) {
				if( NULL != hash[i].entry.functions[a].name ) {
					free( hash[i].entry.functions[a].name );
				}
			}
			free( hash[i].entry.functions );
		}
		
		/* del functiongroups (free namestrings) */
		if( hash[i].entry.functiongroups != NULL ) {
		
			for( a= 0; a < hash[i].entry.nfunctiongroups; ++a ) {
				if( NULL != hash[i].entry.functiongroups[a].name ) {
					free( hash[i].entry.functiongroups[a].name );
				}
			}
			free( hash[i].entry.functiongroups );
		}


		if ( hash[i].entryvecsize > 0 ) {

			for( a= 0; a < hash[i].entryvecsize ; ++a ) {
			
				/* del functions (free namestrings) */
				if( hash[i].p_entryvec[a].functions != NULL ) {
		
					for( b= 0; b < hash[i].p_entryvec[a].nfunctions; ++b ) {
						if( NULL != hash[i].p_entryvec[a].functions[b].name ) {
							free( hash[i].p_entryvec[a].functions[b].name );
						}
					}
					free( hash[i].p_entryvec[a].functions );
				}
				
				/* del functiongroups (free namestrings) */
				if( hash[i].p_entryvec[a].functiongroups != NULL ) {
				
					for( b= 0; b < hash[i].p_entryvec[a].nfunctiongroups; ++b ) {
						if( NULL != hash[i].p_entryvec[a].functiongroups[b].name ) {
							free( hash[i].p_entryvec[a].functiongroups[b].name );
						}
					}
					free( hash[i].p_entryvec[a].functiongroups );
				}
			}
			
			free( hash[i].p_entryvec );
		}
	}
	
	free( hash );
}


void hash_add( hashtabT *hash, uint32_t entry ) {

	uint32_t hashkey = entry;
	
	HASH_GET_KEY( hashkey );
	
	if ( hash[hashkey].entryvecsize == -1 ) {
	
		hash[hashkey].entry.streamid = entry;
		
		hash[hashkey].entry.ticksPerSecond= 0;
		hash[hashkey].entry.functions= NULL;
		hash[hashkey].entry.nfunctions= 0;
		hash[hashkey].entry.sfunctions= 0;
		hash[hashkey].entry.functiongroups= NULL;
		hash[hashkey].entry.nfunctiongroups= 0;
		hash[hashkey].entry.sfunctiongroups= 0;
		
		hash[hashkey].entryvecsize = 0;
	
	} else { /* realloc the entryvector and insert the new entry */
	
		hash[hashkey].p_entryvec = (streaminfoT *) realloc( hash[hashkey].p_entryvec,
			sizeof( streaminfoT ) * ( hash[hashkey].entryvecsize + 1) );
			
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].streamid = entry;;
		
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].ticksPerSecond= 0;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].functions= NULL;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].nfunctions= 0;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].sfunctions= 0;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].functiongroups= NULL;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].nfunctiongroups= 0;
		hash[hashkey].p_entryvec[hash[hashkey].entryvecsize].sfunctiongroups= 0;
		
		hash[hashkey].entryvecsize++;
		
	}
}

streaminfoT* hash_search( hashtabT *hash, uint32_t entry) {

	int i;
	uint32_t hashkey = entry;
	
	HASH_GET_KEY( hashkey );
	
	if ( hash[hashkey].entry.streamid == entry ) {
		return &hash[hashkey].entry;
	} else {
		for( i = 0; i < hash[hashkey].entryvecsize; i++ ) {
		
			if ( hash[hashkey].p_entryvec[i].streamid == entry )
				return &(hash[hashkey].p_entryvec[i]);
		}
	}
	
	return NULL;
}
