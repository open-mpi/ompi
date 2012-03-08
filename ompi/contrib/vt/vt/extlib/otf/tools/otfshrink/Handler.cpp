/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "Handler.h"
#include <cassert>
#include <iostream>


int handleDefProcess (void *userData, uint32_t stream, uint32_t process,
		const char *name, uint32_t parent, OTF_KeyValueList* list) {


	int ret;
	firstarg *first = (firstarg*) userData;

	if ( ( cpuMap.end() == cpuMap.find( process ) ) != inverse ) {

		/* process was replaced, drop definition */
		return OTF_RETURN_OK;
	}

    /* disable parent process specification if the original parent process was dropped */
    if ( parent ) {

        if ( ( cpuMap.end() == cpuMap.find( parent ) ) != inverse ) {

            parent= 0;
        } 
    }

	/* if process represents other processes, modify its name */

	map< uint32_t, set< uint32_t > >::const_iterator ft= replacementMap.find( process );
	map< uint32_t, set< uint32_t > >::const_iterator ftend= replacementMap.end();
	if ( ftend != ft && 0 < ft->second.size() ) {

		uint32_t len= ft->second.size() +1;
		/* copy of name, truncate at 99 characters */
		char newname[100];
		/* alter process name, append hint about the number of processes replaced by this 
		one including itself */
		snprintf( newname, 100, "%s #%u", name, len );

		uint32_t* substitutes= (uint32_t*) malloc( len * sizeof(uint32_t) );
		assert( substitutes );
		uint32_t* p= substitutes;
		*p= process; ++p;

		set< uint32_t >::const_iterator it= ft->second.begin();
		set< uint32_t >::const_iterator itend= ft->second.end();
		for ( ; it != itend; ++it ) {

			*p= *it; ++p;
		}

		ret= OTF_Writer_writeDefProcessSubstitutes( (OTF_Writer*) first->writer, 
			stream, process, len, substitutes, NULL );
		if ( 0 == ret ) return OTF_RETURN_ABORT;

		free( substitutes );
		substitutes= NULL;

		ret= OTF_Writer_writeDefProcessKV ( (OTF_Writer*) first->writer, 
			stream, process, newname, parent, list);
		if ( 0 == ret ) return OTF_RETURN_ABORT;

	} else {

		ret= OTF_Writer_writeDefProcessKV ( (OTF_Writer*) first->writer, 
		stream, process, name, parent, list);
		if ( 0 == ret ) return OTF_RETURN_ABORT;
	}

	return OTF_RETURN_OK;
}


int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup,
		const char *name, uint32_t numberOfProcs, const uint32_t *procs,
		OTF_KeyValueList* list) {


	firstarg *first = (firstarg*) userData;
	
	uint32_t *mod_procs = new uint32_t[numberOfProcs];
	uint32_t mod_numberOfProcs = 0;
	int ret;

	for(uint32_t i = 0; i < numberOfProcs; i++) {

		if ( ( cpuMap.end() == cpuMap.find( procs[i] ) ) == inverse ) {

			mod_procs[mod_numberOfProcs] = procs[i];
			mod_numberOfProcs++;
		}

	}

	if(mod_numberOfProcs < 1) {
		delete[] mod_procs;
		return OTF_RETURN_OK;
	}
	
	ret = ( 0 == OTF_Writer_writeDefProcessGroupKV ( (OTF_Writer*) first->writer, stream, procGroup, name,
		mod_numberOfProcs, mod_procs, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	delete[] mod_procs;

	return ret;

}


int handleDefProcessSubstitutes (void* userData, uint32_t stream,
		uint32_t representative, uint32_t numberOfProcs,
		const uint32_t* procs, OTF_KeyValueList* list) {


	/* it isn't clear yet how to handle this definition; abort for now */

	cerr << endl << "Conflict: The input trace already contains process substitution information."
        "This probably means that it has been created by otfshrink and cannot be processed again. "
        "Please start with the original trace instead." << endl << endl;

	return OTF_RETURN_ABORT;
}
