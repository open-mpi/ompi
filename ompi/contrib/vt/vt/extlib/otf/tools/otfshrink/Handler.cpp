/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "Handler.h"


/* most of these hanlders act like copyhandlers, except handleDefProcess and handleDefProcessGroup */

int handleDefinitionComment (void *userData, uint32_t stream, const char *comment) {
	
	return ( 0 == OTF_Writer_writeDefinitionComment ( (OTF_Writer*) userData, stream, comment) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefTimerResolution (void *userData, uint32_t stream, uint64_t ticksPerSecond) {

	return ( 0 == OTF_Writer_writeDefTimerResolution ( (OTF_Writer*) userData, stream, ticksPerSecond) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefProcess (void *userData, uint32_t stream, uint32_t process, const char *name, uint32_t parent) {
	
	firstarg *first = (firstarg*) userData;

	if (  first->procMap.end() == first->procMap.find(process) ) {
		return OTF_RETURN_OK;
	}

	return ( 0 == OTF_Writer_writeDefProcess ( (OTF_Writer*) first->writer, stream, process, name, parent) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup, const char *name, uint32_t numberOfProcs, const uint32_t *procs) {

	firstarg *first = (firstarg*) userData;
	
	uint32_t *mod_procs = new uint32_t[numberOfProcs];
	uint32_t mod_numberOfProcs = 0;
	int ret;

	for(uint32_t i = 0; i < numberOfProcs; i++) {
		if (  first->procMap.end() != first->procMap.find(procs[i]) ) {
			mod_procs[mod_numberOfProcs] = procs[i];
			mod_numberOfProcs++;
		}

	}

	if(mod_numberOfProcs < 1) {
		delete[] mod_procs;
		return OTF_RETURN_OK;
	}
	
	ret = ( 0 == OTF_Writer_writeDefProcessGroup ( (OTF_Writer*) first->writer, stream, procGroup, name,
		mod_numberOfProcs, mod_procs) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

	delete[] mod_procs;

	return ret;

}

int handleDefFunction (void *userData, uint32_t stream, uint32_t func, const char *name, uint32_t funcGroup, uint32_t source) {

	return ( 0 == OTF_Writer_writeDefFunction ( (OTF_Writer*) userData, stream, func, name, funcGroup, source) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefFunctionGroup (void *userData, uint32_t stream, uint32_t funcGroup, const char *name) {

	return ( 0 == OTF_Writer_writeDefFunctionGroup ( (OTF_Writer*) userData, stream, funcGroup, name) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefCollectiveOperation (void *userData, uint32_t stream, uint32_t collOp, const char *name, uint32_t type) {

	return ( 0 == OTF_Writer_writeDefCollectiveOperation ( (OTF_Writer*) userData, stream, collOp, name, type) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefCounter (void *userData, uint32_t stream, uint32_t counter, const char *name, uint32_t properties, uint32_t counterGroup, const char *unit) {

	return ( 0 == OTF_Writer_writeDefCounter ( (OTF_Writer*) userData, stream, counter, name, properties,
		counterGroup, unit) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefCounterGroup (void *userData, uint32_t stream, uint32_t counterGroup, const char *name) {

	return ( 0 == OTF_Writer_writeDefCounterGroup ( (OTF_Writer*) userData, stream, counterGroup, name) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefScl (void *userData, uint32_t stream, uint32_t source, uint32_t sourceFile, uint32_t line) {

	return ( 0 == OTF_Writer_writeDefScl ( (OTF_Writer*) userData, stream, source, sourceFile, line) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefSclFile (void *userData, uint32_t stream, uint32_t sourceFile, const char *name) {

	return ( 0 == OTF_Writer_writeDefSclFile ( (OTF_Writer*) userData, stream, sourceFile, name) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefCreator (void *userData, uint32_t stream, const char *creator) {

	return ( 0 == OTF_Writer_writeDefCreator ( (OTF_Writer*) userData, stream, creator) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefVersion (void *userData, uint32_t stream, uint8_t major, uint8_t minor, uint8_t sub, const char *string) {

	/* this is deprecated and not necessary at all */
	/*return ( 0 == OTF_Writer_writeOtfVersion ( (OTF_Writer*) userData, stream) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;*/

	return OTF_RETURN_OK;
}

int handleDefFile (void *userData, uint32_t stream, uint32_t token, const char *name, uint32_t group) {

	return ( 0 == OTF_Writer_writeDefFile ( (OTF_Writer*) userData, stream, token, name, group) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleDefFileGroup (void *userData, uint32_t stream, uint32_t token, const char *name) {

	return ( 0 == OTF_Writer_writeDefFileGroup ( (OTF_Writer*) userData, stream, token, name) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
