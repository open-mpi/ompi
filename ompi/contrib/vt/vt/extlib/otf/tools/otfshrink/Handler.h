/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include <map>
#include <otf.h>

using namespace std;


/* required as FirstHandlerArg later on */
typedef struct {
	OTF_Writer *writer;
	map<int, bool> procMap;
} firstarg;


int handleDefinitionComment (void *userData, uint32_t stream, const char *comment);

int handleDefTimerResolution (void *userData, uint32_t stream, uint64_t ticksPerSecond);

int handleDefProcess (void *userData, uint32_t stream, uint32_t process,
		 const char *name, uint32_t parent);

int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup,
		const char *name, uint32_t numberOfProcs, const uint32_t *procs);

int handleDefFunction (void *userData, uint32_t stream, uint32_t func, const char *name,
		uint32_t funcGroup, uint32_t source);

int handleDefFunctionGroup (void *userData, uint32_t stream, uint32_t funcGroup, const char *name);

int handleDefCollectiveOperation (void *userData, uint32_t stream, uint32_t collOp,
		const char *name, uint32_t type);

int handleDefCounter (void *userData, uint32_t stream, uint32_t counter, const char *name,
		uint32_t properties, uint32_t counterGroup, const char *unit);

int handleDefCounterGroup (void *userData, uint32_t stream, uint32_t counterGroup, const char *name);

int handleDefScl (void *userData, uint32_t stream, uint32_t source, uint32_t sourceFile, uint32_t line);

int handleDefSclFile (void *userData, uint32_t stream, uint32_t sourceFile, const char *name);

int handleDefCreator (void *userData, uint32_t stream, const char *creator);

int handleDefVersion (void *userData, uint32_t stream, uint8_t major, uint8_t minor,
		uint8_t sub, const char *string);

int handleDefFile (void *userData, uint32_t stream, uint32_t token, const char *name, uint32_t group);

int handleDefFileGroup (void *userData, uint32_t stream, uint32_t token, const char *name);

