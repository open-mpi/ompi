/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef PYOTF_WRAPPER_H
#define PYOTF_WRAPPER_H

/* *** PROTOTYPES *********************************************************** */
/* ************************************************************************** */
int pyOTF_COUNTER_VARTYPE_ISINTEGER( uint64_t x );

int pyOTF_COUNTER_VARTYPE_ISSIGNED( uint64_t x );

int pyOTF_COUNTER_VARTYPE_ISUNSIGNED( uint64_t x );


int pyOTF_MasterControl_appendList( OTF_MasterControl* mc, uint32_t argument,
	uint32_t l, PyObject* values );


void pyOTF_HandlerArray_close( OTF_HandlerArray* handlers );

int pyOTF_HandlerArray_setHandler( OTF_HandlerArray* handlers, PyObject* function, uint32_t recordtype );

int pyOTF_HandlerArray_setFirstHandlerArg( OTF_HandlerArray* handlers, PyObject* fha, uint32_t recordtype );


/* *** writer declarations ***************************************************** */

int pyOTF_Writer_writeDefProcessGroup( OTF_Writer* writer, uint32_t stream, 
	uint32_t procGroup, const char* name, uint32_t numberOfProcs, 
	PyObject* procs);



/* *** handler declarations **************************************************** */

int pyOTF_Handler_DefinitionComment( void* userData, uint32_t stream, 
	const char* comment );


int pyOTF_Handler_DefTimerResolution( void* userData, uint32_t stream, 
	uint64_t ticksPerSecond );


int pyOTF_Handler_DefProcess( void* userData, uint32_t stream, 
	uint32_t process, const char* name, uint32_t parent );


int pyOTF_Handler_DefProcessGroup( void* userData, uint32_t stream, 
	uint32_t procGroup, const char* name, uint32_t numberOfProcs, 
	const uint32_t* procs );


int pyOTF_Handler_DefFunction( void* userData, uint32_t stream, 
	uint32_t func, const char* name, uint32_t funcGroup, 
	uint32_t source );


int pyOTF_Handler_DefFunctionGroup( void* userData, uint32_t stream, 
	uint32_t funcGroup, const char* name );


int pyOTF_Handler_DefCollectiveOperation( void* userData, uint32_t stream, 
	uint32_t collOp, const char* name, uint32_t type );


int pyOTF_Handler_DefCounter( void* userData, uint32_t stream, 
	uint32_t counter, const char* name, uint32_t properties, 
	uint32_t counterGroup, const char* unit );


int pyOTF_Handler_DefCounterGroup( void* userData, uint32_t stream, 
	uint32_t counterGroup, const char* name );


int pyOTF_Handler_DefScl( void* userData, uint32_t stream, 
	uint32_t source, uint32_t sourceFile, uint32_t line );


int pyOTF_Handler_DefSclFile( void* userData, uint32_t stream, 
	uint32_t sourceFile, const char* name );


int pyOTF_Handler_DefCreator( void* userData, uint32_t stream, 
	const char* creator );


int pyOTF_Handler_DefVersion( void* userData, uint32_t stream, 
	uint8_t major, uint8_t minor, uint8_t sub, 
	const char* string );


int pyOTF_Handler_DefFile( void* userData, uint32_t stream, 
	uint32_t token, const char *name, uint32_t group );


int pyOTF_Handler_DefFileGroup( void* userData, uint32_t stream, 
	uint32_t token, const char *name );


int pyOTF_Handler_Enter( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint32_t source );


int pyOTF_Handler_Leave( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint32_t source );


int pyOTF_Handler_SendMsg( void* userData, uint64_t time, 
	uint32_t sender, uint32_t receiver, uint32_t group, 
	uint32_t type, uint32_t length, uint32_t source );


int pyOTF_Handler_RecvMsg( void* userData, uint64_t time, 
	uint32_t recvProc, uint32_t sendProc, uint32_t group, 
	uint32_t type, uint32_t length, uint32_t source );


int pyOTF_Handler_Counter( void* userData, uint64_t time, 
	uint32_t process, uint32_t counter, uint64_t value );


int pyOTF_Handler_CollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t collective, uint32_t procGroup, 
	uint32_t rootProc, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t source );


int pyOTF_Handler_BeginCollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t collOp, uint64_t matchingId, 
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, 
	uint64_t received, uint32_t scltoken );


int pyOTF_Handler_EndCollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint64_t matchingId );


int pyOTF_Handler_EventComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment );


int pyOTF_Handler_BeginProcess( void* userData, uint64_t time, 
	uint32_t process );


int pyOTF_Handler_EndProcess( void* userData, uint64_t time, 
	uint32_t process );


int pyOTF_Handler_FileOperation( void* userData, uint64_t time, 
	uint32_t fileid, uint32_t process, uint64_t handleid, 
	uint32_t operation, uint64_t bytes, uint64_t duration, 
	uint32_t source );


int pyOTF_Handler_BeginFileOperation( void* userData, uint64_t time, 
	uint32_t process, uint64_t handleid, uint32_t scltoken );


int pyOTF_Handler_EndFileOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t fileid, uint64_t handleid, 
	uint32_t operation, uint64_t bytes, uint32_t scltoken );


int pyOTF_Handler_RMAPut( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source );


int pyOTF_Handler_RMAPutRemoteEnd( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source );


int pyOTF_Handler_RMAGet( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source );


int pyOTF_Handler_RMAEnd( void* userData, uint64_t time, 
	uint32_t process, uint32_t remote, uint32_t communicator, 
	uint32_t tag, uint32_t source );


int pyOTF_Handler_SnapshotComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment );


int pyOTF_Handler_EnterSnapshot( void *userData, uint64_t time, 
	uint64_t originaltime, uint32_t function, uint32_t process, 
	uint32_t source );


int pyOTF_Handler_SendSnapshot( void *userData, uint64_t time, 
	uint64_t originaltime, uint32_t sender, uint32_t receiver, 
	uint32_t procGroup, uint32_t tag, uint32_t source );


int pyOTF_Handler_OpenFileSnapshot( void* userData, uint64_t time, 
	uint64_t originaltime, uint32_t fileid, uint32_t process, 
	uint64_t handleid, uint32_t source );


int pyOTF_Handler_SummaryComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment );


int pyOTF_Handler_FunctionSummary( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint64_t invocations, 
	uint64_t exclTime, uint64_t inclTime );


int pyOTF_Handler_FunctionGroupSummary( void* userData, uint64_t time, 
	uint32_t funcGroup, uint32_t process, uint64_t invocations, 
	uint64_t exclTime, uint64_t inclTime );


int pyOTF_Handler_MessageSummary( void* userData, uint64_t time, 
	uint32_t process, uint32_t peer, uint32_t comm, 
	uint32_t type, uint64_t sentNumber, uint64_t receivedNumber, 
	uint64_t sentBytes, uint64_t receivedBytes );


int pyOTF_Handler_CollopSummary( void *userData, uint64_t time, 
	uint32_t process, uint32_t comm, uint32_t collective, 
	uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes, 
	uint64_t receivedBytes );


int pyOTF_Handler_FileOperationSummary( void* userData, uint64_t time, 
	uint32_t fileid, uint32_t process, uint64_t nopen, 
	uint64_t nclose, uint64_t nread, uint64_t nwrite, 
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite );


int pyOTF_Handler_FileGroupOperationSummary( void* userData, uint64_t time, 
	uint32_t groupid, uint32_t process, uint64_t nopen, 
	uint64_t nclose, uint64_t nread, uint64_t nwrite, 
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite );


int pyOTF_Handler_UnknownRecord( void *userData, uint64_t time, 
	uint32_t process, const char *record );


int pyOTF_Handler_DefMarker( void *userData, uint32_t stream, 
	uint32_t token, const char* name, uint32_t type );


int pyOTF_Handler_Marker( void *userData, uint64_t time, 
	uint32_t process, uint32_t token, const char* text );




/* *** DEFINITIONS ********************************************************** */
/* ************************************************************************** */
int pyOTF_COUNTER_VARTYPE_ISINTEGER( uint64_t x ) {
	return OTF_COUNTER_VARTYPE_ISINTEGER(x);
}

int pyOTF_COUNTER_VARTYPE_ISSIGNED( uint64_t x ) {
	return OTF_COUNTER_VARTYPE_ISSIGNED(x);
}

int pyOTF_COUNTER_VARTYPE_ISUNSIGNED( uint64_t x ) {
	return OTF_COUNTER_VARTYPE_ISUNSIGNED(x);
}

/* *** MASTERCONTROL ******************************************************** */
/* ************************************************************************** */

int pyOTF_MasterControl_appendList( OTF_MasterControl* mc, uint32_t argument,
	uint32_t l, PyObject* values ) {


	int ret;
	uint32_t* vals= createInt32ArrayFromSequence( values );


	ret= OTF_MasterControl_appendList( mc, argument, l, vals );


	free( vals );

	return ret;

}


/* *** WRITER *************************************************************** */
/* ************************************************************************** */

int pyOTF_Writer_writeDefProcessGroup( OTF_Writer* writer, uint32_t stream, 
	uint32_t procGroup, const char* name, uint32_t numberOfProcs, 
	PyObject* procs) {


	int ret;
	uint32_t* pyprocs= createInt32ArrayFromSequence( procs );

	ret= OTF_Writer_writeDefProcessGroup( writer, stream, 
		procGroup, name, numberOfProcs, 
		pyprocs);

	free( pyprocs );

	return ret;
}



/* *** HANDLER ARRAY ******************************************************** */
/* ************************************************************************** */


/* first handler arg replacement. Containing the Python-function pointer and
the fha specified by the user */
typedef struct {

	PyObject* func;
	PyObject* realfha;
	
} pyOTF_FirstHandlerArgument;



/* *** handler **************************************************** */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
H= unsigned int
K= unsigned long long (if long long exists (its #ifdefd in python sources) )
(python 2.5)

When using l for long int and i for int programs crashed (SEGFAULT) inside
Py_BuildValue() in pyOTF_Handler_EventComment()

H and K are not documented, thus its not "save" to use them, because future and
past versions of python may differ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

int pyOTF_Handler_DefinitionComment( void* userData, uint32_t stream, 
	const char* comment ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHs", fha->realfha, stream, 
		comment );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefTimerResolution( void* userData, uint32_t stream, 
	uint64_t ticksPerSecond ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHK", fha->realfha, stream, 
		ticksPerSecond );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefProcess( void* userData, uint32_t stream, 
	uint32_t process, const char* name, uint32_t parent ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsH", fha->realfha, stream, 
		process, name, parent );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefProcessGroup( void* userData, uint32_t stream, 
	uint32_t procGroup, const char* name, uint32_t numberOfProcs, 
	const uint32_t* procs ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;
	PyObject* pyprocs;
	uint32_t i;

	pyprocs= PyList_New( numberOfProcs );
	for( i= 0; i < numberOfProcs; ++i ) {

		PyList_SetItem( pyprocs, i, PyInt_FromLong((long) procs[i]) );

	}

	arglist= Py_BuildValue("OHHsHO", fha->realfha, stream, 
		procGroup, name, numberOfProcs, 
		pyprocs );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefFunction( void* userData, uint32_t stream, 
	uint32_t func, const char* name, uint32_t funcGroup, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsHH", fha->realfha, stream, 
		func, name, funcGroup, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefFunctionGroup( void* userData, uint32_t stream, 
	uint32_t funcGroup, const char* name ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHs", fha->realfha, stream, 
		funcGroup, name );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefCollectiveOperation( void* userData, uint32_t stream, 
	uint32_t collOp, const char* name, uint32_t type ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsH", fha->realfha, stream, 
		collOp, name, type );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefCounter( void* userData, uint32_t stream, 
	uint32_t counter, const char* name, uint32_t properties, 
	uint32_t counterGroup, const char* unit ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsHHs", fha->realfha, stream, 
		counter, name, properties, 
		counterGroup, unit );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefCounterGroup( void* userData, uint32_t stream, 
	uint32_t counterGroup, const char* name ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHs", fha->realfha, stream, 
		counterGroup, name );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefScl( void* userData, uint32_t stream, 
	uint32_t source, uint32_t sourceFile, uint32_t line ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHHH", fha->realfha, stream, 
		source, sourceFile, line );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefSclFile( void* userData, uint32_t stream, 
	uint32_t sourceFile, const char* name ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHs", fha->realfha, stream, 
		sourceFile, name );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefCreator( void* userData, uint32_t stream, 
	const char* creator ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHs", fha->realfha, stream, 
		creator );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefVersion( void* userData, uint32_t stream, 
	uint8_t major, uint8_t minor, uint8_t sub, 
	const char* string ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHHHs", fha->realfha, stream, 
		major, minor, sub, 
		string );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefFile( void* userData, uint32_t stream, 
	uint32_t token, const char *name, uint32_t group ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsH", fha->realfha, stream, 
		token, name, group );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefFileGroup( void* userData, uint32_t stream, 
	uint32_t token, const char *name ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHs", fha->realfha, stream, 
		token, name );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_Enter( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHH", fha->realfha, time, 
		function, process, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_Leave( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHH", fha->realfha, time, 
		function, process, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_SendMsg( void* userData, uint64_t time, 
	uint32_t sender, uint32_t receiver, uint32_t group, 
	uint32_t type, uint32_t length, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHH", fha->realfha, time, 
		sender, receiver, group, 
		type, length, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_RecvMsg( void* userData, uint64_t time, 
	uint32_t recvProc, uint32_t sendProc, uint32_t group, 
	uint32_t type, uint32_t length, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHH", fha->realfha, time, 
		recvProc, sendProc, group, 
		type, length, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_Counter( void* userData, uint64_t time, 
	uint32_t process, uint32_t counter, uint64_t value ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHK", fha->realfha, time, 
		process, counter, value );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_CollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t collective, uint32_t procGroup, 
	uint32_t rootProc, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHHKH", fha->realfha, time, 
		process, collective, procGroup, 
		rootProc, sent, received, 
		duration, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_BeginCollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t collOp, uint64_t matchingId, 
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, 
	uint64_t received, uint32_t scltoken ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKHHKKH", fha->realfha, time, 
		process, collOp, matchingId, 
		procGroup, rootProc, sent, 
		received, scltoken );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_EndCollectiveOperation( void* userData, uint64_t time, 
	uint32_t process, uint64_t matchingId ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHK", fha->realfha, time, 
		process, matchingId );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_EventComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHs", fha->realfha, time, 
		process, comment );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_BeginProcess( void* userData, uint64_t time, 
	uint32_t process ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKH", fha->realfha, time, 
		process );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_EndProcess( void* userData, uint64_t time, 
	uint32_t process ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKH", fha->realfha, time, 
		process );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_FileOperation( void* userData, uint64_t time, 
	uint32_t fileid, uint32_t process, uint64_t handleid, 
	uint32_t operation, uint64_t bytes, uint64_t duration, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKHKKH", fha->realfha, time, 
		fileid, process, handleid, 
		operation, bytes, duration, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_BeginFileOperation( void* userData, uint64_t time, 
	uint32_t process, uint64_t handleid, uint32_t scltoken ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHKH", fha->realfha, time, 
		process, handleid, scltoken );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_EndFileOperation( void* userData, uint64_t time, 
	uint32_t process, uint32_t fileid, uint64_t handleid, 
	uint32_t operation, uint64_t bytes, uint32_t scltoken ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKHKH", fha->realfha, time, 
		process, fileid, handleid, 
		operation, bytes, scltoken );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_RMAPut( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHKH", fha->realfha, time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_RMAPutRemoteEnd( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHKH", fha->realfha, time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_RMAGet( void* userData, uint64_t time, 
	uint32_t process, uint32_t origin, uint32_t target, 
	uint32_t communicator, uint32_t tag, uint64_t bytes, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHHKH", fha->realfha, time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_RMAEnd( void* userData, uint64_t time, 
	uint32_t process, uint32_t remote, uint32_t communicator, 
	uint32_t tag, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHH", fha->realfha, time, 
		process, remote, communicator, 
		tag, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_SnapshotComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHs", fha->realfha, time, 
		process, comment );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_EnterSnapshot( void *userData, uint64_t time, 
	uint64_t originaltime, uint32_t function, uint32_t process, 
	uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKKHHH", fha->realfha, time, 
		originaltime, function, process, 
		source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_SendSnapshot( void *userData, uint64_t time, 
	uint64_t originaltime, uint32_t sender, uint32_t receiver, 
	uint32_t procGroup, uint32_t tag, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKKHHHHH", fha->realfha, time, 
		originaltime, sender, receiver, 
		procGroup, tag, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_OpenFileSnapshot( void* userData, uint64_t time, 
	uint64_t originaltime, uint32_t fileid, uint32_t process, 
	uint64_t handleid, uint32_t source ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKKHHKH", fha->realfha, time, 
		originaltime, fileid, process, 
		handleid, source );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_SummaryComment( void* userData, uint64_t time, 
	uint32_t process, const char* comment ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHs", fha->realfha, time, 
		process, comment );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_FunctionSummary( void* userData, uint64_t time, 
	uint32_t function, uint32_t process, uint64_t invocations, 
	uint64_t exclTime, uint64_t inclTime ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKKK", fha->realfha, time, 
		function, process, invocations, 
		exclTime, inclTime );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_FunctionGroupSummary( void* userData, uint64_t time, 
	uint32_t funcGroup, uint32_t process, uint64_t invocations, 
	uint64_t exclTime, uint64_t inclTime ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKKK", fha->realfha, time, 
		funcGroup, process, invocations, 
		exclTime, inclTime );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_MessageSummary( void* userData, uint64_t time, 
	uint32_t process, uint32_t peer, uint32_t comm, 
	uint32_t type, uint64_t sentNumber, uint64_t receivedNumber, 
	uint64_t sentBytes, uint64_t receivedBytes ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHHKKKK", fha->realfha, time, 
		process, peer, comm, 
		type, sentNumber, receivedNumber, 
		sentBytes, receivedBytes );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_CollopSummary( void *userData, uint64_t time, 
	uint32_t process, uint32_t comm, uint32_t collective, 
	uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes, 
	uint64_t receivedBytes ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHHKKKK", fha->realfha, time, 
		process, comm, collective, 
		sentNumber, receivedNumber, sentBytes, 
		receivedBytes );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_FileOperationSummary( void* userData, uint64_t time, 
	uint32_t fileid, uint32_t process, uint64_t nopen, 
	uint64_t nclose, uint64_t nread, uint64_t nwrite, 
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKKKKKKK", fha->realfha, time, 
		fileid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_FileGroupOperationSummary( void* userData, uint64_t time, 
	uint32_t groupid, uint32_t process, uint64_t nopen, 
	uint64_t nclose, uint64_t nread, uint64_t nwrite, 
	uint64_t nseek, uint64_t bytesread, uint64_t byteswrite ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHKKKKKKK", fha->realfha, time, 
		groupid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_UnknownRecord( void *userData, uint64_t time, 
	uint32_t process, const char *record ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHs", fha->realfha, time, 
		process, record );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_DefMarker( void *userData, uint32_t stream, 
	uint32_t token, const char* name, uint32_t type ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OHHsH", fha->realfha, stream, 
		token, name, type );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}

int pyOTF_Handler_Marker( void *userData, uint64_t time, 
	uint32_t process, uint32_t token, const char* text ) {


	pyOTF_FirstHandlerArgument* fha= (pyOTF_FirstHandlerArgument*) userData;
	PyObject *result;
	PyObject* arglist;
	int ret;

	arglist= Py_BuildValue("OKHHs", fha->realfha, time, 
		process, token, text );

	result= PyEval_CallObject(fha->func, arglist);

	Py_DECREF(arglist);

	ret= (int) PyInt_AsLong( result );

	if( NULL == PyErr_Occurred() ){

		return ret;

	} else {

		PyErr_Print();
		return OTF_RETURN_ABORT;

	}
}



/* *** other HandlerArray functions ***************************************** */

void pyOTF_HandlerArray_close( OTF_HandlerArray* handlers ) {

	uint32_t i;
	
	for( i= 0; i < OTF_NRECORDS; ++i ) {

		/* delete old fhb, because we allocated it in "pyOTF_HandlerArray_setHandler()" */
		if( NULL != handlers->firsthandlerarg[i] ) {

			Py_XDECREF( ((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[i])->func );
			Py_XDECREF( ((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[i])->realfha );

			free( handlers->firsthandlerarg[i] );
			handlers->firsthandlerarg[i]= NULL;
		}

	}

	OTF_HandlerArray_close( handlers );
}


/* returns OTF_RETURN_ABORT or OTF_RETURN_OK */
int pyOTF_HandlerArray_setHandler( OTF_HandlerArray* handlers, PyObject* function, uint32_t recordtype ) {


	pyOTF_FirstHandlerArgument* fhb;


	/* assertion stuff */
	if ( recordtype > OTF_NRECORDS ) {
#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Unexpected record type");
#endif /* OTF_VERBOSE */
		return OTF_RETURN_ABORT;
	} else if ( NULL == function || 0 == PyCallable_Check(function) ) {
#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Expecting a function object");
#endif /* OTF_VERBOSE */
		return OTF_RETURN_ABORT;
	}
	
	/* if no artificial fha has been set */
	if ( NULL == handlers->firsthandlerarg[recordtype] ) {
	
		/* add a new one  for this record type */
		fhb= malloc( sizeof( pyOTF_FirstHandlerArgument ) );
		if( NULL == fhb ) {
#ifdef OTF_VERBOSE
			PyErr_SetString(PyExc_TypeError,"No Memory left");
#endif /* OTF_VERBOSE */
			return OTF_RETURN_ABORT;
		}
		
		Py_INCREF(Py_None);
		Py_INCREF(Py_None);
		fhb->func= Py_None;
		fhb->realfha= Py_None;

		OTF_HandlerArray_setFirstHandlerArg( handlers, fhb, recordtype );

	}
	

	/* decrease reference counter of the older callback */
	Py_XDECREF( ((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[recordtype])->func );

	/* increase the reference count of the function object */
	Py_XINCREF(function);
	
	/* assign the new callback */
	((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[recordtype])->func= function;



	switch( recordtype ) {

		case OTF_DEFINITIONCOMMENT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefinitionComment, recordtype );

			break;

		case OTF_DEFTIMERRESOLUTION_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefTimerResolution, recordtype );

			break;

		case OTF_DEFPROCESS_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefProcess, recordtype );

			break;

		case OTF_DEFPROCESSGROUP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefProcessGroup, recordtype );

			break;

		case OTF_DEFFUNCTION_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefFunction, recordtype );

			break;

		case OTF_DEFFUNCTIONGROUP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefFunctionGroup, recordtype );

			break;

		case OTF_DEFCOLLOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefCollectiveOperation, recordtype );

			break;

		case OTF_DEFCOUNTER_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefCounter, recordtype );

			break;

		case OTF_DEFCOUNTERGROUP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefCounterGroup, recordtype );

			break;

		case OTF_DEFSCL_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefScl, recordtype );

			break;

		case OTF_DEFSCLFILE_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefSclFile, recordtype );

			break;

		case OTF_DEFCREATOR_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefCreator, recordtype );

			break;

		case OTF_DEFVERSION_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefVersion, recordtype );

			break;

		case OTF_DEFFILE_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefFile, recordtype );

			break;

		case OTF_DEFFILEGROUP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefFileGroup, recordtype );

			break;

		case OTF_ENTER_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_Enter, recordtype );

			break;

		case OTF_LEAVE_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_Leave, recordtype );

			break;

		case OTF_SEND_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_SendMsg, recordtype );

			break;

		case OTF_RECEIVE_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_RecvMsg, recordtype );

			break;

		case OTF_COUNTER_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_Counter, recordtype );

			break;

		case OTF_COLLOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_CollectiveOperation, recordtype );

			break;

		case OTF_BEGINCOLLOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_BeginCollectiveOperation, recordtype );

			break;

		case OTF_ENDCOLLOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_EndCollectiveOperation, recordtype );

			break;

		case OTF_EVENTCOMMENT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_EventComment, recordtype );

			break;

		case OTF_BEGINPROCESS_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_BeginProcess, recordtype );

			break;

		case OTF_ENDPROCESS_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_EndProcess, recordtype );

			break;

		case OTF_FILEOPERATION_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_FileOperation, recordtype );

			break;

		case OTF_BEGINFILEOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_BeginFileOperation, recordtype );

			break;

		case OTF_ENDFILEOP_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_EndFileOperation, recordtype );

			break;

		case OTF_RMAPUT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_RMAPut, recordtype );

			break;

		case OTF_RMAPUTRE_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_RMAPutRemoteEnd, recordtype );

			break;

		case OTF_RMAGET_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_RMAGet, recordtype );

			break;

		case OTF_RMAEND_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_RMAEnd, recordtype );

			break;

		case OTF_SNAPSHOTCOMMENT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_SnapshotComment, recordtype );

			break;

		case OTF_ENTERSNAPSHOT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_EnterSnapshot, recordtype );

			break;

		case OTF_SENDSNAPSHOT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_SendSnapshot, recordtype );

			break;

		case OTF_OPENFILESNAPSHOT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_OpenFileSnapshot, recordtype );

			break;

		case OTF_SUMMARYCOMMENT_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_SummaryComment, recordtype );

			break;

		case OTF_FUNCTIONSUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_FunctionSummary, recordtype );

			break;

		case OTF_FUNCTIONGROUPSUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_FunctionGroupSummary, recordtype );

			break;

		case OTF_MESSAGESUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_MessageSummary, recordtype );

			break;

		case OTF_COLLOPSUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_CollopSummary, recordtype );

			break;

		case OTF_FILEOPERATIONSUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_FileOperationSummary, recordtype );

			break;

		case OTF_FILEGROUPOPERATIONSUMMARY_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_FileGroupOperationSummary, recordtype );

			break;

		case OTF_UNKNOWN_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_UnknownRecord, recordtype );

			break;

		case OTF_DEFMARKER_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_DefMarker, recordtype );

			break;

		case OTF_MARKER_RECORD :

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) pyOTF_Handler_Marker, recordtype );

			break;



	}


	return OTF_RETURN_OK;
}


int pyOTF_HandlerArray_setFirstHandlerArg( OTF_HandlerArray* handlers, PyObject* fha, uint32_t recordtype ) {


	pyOTF_FirstHandlerArgument* fhb;


	/* assertion stuff */
	if ( recordtype > OTF_NRECORDS ) {

#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Unexpected record type");
#endif /* OTF_VERBOSE */
		return OTF_RETURN_ABORT;
	}


	/* if no fha or handler was set before for this record type */
	if ( NULL == handlers->firsthandlerarg[recordtype] ) {
	
		/* create an artificial first handler arg */
		fhb= malloc( sizeof( pyOTF_FirstHandlerArgument ) );
		if( NULL == fhb ) {
#ifdef OTF_VERBOSE
			PyErr_SetString(PyExc_TypeError,"No Memory left");
#endif /* OTF_VERBOSE */
			return OTF_RETURN_ABORT;
		}
		

		Py_INCREF(Py_None);
		Py_INCREF(Py_None);
		fhb->func= Py_None;
		fhb->realfha= Py_None;
		
		OTF_HandlerArray_setFirstHandlerArg( handlers, fhb, recordtype );
	}


	/* decrease reference counter of the older fha */
	Py_XDECREF( ((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[recordtype])->realfha );
	
	/* increase reference count of fha */
	Py_XINCREF(fha);
	
	/* add the python first handler arg */
	((pyOTF_FirstHandlerArgument*)handlers->firsthandlerarg[recordtype])->realfha= fha;
	

	return OTF_RETURN_OK;
}


#endif /* PYOTF_WRAPPER_H */
