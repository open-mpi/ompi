/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/*
 * no structures will be included, so they are no accessable through python
 *
 * Time- and BytesProgress now have 4 return values. 1. real return value and
 *   and min/cur/max - e.g. [ 1, 0, 100, 100000 ]
 *
 */


%module otf
%{

#include "otf.h"

#include "pyOTF_AuxiliaryFunctions.h"

#include "pyOTF_Wrapper.h"

%}


/* *** conversion from C datatypes to python data types *** */
%apply unsigned char { uint8_t }
%apply unsigned int { uint32_t }
%apply unsigned long long { uint64_t }
%apply unsigned long long *OUTPUT { uint64_t* minimum, uint64_t* current, uint64_t* maximum }; /* progress return values */


/* *** other interfaces *** */

/* OTF_Definitions.h */
%include ../otflib/OTF_Definitions.h


/* OTF_FileManager.h */
%ignore OTF_FileManager_guaranteeFile; /* the user shouldn't use this function */
%ignore OTF_FileManager_registerFile; /* the user shouldn't use this function */
%ignore OTF_FileManager_touchFile; /* the user shouldn't use this function */
%ignore OTF_FileManager_suspendFile; /* the user shouldn't use this function */

%include ../otflib/OTF_FileManager.h


/* OTF_Filenames.h */
%include ../otflib/OTF_Filenames.h


/* OTF_HandlerArray.h */
%ignore OTF_HandlerArray_getCopyHandler;

%ignore struct_OTF_HandlerArray;
%ignore OTF_HandlerArray_close;
%ignore OTF_HandlerArray_setHandler;
%ignore OTF_HandlerArray_setFirstHandlerArg;

%ignore OTF_Handler_DefinitionComment;
%ignore OTF_Handler_DefTimerResolution;
%ignore OTF_Handler_DefProcess;
%ignore OTF_Handler_DefProcessGroup;
%ignore OTF_Handler_DefFunction;
%ignore OTF_Handler_DefFunctionGroup;
%ignore OTF_Handler_DefCollectiveOperation;
%ignore OTF_Handler_DefCounter;
%ignore OTF_Handler_DefCounterGroup;
%ignore OTF_Handler_DefScl;
%ignore OTF_Handler_DefSclFile;
%ignore OTF_Handler_DefCreator;
%ignore OTF_Handler_DefVersion;
%ignore OTF_Handler_DefFile;
%ignore OTF_Handler_DefFileGroup;
%ignore OTF_Handler_Enter;
%ignore OTF_Handler_Leave;
%ignore OTF_Handler_SendMsg;
%ignore OTF_Handler_RecvMsg;
%ignore OTF_Handler_Counter;
%ignore OTF_Handler_CollectiveOperation;
%ignore OTF_Handler_BeginCollectiveOperation;
%ignore OTF_Handler_EndCollectiveOperation;
%ignore OTF_Handler_EventComment;
%ignore OTF_Handler_BeginProcess;
%ignore OTF_Handler_EndProcess;
%ignore OTF_Handler_FileOperation;
%ignore OTF_Handler_BeginFileOperation;
%ignore OTF_Handler_EndFileOperation;
%ignore OTF_Handler_RMAPut;
%ignore OTF_Handler_RMAPutRemoteEnd;
%ignore OTF_Handler_RMAGet;
%ignore OTF_Handler_RMAEnd;
%ignore OTF_Handler_SnapshotComment;
%ignore OTF_Handler_EnterSnapshot;
%ignore OTF_Handler_SendSnapshot;
%ignore OTF_Handler_OpenFileSnapshot;
%ignore OTF_Handler_SummaryComment;
%ignore OTF_Handler_FunctionSummary;
%ignore OTF_Handler_FunctionGroupSummary;
%ignore OTF_Handler_MessageSummary;
%ignore OTF_Handler_CollopSummary;
%ignore OTF_Handler_FileOperationSummary;
%ignore OTF_Handler_FileGroupOperationSummary;
%ignore OTF_Handler_UnknownRecord;
%ignore OTF_Handler_DefMarker;
%ignore OTF_Handler_Marker;


%include ../otflib/OTF_HandlerArray.h


/* OTF_inttypes.h */
%include ../otflib/OTF_inttypes.h


/* OTF_MasterControl.h */
%ignore struct_OTF_MapEntry;
%ignore struct_OTF_Pair;
%ignore struct_OTF_MasterControl;
%ignore OTF_MasterControl_appendList; /* replaced by wrapper */

%include ../otflib/OTF_MasterControl.h


/* OTF_Reader.h */

%include ../otflib/OTF_Reader.h


/* OTF_RStream.h */
%ignore struct_OTF_RStream;

%include ../otflib/OTF_RStream.h


/* OTF_Writer.h */
%ignore OTF_Writer_writeOtfVersion;
%ignore OTF_Writer_writeDefProcessGroup;


%include ../otflib/OTF_Writer.h


/* OTF_WStream.h */
%ignore struct_OTF_WStream;

%include ../otflib/OTF_WStream.h


/* *** functions that had to be wrapped up *** */
%rename(OTF_COUNTER_VARTYPE_ISINTEGER) pyOTF_COUNTER_VARTYPE_ISINTEGER;
%rename(OTF_COUNTER_VARTYPE_ISSIGNED) pyOTF_COUNTER_VARTYPE_ISSIGNED;
%rename(OTF_COUNTER_VARTYPE_ISUNSIGNED) pyOTF_COUNTER_VARTYPE_ISUNSIGNED;

%rename(OTF_MasterControl_appendList) pyOTF_MasterControl_appendList;
%rename(OTF_HandlerArray_close) pyOTF_HandlerArray_close;
%rename(OTF_HandlerArray_setHandler) pyOTF_HandlerArray_setHandler;
%rename(OTF_HandlerArray_setFirstHandlerArg) pyOTF_HandlerArray_setFirstHandlerArg;

%rename(OTF_Writer_writeDefProcessGroup) pyOTF_Writer_writeDefProcessGroup;


%ignore pyOTF_FirstHandlerArgument;

%ignore pyOTF_Handler_DefinitionComment;
%ignore pyOTF_Handler_DefTimerResolution;
%ignore pyOTF_Handler_DefProcess;
%ignore pyOTF_Handler_DefProcessGroup;
%ignore pyOTF_Handler_DefFunction;
%ignore pyOTF_Handler_DefFunctionGroup;
%ignore pyOTF_Handler_DefCollectiveOperation;
%ignore pyOTF_Handler_DefCounter;
%ignore pyOTF_Handler_DefCounterGroup;
%ignore pyOTF_Handler_DefScl;
%ignore pyOTF_Handler_DefSclFile;
%ignore pyOTF_Handler_DefCreator;
%ignore pyOTF_Handler_DefVersion;
%ignore pyOTF_Handler_DefFile;
%ignore pyOTF_Handler_DefFileGroup;
%ignore pyOTF_Handler_Enter;
%ignore pyOTF_Handler_Leave;
%ignore pyOTF_Handler_SendMsg;
%ignore pyOTF_Handler_RecvMsg;
%ignore pyOTF_Handler_Counter;
%ignore pyOTF_Handler_CollectiveOperation;
%ignore pyOTF_Handler_BeginCollectiveOperation;
%ignore pyOTF_Handler_EndCollectiveOperation;
%ignore pyOTF_Handler_EventComment;
%ignore pyOTF_Handler_BeginProcess;
%ignore pyOTF_Handler_EndProcess;
%ignore pyOTF_Handler_FileOperation;
%ignore pyOTF_Handler_BeginFileOperation;
%ignore pyOTF_Handler_EndFileOperation;
%ignore pyOTF_Handler_RMAPut;
%ignore pyOTF_Handler_RMAPutRemoteEnd;
%ignore pyOTF_Handler_RMAGet;
%ignore pyOTF_Handler_RMAEnd;
%ignore pyOTF_Handler_SnapshotComment;
%ignore pyOTF_Handler_EnterSnapshot;
%ignore pyOTF_Handler_SendSnapshot;
%ignore pyOTF_Handler_OpenFileSnapshot;
%ignore pyOTF_Handler_SummaryComment;
%ignore pyOTF_Handler_FunctionSummary;
%ignore pyOTF_Handler_FunctionGroupSummary;
%ignore pyOTF_Handler_MessageSummary;
%ignore pyOTF_Handler_CollopSummary;
%ignore pyOTF_Handler_FileOperationSummary;
%ignore pyOTF_Handler_FileGroupOperationSummary;
%ignore pyOTF_Handler_UnknownRecord;
%ignore pyOTF_Handler_DefMarker;
%ignore pyOTF_Handler_Marker;


%include pyOTF_Wrapper.h


/* OTF_Copyhandler.h */
%pythoncode %{

def OTF_CopyHandler_DefinitionComment( writer , stream, 
		comment):

	if OTF_Writer_writeDefinitionComment( writer , stream, 
		comment ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefTimerResolution( writer , stream, 
		ticksPerSecond):

	if OTF_Writer_writeDefTimerResolution( writer , stream, 
		ticksPerSecond ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefProcess( writer , stream, 
		process, name, parent):

	if OTF_Writer_writeDefProcess( writer , stream, 
		process, name, parent ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefProcessGroup( writer , stream, 
		procGroup, name, numberOfProcs, 
		pyprocs):

	if OTF_Writer_writeDefProcessGroup( writer , stream, 
		procGroup, name, numberOfProcs, 
		pyprocs ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefFunction( writer , stream, 
		func, name, funcGroup, 
		source):

	if OTF_Writer_writeDefFunction( writer , stream, 
		func, name, funcGroup, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefFunctionGroup( writer , stream, 
		funcGroup, name):

	if OTF_Writer_writeDefFunctionGroup( writer , stream, 
		funcGroup, name ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefCollectiveOperation( writer , stream, 
		collOp, name, type):

	if OTF_Writer_writeDefCollectiveOperation( writer , stream, 
		collOp, name, type ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefCounter( writer , stream, 
		counter, name, properties, 
		counterGroup, unit):

	if OTF_Writer_writeDefCounter( writer , stream, 
		counter, name, properties, 
		counterGroup, unit ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefCounterGroup( writer , stream, 
		counterGroup, name):

	if OTF_Writer_writeDefCounterGroup( writer , stream, 
		counterGroup, name ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefScl( writer , stream, 
		source, sourceFile, line):

	if OTF_Writer_writeDefScl( writer , stream, 
		source, sourceFile, line ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefSclFile( writer , stream, 
		sourceFile, name):

	if OTF_Writer_writeDefSclFile( writer , stream, 
		sourceFile, name ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefCreator( writer , stream, 
		creator):

	if OTF_Writer_writeDefCreator( writer , stream, 
		creator ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefVersion( writer , stream, 
		major, minor, sub, 
		string):

	#version is writen implicitly

	return OTF_RETURN_OK


def OTF_CopyHandler_DefFile( writer , stream, 
		token, name, group):

	if OTF_Writer_writeDefFile( writer , stream, 
		token, name, group ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefFileGroup( writer , stream, 
		token, name):

	if OTF_Writer_writeDefFileGroup( writer , stream, 
		token, name ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_Enter( writer , time, 
		function, process, source):

	if OTF_Writer_writeEnter( writer , time, 
		function, process, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_Leave( writer , time, 
		function, process, source):

	if OTF_Writer_writeLeave( writer , time, 
		function, process, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_SendMsg( writer , time, 
		sender, receiver, group, 
		type, length, source):

	if OTF_Writer_writeSendMsg( writer , time, 
		sender, receiver, group, 
		type, length, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_RecvMsg( writer , time, 
		recvProc, sendProc, group, 
		type, length, source):

	if OTF_Writer_writeRecvMsg( writer , time, 
		recvProc, sendProc, group, 
		type, length, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_Counter( writer , time, 
		process, counter, value):

	if OTF_Writer_writeCounter( writer , time, 
		process, counter, value ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_CollectiveOperation( writer , time, 
		process, collective, procGroup, 
		rootProc, sent, received, 
		duration, source):

	if OTF_Writer_writeCollectiveOperation( writer , time, 
		process, collective, procGroup, 
		rootProc, sent, received, 
		duration, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_BeginCollectiveOperation( writer , time, 
		process, collOp, matchingId, 
		procGroup, rootProc, sent, 
		received, scltoken):

	if OTF_Writer_writeBeginCollectiveOperation( writer , time, 
		process, collOp, matchingId, 
		procGroup, rootProc, sent, 
		received, scltoken ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_EndCollectiveOperation( writer , time, 
		process, matchingId):

	if OTF_Writer_writeEndCollectiveOperation( writer , time, 
		process, matchingId ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_EventComment( writer , time, 
		process, comment):

	if OTF_Writer_writeEventComment( writer , time, 
		process, comment ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_BeginProcess( writer , time, 
		process):

	if OTF_Writer_writeBeginProcess( writer , time, 
		process ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_EndProcess( writer , time, 
		process):

	if OTF_Writer_writeEndProcess( writer , time, 
		process ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_FileOperation( writer , time, 
		fileid, process, handleid, 
		operation, bytes, duration, 
		source):

	if OTF_Writer_writeFileOperation( writer , time, 
		fileid, process, handleid, 
		operation, bytes, duration, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_BeginFileOperation( writer , time, 
		process, handleid, scltoken):

	if OTF_Writer_writeBeginFileOperation( writer , time, 
		process, handleid, scltoken ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_EndFileOperation( writer , time, 
		process, fileid, handleid, 
		operation, bytes, scltoken):

	if OTF_Writer_writeEndFileOperation( writer , time, 
		process, fileid, handleid, 
		operation, bytes, scltoken ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_RMAPut( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source):

	if OTF_Writer_writeRMAPut( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_RMAPutRemoteEnd( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source):

	if OTF_Writer_writeRMAPutRemoteEnd( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_RMAGet( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source):

	if OTF_Writer_writeRMAGet( writer , time, 
		process, origin, target, 
		communicator, tag, bytes, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_RMAEnd( writer , time, 
		process, remote, communicator, 
		tag, source):

	if OTF_Writer_writeRMAEnd( writer , time, 
		process, remote, communicator, 
		tag, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_SnapshotComment( writer , time, 
		process, comment):

	if OTF_Writer_writeSnapshotComment( writer , time, 
		process, comment ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_EnterSnapshot( writer , time, 
		originaltime, function, process, 
		source):

	if OTF_Writer_writeEnterSnapshot( writer , time, 
		originaltime, function, process, 
		source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_SendSnapshot( writer , time, 
		originaltime, sender, receiver, 
		procGroup, tag, source):

	if OTF_Writer_writeSendSnapshot( writer , time, 
		originaltime, sender, receiver, 
		procGroup, tag, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_OpenFileSnapshot( writer , time, 
		originaltime, fileid, process, 
		handleid, source):

	if OTF_Writer_writeOpenFileSnapshot( writer , time, 
		originaltime, fileid, process, 
		handleid, source ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_SummaryComment( writer , time, 
		process, comment):

	if OTF_Writer_writeSummaryComment( writer , time, 
		process, comment ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_FunctionSummary( writer , time, 
		function, process, invocations, 
		exclTime, inclTime):

	if OTF_Writer_writeFunctionSummary( writer , time, 
		function, process, invocations, 
		exclTime, inclTime ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_FunctionGroupSummary( writer , time, 
		funcGroup, process, invocations, 
		exclTime, inclTime):

	if OTF_Writer_writeFunctionGroupSummary( writer , time, 
		funcGroup, process, invocations, 
		exclTime, inclTime ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_MessageSummary( writer , time, 
		process, peer, comm, 
		type, sentNumber, receivedNumber, 
		sentBytes, receivedBytes):

	if OTF_Writer_writeMessageSummary( writer , time, 
		process, peer, comm, 
		type, sentNumber, receivedNumber, 
		sentBytes, receivedBytes ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_CollopSummary( writer , time, 
		process, comm, collective, 
		sentNumber, receivedNumber, sentBytes, 
		receivedBytes):

	if OTF_Writer_writeCollopSummary( writer , time, 
		process, comm, collective, 
		sentNumber, receivedNumber, sentBytes, 
		receivedBytes ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_FileOperationSummary( writer , time, 
		fileid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite):

	if OTF_Writer_writeFileOperationSummary( writer , time, 
		fileid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_FileGroupOperationSummary( writer , time, 
		groupid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite):

	if OTF_Writer_writeFileGroupOperationSummary( writer , time, 
		groupid, process, nopen, 
		nclose, nread, nwrite, 
		nseek, bytesread, byteswrite ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_UnknownRecord( writer , time, 
		process, record):

	if OTF_Writer_writeUnknownRecord( writer , time, 
		process, record ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_DefMarker( writer , stream, 
		token, name, type):

	if OTF_Writer_writeDefMarker( writer , stream, 
		token, name, type ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_CopyHandler_Marker( writer , time, 
		process, token, text):

	if OTF_Writer_writeMarker( writer , time, 
		process, token, text ) == 0 :

		return OTF_RETURN_ABORT
	else :
		return OTF_RETURN_OK


def OTF_HandlerArray_getCopyHandler( handlers, writer ):

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefinitionComment, OTF_DEFINITIONCOMMENT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFINITIONCOMMENT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefTimerResolution, OTF_DEFTIMERRESOLUTION_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFTIMERRESOLUTION_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefProcess, OTF_DEFPROCESS_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFPROCESS_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefProcessGroup, OTF_DEFPROCESSGROUP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFPROCESSGROUP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefFunction, OTF_DEFFUNCTION_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFFUNCTION_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefFunctionGroup, OTF_DEFFUNCTIONGROUP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFFUNCTIONGROUP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefCollectiveOperation, OTF_DEFCOLLOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFCOLLOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefCounter, OTF_DEFCOUNTER_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFCOUNTER_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefCounterGroup, OTF_DEFCOUNTERGROUP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFCOUNTERGROUP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefScl, OTF_DEFSCL_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFSCL_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefSclFile, OTF_DEFSCLFILE_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFSCLFILE_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefCreator, OTF_DEFCREATOR_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFCREATOR_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefVersion, OTF_DEFVERSION_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFVERSION_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefFile, OTF_DEFFILE_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFFILE_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefFileGroup, OTF_DEFFILEGROUP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFFILEGROUP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_Enter, OTF_ENTER_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_ENTER_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_Leave, OTF_LEAVE_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_LEAVE_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_SendMsg, OTF_SEND_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_SEND_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_RecvMsg, OTF_RECEIVE_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_RECEIVE_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_Counter, OTF_COUNTER_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_COUNTER_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_CollectiveOperation, OTF_COLLOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_COLLOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_BeginCollectiveOperation, OTF_BEGINCOLLOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_BEGINCOLLOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_EndCollectiveOperation, OTF_ENDCOLLOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_ENDCOLLOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_EventComment, OTF_EVENTCOMMENT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_EVENTCOMMENT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_BeginProcess, OTF_BEGINPROCESS_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_BEGINPROCESS_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_EndProcess, OTF_ENDPROCESS_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_ENDPROCESS_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_FileOperation, OTF_FILEOPERATION_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_FILEOPERATION_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_BeginFileOperation, OTF_BEGINFILEOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_BEGINFILEOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_EndFileOperation, OTF_ENDFILEOP_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_ENDFILEOP_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_RMAPut, OTF_RMAPUT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_RMAPUT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_RMAPutRemoteEnd, OTF_RMAPUTRE_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_RMAPUTRE_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_RMAGet, OTF_RMAGET_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_RMAGET_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_RMAEnd, OTF_RMAEND_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_RMAEND_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_SnapshotComment, OTF_SNAPSHOTCOMMENT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_SNAPSHOTCOMMENT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_EnterSnapshot, OTF_ENTERSNAPSHOT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_ENTERSNAPSHOT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_SendSnapshot, OTF_SENDSNAPSHOT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_SENDSNAPSHOT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_OpenFileSnapshot, OTF_OPENFILESNAPSHOT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_OPENFILESNAPSHOT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_SummaryComment, OTF_SUMMARYCOMMENT_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_SUMMARYCOMMENT_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_FunctionSummary, OTF_FUNCTIONSUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_FUNCTIONSUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_FunctionGroupSummary, OTF_FUNCTIONGROUPSUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_FUNCTIONGROUPSUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_MessageSummary, OTF_MESSAGESUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_MESSAGESUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_CollopSummary, OTF_COLLOPSUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_COLLOPSUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_FileOperationSummary, OTF_FILEOPERATIONSUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_FILEOPERATIONSUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_FileGroupOperationSummary, OTF_FILEGROUPOPERATIONSUMMARY_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_FILEGROUPOPERATIONSUMMARY_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_UnknownRecord, OTF_UNKNOWN_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_UNKNOWN_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_DefMarker, OTF_DEFMARKER_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_DEFMARKER_RECORD  )

	OTF_HandlerArray_setHandler( handlers, OTF_CopyHandler_Marker, OTF_MARKER_RECORD  )
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, OTF_MARKER_RECORD  )

	return 1


%}
