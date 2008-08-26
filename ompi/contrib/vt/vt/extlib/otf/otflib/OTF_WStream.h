/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_WStream.h
 *
 *  @brief Provides write access to trace streams, which consist of multiple
 *  buffers.
 *
 *  \ingroup wstream
 */

/** \defgroup wstream Stream Writer Interface
 *
 * This interface provides functions for writing trace at stream level.
 * A stream is part of a trace and consists of up to four buffers
 * (event buffer, definition buffer, snapshots buffer, statistics buffer).
 * 
 * wstream is structured similarly to writer.
 *
 * Use this interface, if you want to a specific stream and the writer
 * interface does not provide the desired access.
 *
 * \section wstream_example A short Example
 *
 *  \code
 *  #include <assert.h>
 *  #include "otf.h"
 *
 *  int main( int argc, char** argv ) {
 *  \endcode
 *
 *      Declare a file manager and a writer.
 *      \code
 * 	    OTF_FileManager* manager;
 * 	    OTF_WStream* wstream;
 * 	    \endcode
 *
 *     	Initialize the file manager. Open at most 100 OS files.
 *     	\code
 *     	manager= OTF_FileManager_open( 100 );
 * 	    assert( manager );
 * 	    \endcode
 *
 * 	    Initialize the wstream object. Open file "test", writing the first stream.
 * 	    \code
 * 	    wstream = OTF_WStream_open( "test", 0, manager );
 *      assert( wstream );
 *      \endcode
 *      
 *      Write some definition records.
 *      \code
 *      OTF_WStream_writeDefTimerResolution( wstream, 0, 1000 );
 *      OTF_WStream_writeDefProcess( wstream, 0, 1, "proc one", 0 );
 *      \endcode
 *
 *      Clean up before exiting the program.
 *      \code
 *      OTF_WStream_close( wstream );
 *      OTF_FileManager_close( manager );
 *
 *		return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o test test.c `otfconfig --libs`.
 *
 * When executing this program it only writes one file (test.0.def),
 * containg the written records.
 *
 */

#ifndef OTF_WSTREAM_H
#define OTF_WSTREAM_H


#include <stdlib.h>


#include "OTF_inttypes.h"


#include "OTF_Definitions.h"
#include "OTF_FileManager.h"
#include "OTF_WBuffer.h"
#include "OTF_Filenames.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_WStream {


	/**	name stub: all files will begin with this name */
	char* namestub;

	/**	Unique id for the current stream. */
	uint32_t id;

	/**	State wether to use long or short format,
		see OTF_WSTREAM_FORMAT_XXX macros above. */
	uint32_t format;


	/**	Definitions buffer. Definitions buffer carries definition
		records. */
	OTF_WBuffer* defBuffer;

	/**	Event buffer. The event buffer carries records for actual
		events, i.e. records with a time stamp */
	OTF_WBuffer* eventBuffer;

	/**	Snaps (snapshots) buffer. The snapshots buffer carries
		snapshots of the whole state at a point in time - as oppossed to
		events which only show changes in the state. This can be used to
		start from such a snapshot instead of from the very begining. */
	OTF_WBuffer* snapsBuffer;

	/**	Statistics buffer. Statistics buffer carries statistical
		information information about a certain time interval resp.
		data at points of time that allow to derive statistics without
		reading through all events of that interval. */
	OTF_WBuffer* statsBuffer;

	/** Default compression method for all buffers managed by this stream */
	OTF_FileCompression compression;
	
	/** Default size of buffers managed by this WStream. */
	uint32_t buffersizes;

#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this RStream. */
	uint32_t zbuffersizes;
#endif /* HAVE_ZLIB */
	
	/** file handle manager */
	OTF_FileManager* manager;
};
/** wstream object \ingroup wstream */
typedef struct struct_OTF_WStream OTF_WStream;


/**     
 * Create a new OTF_WStream instance.
 *
 * @param namestub     File name prefix which is going to be used by 
 *                     all sub-files which belong to the writer stream.
 * @param id           Abitrary but unique identifier of the writer stream.
 *                     Cannot be '0'.
 * @param manager      File handle manager. 
 *
 * @return             Initialized OTF_WStream instance or 0 if an error
 *                     occurred.
 *
 * \ingroup wstream
 */
OTF_WStream* OTF_WStream_open( const char* namestub, uint32_t id, 
	OTF_FileManager* manager );


/** 
 * Close an OTF_WStream instance and all its related files.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         1 if instance was closed successfully and 0 otherwise.
 *
 * \ingroup wstream
 */
int OTF_WStream_close( OTF_WStream* wstream );

			
/** 
 * Flush an OTF_WStream instance, i.e. flush all associated buffers if existing.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         1 if everything flushed successfully and 0 otherwise.
 *
 * \ingroup wstream
 */
int OTF_WStream_flush( OTF_WStream* wstream );


/** 
 * Returns the definition buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getDefBuffer( OTF_WStream* wstream );


/** 
 * Returns the event buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getEventBuffer( OTF_WStream* wstream );


/** 
 * Returns the snapshots buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getSnapshotBuffer( OTF_WStream* wstream );


/** 
 * Returns the statistics buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getStatsBuffer( OTF_WStream* wstream );


/**
 * Set the standard compression method for all buffers managed by this writer
 * stream
 *
 * @param wstream      Pointer to an initialized OTF_WStream object. See 
 *                     also OTF_WStream_open().
 *
 * @param compression  Default compression level.
 *                     0-9, where 0 means no compression is applied, and 9 is
 *                     the highest level of compression.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup wstream
 */
int OTF_WStream_setCompression( OTF_WStream* wstream, OTF_FileCompression
	compression );
	
	
/**
 * Return the standard compression method for all buffers managed by this writer
 * stream
 *
 * @param wstream      Pointer to an initialized OTF_WStream object. See 
 *                     also OTF_WStream_open().
 *
 * @return             Standard compression level for all buffers managed by
 *                     this writer stream.
 *
 * \ingroup wstream
 */
OTF_FileCompression OTF_WStream_getCompression( OTF_WStream* wstream );


/**
 * Set the default buffer size for all buffers managed by this writer stream. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @param size     Intended buffer size.
 *
 * \ingroup wstream
 */
void OTF_WStream_setBufferSizes( OTF_WStream* wstream, uint32_t size );

/** 
 * Get the default buffer size for all buffers managed by this writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Default buffer size for all buffers managed by this writer
 *                 stream.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getBufferSizes( OTF_WStream* wstream );

/** 
 * Set the default zbuffer size for all files managed by this writer stream.
 * This is only effective for future files and will not change already
 * allocated buffers. Those can be changed with the files directly.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @param size     Intended buffer size.
 *
 * \ingroup wstream
 */
void OTF_WStream_setZBufferSizes( OTF_WStream* wstream, uint32_t size );


/** 
 * Get the default zbuffer size for all files managed by this writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @return         Default buffer size for all buffers managed by this reader
 *                 stream.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getZBufferSizes( OTF_WStream* wstream );

/**
 * Set the default ouput format.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @param format   Intended output format (OTF_WSTREAM_FORMAT_{LONG,SHORT})
 *
 * \ingroup wstream
 */
void OTF_WStream_setFormat( OTF_WStream* wstream, uint32_t format );

/**
 * Get the default output format
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @return         Default output format.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getFormat( OTF_WStream* wstream );


/* *** definition record write handlers *** ******************************** */


/**	Write a DEFINITIONCOMMENT record to stream 'wstream'. \ingroup wstream */
int OTF_WStream_writeDefinitionComment( OTF_WStream* wstream,
	const char* comment );

/**	Write a DEFTIMERRESOLUTION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefTimerResolution( OTF_WStream* wstream,
	uint64_t ticksPerSecond );

/**	Write a DEFPROCESS record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefProcess( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t parent );

/**	Write a DEFPROCESSGROUP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefProcessGroup( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t n, const uint32_t* array );

/**	Write a DEFFUNCTION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefFunction( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t group, uint32_t scltoken );

/**	Write a DEFFUNCTIONGROUP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefFunctionGroup( OTF_WStream* wstream,
	uint32_t deftoken, const char* name );

/** Write a DEFCOLLECTIVEOPERATION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefCollectiveOperation( OTF_WStream* wstream, 
	uint32_t collOp, const char* name, uint32_t type );

/**	Write a DEFCOUNTER record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefCounter( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t properties, uint32_t countergroup, 
	const char* unit );

/**	Write a DEFCOUNTERGROUP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefCounterGroup( OTF_WStream* wstream,
	uint32_t deftoken, const char* name );

/**	Write a DEFSCL record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefScl( OTF_WStream* wstream, uint32_t deftoken,
	uint32_t sclfile, uint32_t sclline );

/**	Write a DEFSCLFILE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefSclFile( OTF_WStream* wstream,
	uint32_t deftoken, const char* filename );

/**	Write a DEFCREATOR record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefCreator( OTF_WStream* wstream, const char* creator );

/** Write a DEFVERSION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeOtfVersion( OTF_WStream* wstream );

/** Write a DEFFILE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefFile( OTF_WStream* wstream, uint32_t token,
	const char* name, uint32_t group );

/** Write a DEFFILEGROUP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefFileGroup( OTF_WStream* wstream, uint32_t token,
	const char* name );


/* *** event record write handlers *** ************************************* */


/**	Write a ENTER record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeEnter( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/**	Write a RECEIVE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeRecvMsg( OTF_WStream* wstream, uint64_t time,
    uint32_t receiver, uint32_t sender, uint32_t communicator,
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

/**	Write a SEND record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeSendMsg( OTF_WStream* wstream, uint64_t time, 
    uint32_t sender, uint32_t receiver, uint32_t communicator, 
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

/**	Write a LEAVE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeLeave( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/**	Write a COUNTER record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeCounter( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t counter_token, uint64_t value );

/** Write a COLLOP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeCollectiveOperation( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken );

/** Write a #EVTCOMMENT record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeEventComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a PROCESSBEGIN record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeBeginProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process );

/** Write a PROCESSEND record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeEndProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process );

int OTF_WStream_writeFileOperation( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source );


/* *** public snapshot record write handlers *** */


/** Write a #TCOMMENT record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeSnapshotComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a TENTER record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeEnterSnapshot( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/** Write a TSEND record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeSendSnapshot( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t type, uint32_t source );

/** Write a TOPENFILE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeOpenFileSnapshot( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source );

		
/* *** public statistics record write handlers *** */


/** Write a SUMCOMMENT record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeSummaryComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a SUMFUNCTION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeFunctionSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t function, uint32_t process, 
	uint64_t count, uint64_t excltime, uint64_t incltime );

/** Write a SUMFUNCTIONGROUP record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeFunctionGroupSummary( OTF_WStream* wstream, 
	uint64_t time,  uint32_t functiongroup,  uint32_t process,  
	uint64_t count,  uint64_t excltime,  uint64_t incltime );

/** Write a SUMMESSAGE record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeMessageSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t process, uint32_t peer, 
	uint32_t comm,  uint32_t tag, uint64_t number_sent, uint64_t number_recved,
	uint64_t bytes_sent,  uint64_t bytes_recved );

/** Write a SUMFILEOPERATION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeFileOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

/** Write a SUMFILEGROUPOPERATION record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeFileGroupOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

/*
int OTF_WStream_writeCounterSummary( OTF_WStream* wstream, 
	uint64_t time,  uint32_t process,  
	uint32_t counter,  uint64_t value );

int OTF_WStream_writeCollOpSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t process,  uint32_t root,  
	uint64_t bytes_sent, uint64_t bytes_recved );
*/

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_WSTREAM_H */

