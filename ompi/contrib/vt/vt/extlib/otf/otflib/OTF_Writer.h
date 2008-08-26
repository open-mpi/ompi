/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Writer.h
 *
 *  @brief Transparently writes OTF traces which consist of multiple streams.
 *
 * \ingroup writer
 */

/** \defgroup writer Writer Interface
 *
 *  This interface should be used whenever a trace file is to be written as a
 *  whole.  Therefore, an initial call to the OTF_Writer_open() function
 *  allows to specify a number of streams which are going to be used to
 *  automatically partition the recorded event data. OTF than takes over the
 *  duty of distributing the data on multiple files.
 *
 *  \section writer_example A simple Example
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
 * 	    OTF_Writer* writer;
 * 	    \endcode
 *
 *     	Initialize the file manager. Open at most 100 OS files.
 *     	\code
 *     	manager= OTF_FileManager_open( 100 );
 * 	    assert( manager );
 * 	    \endcode
 *
 * 	    Initialize the writer. Open file "test", writing one stream.
 * 	    \code
 * 	    writer = OTF_Writer_open( "test", 1, manager );
 *      assert( writer );
 *      \endcode
 *      
 *      Write some important Definition Records.
 *      Have a look at the specific functions to see what the parameters mean.
 *      \code
 *      OTF_Writer_writeDefTimerResolution( writer, 0, 1000 );
 *      OTF_Writer_writeDefProcess( writer, 0, 1, "proc one", 0 );
 *      OTF_Writer_writeDefFunctionGroup( writer, 0, 1000, "all functions" );
 *      OTF_Writer_writeDefFunction( writer, 0, 1, "main", 1000, 0 );
 *      \endcode
 *
 *
 *      Write an enter and a leave record.
 *      time = 10000, 20000
 *      process = 1
 *      function = 1
 *      Sourcecode location doesn't matter, so it's zero.
 *      \code
 *      OTF_Writer_writeEnter( writer, 10000, 1, 1, 0 );
 *      OTF_Writer_writeLeave( writer, 20000, 1, 1, 0 );
 *      \endcode
 *
 *      Clean up before exiting the program.
 *      \code
 *      OTF_Writer_close( writer );
 *      OTF_FileManager_close( manager );
 *
 *		return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o test test.c `otfconfig --libs`.
 *
 */

#ifndef OTF_WRITER_H
#define OTF_WRITER_H


#include "OTF_MasterControl.h"
#include "OTF_FileManager.h"
#include "OTF_WBuffer.h"
#include "OTF_WStream.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** writer object \ingroup writer */
typedef struct struct_OTF_Writer OTF_Writer;

/* *** public member functions *** */


/**     
 * Create a new OTF_Writer instance with a given number of automatic streams.
 *
 * Setting the number of streams to 0 causes the OTF_Writer object to create a
 * separate stream for each process. Important! Explicit calls to
 * OTF_Writer_assignProcess() can lead to an overall number of streams which
 * exceeds the initial number of streams in this call. 
 * OTF can reduce its file handle usage to a given number. Therefore, an 
 * initialized file manager instance is needed as parameter. 
 * See OTF_FileManager for further details.
 *
 * @param  fileNamePrefix   File name prefix which is going to be used by 
 *                          all sub-files which belong to the trace.
 * @param  numberOfStreams  Initial number of independent data streams to 
 *                          be generated.
 * @param  fileManager      File handle manager. 
 *
 *
 * @return                  Initialized OTF_Writer instance or 0 if a failure
 *                          occurred.
 *
 * \ingroup writer
 */
OTF_Writer* OTF_Writer_open( const char* fileNamePrefix, 
                             uint32_t numberOfStreams, 
                             OTF_FileManager* fileManager );


/** 
 * Close an OTF_Writer instance and all its related files.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @return        1 if instance was closed successfully and 0 otherwise.       
 *
 * \ingroup writer
 */
int OTF_Writer_close( OTF_Writer* writer );


/** 
 * Close all streams that are open in this writer instance.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return        1 on success, 0 if an error occurs.
 * 
 * \ingroup writer
 */
int OTF_Writer_closeAllStreams( OTF_Writer* writer );


/**
 * Set the standard compression method for all buffers managed by this writer
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 *
 * @param compression  compression level to apply to all following streams
 *                     0-9, where 0 means no compression is applied, and 9 is
 *                     the highest level of compression.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_setCompression( OTF_Writer* writer, OTF_FileCompression
	compression );

	
/**
 * Return the standard compression method for all buffers managed by this writer
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 *
 * @return             Standard compression level for all buffers managed by
 *                     this writer.
 *
 * \ingroup writer
 */
OTF_FileCompression OTF_Writer_getCompression( OTF_Writer* writer );


/**
 * Set the default buffer size for all buffers managed by this Writer. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @param size    Intended buffer size.
 *
 * \ingroup writer
 */
void OTF_Writer_setBufferSizes( OTF_Writer* writer, uint32_t size );


/** 
 * Get the default buffer size for all buffers managed by this Writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @return        Default buffer size for all buffers managed by this Writer.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getBufferSizes( OTF_Writer* writer );


/**
 * Set the default zbuffer size for all buffers managed by this Reader.
 * This is only effective for future files and will not change already
 * allocated zbuffers. Those can be changed with the files directly.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @param size    Intended zbuffer size.
 *
 * \ingroup writer
 */
void OTF_Writer_setZBufferSizes( OTF_Writer* writer, uint32_t size );

/** 
 * Get the default zbuffer size.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return  zbuffer size.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getZBufferSizes( OTF_Writer* writer );

/**
 * Set the default ouput format. The format is applied to all streams opened by
 * the writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @param format  Intended output format (OTF_WSTREAM_FORMAT_{LONG,SHORT}).
 *
 * \ingroup writer
 */
void OTF_Writer_setFormat( OTF_Writer* writer, uint32_t format );


/**
 * Get the default output format of all streams managed by this writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return        Default output format.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getFormat( OTF_Writer* writer );


/**     
 * Explicitly assign a given process to a specific stream.
 *
 * Mind that 0 is not a valid stream or process identifier but a reserved
 * value. By default, processes are automatically assigned to streams.
 * Therefore, this call is optional. 
 * 
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param process  Process identifier. See also OTF_Writer_writeDefProcess().
 * @param stream   Target stream identifier with 
 *                 0 < stream <= number of streams as defined in 
 *                 OTF_Writer_open().
 *
 * @return         1 on success, 0 if an error occurs. 
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_assignProcess( OTF_Writer* writer,
                                   uint32_t process, 
                                   uint32_t stream );


/**
 * Get a pointer to the master control object of the given writer instance.
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 *
 * @return         Pointer to a master control object. See OTF_MasterControl.
 *
 * \ingroup writer
 */
OTF_MasterControl* OTF_Writer_getMasterControl( OTF_Writer* writer );


/**
 * Set an alternative master control object. Use this only right after 
 * initialization but never after having written some records already!
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param mc       new master control object
 *                 
 *
 * \ingroup writer
 */
void OTF_Writer_setMasterControl( OTF_Writer* writer, OTF_MasterControl* mc );


/* Methods for writing public definition records ************************** */


/**
 * Write a comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param stream    Target stream identifier with 
 *                  0 < stream <= number of streams as defined in 
 *                  OTF_Writer_open().
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefinitionComment( OTF_Writer* writer, 
                                       uint32_t stream, 
                                       const char* comment );


/**
 * Write the timer resolution definition record. All timed event records
 * will be interpreted according to this definition. By default, a timer
 * resultion of 1 us i.e. 1,000,000 clock ticks is assumed. 
 *
 * @param writer          Pointer to an initialized OTF_Writer object. See 
 *                        also OTF_Writer_open().
 * @param stream          Target stream identifier with 
 *                        0 < stream <= number of streams as defined in 
 *                        OTF_Writer_open().
 * @param ticksPerSecond  Clock ticks per second of the timer.
 *
 * @return                1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefTimerResolution( OTF_Writer* writer, 
                                        uint32_t stream,
                                        uint64_t ticksPerSecond );


/**
 * Write a process definition record. 
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param stream   Target stream identifier with 
 *                 0 < stream <= number of streams as defined in 
 *                 OTF_Writer_open().
 * @param process  Arbitrary but unique process identifier > 0.        
 * @param name     Name of the process e.g. "Process X".
 * @param parent   Previously declared parent process identifier or 0 if 
 *                 process has no parent.
 *
 * @return         1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcess( OTF_Writer* writer, 
                                uint32_t stream,
                                uint32_t process, 
                                const char* name, 
                                uint32_t parent );


/**
 * Write a process group definition record.
 *
 * OTF supports groups of processes. Their main objective is to classify
 * processes depending on arbitrary characteristics. Processes can reside
 * in multiple groups. This record type is optional.
 *
 * @param writer         Pointer to an initialized OTF_Writer object. See 
 *                       also OTF_Writer_open().
 * @param stream         Target stream identifier with 
 *                       0 < stream <= number of streams as defined in 
 *                       OTF_Writer_open().
 * @param procGroup      Arbitrary but unique process group identifier > 0.
 * @param name           Name of the process group e.g. "Well Balanced".
 * @param numberOfProcs  The number of processes in the process group.
 * @param procs          Vector of process identifiers or previously defined
 *                       process group identifiers as defined with
 *                       OTF_Writer_writeDefProcess() resp. 
 *                       OTF_Writer_writeDefProcessGroup.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessGroup( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t procGroup, 
                                     const char* name, 
                                     uint32_t numberOfProcs, 
                                     const uint32_t* procs );


/**
 * Write a function definition record.
 *
 * Defines a function of the given name. Functions can optionally belong to a
 * certain function group to be defined with the
 * OTF_Writer_writeDefFunctionGroup() call. A source code reference can
 * be added to the definition aswell.
 *
 * @param writer     Pointer to an initialized OTF_Writer object. See 
 *                   also OTF_Writer_open().
 * @param stream     Target stream identifier with 
 *                   0 < stream <= number of streams as defined in 
 *                   OTF_Writer_open().
 * @param func       Arbitrary but unique function identifier > 0.
 * @param name       Name of the function e.g. "DoSomething".
 * @param funcGroup  A function group identifier preliminary defined with
 *                   OTF_Writer_writeDefFunctionGroup() or 0 for no
 *                   function group assignment.        
 * @param source     Reference to the function's source code location 
 *                   preliminary defined with OTF_Writer_writeDefScl() or
 *                   0 for no source code location assignment.
 *
 * @return           1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFunction( OTF_Writer* writer, 
                                 uint32_t stream,
                                 uint32_t func, 
                                 const char* name, 
                                 uint32_t funcGroup, 
                                 uint32_t source );


/**
 * Write a function group definition record.
 *
 * @param writer     Pointer to an initialized OTF_Writer object. See 
 *                   also OTF_Writer_open().
 * @param stream     Target stream identifier with 
 *                   0 < stream <= number of streams as defined in 
 *                   OTF_Writer_open().
 * @param funcGroup  An arbitrary but unique function group identifier > 0.
 * @param name       Name of the function group e.g. "Computation".
 *
 * @return           1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFunctionGroup( OTF_Writer* writer, 
                                      uint32_t stream,
                                      uint32_t funcGroup, 
                                      const char* name );


/**
 * Write a collective operation definition record.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param stream      Target stream identifier with 
 *                    0 < stream <= number of streams as defined in 
 *                    OTF_Writer_open().
 * @param collOp      An arbitrary but unique collective op. identifier > 0.
 * @param name        Name of the collective operation e.g. "MPI_Bcast".
 * @param type        One of the five supported collective classes:
 *                    OTF_COLLECTIVE_TYPE_UNKNOWN (default),
 *                    OTF_COLLECTIVE_TYPE_BARRIER,
 *                    OTF_COLLECTIVE_TYPE_ONE2ALL,
 *                    OTF_COLLECTIVE_TYPE_ALL2ONE,
 *                    OTF_COLLECTIVE_TYPE_ALL2ALL.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCollectiveOperation( OTF_Writer* writer, 
                                            uint32_t stream,
                                            uint32_t collOp,
                                            const char* name,
                                            uint32_t type );


/**
 * Write a counter definition record.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param stream        Target stream identifier with 
 *                      0 < stream <= number of streams as defined in 
 *                      OTF_Writer_open().
 * @param counter       An arbitrary but unique counter identifier.
 * @param name          Name of the counter e.g. "Cache Misses".
 * @param properties    A combination of a type and scope counter property.
 *                      OTF_COUNTER_TYPE_ACC (default) represents a counter
 *                      with monotonously increasing values e.g. a FLOP 
 *                      counter. OTF_COUNTER_TYPE_ABS on the other hand 
 *                      defines a counter with alternating absolute values e.g.
 *                      the memory usage of a process. The following counter
 *                      measurement scopes are supported: 
 *                      OTF_COUNTER_SCOPE_START (default) always refers to the 
 *                      start of the process, OTF_COUNTER_SCOPE_POINT refers
 *                      to exactly this moment in time, OTF_COUNTER_SCOPE_LAST
 *                      relates to the previous measurement, and
 *                      OTF_COUNTER_SCOPE_NEXT to the next measurement.
 *                      Examples: OTF_COUNTER_TYPE_ACC + 
 *                      OTF_COUNTER_SCOPE_START should be used for most 
 *                      standard hardware (PAPI) counters. 
 *                      OTF_COUNTER_TYPE_ABS + OTF_COUNTER_SCOPE_POINT could
 *                      be used to record information 'spikes'.
 *                      OTF_COUNTER_TYPE_ABS + OTF_COUNTER_SCOPE_NEXT works
 *                      for memory allocation recording. 
 * @param counterGroup  A previously defined counter group identifier or 0 
 *                      for no group.
 * @param unit          Unit of the counter e.g. "#" for "number of..." or 0 
 *                      for no unit. 
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCounter( OTF_Writer* writer,
                                uint32_t stream,
                                uint32_t counter,
                                const char* name,
                                uint32_t properties, 
                                uint32_t counterGroup, 
                                const char* unit );


/**
 * Write a counter group definition record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param counterGroup An arbitrary but unique counter group identifier.
 * @param name         Counter group name.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCounterGroup( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t counterGroup, 
                                     const char* name );


/**
 * Write a source code location (SCL) record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param source       Arbitrary but unique source code location 
 *                     identifier > 0.
 * @param sourceFile   Previously defined source file identifier. See 
 *                     OTF_Writer_writeDefSclFile(). 
 * @param line         Line number.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefScl( OTF_Writer* writer,
                            uint32_t stream,
                            uint32_t source,
                            uint32_t sourceFile, 
                            uint32_t line );


/**
 * Write a source code location (SCL) file record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param sourceFile   Arbitrary but unique source code location 
 *                     identifier != 0.
 * @param name         File name. 
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefSclFile( OTF_Writer* writer,
                                uint32_t stream, 
                                uint32_t sourceFile,
                                const char* name );


/**
 * depricated. The Otf-Version-record is generated automatically at beginning of
 * tracing in the global definiton stream.
 *
 * \ingroup writer
 */
int OTF_Writer_writeOtfVersion( OTF_Writer* writer, uint32_t stream );


/**
 * Write a creator record. 
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param creator      String which identifies the creator of the 
 *                     file e.g. "TAU Version x.y.z".
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCreator( OTF_Writer* writer, uint32_t stream,
                                const char* creator );




/**
 * Write a file definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param token        Arbitrary, unique identifier for the file.
 *                     Has to be > 0.
 * @param name         Name of the file.
 * @param group        File group identifier or 0 for no group.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFile( OTF_Writer* writer,
                             uint32_t stream,
                             uint32_t token,
                             const char* name,
                             uint32_t group );


/**
 * Write a file group definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param token        Arbitrary, unique identifier for the file group.
 *                     Has to be > 0.
 * @param name         Name of the file group.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFileGroup( OTF_Writer* writer,
                                  uint32_t stream,
                                  uint32_t token,
                                  const char* name );


/**
 * Write a function entry record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the function entry took place.
 * @param function  Function to be entered as defined with 
 *                  OTF_Writer_defFunction.
 * @param process   Process where action took place.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEnter( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/**
 * Write a function leave record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the function leave took place.
 * @param function  Function which was left or 0 if stack integrety checking
 *                  is not needed.
 * @param process   Process where action took place.
 * @param source    Explicit source code location or 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeLeave( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/**
 * Write a message retrieval record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the message was received.
 * @param receiver  Identifier of receiving process.
 * @param sender    Identifier of sending process.
 * @param procGroup Optional process-group sender and receiver belong to,
 *                  '0' for no group.
 * @param tag       Optional message type information.
 * @param length    Optional message length information.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeRecvMsg( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t receiver, 
                             uint32_t sender, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source );


/**
 * Write a message send record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the message was send.
 * @param sender    Sender of the message.
 * @param receiver  Receiver of the message.
 * @param procGroup Optional process-group sender and receiver belong to,
 *                  '0' for no group.
 * @param tag       Optional message type information.
 * @param length    Optional message length information.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeSendMsg( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t sender, 
                             uint32_t receiver, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source );


/**
 * Write a counter measurement record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when counter was measured.
 * @param process   Process where counter measurment took place.
 * @param counter   Counter which was measured. 
 * @param value     Counter value.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeCounter( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t process, 
                             uint32_t counter, 
                             uint64_t value );


/**
 * Write a collective operation member record.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param collective  Collective identifier to be defined with
 *                    OTF_Writer_writeDefCollectiveOperation(). 
 * @param procGroup   Group of processes participating in this collective.
 * @param rootProc    Root process if != 0.
 * @param sent        Data volume sent by member or 0.
 * @param received    Data volumd received by member or 0.
 * @param duration    Time spent in collective operation.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeCollectiveOperation( OTF_Writer* writer, 
                                         uint64_t time, 
                                         uint32_t process, 
                                         uint32_t collective, 
                                         uint32_t procGroup, 
                                         uint32_t rootProc, 
                                         uint32_t sent, 
                                         uint32_t received, 
                                         uint64_t duration, 
                                         uint32_t source );


/**
 * Write a comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeEventComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/**
 * Write a begin process record
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when process was referenced for the first time. 
 * @param process   Process identifier > 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeBeginProcess( OTF_Writer* writer,
                                  uint64_t time,
                                  uint32_t process );


/**
 * Write a end process record
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when process was referenced for the last time. 
 * @param process   Process identifier > 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeEndProcess( OTF_Writer* writer,
                                uint64_t time,
                                uint32_t process );


/**
 * Write an file operation record
 *
 * @param writer    Initialized OTF_Writer instance.
 *
 * @param time      Time when process was referenced for the last time. 
 *
 * @param fileid    File identifier > 0.
 *
 * @param handleid  File open identifier.
 *
 * @param process   Process identifier > 0.
 *
 * @param operation Type of file operation @see OTF_Handler_FileOperation()
 *
 * @param bytes     Depends on operation @see OTF_Handler_FileOperation()
 * @param duration  time spent in the file operation
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileOperation( OTF_Writer* writer,
                                   uint64_t time,
                                   uint32_t fileid,
                                   uint32_t process,
                                   uint64_t handleid,
                                   uint32_t operation,
                                   uint64_t bytes,
                                   uint64_t duration,
                                   uint32_t source );

/* *** public snapshot record write handlers *** */

/**
 * Write a snapshot comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeSnapshotComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/** 
 * Write an enter snapshot which provides information about a past
 * function call
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the according enter record was entered.
 *                      This call is still on the stack.(It has not been left
 *                      yet)
 * @param function      Function that the has been entered
 *                      OTF_Writer_defFunction.
 * @param process       Process where action took place.
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEnterSnapshot( OTF_Writer* writer, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );

/**
 * Write a send snapshot which provides information about a past
 * message send operation that is still pending, i.e. not yet received
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the message was sent
 * @param sender        Sender of the message.
 * @param receiver      Receiver of the message.
 * @param procGroup     Optional process-group sender and receiver belong to,
 *                      '0' for no group.
 * @param tag           Optional message type information.
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeSendSnapshot( OTF_Writer* writer,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t sender,
                                  uint32_t receiver,
                                  uint32_t procGroup,
                                  uint32_t tag,
                                  uint32_t source );


/**
 * Write a snapshot record for an open (and not yet closed) file
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the message was sent.
 * @param fileid        File identifier.
 * @param process       Process where the file was opened.
 * @param handleid      Unique file open identifier. @see OTF_Handler_FileOperation()
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeOpenFileSnapshot( OTF_Writer* writer,
                                      uint64_t time,
                                      uint64_t originaltime,
                                      uint32_t fileid,
                                      uint32_t process,
                                      uint64_t handleid,
                                      uint32_t source );


                           
/* *** public statistics record write handlers *** */


/**
 * Write a summary comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeSummaryComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/**
 * Write a function summary record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param time         Time when summary was computed. 
 * @param function     Function as defined with 
 *                     OTF_Handler_DefFunction.
 * @param process      Process of the given function.
 * @param count        Number of invocations.
 * @param excltime     Time spent exclusively in the given function.
 * @param incltime     Time spent in the given function including all
 *                     sub-routine calls.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeFunctionSummary( OTF_Writer* writer, 
        uint64_t time, uint32_t function, uint32_t process, 
        uint64_t count, uint64_t excltime, uint64_t incltime );

/**
 * Write a functiongroup summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed. 
 * @param functiongroup  Function group as defined with 
 *                       OTF_Handler_DefFunctionGroup.
 * @param process        Process of the given function group.
 * @param count          Number of invocations.
 * @param excltime       Time spent exclusively in the given function group.
 * @param incltime       Time spent in the given function group including all
 *                       sub-routine calls.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeFunctionGroupSummary( OTF_Writer* writer, 
        uint64_t time,  uint32_t functiongroup,  uint32_t process,  
        uint64_t count,  uint64_t excltime,  uint64_t incltime );

/**
 * Write a message summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed. 
 * @param process        Process where messages originated.
 * @param peer           Process where the message is sent to
 * @param comm           Communicator of message summary
 * @param tag            Message type/tag.
 * @param number_sent    The number of messages sent.
 * @param number_recved  The number of messages received.
 * @param bytes_sent     The number of bytes sent via messages of the given
 *                       type.
 * @param bytes_recved   The number of bytes received through messages of the 
 *                       given type.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeMessageSummary( OTF_Writer* writer, 
        uint64_t time, uint32_t process, uint32_t peer,
        uint32_t comm, uint32_t tag,  uint64_t number_sent,
        uint64_t number_recved, uint64_t bytes_sent, uint64_t bytes_recved );


/**
 * Writes a file operation summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed.
 * @param fileid         File identifier or 0 for all  files.
 * @param process        Process where file operations occured.
 * @param nopen          Number of files opened.
 * @param nclose         Number of files closed.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );


/**
 * Writes a file group operation summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed.
 * @param groupid        File group identifier or 0 for all  files/groups.
 * @param process        Process where file operations occured.
 * @param nopen          Number of files opened.
 * @param nclose         Number of files closed.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileGroupOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );
/* *** private member functions *** */


/** For a process with id 'processId' return a stream id of the stream the
    data is to be written to. If no mapping has been set so far it is defined
    in a way such that it is added to the stream with the least processes.
	\ingroup writer */
uint32_t OTF_Writer_mapProcess( OTF_Writer* writer, uint32_t processId );


/** Return the stream with the given stream id. If there is no such stream yet
    create one and append it to 'streams'. \ingroup writer */
OTF_WStream* OTF_Writer_getStream( OTF_Writer* writer, uint32_t stream );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_WRITER_H */

