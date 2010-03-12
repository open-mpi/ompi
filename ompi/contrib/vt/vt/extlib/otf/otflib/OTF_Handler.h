/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Handler.h
 *
 *  @brief Provides read access to OTF traces which consist of multiple
 *  streams.
 *
 */

/** \defgroup handler Handler Interface
 *
 * In the following, the handler interfaces for all record types are
 * specified. The signature of callback handler functions is equal to the
 * signature of corresponding record write functions except for the first
 * argument. The first argument common to all callback handler functions is
 * \em userData -- a generic pointer to custom user data. The second common
 * argument to all callback hander functions is \em stream which identifies the
 * stream where the definition occurred. A stream parameter = 0 indicates a
 * global definition which is the default.
 */

/** 
 * \defgroup ha Handler Array Interface
 *
 * Using this interface you can setup a handler array for reading traces.
 * 
 */


/* Handlers for OTF definition records *****+++***************************** */


/**
 * Provides a comment record.
 *
 * @param userData        Pointer to user data which can be set with
 *                        OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream          Identifies the stream to which this definition
 *                        belongs to. stream = 0 represents a global
 *                        definition.  
 * @param comment         Arbitrary comment string.
 *
 * @return                OTF_RETURN_ABORT  for aborting the reading process immediately
 *                        OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefinitionComment( void* userData, uint32_t stream, const char* comment );



/**
 * Provides the timer resolution. All timed event records need to be
 * interpreted according to this definition. By default, a timer resolution of
 * 1 us i.e. 1,000,000 clock ticks is assumed.
 *
 * @param userData        Pointer to user data which can be set with
 *                        OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream          Identifies the stream to which this definition
 *                        belongs to. stream = 0 represents a global
 *                        definition.  
 * @param ticksPerSecond  Clock ticks per second of the timer.
 *
 * @return                OTF_RETURN_ABORT  for aborting the reading process immediately
 *                        OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefTimerResolution( void* userData, 
                                    uint32_t stream,
                                    uint64_t ticksPerSecond );


/**
 * Provides a process definition. 
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream    Identifies the stream to which this definition
 *                  belongs to. stream = 0 represents a global
 *                  definition.  
 * @param process   Arbitrary but unique process identifier > 0.        
 * @param name      Name of the process e.g. "Process X".
 * @param parent    Previously declared parent process identifier or 0 if 
 *                  process has no parent.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefProcess( void* userData, 
                            uint32_t stream,
                            uint32_t process, 
                            const char* name, 
                            uint32_t parent );


/**
 * Provides a process group definition.
 *
 * OTF supports groups of processes. Their main objective is to classify
 * processes depending on arbitrary characteristics. Processes can reside
 * in multiple groups. This record type is optional.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream         Identifies the stream to which this definition
 *                       belongs to. stream = 0 represents a global
 *                       definition.  
 * @param procGroup      Arbitrary but unique process group identifier > 0.
 * @param name           Name of the process group e.g. "Well Balanced".
 * @param numberOfProcs  The number of processes in the process group.
 * @param procs          Vector of process identifiers as provided by
 *                       OTF_Handler_DefProcess().
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefProcessGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t procGroup, 
                                 const char* name, 
                                 uint32_t numberOfProcs, 
                                 const uint32_t* procs );


/**
 * Provides a function definition.
 *
 * Defines a function of the given name. Functions can optionally belong to a
 * certain function group provided by the
 * OTF_Handler_DefFunctionGroup() handler. A source code reference can
 * be provided aswell.
 *
 * @param userData   Pointer to user data which can be set with
 *                   OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream     Identifies the stream to which this definition
 *                   belongs to. stream = 0 represents a global
 *                   definition.  
 * @param func       Arbitrary but unique function identifier > 0.
 * @param name       Name of the function e.g. "DoSomething".
 * @param funcGroup  A function group identifier preliminary provided by 
 *                   OTF_Handler_DefFunctionGroup() or 0 for no
 *                   function group assignment.        
 * @param source     Reference to the function's source code location 
 *                   preliminary provided by OTF_Handler_DefScl() or
 *                   0 for no source code location assignment.
 *
 * @return           OTF_RETURN_ABORT  for aborting the reading process immediately
 *                   OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefFunction( void* userData, 
                             uint32_t stream,
                             uint32_t func, 
                             const char* name, 
                             uint32_t funcGroup, 
                             uint32_t source );


/**
 * Provides a function group definition.
 *
 * @param userData   Pointer to user data which can be set with
 *                   OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream     Identifies the stream to which this definition
 *                   belongs to. stream = 0 represents a global
 *                   definition.  
 * @param funcGroup  An arbitrary but unique function group identifier > 0.
 * @param name       Name of the function group e.g. "Computation".
 *
 * @return           OTF_RETURN_ABORT  for aborting the reading process immediately
 *                   OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefFunctionGroup( void* userData, 
                                  uint32_t stream,
                                  uint32_t funcGroup, 
                                  const char* name );


/**
 * Provides a collective operation definition.
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream      Identifies the stream to which this definition
 *                    belongs to. stream = 0 represents a global
 *                    definition.  
 * @param collOp      An arbitrary but unique collective op. identifier > 0.
 * @param name        Name of the collective operation e.g. "MPI_Bcast".
 * @param type        One of the five supported collective classes:
 *                    OTF_COLLECTIVE_TYPE_UNKNOWN (default),
 *                    OTF_COLLECTIVE_TYPE_BARRIER,
 *                    OTF_COLLECTIVE_TYPE_ONE2ALL,
 *                    OTF_COLLECTIVE_TYPE_ALL2ONE,
 *                    OTF_COLLECTIVE_TYPE_ALL2ALL.
 *
 * @return            OTF_RETURN_ABORT  for aborting the reading process immediately
 *                    OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefCollectiveOperation( void* userData, 
                                        uint32_t stream,
                                        uint32_t collOp,
                                        const char* name,
                                        uint32_t type );


/**
 * Provides a counter definition.
 *
 * @param userData      Pointer to user data which can be set with
 *                      OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream        Identifies the stream to which this definition
 *                      belongs to. stream = 0 represents a global
 *                      definition.  
 * @param counter       An arbitrary but unique counter identifier.
 * @param name          Name of the counter e.g. "Cache Misses".
 * @param properties    A combination of a type, scope and vartype counter property.
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
 *                      The data type can be one of the following:
 *                      COUNTER_VARTYPE_{UNSIGNED8,SIGNED8,UNSIGNED4,SIGNED4,
 *                      UNSIGNED2,SIGNED2,DOUBLE,FLOAT}
 *                      You may also use COUNTER_VARTYPE_IS{INTEGER,SIGNED,
 *                      UNSIGNED}(property) to a get a clue about the data type.
 * @param counterGroup  A previously defined counter group identifier or 0 for
 *                      no group.
 * @param unit          Unit of the counter e.g. "#" for "number of..." or 0 
 *                      for no unit. 
 *
 * @return              OTF_RETURN_ABORT  for aborting the reading process immediately
 *                      OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefCounter( void* userData, 
                            uint32_t stream,
                            uint32_t counter,
                            const char* name, 
                            uint32_t properties,
                            uint32_t counterGroup,
                            const char* unit );


/**
 * Provides a counter group definition.
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.  
 * @param counterGroup An arbitrary but unique counter group identifier > 0.
 * @param name         Counter group name.
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefCounterGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t counterGroup, 
                                 const char* name );


/**
 * Provides a source code location (SCL).
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.  
 * @param source       Arbitrary but unique source code location 
 *                     identifier > 0.
 * @param sourceFile   Previously defined source file identifier. See 
 *                     OTW_Handler_DefSclFile(). 
 * @param line         Line number.
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefScl( void* userData, 
                        uint32_t stream,
                        uint32_t source,
                        uint32_t sourceFile, 
                        uint32_t line );


/**
 * Provides a source code location (SCL) file.
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.  
 * @param sourceFile   Arbitrary but unique source code location 
 *                     identifier != 0.
 * @param name         File name. 
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefSclFile( void* userData, 
                            uint32_t stream,
                            uint32_t sourceFile,
                            const char* name );


/**
 * Provides file creator information. 
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.  
 * @param creator      String which identifies the creator of the 
 *                     file e.g. "TAU Version x.y.z".
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefCreator( void* userData, 
                            uint32_t stream,
                            const char* creator );


/**
 * Provides information on the trace´s otf-version. This record can only be read
 * and not be written.
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 * @param major        major version number
 * @param minor        minor version number
 * @param sub          sub version number
 * @param string       string identifing the version
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefVersion( void* userData,
                            uint32_t stream,
                            uint8_t major,
                            uint8_t minor,
                            uint8_t sub,
                            const char* string );


/**
 * Provides a file definition
 * NOTE: this is experimental
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 *
 * @param token        Arbitrary, unique identifier of the file.
 *                     Has to be > 0.
 *
 * @param name         name of the file
 *
 * @param group        A previously defined file group identifier or 0 for
 *                     no group.
 *
 * \ingroup handler
 */
int OTF_Handler_DefFile( void* userData,
                         uint32_t stream,
                         uint32_t token,
                         const char *name,
                         uint32_t group );


/**
 * Provides a file group definition
 * NOTE: this is experimental
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream       Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 *
 * @param token        Arbitrary, unique identifier of the file group
 *                     Has to be > 0.
 *
 * @param name         Name of the file group
 *
 * \ingroup handler
 */
int OTF_Handler_DefFileGroup( void* userData,
                              uint32_t stream,
                              uint32_t token,
                              const char *name );

/* Handlers for OTF event records ****************************************** */


/**
 * Provides a function entry event.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      The time when the function entry took place.
 * @param function  Function which has been entered as defined with 
 *                  OTF_Writer_defFunction.
 * @param process   Process where action took place.
 * @param source    Explicit source code location identifier > 0 or 0 if 
 *                  no source information available.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_Enter( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source );


/**
 * Provides a function leave event.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      The time when the function leave took place.
 * @param function  Function which was left or 0 if stack integrety checking
 *                  is not available.
 * @param process   Process where action took place.
 * @param source    Explicit source code location identifier > 0 or 0 if 
 *                  no source information available.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_Leave( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source );


/**
 * Provides a message send event.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      The time when the message was send.
 * @param sender    Sender of the message.
 * @param receiver  Receiver of the message.
 * @param group     Process-group to which sender and receiver belong to or 
 *                  0 for no group assignment.
 * @param type      Message type information > 0 or 0 for no information.
 * @param length    Optional message length information.
 * @param source    Explicit source code location identifier > 0 or 0 if 
 *                  no source information available.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_SendMsg( void* userData, 
                         uint64_t time, 
                         uint32_t sender, 
                         uint32_t receiver, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source );


/**
 * Provides a message retrieval event.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      The time when the message was received.
 * @param recvProc  Identifier of receiving process.
 * @param sendProc  Identifier of sending process.
 * @param group     Process-group to which sender and receiver belong to or 
 *                  0 for no group assignment.
 * @param type      Message type information > 0 or 0 for no information.
 * @param length    Optional message length information.
 * @param source    Explicit source code location identifier > 0 or 0 if 
 *                  no source information available.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_RecvMsg( void* userData, 
                         uint64_t time, 
                         uint32_t recvProc, 
                         uint32_t sendProc, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source );


/**
 * Provides a counter measurement.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Time when counter was measured.
 * @param process   Process where counter measurment took place.
 * @param counter   Counter which was measured. 
 * @param value     Counter value.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_Counter( void* userData, 
                         uint64_t time, 
                         uint32_t process, 
                         uint32_t counter, 
                         uint64_t value );


/**
 * Provides a collective operation member event.
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param collective  Collective identifier as defined with
 *                    OTF_Handler_DefCollectiveOperation(). 
 * @param procGroup   Group of processes participating in this collective.
 * @param rootProc    Root process if != 0.
 * @param sent        Data volume sent by member or 0.
 * @param received    Data volume received by member or 0.
 * @param duration    Time spent in collective operation.
 * @param source      Explicit source code location or 0.
 *
 * @return            OTF_RETURN_ABORT  for aborting the reading process immediately
 *                    OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_CollectiveOperation( void* userData, 
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
 * Provides a begin collective operation member event.
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param collOp      Collective identifier to be defined with
 *                    OTF_Writer_writeDefCollectiveOperation(). 
 * @param matchingId  Identifier for finding the associated end collective event
 *                    record. It must be unique within this process.
 * @param procGroup   Group of processes participating in this collective.
 * @param rootProc    Root process if != 0.
 * @param sent        Data volume sent by member or 0.
 * @param received    Data volume received by member or 0.
 * @param scltoken    Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup handler
 */
int OTF_Handler_BeginCollectiveOperation( void* userData,
					  uint64_t time,
					  uint32_t process,
					  uint32_t collOp,
					  uint64_t matchingId,
					  uint32_t procGroup,
					  uint32_t rootProc,
					  uint64_t sent,
					  uint64_t received,
					  uint32_t scltoken );


/**
 * Provides an end collective operation member event.
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param matchingId  Matching identifier, must match a previous start
 *                    collective operation.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup handler
 */
int OTF_Handler_EndCollectiveOperation( void* userData,
					uint64_t time,
					uint32_t process,
					uint64_t matchingId );


/**
 * Provide a comment record.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_EventComment( void* userData, 
                              uint64_t time, 
                              uint32_t process, 
                              const char* comment );


/**
 * Provides a process creation event.
 *
 * Marks the explicit begin of a process. This event precedes the very first
 * event of the respective process and should carry the same time stamp. This
 * is especially useful with on-line analysis. It tells whether there will be
 * additional records for the given process or not. Without this record type,
 * it could only be guessed that there might not follow more events after a
 * process has reached the bottom of the call stack.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Time when process was referenced for the first time. 
 * @param process   Process identifier > 0.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_BeginProcess( void* userData, 
                              uint64_t time, 
                              uint32_t process );


/**
 * Provides a process destruction event.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Time when process is referenced for the last time. 
 *                  Process identifiers must not be recycled!
 * @param process   Process identifier > 0.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_EndProcess( void* userData, 
                            uint64_t time, 
                            uint32_t process );


/**
 * Provides a file operation event
 * NOTE: this is experimental
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when process is referenced for the last time. 
 *                    Process identifiers must not be recycled!
 * @param process     Process identifier > 0.
 *
 * @param handleid    Unique identifier. This parameter is important
 *                    for files that are opened multiple times at the same time,
 *                    to match the close to the correct open.
 *                    This number has to be unique for every opened file (
 *                    Files with the same id must have different handleid !!! ).
 *                    Recommendation: use the timestamp of the openfile record,
 *                    or an increasing(with every fileopen record) variable for
 *                    this.
 *
 * @param operation   kind of operation done on the file.
 *                    has to be OTF_FILEOP_{OPEN,CLOSE,READ,WRITE,SEEK}
 *
 * @param bytes       Should be 0 for open and close.
 *                    Number of read/written bytes for read/write operations.
 *                    New position in the file after a seek operation.
 *
 * @param duration    Time spent in the file operation.
 *
 * @param source      Explicit source code location or 0.
 *
 * \ingroup handler
 */
int OTF_Handler_FileOperation( void* userData,
                               uint64_t time,
                               uint32_t fileid,
                               uint32_t process,
                               uint64_t handleid,
                               uint32_t operation,
                               uint64_t bytes,
                               uint64_t duration,
                               uint32_t source );


/**
 * Write a begin file operation record
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Start time of file operation. 
 * @param process   Process identifier > 0.
 * @param handleid  Operation identifier, used for finding the associated end
 *                  file operation event record.
 * @param scltoken  Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup handler
 */
int OTF_Handler_BeginFileOperation( void* userData,
					uint64_t time,
					uint32_t process,
					uint64_t handleid,
					uint32_t scltoken );


/**
 * Write an end file operation record
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      End time of file operation. 
 * @param process   Process identifier > 0.
 * @param fileid    File identifier > 0.
 * @param handleid  Operation identifier, must match a previous start file
 *                  operation event record.
 * @param operation Type of file operation @see OTF_Handler_FileOperation()
 * @param bytes     Depends on operation @see OTF_Handler_FileOperation()
 * @param scltoken  Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup handler
 */
int OTF_Handler_EndFileOperation( void* userData,
				  uint64_t time,
				  uint32_t process,
				  uint32_t fileid,
				  uint64_t handleid,
				  uint32_t operation,
				  uint64_t bytes,
				  uint32_t scltoken );


/**
 * Provides a RMA put event - end marker is anticipated on initiating Process.
 * NOTE: this is experimental
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when process is referenced for the last time. 
 *                    Process identifiers must not be recycled!
 * @param process     Process identifier > 0.
 *                    This is the Process that initiates the transfer.
 * @param origin      If >0, Process whose memory will be transferred, instead
                      of this <process>.
 * @param target      Process whose memory will be written.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record. This will be the process
 *                    group of the RMA Window in case of MPI one-sided
 *                    communication.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record. Usually this will be counted
 *                    upwards to distinguish multiple transfer sections with the
 *                    same communicator BUT THIS IS NOT MANDATORY, i.e. multiple
 *                    RMA end records with the same comm/tag combination may
 *                    exist.
 * @param bytes       Number of bytes that have been transferred by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup handler
 */
int OTF_Handler_RMAPut( void* userData,
                        uint64_t time,
                        uint32_t process,
                        uint32_t origin,
                        uint32_t target,
                        uint32_t communicator,
                        uint32_t tag,
                        uint64_t bytes,
                        uint32_t source );

/**
 * Provides a RMA put event with remote finalization marker, i.e. RMA end marker
 * is anticipated on target Process.
 * NOTE: this is experimental
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when process is referenced for the last time. 
 *                    Process identifiers must not be recycled!
 * @param process     Process identifier > 0.
 *                    This is the Process that initiates the transfer.
 * @param origin      If >0, Process whose memory will be transferred, instead
                      of this <process>.
 * @param target      Process whose memory will be written.
 *                    That Process' stream will also carry the end record for
 *                    this transaction.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record. This will be the process
 *                    group of the RMA Window in case of MPI one-sided
 *                    communication.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record. Usually this will be counted
 *                    upwards to distinguish multiple transfer sections with the
 *                    same communicator BUT THIS IS NOT MANDATORY, i.e. multiple
 *                    RMA end records with the same comm/tag combination may
 *                    exist.
 * @param bytes       Number of bytes that have been transferred by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup handler
 */
int OTF_Handler_RMAPutRemoteEnd( void* userData,
                                 uint64_t time,
                                 uint32_t process,
                                 uint32_t origin,
                                 uint32_t target,
                                 uint32_t communicator,
                                 uint32_t tag,
                                 uint64_t bytes,
                                 uint32_t source );

/**
 * Provides a RMA get event - end marker is anticipated on initiating Process.
 * NOTE: this is experimental
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when process is referenced for the last time. 
 *                    Process identifiers must not be recycled!
 * @param process     Process identifier > 0.
 *                    This is the Process that initiates the transfer.
 * @param origin      If >0, the Process whose memory will receive the data
 *                    from <target>, instead of this <process>.
 * @param target      Process whose memory will be read.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record. This will be the process
 *                    group of the RMA Window in case of MPI one-sided
 *                    communication.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record. Usually this will be counted
 *                    upwards to distinguish multiple transfer sections with the
 *                    same communicator BUT THIS IS NOT MANDATORY, i.e. multiple
 *                    RMA end records with the same comm/tag combination may
 *                    exist.
 * @param bytes       Number of bytes that have been transferred by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup handler
 */
int OTF_Handler_RMAGet( void* userData,
                        uint64_t time,
                        uint32_t process,
                        uint32_t origin,
                        uint32_t target,
                        uint32_t communicator,
                        uint32_t tag,
                        uint64_t bytes,
                        uint32_t source );

/**
 * Provide a RMA end event.
 * The end record marks the finalization of all RMA operations with the
 * communicator and tag.
 *
 * @param userData    Pointer to user data which can be set with
 *                    OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time        Time when process is referenced for the last time. 
 *                    Process identifiers must not be recycled!
 * @param process     Process identifier > 0.
 * @param remote      If >0, ends RMA transfers on Process <remote>, instead of
                      this <process>.
 * @param communicator Together with tag, it is used to identify the related RMA
 *                    transfer records.
 * @param tag         Together with communicator, it is used to identify the
 *                    related RMA transfer records.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup handler
 */
int OTF_Handler_RMAEnd( void* userData,
                        uint64_t time,
                        uint32_t process,
                        uint32_t remote,
                        uint32_t communicator,
                        uint32_t tag,
                        uint32_t source );


/* Handlers for OTF snapshot records *************************************** */


/**
 * Provides a snapshot comment.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
 
int OTF_Handler_SnapshotComment( void* userData, 
                                     uint64_t time, 
                                     uint32_t process, 
                                     const char* comment );


/** provides information about a past function call at the time 'originaltime'.
Parameters 'time', 'function', 'process' ,'source' and the return value have the
same meaning as in OTF_Handler_Enter(). 
\ingroup handler */
int OTF_Handler_EnterSnapshot( void *userData, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/** provides information about a past message send operation at the time
'originaltime'. Parameters 'time', 'sender', 'receiver', 'procGroup', 'tag',
'source' and the return value have the same meaning as in OTF_Handler_SendMsg().
\ingroup handler */
int OTF_Handler_SendSnapshot( void *userData,
                           uint64_t time,
                           uint64_t originaltime,
                           uint32_t sender,
                           uint32_t receiver,
                           uint32_t procGroup,
                           uint32_t tag,
                           uint32_t source );


/**
 * Provides a snapshot record for opened(and not yet closed) files
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time         Current timestamp.
 * @param originaltime Timestamp when the file has been opened.
 * @param process      Process identifier.
 * @param handleid     Unique file open identifier. @see OTF_Handler_FileOperation()
 * @param source       Optional reference to source code.
 *
 * \ingroup handler
 */
int OTF_Handler_OpenFileSnapshot( void* userData,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t fileid,
                                  uint32_t process,
                                  uint64_t handleid,
                                  uint32_t source );


/* Handlers for OTF summary records **************************************** */


/** Provides a summary comment.
 *
 * @param userData  Pointer to user data which can be set with
 *                  OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          OTF_RETURN_ABORT  for aborting the reading process immediately
 *                  OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
 
int OTF_Handler_SummaryComment( void * userData, 
                                uint64_t time, 
                                uint32_t process, 
                                const char* comment );


/**
 * Provides summarized information for a given function.
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time         Time when summary was computed. 
 * @param function     Function as defined with 
 *                     OTF_Handler_DefFunction.
 * @param process      Process of the given function.
 * @param invocations  Number of invocations.
 * @param exclTime     Time spent exclusively in the given function.
 * @param inclTime     Time spent in the given function including all
 *                     sub-routine calls.
 *
 * @return            OTF_RETURN_ABORT  for aborting the reading process immediately
 *                    OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_FunctionSummary( void* userData, 
                                 uint64_t time, 
                                 uint32_t function, 
                                 uint32_t process, 
                                 uint64_t invocations, 
                                 uint64_t exclTime, 
                                 uint64_t inclTime );


/**
 * Provides summarized information for a given group of functiongroups.
 *
 * @param userData     Pointer to user data which can be set with
 *                     OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time         Time when summary was computed. 
 * @param funcGroup    Function group as defined with 
 *                     OTF_Handler_DefFunctionGroup.
 * @param process      Process of the given function group.
 * @param invocations  Number of invocations.
 * @param exclTime     Time spent exclusively in the given function group.
 * @param inclTime     Time spent in the given function group including all
 *                     sub-routine calls.
 *
 * @return             OTF_RETURN_ABORT  for aborting the reading process immediately
 *                     OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_FunctionGroupSummary( void* userData, 
                                      uint64_t time,  
                                      uint32_t funcGroup,  
                                      uint32_t process,  
                                      uint64_t invocations,  
                                      uint64_t exclTime,  
                                      uint64_t inclTime );


/**
 * Provides summarized information for a given message type.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time when summary was computed. 
 * @param process        Process where messages originated.
 * @param peer           Process where the message is sent to
 * @param comm           Communicator of message summary
 * @param type           Message type/tag.
 * @param sentNumber     The number of messages sent.
 * @param receivedNumber The number of messages received.
 * @param sentBytes      The number of bytes sent via messages of the given
 *                       type.
 * @param receivedBytes  The number of bytes received through messages of the 
 *                       given type.
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_MessageSummary( void* userData,
                                uint64_t time,
                                uint32_t process,
                                uint32_t peer,
                                uint32_t comm,
                                uint32_t type,
                                uint64_t sentNumber,
                                uint64_t receivedNumber,
                                uint64_t sentBytes,
                                uint64_t receivedBytes );


/**
 * Provides summarized information for collective operations.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg().
 * @param time           Time when summary was computed.
 * @param process        Process identifier i.e. collective member.
 * @param comm           Communicator of collective operation summary.
 * @param collective     Collective identifier as defined with
 *                       OTF_Handler_DefCollectiveOperation().
 * @param sentNumber     The number of messages sent by member or 0.
 * @param receivedNumber The number of messages received by member or 0.
 * @param sentBytes      The number of bytes sent by member or 0.
 * @param receivedBytes  The number of bytes received by member or 0.
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_CollopSummary( void *userData,
                               uint64_t time,
                               uint32_t process,
                               uint32_t comm,
                               uint32_t collective,
                               uint64_t sentNumber,
                               uint64_t receivedNumber,
                               uint64_t sentBytes,
                               uint64_t receivedBytes );


/**
 * Provides summarized information about file operations.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time when summary was computed.
 * @param fileid         File identifier or 0 for all files.
 * @param process        Process where file operations occured.
 * @param nopen          Number of open events.
 * @param nclose         Number of close events.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * \ingroup handler
 */
int OTF_Handler_FileOperationSummary( void* userData,
                                      uint64_t time,
                                      uint32_t fileid,
                                      uint32_t process,
                                      uint64_t nopen,
                                      uint64_t nclose,
                                      uint64_t nread,
                                      uint64_t nwrite,
                                      uint64_t nseek,
                                      uint64_t bytesread,
                                      uint64_t byteswrite );


/**
 * Provides summarized information about file operations in a file group.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time when summary was computed.
 * @param groupid        Group identifier or 0 for all files.
 * @param process        Process where file operations occured.
 * @param nopen          Number of open events.
 * @param nclose         Number of close events.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * \ingroup handler
 */
int OTF_Handler_FileGroupOperationSummary( void* userData,
                                           uint64_t time,
                                           uint32_t groupid,
                                           uint32_t process,
                                           uint64_t nopen,
                                           uint64_t nclose,
                                           uint64_t nread,
                                           uint64_t nwrite,
                                           uint64_t nseek,
                                           uint64_t bytesread,
                                           uint64_t byteswrite );


/**
 * Can be used to handle records which cannot be read.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time when summary was computed. 
 * @param process        If 'time' equals (uin64_t) -1, the unknown record
 *                       is a definiton record and 'process'
 *                       represents the streamid of the record.
 *                       If 'time' has a valid value ( not (uint64)-1 ) the
 *                       unknown record is an event-, statistics- or
 *                       snapshotrecord and 'process' represents
 *                       the processid of the record.
 * @param record         string which contains the record.
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_UnknownRecord( void *userData,
                              uint64_t time,
                              uint32_t process,
                              const char *record );


/*
 * Provides summarized information about a counter.
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time when summary was computed. 
 * @param process        Process where messages originated.
 * @param peer           Token of counter.
 * @param value          Counter Value at current time.
 *
 * \ingroup handler
 */
/*int OTF_Handler_CounterSummary( void* userData, 
                                uint64_t time, 
                                uint32_t process, 
                                uint32_t counterid, 
                                uint64_t value );
*/


/* Handlers for OTF marker records **************************************** */


/**
 * Define a marker
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param stream         stream ID is ignored here
 * @param token          The newly defined marker token.
 * @param name           Its name
 * @param type           Marker type, one of OTF_MARKER_TYPE_xxx
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_DefMarker( void *userData,
                           uint32_t stream,
                           uint32_t token,
                           const char* name,
                           uint32_t type );


/**
 * Define a marker
 *
 * @param userData       Pointer to user data which can be set with
 *                       OTF_HandlerArray_setFirstHandlerArg(). 
 * @param time           Time stamp of the marker record. Note that marker records are 
 *                       not sorted according to time stamps!
 * @param process        The process or process group of the marker.
 * @param token          A marker token defined by 'DefMarker' before.
 * @param text           Descriptive text.
 *
 * @return               OTF_RETURN_ABORT  for aborting the reading process immediately
 *                       OTF_RETURN_OK     for continue reading
 *
 * \ingroup handler
 */
int OTF_Handler_Marker( void *userData,
                        uint64_t time,
                        uint32_t process,
                        uint32_t token,
                        const char* text );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_HANDLER_H */
