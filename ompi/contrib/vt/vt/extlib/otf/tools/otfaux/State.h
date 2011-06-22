/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef STATE_H
#define STATE_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <vector>
#include <deque>
#include <map>
#include <list>

#include "OTF_inttypes.h"

#include "otf.h"


/* *** function stuff *** ********************************* */

struct FunctionCall {


public:

	uint64_t time;
	uint32_t token;
    OTF_KeyValueList *kvlist;


public:

	/** constructor */
	FunctionCall( uint64_t _time, uint32_t _token, OTF_KeyValueList *_kvlist );

	/** copy constructor */
	FunctionCall( const FunctionCall& fc );
    
    /** destructor */
    ~FunctionCall();
    
    /** assignment operator */
    FunctionCall operator=( const FunctionCall& fc );
};


struct FunctionStatistics {


public:

	int64_t occurrences;
	int64_t exclusiveTime;
	int64_t inclusiveTime;


public:

	/** constructor */
	FunctionStatistics( int64_t o= 0, int64_t ex= 0, int64_t in= 0 ) : 
		occurrences( o ), exclusiveTime( ex ), inclusiveTime( in ) {};

	/** copy constructor */
	FunctionStatistics( const FunctionStatistics& other ) : 
		occurrences( other.occurrences ), exclusiveTime( other.exclusiveTime ), 
		inclusiveTime( other.inclusiveTime ) {};
};


/* *** send stuff ************************************** */
struct Send {


public:
	
	uint64_t originaltime;
	uint32_t receiver;
	uint32_t procGroup;
	uint32_t tag;
    uint32_t length;
	uint32_t source;
    OTF_KeyValueList *kvlist;
	
public:
  
    /** constructor */
	Send( uint64_t _originaltime, uint32_t _receiver, uint32_t _procGroup, uint32_t _tag,
          uint32_t _length, uint32_t _source, OTF_KeyValueList *_kvlist );
          
    /** copy constructor */
	Send( const Send& s );
    
    /** destructor */
    ~Send();
    
    /** assignment operator */
    Send operator=( const Send& s );
};


struct SendStatistics {

public:
	
	uint64_t number_sent;
	uint64_t number_recvd;
	uint64_t bytes_sent;
	uint64_t bytes_recvd;
	
public:

	SendStatistics( uint64_t ns= 0, uint64_t nr= 0, uint64_t bs= 0, uint64_t br= 0 ) :
		number_sent( ns ), number_recvd( nr ), bytes_sent( bs ),
		bytes_recvd( br ) {};
	
	SendStatistics( const SendStatistics& other ) :
		number_sent( other.number_sent ), number_recvd( other.number_recvd ),
		bytes_sent( other.bytes_sent ), bytes_recvd( other.bytes_recvd ) {};
};


struct CollectiveOperations {

public:

	mutable std::map<uint32_t,uint64_t> numSent;
	mutable std::map<uint32_t,uint64_t> numRecv;
	mutable std::map<uint32_t,uint64_t> bytesSent;
	mutable std::map<uint32_t,uint64_t> bytesRecv;
	mutable std::map<uint32_t,uint32_t> Type2Col;
};


struct BeginCollOperation {

public:
  
    uint64_t time;
    uint32_t root;
    uint32_t procGroup;
    uint32_t col;
    uint32_t type;
    uint64_t invoc_sent;
    uint64_t invoc_recv;
    uint64_t bytesSent;
    uint64_t bytesRecv;
    uint32_t scltoken;
    OTF_KeyValueList *kvlist;
    
public:    
 
    /** constructor */
    BeginCollOperation( uint64_t _time, uint32_t _root, uint32_t _procGroup,
        uint32_t _col, uint32_t _type, uint64_t _invoc_sent, uint64_t _invoc_recv,
        uint64_t _bytesSent, uint64_t _bytesRecv, uint32_t _scltoken, OTF_KeyValueList *_kvlist );
        
    /** copy constructor */
    BeginCollOperation( const BeginCollOperation& cop );
    
    /** destructor */
    ~BeginCollOperation();
    
    /** assignment operator */
    BeginCollOperation operator=( const BeginCollOperation& cop );
  
};

struct BeginFileOperation {

public:
  
    uint64_t time;
    uint32_t scltoken;
    OTF_KeyValueList *kvlist;
    
    
public:    
 
    /** constructor */
    BeginFileOperation( uint64_t _time, uint32_t _scltoken, OTF_KeyValueList *_kvlist );
    
    /** copy constructor */
    BeginFileOperation( const BeginFileOperation& fop );
    
    /** destructor */
    ~BeginFileOperation();
    
    /** assignment operator */
    BeginFileOperation operator=( const BeginFileOperation& fop );
    
};

/* *** file operation stuff ******************************** */
struct FileOpen {

public:

	uint64_t time;
	uint32_t fileid;
	uint32_t source;
    OTF_KeyValueList *kvlist;

public:
  
    /** constructor */
	FileOpen( uint64_t _time, uint32_t _fileid, uint32_t _source, OTF_KeyValueList *_kvlist );
    
    /** copy constructor */
    FileOpen( const FileOpen& fo );
    
    /** destructor */
    ~FileOpen();
    
    /** assignment operator */
    FileOpen operator=( const FileOpen& fo );
};


struct FileOperationStatistics {

public:

	uint64_t nopen;
	uint64_t nclose;
	uint64_t nread;
	uint64_t nwrite;
	uint64_t nseek;
	uint64_t bytesread;
	uint64_t byteswrite;

public:

	FileOperationStatistics( uint64_t no= 0, uint64_t nc= 0, uint64_t nr= 0,
		uint64_t nw= 0, uint64_t ns= 0, uint64_t br= 0, uint64_t bw= 0 ) :
		nopen( no ), nclose( nc ), nread( nr ), nwrite( nw ), nseek( ns ),
		bytesread( br ), byteswrite( bw ) {};

	FileOperationStatistics( const FileOperationStatistics& other ) :
		nopen( other.nopen ), nclose( other.nclose ), nread( other.nread ),
		nwrite( other.nwrite ), nseek( other.nseek ),
		bytesread( other.bytesread ), byteswrite( other.byteswrite ) {};
};


/* *** ProcessState *** ********************************* */


/** class containing the state of a process trace at a given */
class ProcessState {


public:

	/* function stack */
	std::deque<FunctionCall> fstack;
	
	/* list of pending messages */
	std::deque<Send> sstack;

	/* map of open files */
	std::map<uint64_t/*handleid*/, FileOpen> openfiles;
    
    /* map of begun collective operations */
    std::map<uint64_t/*matchingId*/, BeginCollOperation> beginCollOps;
    
    /* map of unfinished file operations */
    std::map<uint64_t/*handleid*/, BeginFileOperation> beginFileOps;

	/* statistic per function since the beginning of the trace */
	std::map<uint32_t,FunctionStatistics> fstatistics;

	/* statistics for messages */
	SendStatistics sstatistics;

	/* statistics for collective operations */
	CollectiveOperations CollOps;

	/* statistic per file since the beginning of the trace */
	std::map<uint32_t,FileOperationStatistics> fostatistics;


public:

	/** constructor */
	ProcessState() {};


	void enterFunction( uint64_t time, uint32_t token, OTF_KeyValueList *kvlist );

	void leaveFunction( uint64_t time, uint32_t token );

	void sendMessage( uint64_t time, uint32_t receiver, uint32_t procGroup,
		uint32_t tag, uint32_t msglength, uint32_t source, OTF_KeyValueList *kvlist );

	void collOperation( uint64_t time, uint32_t col, uint32_t type, uint32_t numSent, uint32_t numRecv, 
		uint32_t bytesSent, uint32_t bytesRecv );
	
	void recvMessage( uint32_t msglength );
		
	void matchMessage( uint32_t receiver, uint32_t procGroup, uint32_t tag );

	int openFile( uint64_t time, uint32_t fileid, uint64_t handleid,
		uint32_t source, OTF_KeyValueList *kvlist );
	int closeFile( uint64_t handleid );
	int writeFile( uint32_t fileid, uint64_t bytes );
	int readFile( uint32_t fileid, uint64_t bytes );
	int seekFile( uint32_t fileid, uint64_t bytes );
    
    
    int beginCollOperation( uint64_t time, uint32_t root, uint32_t procGroup,
        uint32_t col, uint32_t type, uint64_t matchingId, uint64_t invoc_sent,
        uint64_t invoc_recv, uint64_t bytesSent, uint64_t bytesRecv,
        uint32_t scltoken, OTF_KeyValueList *kvlist );
        
    int endCollOperation( uint64_t time, uint32_t matchingId );
    
    int beginFileOperation( uint64_t time, uint64_t matchingId, uint32_t scltoken,
        OTF_KeyValueList *kvlist );
    
    int endFileOperation( uint64_t matchingId );
	
	
	void printStack( uint32_t processid ) const;
	void printStatistics( uint32_t processid, uint64_t time,
		std::map< uint32_t, uint32_t> *functiongroups,
		std::map< uint32_t, uint32_t> *filegroups ) const;
	void printSends( uint32_t processid ) const;
	void printOpenFiles( uint32_t processid ) const;

	void writeStack( OTF_Writer* writer, uint64_t time, uint32_t processid ) const;
	void writeStatistics( OTF_Writer* writer, uint64_t time,
		uint32_t processid,
		std::map< uint32_t, uint32_t> *functiongroups,
		std::map< uint32_t, uint32_t> *filegroups ) const;
	void writeSends( OTF_Writer* writer, uint64_t time, uint32_t processid ) const;
	void writeOpenFiles( OTF_Writer* writer, uint64_t time, uint32_t processid ) const;
    void writeCollOps( OTF_Writer* writer, uint64_t time, uint32_t processid ) const;
    void writeFileOps( OTF_Writer* writer, uint64_t time, uint32_t processid ) const;
};

/* *** State *** **************************************** */

/** state of a whole trace */
class State {


	std::map<uint32_t,ProcessState> processes;

	/* maps the collective operation to its type */
	std::map<uint32_t,uint32_t> Col2Type;

	/* maps the function to its funtiongroupid */
	std::map< uint32_t, uint32_t> functiongroups;
	
	/* maps the files to its filegroupid */
	std::map< uint32_t, uint32_t> filegroups;

	bool usefunctiongroups;
	bool usefilegroups;
	bool doSnapshots;
	bool doStatistics;

public:

	/** constructor */
	State( bool _usefunctiongroups= false, bool _usefilegroups= false,
		bool _doSnapshots= true, bool _doStatistics= true ) :
		usefunctiongroups( _usefunctiongroups ), usefilegroups( _usefilegroups ),
		doSnapshots( _doSnapshots ), doStatistics( _doStatistics ) {};


	void defProcess( uint32_t processid );

	void defFunction( uint32_t function, uint32_t group );

	void defFile( uint32_t fileid, uint32_t group );

	void defCollOp( uint32_t col, uint32_t type );

	void enterFunction( uint64_t time, uint32_t processid, uint32_t token, OTF_KeyValueList *kvlist );

	void leaveFunction( uint64_t time, uint32_t processid, uint32_t token );
	
	void sendMessage( uint64_t time, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source,
        OTF_KeyValueList *kvlist );

	void collOperation( uint64_t time, uint32_t proc, uint32_t root, uint32_t col,
		uint32_t bytesSent, uint32_t bytesRecv );
	
	void recvMessage( uint32_t sender, uint32_t receiver, uint32_t procGroup,
		uint32_t tag, uint32_t msglength );

	int fileOperation( uint64_t time, uint32_t fileid, uint32_t process,
		uint64_t handleid, uint32_t operation, uint64_t bytes,
		uint64_t duration, uint32_t source, OTF_KeyValueList *kvlist );
        
    int beginCollOperation( uint64_t time, uint32_t proc, uint32_t root, uint32_t procGroup,
        uint32_t col, uint64_t matchingId, uint64_t bytesSent, uint64_t bytesRecv,
        uint32_t scltoken, OTF_KeyValueList *kvlist );
        
    int endCollOperation( uint64_t time, uint32_t proc, uint64_t matchingId );
    
    int beginFileOperation( uint64_t time, uint32_t process, uint64_t matchingId,
        uint32_t scltoken, OTF_KeyValueList *kvlist );
    
    int endFileOperation( uint64_t time, uint32_t process, uint32_t fileid, uint64_t matchingId,
        uint64_t handleId, uint32_t operation, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *kvlist );
	

	void printStack() const;
	void printStatistics( uint64_t time );
	void printSends() const;
	void printOpenFiles() const;
	
	
	void writeSnapshot( OTF_Writer* writer, uint64_t time );
	void writeStatistics( OTF_Writer* writer, uint64_t time );
};

#endif /* STATE_H */

