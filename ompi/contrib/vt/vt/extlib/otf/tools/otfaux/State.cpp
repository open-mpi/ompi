/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "OTF_Platform.h"

#include "State.h"


#include <iostream>
#include <cassert>
using namespace std;


FunctionCall::FunctionCall( uint64_t _time, uint32_t _token, OTF_KeyValueList *_kvlist ) {
  
    time = _time;
    token = _token;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, _kvlist );
  
}

FunctionCall::FunctionCall( const FunctionCall& fc ) {
  
    time = fc.time;
    token = fc.token;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, fc.kvlist );
  
}

FunctionCall::~FunctionCall() {
 
    OTF_KeyValueList_close( kvlist );
  
}

FunctionCall FunctionCall::operator=( const FunctionCall& fc ) {
  
    if( this == &fc )
        return *this;
  
    time = fc.time;
    token = fc.token;
    
    OTF_KeyValueList_reset( kvlist );
        
    OTF_KeyValueList_appendKeyValueList( kvlist, fc.kvlist );
    
    return *this;
    
}


Send::Send( uint64_t _originaltime, uint32_t _receiver, uint32_t _procGroup,
    uint32_t _tag, uint32_t _length, uint32_t _source, OTF_KeyValueList *_kvlist ) {
  
    originaltime = _originaltime;
    receiver = _receiver;
    procGroup = _procGroup;
    tag = _tag;
    length = _length;
    source = _source;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, _kvlist );
  
}
      
Send::Send( const Send& s ) {
  
    originaltime = s.originaltime;
    receiver = s.receiver;
    procGroup = s.procGroup;
    tag = s.tag;
    length = s.length;
    source = s.source;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, s.kvlist );
  
}

Send::~Send() {
  
    OTF_KeyValueList_close( kvlist );
  
}

Send Send::operator=( const Send& s ) {
  
    if( this == &s )
        return *this;
  
    originaltime = s.originaltime;
    receiver = s.receiver;
    procGroup = s.procGroup;
    tag = s.tag;
    length = s.length;
    source = s.source;
    
    OTF_KeyValueList_reset( kvlist );
        
    OTF_KeyValueList_appendKeyValueList( kvlist, s.kvlist );
    
    return *this;
    
}


BeginCollOperation::BeginCollOperation( uint64_t _time, uint32_t _root, uint32_t _procGroup,
    uint32_t _col, uint32_t _type, uint64_t _invoc_sent, uint64_t _invoc_recv, uint64_t _bytesSent,
    uint64_t _bytesRecv, uint32_t _scltoken, OTF_KeyValueList *_kvlist ) {
  
    time = _time;
    root = _root;
    procGroup = _procGroup;
    col = _col;
    type = _type;
    invoc_sent = _invoc_sent;
    invoc_recv = _invoc_recv;
    bytesSent = _bytesSent;
    bytesRecv = _bytesRecv;
    scltoken = _scltoken;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, _kvlist );
    
}
    
BeginCollOperation::BeginCollOperation( const BeginCollOperation& cop ) {
  
    time = cop.time;
    root = cop.root;
    procGroup = cop.procGroup;
    col = cop.col;
    type = cop.type;
    invoc_sent = cop.invoc_sent;
    invoc_recv = cop.invoc_recv;
    bytesSent = cop.bytesSent;
    bytesRecv = cop.bytesRecv;
    scltoken = cop.scltoken;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, cop.kvlist );
  
}

BeginCollOperation::~BeginCollOperation() {
  
    OTF_KeyValueList_close( kvlist );
  
}

BeginCollOperation BeginCollOperation::operator=( const BeginCollOperation& cop ) {
  
    if( this == &cop )
        return *this;
  
    time = cop.time;
    root = cop.root;
    procGroup = cop.procGroup;
    col = cop.col;
    type = cop.type;
    invoc_sent = cop.invoc_sent;
    invoc_recv = cop.invoc_recv;
    bytesSent = cop.bytesSent;
    bytesRecv = cop.bytesRecv;
    scltoken = cop.scltoken;
    
    OTF_KeyValueList_reset( kvlist );
        
    OTF_KeyValueList_appendKeyValueList( kvlist, cop.kvlist );
    
    return *this;
    
}


BeginFileOperation::BeginFileOperation( uint64_t _time, uint32_t _scltoken, OTF_KeyValueList *_kvlist ) {
  
    time = _time;
    scltoken = _scltoken;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, _kvlist );
  
}

BeginFileOperation::BeginFileOperation( const BeginFileOperation& fop ) {

    time = fop.time;
    scltoken = fop.scltoken;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, fop.kvlist );
}

BeginFileOperation::~BeginFileOperation() {
  
    OTF_KeyValueList_close( kvlist ); 
  
}

BeginFileOperation BeginFileOperation::operator=( const BeginFileOperation& fop ) {
  
    if( this == &fop )
        return *this;
  
    time = fop.time;
    scltoken = fop.scltoken;
    
    OTF_KeyValueList_reset( kvlist );
        
    OTF_KeyValueList_appendKeyValueList( kvlist, fop.kvlist );
    
    return *this;
    
}


FileOpen::FileOpen( uint64_t _time, uint32_t _fileid, uint32_t _source, OTF_KeyValueList *_kvlist ) {
 
    time = _time;
    fileid = _fileid;
    source = _source;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, _kvlist );
    
}

FileOpen::FileOpen( const FileOpen& fo ) {
 
    time = fo.time;
    fileid = fo.fileid;
    source = fo.source;
    
    kvlist = OTF_KeyValueList_new();
    
    OTF_KeyValueList_appendKeyValueList( kvlist, fo.kvlist );
  
}

FileOpen::~FileOpen() {
  
    OTF_KeyValueList_close( kvlist );
  
}

FileOpen FileOpen::operator=( const FileOpen& fo ) {
  
    if( this == &fo )
        return *this;
  
    time = fo.time;
    fileid = fo.fileid;
    source = fo.source;
    
    OTF_KeyValueList_reset( kvlist );
        
    OTF_KeyValueList_appendKeyValueList( kvlist, fo.kvlist );
    
    return *this;
    
}


/* *** ProcessState *** ********************************* */


void ProcessState::enterFunction( uint64_t time, uint32_t token, OTF_KeyValueList *kvlist ) {



	fstack.push_back( FunctionCall( time, token, kvlist ) );
	
	FunctionStatistics& stat= fstatistics[ token ];
	stat.occurrences++;
}


void ProcessState::leaveFunction( uint64_t time, uint32_t token ) {


	assert( ! fstack.empty() );

	const FunctionCall& call= fstack.back();

	/* if not special token 0 tokens must match */
	if ( ( 0 != token ) && ( call.token != token ) ) {

		cerr << "  leave at " << time << " with corrupt stack " << 
			call.token << " != " << token << endl;
	}

	/* update stack */
	fstack.pop_back();

	/* update statistics */
	FunctionStatistics& stat= fstatistics[ call.token ];

	stat.exclusiveTime += time - call.time;
	stat.inclusiveTime += time - call.time;

	/* subtract time from parents 'exclusiveTime' */
	if ( ! fstack.empty() ) {

		const FunctionCall& parent= fstack.back();

		FunctionStatistics& parentstat= fstatistics[ parent.token ];
		
		parentstat.exclusiveTime -= time - call.time;
	}
}

void ProcessState::collOperation( uint64_t time, uint32_t col, uint32_t type, uint32_t numSent,
	uint32_t numRecv, uint32_t bytesSent, uint32_t bytesRecv ) {
	 
	CollOps.numSent[type] += numSent;
	CollOps.numRecv[type] += numRecv;
	CollOps.bytesSent[type] += bytesSent;
	CollOps.bytesRecv[type] += bytesRecv;
	CollOps.Type2Col[type] = col;
}

int ProcessState::beginCollOperation( uint64_t time, uint32_t root, uint32_t procGroup,
        uint32_t col, uint32_t type, uint64_t matchingId, uint64_t invoc_sent,
        uint64_t invoc_recv, uint64_t bytesSent, uint64_t bytesRecv, uint32_t scltoken,
        OTF_KeyValueList *kvlist ) {
  
  
    std::map<uint64_t, BeginCollOperation>::iterator it; 

    it = beginCollOps.find( matchingId );

    if( it != beginCollOps.end() ) {
           
#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "MatchingId %llu has already been used. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    /* insert the record into the list of begun collective operations */
    beginCollOps.insert( pair<uint64_t,BeginCollOperation>( matchingId,
        BeginCollOperation( time, root, procGroup, col, type, invoc_sent,
            invoc_recv, bytesSent, bytesRecv, scltoken, kvlist ) ) );
      
      
    return OTF_RETURN_OK;
  
}

int ProcessState::endCollOperation( uint64_t time, uint32_t matchingId ) {
 
    std::map<uint64_t, BeginCollOperation>::iterator it; 

    it = beginCollOps.find( matchingId );

    if( it == beginCollOps.end() ) {
      
#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "MatchingId %llu was not found. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    collOperation( time /*?*/, it->second.col, it->second.type, it->second.invoc_sent,
                   it->second.invoc_recv, it->second.bytesSent, it->second.bytesRecv );
                   
    beginCollOps.erase( it );

    return OTF_RETURN_OK;
        
}

int ProcessState::beginFileOperation( uint64_t time, uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *kvlist ) {
  
    std::map<uint64_t, BeginFileOperation>::iterator it;
    
    it = beginFileOps.find( matchingId );
    
    if( it != beginFileOps.end() ) {
      
#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "Handleid %llu has already been used. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    /* insert the record into the list of unfinished file operations */
    beginFileOps.insert( pair<uint64_t, BeginFileOperation>( matchingId, BeginFileOperation( time, scltoken, kvlist ) ) );
                                                                      
    return OTF_RETURN_OK;
  
}

int ProcessState::endFileOperation( uint64_t matchingId ) {
  
  
    std::map<uint64_t, BeginFileOperation>::iterator it;
    
    it = beginFileOps.find( matchingId );
    
    if( it == beginFileOps.end() ) {

#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "Handleid %llu was not found. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    beginFileOps.erase( it );
 
    return OTF_RETURN_OK;
  
}


void ProcessState::sendMessage( uint64_t time, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t msglength, uint32_t source,
    OTF_KeyValueList *kvlist ) {
	
	
	sstatistics.bytes_sent+= msglength;
	sstatistics.number_sent++;

	
	
	sstack.push_back( Send( time, receiver, procGroup, tag, msglength, source, kvlist ) );
}


void ProcessState::recvMessage( uint32_t msglength ) {

	sstatistics.bytes_recvd+= (uint64_t) msglength;
	sstatistics.number_recvd++;
}


void ProcessState::matchMessage( uint32_t receiver, uint32_t procGroup, uint32_t tag ) {


	deque<Send>::iterator jt= sstack.begin();
	deque<Send>::iterator jtend= sstack.end();

	for ( ; jt != jtend; ++jt ) {

		if (jt->receiver == receiver
			&& procGroup == jt->procGroup
			&& tag == jt->tag ) {
			
			sstack.erase( jt );
			break;
		}
	}
}


int ProcessState::openFile( uint64_t time, uint32_t fileid, uint64_t handleid,
	uint32_t source, OTF_KeyValueList *kvlist ) {


	std::map<uint64_t, FileOpen>::iterator it;

	it= openfiles.find( handleid );

	if( it == openfiles.end() ) {

		/* insert the file into the list of opened files */
		openfiles.insert( pair<uint64_t,FileOpen>( handleid,
			FileOpen( time, fileid, source, kvlist ) ) );

		
		/* make the statistics */
		map<uint32_t,FileOperationStatistics>::iterator it2;
		
		it2= fostatistics.find( fileid );
	
		if( it2 != fostatistics.end() ) {
	
			FileOperationStatistics& fos= it2->second;
	
			++fos.nopen;

		} else {

			fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
				FileOperationStatistics( 1, 0, 0, 0, 0, 0, 0 ) ) );
		
		}
		
		return OTF_RETURN_OK;

	} else {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"Handleid %llu has already been used. aborting\n",
				__FUNCTION__, __FILE__, __LINE__, (long long unsigned) handleid );
#		endif

		return OTF_RETURN_ABORT;
	}
}


int ProcessState::closeFile( uint64_t handleid ) {


    uint32_t ret;
    map<uint64_t/*handleid*/, FileOpen>::iterator it;

    it= openfiles.find( handleid );


    if( it == openfiles.end() ) {

#       ifdef OTF_VERBOSE
            fprintf( stderr, "WARNING in function %s, file: %s, line: %i:\n "
                "Trying to close a file that is not open with handle %llu. "
                "This might be caused by a VT error, please check! Ignore this for now.\n",
            __FUNCTION__, __FILE__, __LINE__, (long long unsigned) handleid );
#       endif

        /* make it a warning that cannot be disabled because I suspect an error in VT ! */
        /* return OTF_RETURN_ABORT; */
        return OTF_RETURN_OK;
    }



    /* make the statistics */
    map<uint32_t,FileOperationStatistics>::iterator it2;

    it2= fostatistics.find( it->second.fileid );	

    if( it2 != fostatistics.end() ) {

        FileOperationStatistics& fos= it2->second;

        ++fos.nclose;

        ret= OTF_RETURN_OK;

    } else {

#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "Trying to close not yet opened file. aborting\n",
                __FUNCTION__, __FILE__, __LINE__ );
#       endif

        ret= OTF_RETURN_ABORT;
    }

    /* erase the file from the opened files list */
    openfiles.erase( it );

    return ret;
}


int ProcessState::writeFile( uint32_t fileid, uint64_t bytes ) {


	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nwrite;
		fos.byteswrite+= bytes;

	} else {
	
		fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
			FileOperationStatistics( 0, 0, 0, 1, 0, 0, bytes ) ) );

	}
	
	return OTF_RETURN_OK;
}


int ProcessState::readFile( uint32_t fileid, uint64_t bytes ) {


	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nread;
		fos.bytesread+= bytes;

	} else {
	
		fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
			FileOperationStatistics( 0, 0, 1, 0, 0, bytes, 0 ) ) );

	}
	
	return OTF_RETURN_OK;
}


int ProcessState::seekFile( uint32_t fileid, uint64_t bytes ) {
	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nseek;

		return OTF_RETURN_OK;

	} else {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"Trying to seek in a not yet opened file. aborting\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return OTF_RETURN_ABORT;
	}
}


void ProcessState::printStack( uint32_t processid ) const {


	cerr << "  stack of process " << processid << endl;

	deque<FunctionCall>::const_iterator it= fstack.begin();
	deque<FunctionCall>::const_iterator itend= fstack.end();
	for ( ; it != itend; ++it ) {

		cerr << "    " << it->time << ":    " << it->token << endl;
	}
}


void ProcessState::printSends( uint32_t processid ) const {


	cerr << "  pending sends on process " << processid << endl;
	
	deque<Send>::const_iterator it= sstack.begin();
	deque<Send>::const_iterator itend= sstack.end();
	
	for ( ; it != itend; ++it ) {

		cerr << "    " << "otime: " << it->originaltime << " recver: " << it->receiver <<
		" group: " << it->procGroup << " tag: " << it->tag << " source: " <<
		it->source << endl;
	}
}


void ProcessState::printOpenFiles( uint32_t processid ) const {


	cerr << "  opened files on process " << processid << endl;

	map<uint64_t/*handleid*/, FileOpen>::const_iterator it;
	map<uint64_t/*handleid*/, FileOpen>::const_iterator itend= openfiles.end();


	for( it= openfiles.begin(); it != itend; ++it ) {

		cerr << "    " << "time " << it->second.time << " handleid " << it->first << " fileid "
			<< it->second.fileid << " source " << it->second.source << endl;

	}
}


void ProcessState::printStatistics( uint32_t processid, uint64_t time,
	map< uint32_t, uint32_t> *functiongroups,
	map< uint32_t, uint32_t> *filegroups ) const {

	
	map<uint32_t,FunctionStatistics> localStatistics;
	map<uint32_t,FunctionStatistics> groupStatistics;

	uint64_t lasttime= time;

	deque<FunctionCall>::const_reverse_iterator it= fstack.rbegin();
	deque<FunctionCall>::const_reverse_iterator itend= fstack.rend();
	for ( ; it != itend; ++it ) {

		FunctionStatistics& lstats= localStatistics[ it->token ];
		/* lstats.occurrences++; */
		lstats.exclusiveTime += lasttime - it->time;
		lstats.inclusiveTime += time - it->time;

		assert( lasttime >= it->time );
			
		lasttime= it->time;
	}


	/* actually write statistics */
	cerr << "  statistics of process " << processid << endl;

	map<uint32_t,FunctionStatistics>::const_iterator jt= fstatistics.begin();
	map<uint32_t,FunctionStatistics>::const_iterator jtend= fstatistics.end();

	for ( ; jt != jtend; ++jt ) {

		uint64_t oc= jt->second.occurrences;
		uint64_t ex= jt->second.exclusiveTime;
		uint64_t in= jt->second.inclusiveTime;

		map<uint32_t,FunctionStatistics>::const_iterator kt= localStatistics.find( jt->first );
		map<uint32_t,FunctionStatistics>::const_iterator ktend= localStatistics.end();

		if ( kt != ktend ) {
		
			oc += kt->second.occurrences;
			ex += kt->second.exclusiveTime;
			in += kt->second.inclusiveTime;
		}

		if ( NULL == functiongroups ) {
		
			cerr << "    func " << jt->first << ":    " << 
				oc << ",  " <<
				ex << ",  " <<
				in << ",  " << endl;
				
		} else {
					
			groupStatistics[(*functiongroups)[jt->first]].occurrences+= oc;
			groupStatistics[(*functiongroups)[jt->first]].exclusiveTime+= ex;
			groupStatistics[(*functiongroups)[jt->first]].inclusiveTime+= in;
		}
	}
	
	/* write functiongroup summary */
	if ( NULL != functiongroups ) {
	
		map<uint32_t,FunctionStatistics>::const_iterator kt= groupStatistics.begin();
		map<uint32_t,FunctionStatistics>::const_iterator ktend= groupStatistics.end();
		
		for( ; kt != ktend; kt++ ) {
		
			uint64_t oc= kt->second.occurrences;
			uint64_t ex= kt->second.exclusiveTime;
			uint64_t in= kt->second.inclusiveTime;
		
			cerr << "    funcgroup" << kt->first << ":    " << 
				oc << ",  " <<
				ex << ",  " <<
				in << ",  " << endl;
		}	
	}
	
	/* write the message summary if any message was sent */
	cerr << "  " << sstatistics.number_sent << " messages sent, " << sstatistics.number_recvd
		<< " messages received, " << sstatistics.bytes_sent <<
		" sent bytes, " << sstatistics.bytes_recvd << " bytes received" << endl;


	/* write file operation statistics */
	map<uint32_t,FileOperationStatistics>::const_iterator itfo;
	map<uint32_t,FileOperationStatistics>::const_iterator itendfo= fostatistics.end();

	if( NULL == filegroups ) {

		/* print out alle statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			const FileOperationStatistics& fost= itfo->second;

			cerr << "  file " << itfo->first << ": " << fost.nopen << " opened files, "
				<< fost.nclose << " closed files, " << fost.nread << " read events, "
				<< fost.nwrite << " write events, " << fost.nseek << " seek events, "
				<< fost.bytesread << " read bytes, " << fost.byteswrite << " written bytes "
				<< endl;
		}
		
	} else {

		map<uint32_t/*groupid*/,FileOperationStatistics> groupstats;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itgr;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itendgr;

		/* calculate group statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			itgr= groupstats.find( (*filegroups)[itfo->first] );

			if( itgr != groupstats.end() ) {

				FileOperationStatistics& fost= itgr->second;
				
				fost.nopen+= itfo->second.nopen;
				fost.nclose+= itfo->second.nclose;
				fost.nread+= itfo->second.nread;
				fost.nwrite+= itfo->second.nwrite;
				fost.nseek+= itfo->second.nseek;
				fost.bytesread+= itfo->second.bytesread;
				fost.byteswrite+= itfo->second.byteswrite;

			} else {

				groupstats.insert( pair<uint32_t,FileOperationStatistics>(
					(*filegroups)[itfo->first],
					FileOperationStatistics( itfo->second.nopen, itfo->second.nclose,
					itfo->second.nread, itfo->second.nwrite, itfo->second.nseek,
					itfo->second.bytesread, itfo->second.byteswrite ) ) );
				
			}
		}


		/* print out all group statistics */
		for( itgr= groupstats.begin(), itendgr= groupstats.end(); itgr != itendgr; ++itgr ) {

			const FileOperationStatistics& fost= itgr->second;

			cerr << "  filegroup " << itgr->first << ": " << fost.nopen << " opened files, "
				<< fost.nclose << " closed files, " << fost.nread << " read events, "
				<< fost.nwrite << " write events, " << fost.nseek << " seek events, "
				<< fost.bytesread << " read bytes, " << fost.byteswrite << " written bytes "
				<< endl;
		}
	}
}


void ProcessState::writeStack( OTF_Writer* writer, uint64_t time, uint32_t processid ) const {

  
	if ( fstack.empty() ) {
	
		OTF_Writer_writeSnapshotComment( writer, 
			time, processid, "empty stack" );
		return;
	}
    
    OTF_KeyValueList *kvlist = OTF_KeyValueList_new();

	deque<FunctionCall>::const_iterator jt= fstack.begin();
	deque<FunctionCall>::const_iterator jtend= fstack.end();

	for ( ; jt != jtend; ++jt ) {

        /* make a copy of the key-value list to keep the data */
        OTF_KeyValueList_appendKeyValueList( kvlist, jt->kvlist );
      
		/* this will reset the key-value list */
		OTF_Writer_writeEnterSnapshotKV( writer, time, 
			jt->time /* uint64_t originaltime */, 
			jt->token /* uint32_t function */, 
			processid /* uint32_t process */, 
			0 /* uint32_t source */,
            kvlist /* key-value list */ );
	}
    
    OTF_KeyValueList_close( kvlist );
}


void ProcessState::writeStatistics( OTF_Writer* writer, uint64_t time,
		uint32_t processid, map< uint32_t,uint32_t> *functiongroups,
		map< uint32_t,uint32_t> *filegroups ) const {


	/* all past function calls are considered in 'fstatistics' already,
	now the time and occurrences of all active functions need to be added.
	one could modify & restore the 'fstatistics' but this seems unsafe. 
	furthermore, this is a 'const' method. therfore we use a temporary
	data structure even though it is kind of overkill :( */

	map<uint32_t,FunctionStatistics> localStatistics;
	map<uint32_t,FunctionStatistics> groupStatistics;

	uint64_t lasttime= time;


	/* cerr << "writeStatistics p" << processid << ", t" << time << endl; */


	deque<FunctionCall>::const_reverse_iterator it= fstack.rbegin();
	deque<FunctionCall>::const_reverse_iterator itend= fstack.rend();
	for ( ; it != itend; ++it ) {

		FunctionStatistics& lstats= localStatistics[ it->token ];
		/* lstats.occurrences++; */
		lstats.exclusiveTime += lasttime - it->time;
		lstats.inclusiveTime += time - it->time;

		assert( lasttime >= it->time );
			
		lasttime= it->time;
	}


	/* actually write statistics */

	map<uint32_t,FunctionStatistics>::const_iterator jt= fstatistics.begin();
	map<uint32_t,FunctionStatistics>::const_iterator jtend= fstatistics.end();

	for ( ; jt != jtend; ++jt ) {

		uint64_t oc= jt->second.occurrences;
		uint64_t ex= jt->second.exclusiveTime;
		uint64_t in= jt->second.inclusiveTime;

		map<uint32_t,FunctionStatistics>::const_iterator kt= localStatistics.find( jt->first );
		map<uint32_t,FunctionStatistics>::const_iterator ktend= localStatistics.end();

		if ( kt != ktend ) {
		
			oc += kt->second.occurrences;
			ex += kt->second.exclusiveTime;
			in += kt->second.inclusiveTime;
		}

		if ( NULL == functiongroups ) {
		
			OTF_Writer_writeFunctionSummary( writer, 
				time /* uint64_t time */, 
				jt->first /* uint32_t function */, 
				processid /* uint32_t process */, 
				oc /* uint64_t count */, 
				ex /* uint64_t excltime */, 
				in /* uint64_t incltime */ );
				
		} else {
					
			groupStatistics[(*functiongroups)[jt->first]].occurrences+= oc;
			groupStatistics[(*functiongroups)[jt->first]].exclusiveTime+= ex;
			groupStatistics[(*functiongroups)[jt->first]].inclusiveTime+= in;
		}
			
	}
	
	/* write functiongroup summary */
	if ( NULL != functiongroups ) {
	
		map<uint32_t,FunctionStatistics>::const_iterator kt= groupStatistics.begin();
		map<uint32_t,FunctionStatistics>::const_iterator ktend= groupStatistics.end();
		
		for( ; kt != ktend; kt++ ) {
		
			uint64_t oc= kt->second.occurrences;
			uint64_t ex= kt->second.exclusiveTime;
			uint64_t in= kt->second.inclusiveTime;
		
			OTF_Writer_writeFunctionGroupSummary( writer, 
				time /* uint64_t time */, 
				kt->first /* uint32_t functiongroup */, 
				processid /* uint32_t process */, 
				oc /* uint64_t count */, 
				ex /* uint64_t excltime */, 
				in /* uint64_t incltime */ );
		}	
	}
	
	/* write the message summary if any message was sent */

	if ( sstatistics.number_sent > 0 || sstatistics.number_recvd > 0) {
	
		OTF_Writer_writeMessageSummary( writer, time /* current time */,
			processid /* id of the process */, 0 /* peer */, 0 /* communicator */,
			0 /* message tag */, sstatistics.number_sent, sstatistics.number_recvd,
			sstatistics.bytes_sent, sstatistics.bytes_recvd );
	}

	/* write the collop summary */
	map<uint32_t,uint32_t>::iterator Iter;

	for(Iter=CollOps.Type2Col.begin(); Iter!=CollOps.Type2Col.end(); ++Iter) {
	     OTF_Writer_writeCollopSummary(writer,time,processid,0,Iter->second,CollOps.numSent[Iter->first],
                 CollOps.numRecv[Iter->first],CollOps.bytesSent[Iter->first],CollOps.bytesRecv[Iter->first]);
	}

	/* write file operation statistics */
	map<uint32_t,FileOperationStatistics>::const_iterator itfo;
	map<uint32_t,FileOperationStatistics>::const_iterator itendfo= fostatistics.end();

	if( NULL == filegroups ) {

		/* print out alle statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			const FileOperationStatistics& fost= itfo->second;

			OTF_Writer_writeFileOperationSummary( writer, time, itfo->first,
				processid, fost.nopen, fost.nclose, fost.nread, fost.nwrite,
				fost.nseek, fost.bytesread, fost.byteswrite );
		}
		
	} else {

		map<uint32_t/*groupid*/,FileOperationStatistics> groupstats;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itgr;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itendgr;

		/* calculate group statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			itgr= groupstats.find( (*filegroups)[itfo->first] );

			if( itgr != groupstats.end() ) {

				FileOperationStatistics& fost= itgr->second;
				
				fost.nopen+= itfo->second.nopen;
				fost.nclose+= itfo->second.nclose;
				fost.nread+= itfo->second.nread;
				fost.nwrite+= itfo->second.nwrite;
				fost.nseek+= itfo->second.nseek;
				fost.bytesread+= itfo->second.bytesread;
				fost.byteswrite+= itfo->second.byteswrite;

			} else {

				groupstats.insert( pair<uint32_t,FileOperationStatistics>(
					(*filegroups)[itfo->first],
					FileOperationStatistics( itfo->second.nopen, itfo->second.nclose,
					itfo->second.nread, itfo->second.nwrite, itfo->second.nseek,
					itfo->second.bytesread, itfo->second.byteswrite ) ) );
				
			}
		}


		/* print out all group statistics */
		for( itgr= groupstats.begin(), itendgr= groupstats.end(); itgr != itendgr; ++itgr ) {

			const FileOperationStatistics& fost= itgr->second;

			OTF_Writer_writeFileGroupOperationSummary( writer, time,
				itgr->first, processid, fost.nopen, fost.nclose, fost.nread,
				fost.nwrite, fost.nseek, fost.bytesread, fost.byteswrite );
		}
	}
}


void ProcessState::writeSends( OTF_Writer* writer, uint64_t time,
		uint32_t processid ) const {


	/*
	if ( sstack.empty() ) {

		OTF_Writer_writeSnapshotComment( writer, 
			time, processid, "no msgs" );

		return;
	}
	*/

    OTF_KeyValueList *kvlist = OTF_KeyValueList_new();
    
	deque<Send>::const_iterator jt= sstack.begin();
	deque<Send>::const_iterator jtend= sstack.end();

	for ( ; jt != jtend; ++jt ) {

        /* make a copy of the key-value list to keep the data */
		OTF_KeyValueList_appendKeyValueList( kvlist, jt->kvlist );
      
        /* this will reset the key-value list */
		OTF_Writer_writeSendSnapshotKV( writer,
			time, /* current time */
			jt->originaltime /* uint64_t originaltime */, 
			processid /* sender */,
			jt->receiver /* receiver */,
			jt->procGroup /* proc group */,
			jt->tag /* message tag */,
            jt->length /* message length */,
			jt->source /* source code location */,
            kvlist /* key-value list */ );
			
	}
    
    OTF_KeyValueList_close( kvlist );
}


void ProcessState::writeOpenFiles( OTF_Writer* writer, uint64_t time,
		uint32_t processid ) const {


	/*
	if( openfiles.empty() ) {

		OTF_Writer_writeSnapshotComment( writer, 
			time, processid, "no openfiles" );

		return;
	}
	*/
    
    OTF_KeyValueList *kvlist = OTF_KeyValueList_new();

	map<uint64_t, FileOpen>::const_iterator it;
	map<uint64_t, FileOpen>::const_iterator itend= openfiles.end();


	for( it= openfiles.begin(); it != itend; ++it ) {

        /* make a copy of the key-value list to keep the data */
        OTF_KeyValueList_appendKeyValueList( kvlist, it->second.kvlist );
      
        /* this will reset the key-value list */
		OTF_Writer_writeOpenFileSnapshotKV( writer,
			time,
			it->second.time,
			it->second.fileid,
			processid,
			it->first,
			it->second.source,
            kvlist );
	}
    
    OTF_KeyValueList_close( kvlist );
}

void ProcessState::writeCollOps( OTF_Writer* writer, uint64_t time,
        uint32_t processid ) const {

          
    OTF_KeyValueList *kvlist = OTF_KeyValueList_new();
    map<uint64_t, BeginCollOperation>::const_iterator it;
          
    
    for ( it = beginCollOps.begin(); it != beginCollOps.end(); ++it ) {

        /* make a copy of the key-value list to keep the data */
        OTF_KeyValueList_appendKeyValueList( kvlist, it->second.kvlist );      
      
        /* this will reset the key-value list */
        OTF_Writer_writeBeginCollopSnapshotKV( writer,
            time, /* current time */
            it->second.time, /* originaltime */
            processid, /* process */
            it->second.col,
            it->first, /* matchingId */
            it->second.procGroup,
            it->second.root,
            it->second.bytesSent,
            it->second.bytesRecv,
            it->second.scltoken,
            kvlist );
            
    }
    
    OTF_KeyValueList_close( kvlist );
    
}

void ProcessState::writeFileOps( OTF_Writer* writer, uint64_t time,
        uint32_t processid ) const {

    
    OTF_KeyValueList *kvlist = OTF_KeyValueList_new();
    map<uint64_t, BeginFileOperation>::const_iterator it;
          
    
    for ( it = beginFileOps.begin(); it != beginFileOps.end(); ++it ) {      
    
        /* make a copy of the key-value list to keep the data */
        OTF_KeyValueList_appendKeyValueList( kvlist, it->second.kvlist );
      
        /* this will reset the key-value list */
        OTF_Writer_writeBeginFileOpSnapshotKV( writer,
            time,
            it->second.time,
            processid,
            it->first,
            it->second.scltoken,
            kvlist );
      
    }
    
    OTF_KeyValueList_close( kvlist );
    
}
         
void State::defProcess( uint32_t processid ) {


	/* explicit creation is not necessary. 
	it would furthermore disturbs selective creation of statistics
	
	processes[ processid ];
	*/
}


void State::defFunction( uint32_t function, uint32_t group ) {


	functiongroups[function]= group;
}


void State::defFile( uint32_t fileid, uint32_t group ) {


	filegroups[fileid]= group;
}

void State::defCollOp( uint32_t col, uint32_t type) {

	Col2Type[col] = type;
}

void State::enterFunction( uint64_t time, uint32_t processid, uint32_t token, OTF_KeyValueList *kvlist ) {


	/* cerr << "  " << time << " enter " << token << " on " << processid << endl; */
	
	processes[ processid ].enterFunction( time, token, kvlist );

}


void State::leaveFunction( uint64_t time, uint32_t processid, uint32_t token ) {


	/* cerr << "  " << time << " leave " << token << " on " << processid << endl; */
	
	processes[ processid ].leaveFunction( time, token );

}


void State::sendMessage( uint64_t time, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source,
    OTF_KeyValueList *kvlist ) {


	processes[ sender ].sendMessage( time, receiver, procGroup, tag, length, source, kvlist );
}


void State::recvMessage( uint32_t sender, uint32_t receiver, uint32_t procGroup,
	uint32_t tag, uint32_t msglength ) {


	processes[ receiver ].recvMessage( msglength );


	/* only touch other process if 'doSnapshots' is set to true */
	if ( doSnapshots ) {

		processes[ sender ].matchMessage( receiver, procGroup, tag  );
	}
}


void State::collOperation( uint64_t time, uint32_t proc, uint32_t root,	uint32_t col,
	uint32_t bytesSent, uint32_t bytesRecv ) {

	uint32_t invoc_sent = 0;
  	uint32_t invoc_recv = 0;

	switch (Col2Type[col])
  	{
    	  case OTF_COLLECTIVE_TYPE_ALL2ONE:
	    if(proc == root) {
	      invoc_sent = 1;
	      invoc_recv = 1;
	    } else {
 	      invoc_sent = 1;
	    }
	    break;
          case OTF_COLLECTIVE_TYPE_ONE2ALL:
	    if(proc == root) {
	      invoc_sent = 1;
	      invoc_recv = 1;
	    } else {
 	      invoc_recv = 1;
	    }
            break;
          case OTF_COLLECTIVE_TYPE_ALL2ALL:
	    invoc_sent = 1;
	    invoc_recv = 1;
            break;
    	  case OTF_COLLECTIVE_TYPE_BARRIER:
	    invoc_sent = 1;
	    break;
 	}	
	
	processes[proc].collOperation( time, col, Col2Type[col], invoc_sent, invoc_recv, bytesSent, bytesRecv );
}


int State::fileOperation( uint64_t time, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t operation, uint64_t bytes, uint64_t duration,
	uint32_t source, OTF_KeyValueList *kvlist ) {

	switch( operation & OTF_FILEOP_BITS ) {

		case OTF_FILEOP_OPEN:
			return processes[ process ].openFile( time, fileid, handleid, source, kvlist );
		case OTF_FILEOP_CLOSE:
			return processes[ process ].closeFile( handleid );
		case OTF_FILEOP_READ:
			return processes[ process ].readFile( fileid, bytes );
		case OTF_FILEOP_WRITE:
			return processes[ process ].writeFile( fileid, bytes );
		case OTF_FILEOP_SEEK:
			return processes[ process ].seekFile( fileid, bytes );
	}

	return OTF_RETURN_OK;
}

int State::beginCollOperation( uint64_t time, uint32_t proc, uint32_t root, uint32_t procGroup,
    uint32_t col, uint64_t matchingId, uint64_t bytesSent, uint64_t bytesRecv, uint32_t scltoken,
    OTF_KeyValueList *kvlist ) {
  
  
    uint64_t invoc_sent = 0;
    uint64_t invoc_recv = 0;
    
    switch (Col2Type[col])
    {
        case OTF_COLLECTIVE_TYPE_ALL2ONE:
          
            if(proc == root) {
                invoc_sent = 1;
                invoc_recv = 1;
            } else {
                invoc_sent = 1;
            }
            break;
            
        case OTF_COLLECTIVE_TYPE_ONE2ALL:
          
            if(proc == root) {
                invoc_sent = 1;
                invoc_recv = 1;
            } else {
                invoc_recv = 1;
            }
            break;
            
        case OTF_COLLECTIVE_TYPE_ALL2ALL:
          
            invoc_sent = 1;
            invoc_recv = 1;
            break;
            
        case OTF_COLLECTIVE_TYPE_BARRIER:
          
            invoc_sent = 1;
            break;
    }
     
    return processes[proc].beginCollOperation( time, root, procGroup, col, Col2Type[col], matchingId, invoc_sent,
                                               invoc_recv, bytesSent, bytesRecv, scltoken, kvlist );
  
}

 int State::endCollOperation( uint64_t time, uint32_t proc, uint64_t matchingId ) {
  
    
    return processes[proc].endCollOperation( time, matchingId );
   
 }
 
 int State::beginFileOperation( uint64_t time, uint32_t process, uint64_t matchingId,
        uint32_t scltoken, OTF_KeyValueList *kvlist ) {
     
      
    return processes[process].beginFileOperation( time, matchingId, scltoken, kvlist );
 }
 
 
  int State::endFileOperation( uint64_t time, uint32_t process, uint32_t fileid,
        uint64_t matchingId, uint64_t handleId, uint32_t operation, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList *kvlist ) {
   
    int ret;
     
    ret = processes[process].endFileOperation( matchingId );
        
    if( ret == OTF_RETURN_ABORT ) {
        return ret; 
    }
    
    switch( operation & OTF_FILEOP_BITS ) {

        case OTF_FILEOP_OPEN:
            return processes[ process ].openFile( time, fileid, handleId, scltoken, kvlist );
        case OTF_FILEOP_CLOSE:
            return processes[ process ].closeFile( handleId );
        case OTF_FILEOP_READ:
            return processes[ process ].readFile( fileid, bytes );
        case OTF_FILEOP_WRITE:
            return processes[ process ].writeFile( fileid, bytes );
        case OTF_FILEOP_SEEK:
            return processes[ process ].seekFile( fileid, bytes );
    }
    
    return OTF_RETURN_OK;
}


void State::printStack() const {


	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	for ( ; it != itend; ++it ) {

		it->second.printStack( it->first );
	}
}


void State::printSends() const {


	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	for ( ; it != itend; ++it ) {

		it->second.printSends( it->first );
	}
}


void State::printOpenFiles() const {


	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	for ( ; it != itend; ++it ) {

		it->second.printOpenFiles( it->first );
	}
}



void State::printStatistics( uint64_t time ) {


	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	
	for ( ; it != itend; ++it ) {

		it->second.printStatistics( it->first, time, false == usefunctiongroups ?
				NULL : &functiongroups, false == usefilegroups ? NULL : &filegroups );
	}
	
}


void State::writeSnapshot( OTF_Writer* writer,
                           uint64_t time ) {


	if ( ! doSnapshots ) return;


	assert( NULL != writer );

	/* cout << " SNAPSHOT " << time << endl; */
	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	for ( ; it != itend; ++it ) {


		it->second.writeStack( writer, time, it->first /* processid */ );
		it->second.writeSends( writer, time, it->first /* processid */ );
		it->second.writeOpenFiles( writer, time, it->first /* processid */ );
        it->second.writeCollOps( writer, time, it->first /* processid */ );
        it->second.writeFileOps( writer, time, it->first /* processid */ );
	}
}


void State::writeStatistics( OTF_Writer* writer,
                             uint64_t time ) {


	if ( ! doStatistics ) return;


	assert( NULL != writer );

	/* cout << " STATISTICS " << time << endl; */
	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	
	for ( ; it != itend; ++it ) {
	
		it->second.writeStatistics( writer, time,
			it->first /* processid */, false == usefunctiongroups ?
			NULL : &functiongroups, false == usefilegroups ? NULL : &filegroups );
	}
}
