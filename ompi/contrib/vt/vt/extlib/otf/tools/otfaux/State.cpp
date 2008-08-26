/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "State.h"


#include <iostream>
#include <cassert>
using namespace std;


/* *** ProcessState *** ********************************* */


void ProcessState::enterFunction( uint64_t time, uint32_t token ) {



	fstack.push_back( FunctionCall( time, token) );
	
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


void ProcessState::sendMessage( uint64_t time, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t msglength, uint32_t source ) {
	
	
	sstatistics.bytes_sent+= msglength;
	sstatistics.number_sent++;

	
	
	sstack.push_back( Send( time, receiver, procGroup, tag, source ) );
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
	uint32_t source ) {


	std::map<uint64_t, OpenFile>::iterator it;

	it= openfiles.find( handleid );

	if( it == openfiles.end() ) {

		/* insert the file into the list of opened files */
		openfiles.insert( pair<uint64_t,OpenFile>( handleid,
			OpenFile( time, fileid, source ) ) );

		
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
	map<uint64_t/*handleid*/, OpenFile>::iterator it;

	it= openfiles.find( handleid );
	
	if( it != openfiles.end() ) {

		/* make the statistics */
		map<uint32_t,FileOperationStatistics>::iterator it2;
		
		it2= fostatistics.find( it->second.fileid );	
	
		if( it2 != fostatistics.end() ) {
	
			FileOperationStatistics& fos= it2->second;
	
			++fos.nclose;

			ret= OTF_RETURN_OK;
	
		} else {
		
	#		ifdef OTF_VERBOSE
				fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"Trying to close not yet opened file. aborting\n",
					__FUNCTION__, __FILE__, __LINE__ );
	#		endif
	
			ret= OTF_RETURN_ABORT;
		}
		
		/* erase the file from the opened files list */
		openfiles.erase( it );
	
	} else {

#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"Trying to close not yet opened file. aborting\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		ret= OTF_RETURN_ABORT;

	}

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

	map<uint64_t/*handleid*/, OpenFile>::const_iterator it;
	map<uint64_t/*handleid*/, OpenFile>::const_iterator itend= openfiles.end();


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

	deque<FunctionCall>::const_iterator jt= fstack.begin();
	deque<FunctionCall>::const_iterator jtend= fstack.end();

	for ( ; jt != jtend; ++jt ) {

		/* cout << "     " << jt->time << ":    " << jt->token << endl; */
			
		OTF_Writer_writeEnterSnapshot( writer, time, 
			jt->time /* uint64_t originaltime */, 
			jt->token /* uint32_t function */, 
			processid /* uint32_t process */, 
			0 /* uint32_t source */ );
	}
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

	deque<Send>::const_iterator jt= sstack.begin();
	deque<Send>::const_iterator jtend= sstack.end();

	for ( ; jt != jtend; ++jt ) {

			
		OTF_Writer_writeSendSnapshot( writer,
			time, /* current time */
			jt->originaltime /* uint64_t originaltime */, 
			processid /* sender */,
			jt->receiver /* receiver */,
			jt->procGroup /* proc group */,
			jt->tag /* message tag */,
			jt->source /* source code location */ );
			
	}
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


	map<uint64_t, OpenFile>::const_iterator it;
	map<uint64_t, OpenFile>::const_iterator itend= openfiles.end();


	for( it= openfiles.begin(); it != itend; ++it ) {

		OTF_Writer_writeOpenFileSnapshot( writer,
			time,
			it->second.time,
			it->second.fileid,
			processid,
			it->first,
			it->second.source );
	}
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


void State::enterFunction( uint64_t time, uint32_t processid, uint32_t token ) {


	/* cerr << "  " << time << " enter " << token << " on " << processid << endl; */
	
	processes[ processid ].enterFunction( time, token );

}


void State::leaveFunction( uint64_t time, uint32_t processid, uint32_t token ) {


	/* cerr << "  " << time << " leave " << token << " on " << processid << endl; */
	
	processes[ processid ].leaveFunction( time, token );

}


void State::sendMessage( uint64_t time, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source ) {


	processes[ sender ].sendMessage( time, receiver, procGroup, tag, length, source );
}


void State::recvMessage( uint32_t sender, uint32_t receiver, uint32_t procGroup,
	uint32_t tag, uint32_t msglength ) {


	processes[ receiver ].recvMessage( msglength );


	/* only touch other process if 'doSnapshots' is set to true */
	if ( doSnapshots ) {

		processes[ sender ].matchMessage( receiver, procGroup, tag  );
	}
}


int State::fileOperation( uint64_t time, uint32_t fileid, uint32_t process,
	uint64_t handleid, uint32_t operation, uint64_t bytes, uint64_t duration,
	uint32_t source ) {


	switch( operation ) {

		case OTF_FILEOP_OPEN:
			return processes[ process ].openFile( time, fileid, handleid, source );
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


void State::writeSnapshot( OTF_Writer* writer, uint64_t time ) const {


	if ( ! doSnapshots ) return;


	assert( NULL != writer );

	/* cout << " SNAPSHOT " << time << endl; */
	map<uint32_t,ProcessState>::const_iterator it= processes.begin();
	map<uint32_t,ProcessState>::const_iterator itend= processes.end();
	for ( ; it != itend; ++it ) {


		it->second.writeStack( writer, time, it->first /* processid */ );
		it->second.writeSends( writer, time, it->first /* processid */ );
		it->second.writeOpenFiles( writer, time, it->first /* processid */ );
	}
}


void State::writeStatistics( OTF_Writer* writer, uint64_t time ) {


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
