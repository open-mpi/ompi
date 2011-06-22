/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <cassert>

#include <iostream>
using namespace std;


#include "Control.h"


Control::Control( OTF_Writer* w, bool _verbose, bool _usefunctiongroups,
		bool _usefilegroups, bool _doSnapshots, bool _doStatistics ) :
		writer( w ), verbose( _verbose ),
		usefunctiongroups( _usefunctiongroups ) {


	nextTime= (uint64_t) -1;
	state= new State( _usefunctiongroups, _usefilegroups, _doSnapshots,
		_doStatistics );
}


Control::~Control() {

    delete state;

}


/** add time stamp where to generate a snapshot */
void Control::addTime( uint64_t time ) {
 
	timestamps.insert( time );
	
	nextTime= *( timestamps.begin() );
}


uint64_t Control::getLastTime() {

    if ( ! timestamps.empty() ) {

        return *( timestamps.rbegin() );

    } else {

        return (uint64_t) -1;
    }
}


double Control::checkTime( uint64_t time ) {


	if ( time >= nextTime ) {
	

		if ( verbose ) {
		
			cout << hex << "[" << time << "]" << endl;
		}
	
		//state->printStatistics( time );
		
		state->writeStatistics( writer, nextTime );

		/* write snapshot only when its not the very end of the trace */
		if ( nextTime != getLastTime() ) {
			

			//state->printStack();
			//state->printSends();
			//state->printOpenFiles();
			state->writeSnapshot( writer, nextTime );
		}


		timestamps.erase( nextTime );

		if ( ! timestamps.empty() ) {

			nextTime= *( timestamps.begin() );

		} else {

			nextTime= (uint64_t) -1;
		}
		
		return (double) nextTime;
	}
	
	return (double) time;
}

