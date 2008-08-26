/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef CONTROL_H
#define CONTROL_H

#include "config.h"

#include <set>

#include "OTF_inttypes.h"
#include "otf.h"

#include "State.h"


struct Control {


	State *state;

	/* time stamps where to generate a snapshots */
	std::set<uint64_t> timestamps;

	uint64_t nextTime;

	OTF_Writer* writer;
	
	bool verbose;
	
	int usefunctiongroups;


	Control( OTF_Writer* writer= NULL, bool _verbose= false, 
		bool _usefunctiongroups= false, bool _usefilegroups= false,
		bool _doSnapshots= true, bool _doStatistics= true );
	~Control();

	/** add time stamp where to generate a snapshot */
	void addTime( uint64_t time );
	
	uint64_t getLastTime();

	double checkTime( uint64_t time );
};


#endif /* CONTROL_H */

