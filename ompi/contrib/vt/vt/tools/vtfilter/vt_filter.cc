/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_filter.h"
using namespace std;

#include <iostream>


Filter::Filter()
	: maxStackDepth(0), totalInvocations(0), maxInvocations(0),
	timerResolution(0), messageCount(0), collectiveCount(0) {
}


void Filter::setTimerResolution( uint64_t tickspersecond ) {


	timerResolution= tickspersecond;
}


void Filter::addFunction( uint32_t func, const std::string& name ) {


	functions.insert( pair<uint32_t, Function>(func, Function( func, name ) ) );
}


void Filter::addEnter( uint32_t func, uint32_t process, uint64_t time ) {


	/* add the new process if it does not already exist */
	stack<StackItem>& rstack= callStack.insert(
		pair<uint32_t, stack<StackItem> >( process, stack<StackItem>() ) ).first->second;


	/* add the new function if it does not already exist - should never happen */
	map<uint32_t, Function>::iterator itfunc= functions.insert(
		pair<uint32_t, Function>(func, Function( func, "_undefined" ) ) ).first;


	/* increase number of invocations */
	++(itfunc->second.invocations);

	
	/* add the function to the parent */
	if( false == rstack.empty() ) {

		/* add the function to the subfunctionlist, if it does not already exist */
		rstack.top().it->second.subFuncs.insert( func );

		rstack.top().it->second.accDurationExcl+= time;
	}

	/* push the callstack */
	rstack.push( StackItem( itfunc ) );
	rstack.top().it->second.accDurationIncl-= time;
	rstack.top().it->second.accDurationExcl-= time;
}


void Filter::addLeave( uint32_t process, uint64_t time ) {


	/* add the new process if it does not already exist */
	stack<StackItem>& rstack= callStack.insert(
		pair<uint32_t, stack<StackItem> >( process, stack<StackItem>() ) ).first->second;
	
	rstack.top().it->second.accDurationIncl+= time;
	rstack.top().it->second.accDurationExcl+= time;
	
	rstack.pop();

	if( false == rstack.empty() ) {

		rstack.top().it->second.accDurationExcl-= time;
	}
}


void Filter::postProcessing() {


	std::map<uint32_t, Function>::iterator itf;
	for( itf= functions.begin(); itf != functions.end(); ++itf ) {

		/* calculate max subcalls */
		if ( itf->second.subFuncs.size() > maxInvocations ) {

			maxInvocations= itf->second.subFuncs.size();
		}
	
		/* count total invocations */
		totalInvocations+= itf->second.invocations;


		/* calculate the stack depth - without recursion!!! */
		stack<PostStackItem> stack;
		stack.push( PostStackItem( itf->second.id, set<uint32_t>() ) );

		/*cerr << "visit: " << itf->second.id << "  " << itf->second.name
			<< " subcalls: " << itf->second.invocations << " subfunctions: " << itf->second.subFuncs.size()<< endl;*/

		itf->second.depth= visitFunction( stack, NULL, NULL, NULL ) - 1;

		if( itf->second.depth > maxStackDepth ) {
			maxStackDepth= itf->second.depth;
		}
	}
}


vector<Function> Filter::getFunctions() const {


	vector<Function> ret;
	map<uint32_t, Function>::const_iterator it;

	for( it= functions.begin(); it != functions.end(); ++it ) {

		ret.push_back( it->second );
	}

	sort( ret.begin(), ret.end() );

	return ret;
}


set<uint32_t> Filter::reduceTo( float* percent, const set<uint32_t>& excludes,
	const set<uint32_t>& includes, bool includechildren, uint64_t limit ) {

	vector<Function> funcs= getFunctions();
	set<uint32_t> killed;
	set<uint32_t> nokill;
	bool killedsomething= true;



	double msgf= 1.0;
	double collf= 1.0;
	
	uint64_t allrecords= (uint64_t) ( (double) getTotalInvocations() +
		msgf * (double) getMessageCount() +
		collf * (double) getCollectiveCount() );

	/*cerr << "invocations: " << getTotalInvocations()
		<< "messages: " << getMessageCount()
		<< "collectives: " << getCollectiveCount() << endl;
	*/

	int64_t invocationstokill= (int64_t)allrecords - 
		((int64_t) ((((double)allrecords * (*percent)) / 100.0) + 0.5));


	
	vector<Function>::iterator itfuncs;
	set<uint32_t>::const_iterator itkilled;

	
	/* add all excludes and their parents recrusively to the nokill-set
	this is important for the includes ( otherwise it is not required, because
	functions with children will not be filtered anyways ) */
	for( itfuncs= funcs.begin(); itfuncs != funcs.end(); ++itfuncs ) {

		stack<PostStackItem> stack;
		stack.push( PostStackItem( itfuncs->id, set<uint32_t>() ) );
		
		/* recursively visit all functions */
		visitFunctionExclude( stack, nokill );
		
		/* if the function should be excluded, do it */
		if( excludes.find( itfuncs->id ) != excludes.end() ) {

			nokill.insert( itfuncs->id );
		}
	}
	

	/* add all includes (and maybe their children recursively) to the killed-set
	*/
	for( itfuncs= funcs.begin(); itfuncs != funcs.end(); ++itfuncs ) {

		/* if the function should not be excluded and should be included */
		if( nokill.find( itfuncs->id ) == nokill.end() &&
			includes.find( itfuncs->id ) != includes.end() ) {

			/* is recursion enabled? */
			if( true == includechildren ) {

				stack<PostStackItem> stack;
				stack.push( PostStackItem( itfuncs->id, set<uint32_t>() ) );
				
				uint64_t killedinvocations= 0;
				visitFunction( stack, &killed, &killedinvocations, &nokill );
				invocationstokill-= killedinvocations;
			}
			
			/* kill this function if it is not already done */
			if( killed.find( itfuncs->id ) == killed.end() ) {
				killed.insert( itfuncs->id );

				/* If the limit is smaller than the invocationcount,
				subtract the invocationcount and add the limit to "invocationstokill" */
				if( limit < itfuncs->invocations ) {
					invocationstokill-= itfuncs->invocations;
					invocationstokill+= limit;
				}
				/* else: do nothing, because limit >= invocationcount */

			}
			
		}
	}
	

	uint64_t candidateinvocations= 0;
	uint32_t candidateid= 0;


	/* normal kill decision loop */
	while( invocationstokill > 0 && false != killedsomething ) {

		killedsomething= false;


		/* iterate through all functions */
		for( itfuncs= funcs.begin(); itfuncs != funcs.end(); ++itfuncs ) {

			/* delete all references to killed functions */
			for( itkilled= killed.begin(); itkilled != killed.end(); ++itkilled ) {

				itfuncs->subFuncs.erase( *itkilled );
			}

			/* are there children? yes if: set-size == 0,
			recursion does not count */
			bool nochildren= true;
			set<uint32_t>::const_iterator itfid;
			for( itfid= itfuncs->subFuncs.begin(); itfid != itfuncs->subFuncs.end(); ++itfid ) {
				if( *itfid != itfuncs->id ) nochildren= false;
			}


			/* strategy 2 - if we are right over the invocationcount
			the function stored in "candidate*" is the right one to kill */
			if( (int64_t)itfuncs->invocations < invocationstokill  &&
				0 != candidateid ) {

				killed.insert( candidateid );
				
				invocationstokill-= candidateinvocations;
				invocationstokill+= limit;

				killedsomething= true;
				candidateid= 0;

				break;
			}
			
			
			/* if the function is not excluded and is not killed */
			if( nokill.find( itfuncs->id ) == nokill.end() &&
				killed.find( itfuncs->id ) == killed.end() ) {

				/* strategy 2 - save this function as a candidate, if
				   it has more invocations than the limit is */

				if( true == nochildren && itfuncs->invocations > limit ) {
					candidateinvocations= itfuncs->invocations;
					candidateid= itfuncs->id;
				}

			}
			
		}

		/* sort newly because the references might have changed, thus the order too */
		sort( funcs.begin(), funcs.end() );
	}


	/* calculate the percentage of remaining events */
	*percent= (float)
		((double)( (((int64_t) ((((double)allrecords * (*percent)) / 100.0) + 0.5))
			+ invocationstokill) * 100 )  / (double)allrecords);


	return killed;
}


void Filter::operator+=( const Filter& filter ) {

	if( maxStackDepth < filter.getMaxStackDepth() ) maxStackDepth = filter.getMaxStackDepth();
	totalInvocations += filter.getTotalInvocations();
	if( maxInvocations < filter.getMaxInvocations() ) maxInvocations = filter.getMaxInvocations();
	messageCount += filter.getMessageCount();
	collectiveCount += filter.getCollectiveCount();
	

	if( timerResolution != 0 && filter.getTimerResolution() != 0
		&& timerResolution != filter.getTimerResolution() ) {

		cerr << "Multiple timerresolutions found. aborting" << endl;
		abort();
		
	} else if ( timerResolution == 0 && filter.getTimerResolution() != 0 ) {
		timerResolution = filter.getTimerResolution();
	}

	/* merge the functions i.e. accumulate all stats, compare max values .... */
	map<uint32_t, Function>::const_iterator itf;
	map<uint32_t, Function>::iterator itf2;
	for( itf = filter.getFunctionMap().begin(); itf !=  filter.getFunctionMap().end(); ++itf ) {
		itf2 = functions.find( itf->first );
		if( itf2 != functions.end() ) {
			itf2->second += itf->second;
		} else {
			functions.insert(pair<uint32_t,Function>(itf->first, itf->second));
		}
	}
}



/* *** protected *** */
uint32_t Filter::visitFunction( stack<PostStackItem>& stackx, set<uint32_t>* killed,
	uint64_t* killedinvocations, const set<uint32_t>* nokill ) {


	map<uint32_t, Function>::const_iterator itf= functions.find( stackx.top().id );
	uint32_t ret= stackx.size();
	

	/* kill the function */
	if( NULL != killed &&
		NULL != nokill &&
		nokill->find( itf->second.id ) == nokill->end() &&
		killed->find( itf->second.id ) == killed->end() ) {

		killed->insert( itf->second.id );

		if( NULL != killedinvocations ) {

			(*killedinvocations)+= itf->second.invocations;
		}
		
	} else if ( NULL != killed &&
		NULL != nokill &&
		( nokill->find( itf->second.id ) != nokill->end() ||
		killed->find( itf->second.id ) != killed->end() ) ) {

		return ret;
	}



	/* recursively visit the functions with having an eye on the stack */
	uint32_t tmpret;
	set<uint32_t>::const_iterator its;
	
	for( its= itf->second.subFuncs.begin(); its != itf->second.subFuncs.end(); ++its ) {

		if( stackx.size() == stackx.top().visited.size() &&
			stackx.top().visited.end() == stackx.top().visited.find(*its) ) {
			/* if we are still not finished & we didn´t visit this function yet */

			stackx.push( PostStackItem( *its, stackx.top().visited ) );
			

			tmpret= visitFunction( stackx, killed, killedinvocations, nokill );
			if( tmpret > ret ) ret= tmpret;
			

			stackx.pop();
		}
	}

	return ret;
}


void Filter::visitFunctionExclude( stack<PostStackItem>& stackx, set<uint32_t>& nokill ) {



	/* this function is an exclude - store the complete stack into the nokill-set */
	if( nokill.find( stackx.top().id ) != nokill.end() ) {

		stack<PostStackItem> stackcopy= stackx;

		stackcopy.pop();

		while( false == stackcopy.empty() ) {

			nokill.insert( stackcopy.top().id );

			stackcopy.pop();
		}
	}

	
	/* recursively visit the functions with having an eye on the stack */
	map<uint32_t, Function>::const_iterator itf= functions.find( stackx.top().id );
	set<uint32_t>::const_iterator its;
	
	for( its= itf->second.subFuncs.begin(); its != itf->second.subFuncs.end(); ++its ) {

		if( stackx.size() == stackx.top().visited.size() &&
			stackx.top().visited.end() == stackx.top().visited.find(*its) ) {
			/* if we are still not finished & we didn´t visit this function yet */

			stackx.push( PostStackItem( *its, stackx.top().visited ) );

			visitFunctionExclude( stackx, nokill );

			stackx.pop();
		}
	}
}
