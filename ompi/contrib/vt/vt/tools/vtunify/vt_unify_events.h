/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_EVENTS_H_
#define _VT_UNIFY_EVENTS_H_

#include "vt_inttypes.h"

#include <vector>

//
// Events class
//
class Events
{
public:

   // contructor
   Events();

   // destructor
   ~Events();

   bool run();

private:

#ifndef VT_MPI
   void initProgressDisplay( const uint32_t nprogress );
   void finishProgressDisplay();
   void updateProgressDisplay( const uint32_t i, const float progress );

   // vector of progresses
   std::vector<float> m_vecProgress;
#endif // VT_MPI

};

// instance of class Events
extern Events * theEvents;

#endif // _VT_UNIFY_EVENTS_H_
