/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef PRODTEX_H
#define PRODTEX_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <algorithm>
#include <cmath>
#include <map>
#if defined(HAVE_UNISTD_H) && HAVE_UNISTD_H
#	include <unistd.h>
#endif

#include "OTF_inttypes.h"

#include "Handler.h"


using namespace std;

typedef map<uint32_t,double> CountMap2;

struct func_temp
{
	uint32_t func_id;
	uint64_t invoc;
	double incl_time;
	CountMap2 count_map2;
};

struct count_temp
{
	double min;
	double max;
};

int prod_tex(int tex, global_data* gd_ptr, vector<string> counter_names, bool sum);

#endif /* PRODTEX_H */
