/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <cassert>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "otf.h"
#include "otfaux.h"

#include "process_dispersion.h"
#include "otfprofile.h"


using namespace std;


bool ProcessDispersion( AllData& alldata ) {

    bool error= false;

    /* start runtime measurement for process dispersion information */
    StartMeasurement( alldata, 1, true, "process dispersion information" );
    
    VerbosePrint( alldata, 1, true, "process dispersion information\n" );
    
    if ( 0 == alldata.myRank ) {

        if ( alldata.params.create_tex ) {
            
            map< Pair, FunctionData, ltPair >::const_iterator it= alldata.functionDurationSectionMapGlobal.begin();
            map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.functionDurationSectionMapGlobal.end();

            /*
            cout << " Size of FunctionDurationSectionMapGlobal: " << alldata.functionDurationSectionMapGlobal.size() << endl;
#   define PRINT_MIN_MAX_AVG(v,u) (v.cnt) << " x avg " << ((double)(v.sum))/(v.cnt) << "(" << (v.min) << "-" << (v.max) << ") " << u       
            cout << endl << " global function duration section data per bin: " << endl;
            {
                
                map< Pair, FunctionData, ltPair >::const_iterator it= alldata.functionDurationSectionMapGlobal.begin();
                map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.functionDurationSectionMapGlobal.end();
                
                while ( itend != it ) {
                    
                    cout << "     global function  " << it->first.a << " bin " << it->first.b << " -> ";
                    if ( it->second.count.cnt ) {
                        cout << "\t"<<
                        " cnt: " << PRINT_MIN_MAX_AVG(it->second.count,"[#]") << 
                        " exc: " << PRINT_MIN_MAX_AVG(it->second.excl_time,"[t]") << 
                        " inc: " << PRINT_MIN_MAX_AVG(it->second.incl_time,"[t]") << endl;
                    }
                    
                    it++;
                }
            }
            */
            
            uint64_t funcid= it->first.a;
        
            map< uint64_t, FunctionData >::const_iterator iter_funcMapGlobal;
            iter_funcMapGlobal=alldata.functionMapGlobal.find( funcid );
            assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
        
            uint64_t n= iter_funcMapGlobal->second.count.sum;
        
            uint64_t n_temp= 0;
            uint64_t n_25= n/4;
            uint64_t n_50= n/2;
            uint64_t n_75= (3*n)/4;
        
            double t_min=iter_funcMapGlobal->second.excl_time.min;
            double t_max=iter_funcMapGlobal->second.excl_time.max;
            double t_sum=iter_funcMapGlobal->second.excl_time.sum;
        
            double t_25= 0.0;
            double t_50= 0.0;
            double t_75= 0.0;
        
            for ( ; it != itend; ++it ) {
            
                //cerr << " funcid " << funcid << endl;
            
                if ( funcid != it->first.a ) {
                
                    /*
                     cerr << " function: " << funcid << " , n: " << n << 
                     " , t_sum: " << t_sum << " , t_min: " << t_min << 
                     " , t_25: " << t_25 << " , t_50: " << t_50 << 
                     " , t_75: " << t_75 << " , t_max: " << t_max << endl;
                     */
                
                    alldata.functionDispersionMap[ Pair( (uint64_t)((t_max-t_75)), funcid ) ]
                        = FunctionDispersionData( n, t_sum, t_min, t_25, t_50, t_75, t_max );
                
                    funcid= it->first.a;
                
                    iter_funcMapGlobal=alldata.functionMapGlobal.find( funcid );
                    assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
                    n= iter_funcMapGlobal->second.count.sum;
                
                    n_temp= 0;
                    n_25= n/4;
                    n_50= n/2;
                    n_75= (3*n)/4;
                
                    t_min=iter_funcMapGlobal->second.excl_time.min;
                    t_max=iter_funcMapGlobal->second.excl_time.max;
                    t_sum=iter_funcMapGlobal->second.excl_time.sum;
                    t_25= 0.0;
                    t_50= 0.0;
                    t_75= 0.0;
                
                }
            
                n_temp+= it->second.count.sum;
            
                /* determine lower quartile, median, and upper quartile */
                
                if ( 0.0 == t_75 ) { 
                
                    if ( n_temp >= n_75 ) {
                        t_75=  ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                    }
                
                    if ( 0.0 == t_50 ) {
                    
                        if ( n_temp >= n_50 ) {
                            t_50= ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                        }
                    
                        if ( 0.0 == t_25 ) {
                    
                            if ( n_temp >= n_25 ) {
                                t_25= ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                            }
                        
                        }
                    
                    }
                
                } 
            
            }
    
            alldata.functionDispersionMap[ Pair( (uint64_t)( (t_max-t_75)), funcid ) ]
            = FunctionDispersionData( n, t_sum, t_min, t_25, t_50, t_75, t_max );
        }
        
        if ( alldata.params.create_csv ) {
            
            map< Triple, FunctionData, ltTriple >::const_iterator it= alldata.functionDurationSectionMapPerRank.begin();
            map< Triple, FunctionData, ltTriple >::const_iterator itend= alldata.functionDurationSectionMapPerRank.end();
            
            uint64_t rank= it->first.a;
            uint64_t funcid= it->first.b;
            
            map< uint64_t, FunctionData >::const_iterator iter_funcMapGlobal;
            iter_funcMapGlobal=alldata.functionMapGlobal.find( funcid );
            assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
            
            uint64_t n= iter_funcMapGlobal->second.count.sum;
            
            uint64_t n_temp= 0;
            uint64_t n_25= n/4;
            uint64_t n_50= n/2;
            uint64_t n_75= (3*n)/4;
            
            double t_min=iter_funcMapGlobal->second.excl_time.min;
            double t_max=iter_funcMapGlobal->second.excl_time.max;
            double t_sum=iter_funcMapGlobal->second.excl_time.sum;
            
            double t_25= 0.0;
            double t_50= 0.0;
            double t_75= 0.0;
            
            for ( ; it != itend; ++it ) {
                
                //cerr << " funcid " << funcid << endl;
                
                if ( funcid != it->first.a ) {
                    
                    /*
                     cerr << " function: " << funcid << " , n: " << n << 
                     " , t_sum: " << t_sum << " , t_min: " << t_min << 
                     " , t_25: " << t_25 << " , t_50: " << t_50 << 
                     " , t_75: " << t_75 << " , t_max: " << t_max << endl;
                     */
                    
                    alldata.functionDispersionMapPerRank[ Triple( (uint64_t)((t_max/t_75)*100), funcid, rank ) ]
                    = FunctionDispersionData( n, t_sum, t_min, t_25, t_50, t_75, t_max );
                    
                    rank= it->first.a;
                    funcid= it->first.b;
                    
                    iter_funcMapGlobal=alldata.functionMapGlobal.find( funcid );
                    assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
                    n= iter_funcMapGlobal->second.count.sum;
                    
                    n_temp= 0;
                    n_25= n/4;
                    n_50= n/2;
                    n_75= (3*n)/4;
                    
                    t_min=iter_funcMapGlobal->second.excl_time.min;
                    t_max=iter_funcMapGlobal->second.excl_time.max;
                    t_sum=iter_funcMapGlobal->second.excl_time.sum;
                    t_25= 0.0;
                    t_50= 0.0;
                    t_75= 0.0;
                    
                }
                
                n_temp+= it->second.count.sum;
                
                /* determine lower quartile, median, and upper quartile */
                
                if ( 0.0 == t_75 ) { 
                    
                    if ( n_temp >= n_75 ) {
                        t_75=  ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                    }
                    
                    if ( 0.0 == t_50 ) {
                        
                        if ( n_temp >= n_50 ) {
                            t_50= ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                        }
                        
                        if ( 0.0 == t_25 ) {
                            
                            if ( n_temp >= n_25 ) {
                                t_25= ( it->second.excl_time.max - it->second.excl_time.min ) / 2 + it->second.excl_time.min ;
                            }
                            
                        }
                        
                    }
                    
                } 
                
            }
            
            alldata.functionDispersionMapPerRank[ Triple( (uint64_t)((t_max/t_75)*100), funcid, rank ) ]
            = FunctionDispersionData( n, t_sum, t_min, t_25, t_50, t_75, t_max );
            
        }

        /*        
        cout << " Size of FunctionDispersionMap: " << alldata.functionDispersionMap.size() << endl; 
        
        cout << endl << " global function dispersion: " << endl;
        {
            
            map< Pair, FunctionDispersionData, gtPair >::const_iterator it= alldata.functionDispersionMap.begin();
            map< Pair, FunctionDispersionData, gtPair >::const_iterator itend= alldata.functionDispersionMap.end();
            
            while ( itend != it ) {
                
                cout << "     dispersion  " << it->first.a << " global function " << it->first.b << " -> ";
                
                if ( it->second.count ) {
                    cout << "\t" <<
                    " tmin: " << it->second.excl_time_minimum <<
                    "\t t_25: " << it->second.excl_time_low_quartile <<
                    "\t tmed: " << it->second.excl_time_median <<
                    "\t t_75: " << it->second.excl_time_top_quartile <<
                    "\t tmax: " << it->second.excl_time_maximum <<
                    "\t tavg: " << it->second.excl_time_sum / it->second.count << endl;
                }
                
                it++;
            }
            
        }
        */
    }
    //alldata.functionDurationSectionMapGlobal.clear();
    
    if ( !error ) {
        
        /* stop runtime measurement for process data */
        StopMeasurement( alldata, true, "process dispersion information" );
        
    }
    
    return !error;
}
