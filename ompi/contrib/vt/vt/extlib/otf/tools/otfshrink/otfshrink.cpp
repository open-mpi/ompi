/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <iostream>
#include <string>

#include <map>
#include <set>

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>

#include "OTF_Platform.h"

#include "Handler.h"

#define MAX_L	10
#define LIST_MODE 0
#define RANGE_MODE 1
#define TABLE_MODE 2

#define CLEAR_EVERYTHING free_all_pointers(ch_i, first, entries, writer, reader, \
					   handlers, manager, master, new_master);

#define HELPTEXT "" \
"                                                                  \n" \
" otfshrink  -  creates a new otf file that only includes          \n" \
"               specified processes                                \n" \
"                                                                  \n" \
"  options:                                                        \n" \
"      -h, --help    show this help message                        \n" \
"      -V            show OTF version                              \n" \
"      -i <file>     specify the input trace file                  \n" \
"      -o <file>     specify the output file                       \n" \
"      -l \"<list>\"   a space-separated list of processes in quotes \n" \
"                    to enable, i.e. keep in the copy,             \n" \
"                    e.g. '-l \"1 2 3 4 8 5\"'                       \n" \
"      -v            invert setting from '-l',                     \n" \
"                    i.e. deactivate/exclude listed processes      \n" \
"      -m \"<list>\"   map all listed processes to one representative\n" \
"                    and remove all remaining ones                 \n" \
"                    must not be mixed with '-l' and '-v'          \n" \
"      -f <file>     read multiple '-m' lists from the given file  \n" \
"                    one list/group per line, empty lines allowed  \n" \
"      -s <mode>     simulation mode: display all selected         \n" \
"                    processes, no files are created,              \n" \
"                    display modes: (l)ist, (r)ange or (t)able     \n" \
"                    defaut: range                                 \n" \
"                                                                  \n" \
"   Multiple instances of '-l', '-m', and '-f' may be used         \n" \



#include "otfshrink.h"


/* well, we have some global variables */


/* this map contains the specified process IDs which are 
either to keep or to drop depending on 'mode' */
set< uint32_t > cpuMap;
bool inverse= false;


/* map key is the id of the representative, the value set contains all ids of 
processes to be replaced by the representative (including the key) */
map< uint32_t, set< uint32_t > > replacementMap;

#define MODE_DEFAULT    0
#define MODE_NORMAL     1
#define MODE_INVERSE    2
#define MODE_MAP        3

uint32_t mode= MODE_DEFAULT; /* MODE_DEFAULT, MODE_NORMAL, MODE_INVERSE, MODE_MAP */


bool simulation = false;
int sim_mode = RANGE_MODE;
string input_path;
string input_file;
string input_folder;
string output_file;
string output_folder;
string output_path;



int write_master(string input, string output, bool show, int sim_mode);
int display_processes(firstarg *first, int sim_mode);
void free_all_pointers(char *ch_i, firstarg *first, OTF_MapEntry** entries, OTF_Writer *writer,
		      OTF_Reader *reader, OTF_HandlerArray *handlers, OTF_FileManager *manager,
		      OTF_MasterControl *master, OTF_MasterControl *new_master);

int parse_parameters( int argc, char* argv[] );


int parse_replacement_file( const char* filename );
int parse_replacement_line( char* line );
int parse_list_line( char* line );


int parse_replacement_file( const char* filename ) {

    FILE* f= fopen( filename, "r" );
    assert( f );

    /* the commeted-out parts are for using getline which is much safer than fgets.
    Unfortunately, getline is not avail on Mac and whatnot, even though it is in the
    POSIX standard after it started as a GNU extension
    
    Maybe introduce a HAVE_GETLINE autoconf test sometime
    */

    /*
    char* line= NULL;
    size_t len= 0;
    */

    const size_t len= 100000;
    char line[len];

    /*
    while ( -1 != getline( &line, &len, f ) ) {
    */
    while ( NULL != fgets( line, len, f ) ) {

        if ( 0 != parse_replacement_line( line ) ) return 1;
    }

    fclose( f );
    /*
    free( line );
    */

    return 0;
}


int parse_replacement_line( char* line ) {

    char* tmp;
    char* token;
    const char* delim= " {}\t\n";

    token= strtok_r( line, delim, &tmp );
    if ( NULL == token ) return 0 ; // ignore blank lines without an error

    int64_t id= strtoll( token, NULL, 10 );
    if ( 0 >= id ) {

        cerr << "Error: could not parse '" << token << "', abort" << endl;
        return 1;
    }
    set<uint32_t >& s= replacementMap[ id ];

    /* do not add first entry in */

    while ( NULL != ( token= strtok_r( NULL, delim, &tmp ) ) ) {

        id= strtoll( token, NULL, 10 );
        if ( 0 >= id ) {

            cerr << "Error: could not parse '" << token << "', abort" << endl;
            return 1;
        }

        s.insert( id );
    }

    return 0;
}


int parse_list_line( char* line ) {


    char* tmp;
    char* token;
    const char* delim= " {}\t\n";

    token= strtok_r( line, delim, &tmp );
    if ( NULL == token ) return 0 ; // ignore blank lines without an error

    int64_t id= strtoll( token, NULL, 10 );
    if ( 0 >= id ) {
        cerr << "Error: could not parse '" << token << "', abort" << endl;
        return 1;
    }
    cpuMap.insert( id );


    while ( NULL != ( token= strtok_r( NULL, delim, &tmp ) ) ) {

        id= strtoll( token, NULL, 10 );
        if ( 0 >= id ) {

            cerr << "Error: could not parse '" << token << "', abort" << endl;
            return 1;
        }

        cpuMap.insert( id );
    }

    return 0;
}


int parse_parameters( int argc, char* argv[] ) {


	/* check for parameter list */
	for ( int i = 1; i < argc; i++ ) {
		if ( 0 == strcmp("-h", argv[i]) || 0 == strcmp("--help", argv[i]) ) {

			cout << HELPTEXT << endl;
			exit( 0 );
		}
	}

	for ( int i = 1; i < argc; i++ ) {

		if ( 0 == strcmp( "-V", argv[i] ) ) {

			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else if ( 0 == strcmp( "-m", argv[i] ) ) {


            if ( ( i +1 >= argc ) || ( '-' == argv[i+1][0] ) ) {

				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}

            if ( MODE_NORMAL == mode ) {

                cerr << "Error: must not mix '-l' and '-m'" << endl;
                return 1;
            }

            if ( MODE_INVERSE == mode ) {

                cerr << "Error: must not mix '-v' and '-m'" << endl;
                return 1;
            }

            mode= MODE_MAP;
            inverse= true;


            int ret= parse_replacement_line( argv[i+1] );
            if ( 0 != ret ) {
            
                cerr << "Error parsing '"<< argv[i+1] << "'" << endl;
                return 1;
            }

            ++i;

		} else if ( 0 == strcmp( "-f", argv[i] ) ) {


            if ( ( i +1 >= argc ) || ( '-' == argv[i+1][0] ) ) {

				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}

            if ( MODE_NORMAL == mode ) {

                cerr << "Error: must not mix '-l' and '-m'" << endl;
                return 1;
            }

            if ( MODE_INVERSE == mode ) {

                cerr << "Error: must not mix '-v' and '-m'" << endl;
                return 1;
            }

            mode= MODE_MAP;
            inverse= true;


            int ret= parse_replacement_file( argv[i+1] );
            if ( 0 != ret ) {
            
                cerr << "Error parsing '"<< argv[i+1] << "'" << endl;
                return 1;
            }

            ++i;

		} else if ( 0 == strcmp("-l", argv[i]) ) {

            if ( ( i +1 >= argc ) || ( '-' == argv[i+1][0] ) ) {

				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}

            if ( MODE_MAP == mode ) {

                cerr << "Error: must not mix '-m' and '-l'" << endl;
                return 1;
            }

            if ( MODE_DEFAULT == mode ) {
            
                mode= MODE_NORMAL;
            }

            int ret= parse_list_line( argv[i+1] );
            if ( 0 != ret ) {
            
                cerr << "Error parsing '"<< argv[i+1] << "'" << endl;
                return 1;
            }

            ++i;

		} else if ( 0 == strcmp("-v", argv[i]) ) {

                if ( MODE_MAP == mode  ) {

                    cerr << "Error: must not mix '-m' and '-v'" << endl;
                    return 1;
                }

                mode= MODE_INVERSE;
                inverse= true;

		} else if ( 0 == strcmp("-i", argv[i]) ) {

            if ( ( i +1 >= argc ) || ( '-' == argv[i+1][0] ) ) {

				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}
			
			i++;
			input_path = argv[i];

		} else if ( 0 == strcmp("-o", argv[i]) ) {

            if ( ( i +1 >= argc ) || ( '-' == argv[i+1][0] ) ) {

				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}

			i++;
			output_path = argv[i];

		} else if ( 0 == strcmp("-s", argv[i]) ) {
			simulation = true;
			if ( (i+1) < argc ) {
				if ( argv[i+1][0] != '-' ) {
					i++;
					if ( 0 == strcmp("l", argv[i]) || 0 == strcmp("list", argv[i]) ) {
						sim_mode = LIST_MODE;
					} else if ( 0 == strcmp("r", argv[i]) || 0 == strcmp("range", argv[i]) ) {
						sim_mode = RANGE_MODE;
					} else if ( 0 == strcmp("t", argv[i]) || 0 == strcmp("table", argv[i]) ) {
						sim_mode = TABLE_MODE;
					} else {
						cerr << "Error: Wrong argument after " << argv[i-1] << endl;
						return 1;
					}
				}
			}

		} else {
			cerr << "Error: Unknown argument " << argv[i] << endl;
			return 1;
		}

	}

    return 0;
}



int main ( int argc, char* argv[] ) {
	
//	char *pwd = NULL;
	size_t found;

	if ( argc <= 1 ) {
		cout << HELPTEXT << endl;
		return 0;
	}


    int ret= parse_parameters( argc, argv );
    if ( 0 != ret ) return 100;


    map< uint32_t, set< uint32_t > >::const_iterator it= replacementMap.begin();
    map< uint32_t, set< uint32_t > >::const_iterator itend= replacementMap.end();
    for ( ; it != itend ; ++it ) {

        /*cout << "   " << it->first << " : ";*/

        set< uint32_t >::const_iterator jt= it->second.begin();
        set< uint32_t >::const_iterator jtend= it->second.end();

        for ( ; jt != jtend ; ++jt ) {

            /*cout << *jt << " ";*/

            cpuMap.insert( *jt );
        }

        /*cout << endl;*/
    }

	/* string operations to handle input and output path */
	/* check if -i was set */
	if ( input_path.empty() ) {
		cerr << "Error: No input file given." << endl;
		return 101;
	}


#if 0 /* the current working directory seems unnecessary, because we want 
        local links by default, no global paths in links */

	/* get current working directory */
  	pwd = new char[OTF_PATH_MAX];
  	*pwd = '\0';
	pwd = getcwd(pwd, OTF_PATH_MAX);
	if ( pwd == NULL) {
		cerr << "Error: Path length greater than the maximum." << endl;
		delete[] pwd;
		return 102;
	}
	
	/* make absolute path - necessary to create a symbolic link later on */
	if (input_path[0] != '/') {
		input_path = pwd + string("/") + input_path;
	}

	if (output_path[0] != '/') {
		output_path = pwd + string("/") + output_path;
	}

	delete[] pwd;

#endif /* 0 */

	/* input strings */
	/* search for ".otf" and cut it off if found */
	found = input_path.find_last_of(".");
	if (found != string::npos) {
		if ( input_path.substr(found) == ".otf" ) {
			input_path = input_path.erase(found);
		}
	}

	/* divide input path in file and folder */
	found = input_path.find_last_of("/");
	if (found != string::npos) {
		input_folder = input_path.substr(0, found + 1);
		input_file = input_path.substr(found + 1);

	} else {

		// keep input_folder empty
		input_file = input_path;
    }

	/* output strings */
	/* search for ".otf" and cut it off if found */
	found = output_path.find_last_of(".");
	if (found != string::npos) {
		if ( output_path.substr(found) == ".otf" ) {
			output_path = output_path.erase(found);
		}
	}

	/* divide output path in file and folder */
	found = output_path.find_last_of("/");
	if (found != string::npos) {
		output_folder = output_path.substr(0, found + 1);
		output_file = output_path.substr(found + 1);

	} else {

        // keep output_folder empty
        output_file = output_path;
    }

	/* check if output directory exists */
	if ( ! output_folder.empty() && access(output_folder.c_str(), F_OK) ) {

		cerr << "Error: Directory '" << output_folder << "' does not exist!" << endl;
		return 103;
	}

	/* check if input and output path were identical */
	if ( input_path == output_path ) {
		cerr << "Error: The input and output file cannot be indentical." << endl;
		return 104;
	}

	/* make output path if some information are missing */
	if ( output_file.empty() ) {
		if ( output_folder.empty() ) {
			output_path = input_folder + string("s_") + input_file;
		} else {
			output_path = output_folder + string("s_") + input_file;
		}
	}

	/*** end string operations ***/

	/* create symbolic links, definiton file and master file */
	return write_master( input_path, output_path, simulation, sim_mode );
}



int write_master(string input, string output, bool show, int sim_mode) {

	/* create symbolic links, definiton file and master file */

	uint32_t num_args;
	bool append = false;
	char *ch_i = new char [MAX_L + 1];
	firstarg *first = new firstarg;
	uint64_t read;

	string file;
	string s_link;
	
	string file_suffix[4] = {".events", ".snaps", ".stats", ".marker"};

	OTF_MapEntry** entries = NULL;
	OTF_Writer *writer = NULL;
	OTF_Reader *reader = NULL;
	OTF_HandlerArray *handlers = NULL;
	OTF_FileManager *manager = NULL;
	OTF_MasterControl *master = NULL;
	OTF_MasterControl *new_master = NULL;

	manager = OTF_FileManager_open(2);
	master = OTF_MasterControl_new(manager);
	OTF_MasterControl_read(master, input.c_str());

	/* get number of entries in master file, on error abort */
	num_args = OTF_MasterControl_getCount(master);
	if(num_args == 0) {
		if ( access((input + ".otf").c_str(), F_OK) ) {
			cerr << "Error while reading tracefile. File does not exist." << endl;
		} else {
			cerr << "Error while reading tracefile. No entries in file found." << endl;
		}
		CLEAR_EVERYTHING
		return 110;
	}
	
	/* modifies the MapEntries of otf masterfile according to entries in cpuMap*/
	entries= (OTF_MapEntry**) malloc( num_args * sizeof(OTF_MapEntry*) );
    assert( NULL != entries );

	for(uint32_t i = 0; i < num_args; i++) {
		entries[i]= OTF_MasterControl_getEntryByIndex( master, i );
        assert( NULL != entries[i] );

        for(uint32_t j = 0; j < entries[i]->n; j++) {
            if ( ( cpuMap.end() == cpuMap.find( entries[i]->values[j] ) ) == inverse ) {

                /* either ( not_in_list in inverse_mode ) or 
                ( found_in_list in normal_mode ) --> keep entry
                */

                // cerr << "    keep " << entries[i]->values[j] << endl;

            } else {

                /* either ( not_in_list in normal_mode ) or 
                ( found_in_list in inverse_mode ) --> mark to ignore it
                */

                // cerr << "            drop " << entries[i]->values[j] << endl;

                entries[i]->values[j] = 0;
            }
        }
    }

	/* create new empty master and symbolic links */
	new_master = OTF_MasterControl_new(manager);
	for(uint32_t i = 0; i < num_args; i++) {
		append = false;
		for(uint32_t j = 0; j < entries[i]->n; j++) {
			if(entries[i]->values[j] > 0) {

				OTF_MasterControl_append(new_master, entries[i]->argument, entries[i]->values[j]);
//				first->procMap[ entries[i]->values[j] ] = true;
				append = true;
			}
		}

		/* if there is nothing to append or show_mode is active, don't create symbolic link */
		if ( ( ! append) || (show) ) {
			continue;
		}

		/* create symbolic links */
		sprintf(ch_i, "%x", entries[i]->argument );

		for(int k = 0; k < 4; k++) {
			
			file = input + string(".") + ch_i + file_suffix[k] + string(".z");
			s_link = output + string(".") + ch_i + file_suffix[k] + string(".z");

			if ( ! access(file.c_str(), F_OK) ) {
				if ( ! access(s_link.c_str(), F_OK) ) {
					if ( unlink(s_link.c_str()) ) {
						cerr << "Error while removing symbolic link " << s_link << endl;
						CLEAR_EVERYTHING
						return 2;
					}
				}
				if ( symlink( file.c_str(), s_link.c_str() ) ) {
					cerr << "Error while creating symbolic link " << s_link << endl;
					CLEAR_EVERYTHING
					return 2;
				}

			} else {

				file = input + string(".") + ch_i + file_suffix[k];
				s_link = output + string(".") + ch_i + file_suffix[k];

				if ( ! access(file.c_str(), F_OK) ) {
					if ( ! access(s_link.c_str(), F_OK) ) {
						if ( unlink(s_link.c_str()) ) {
							cerr << "Error while removing symbolic link " << s_link << endl;
							CLEAR_EVERYTHING
							return 2;
						}
					}
					if ( symlink( file.c_str(), s_link.c_str() ) ) {
						cerr << "Error while creating symbolic link " << s_link << endl;
						CLEAR_EVERYTHING
						return 2;
					}
				} else {
					if ( k == 0 ) {
						cerr << "Error: Could not find " << file << endl;
						CLEAR_EVERYTHING
						return 2;
					}
				}
			}
		}
	}

	/* close original master, he is not needed anymore */
	OTF_MasterControl_close(master);
	master = NULL;

	if (show) {
		display_processes(first, sim_mode);
		CLEAR_EVERYTHING
		return 0;
	}

	/* check if there is at least one process to show */
	if (OTF_MasterControl_getCount(new_master) < 1) {
		cerr << "You exclude all processes! Master not wrote." << endl;
		CLEAR_EVERYTHING
		return 2;
	}

	writer = OTF_Writer_open(output.c_str(), 0, manager);
	reader = OTF_Reader_open(input.c_str(), manager);
	assert(reader);
	handlers = OTF_HandlerArray_open();

	first->writer = writer;
	OTF_HandlerArray_getCopyHandler( handlers, writer );

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefProcess, OTF_DEFPROCESS_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) first, OTF_DEFPROCESS_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefProcessGroup, OTF_DEFPROCESSGROUP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) first, OTF_DEFPROCESSGROUP_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefProcessSubstitutes, OTF_DEFPROCESSSUBSTITUTES_RECORD);

	read = OTF_Reader_readDefinitions (reader, handlers);
	if( read == OTF_READ_ERROR ) {
		cerr << "An error occurred while reading the tracefile. It seems to be damaged. Abort." << endl;
		CLEAR_EVERYTHING
		return 111;
	}

	/* in mapping mode write additional definitions */


	/* set the writer's master to the modified master instance */
	/* closing the writer at the end writes the new master file to harddisk */
	OTF_Writer_setMasterControl(writer, new_master);

	/* clear everything */
	CLEAR_EVERYTHING

	return 0;
}

int display_processes(firstarg *first, int sim_mode) {

	set<uint32_t>::iterator it;

	/* cout << "Note: You are in simulation mode at the moment. No files are created.\n" << endl; */
	if ( cpuMap.size() < 1) {
		cout << "You exclude all processes!" << endl;

		return 0;
	}

    if ( inverse ) {

        cout << "You choose to disable following processes: " << endl;

    } else {

        cout << "You choose to enable following processes: " << endl;
    }

	if ( sim_mode == LIST_MODE ) {
		for (it = cpuMap.begin(); it != cpuMap.end(); ++it) {
			cout << *it << endl;
		}
	
	} else if ( sim_mode == RANGE_MODE ) {
		it = cpuMap.begin();
		uint32_t cur = *it;
		uint32_t start = cur;
		uint32_t end = 0;
		for (it = ++it ; it != cpuMap.end(); ++it) {
			if ( (cur + 1) == *it ) {
				cur++;
			} else {
				end = cur;
				if (end > start) {
					cout << start << " - " << end << endl;
				} else {
					cout << start << endl;
				}
				cur = *it;
				start = cur;
				end = 0;
			}
		}
		if (end == 0) {
			cout << start << " - " << cur << endl;
		}

	} else if ( sim_mode == TABLE_MODE ) {
		int i = 0;
		for (it = cpuMap.begin(); it != cpuMap.end(); ++it) {
			cout << *it << "\t";
			i++;
			if (i == 8) {
				cout << endl;
				i = 0;
			}
		}
		if ( i > 0) {
			cout << endl;
		}

	}

	return 0;
}

void free_all_pointers(char *ch_i, firstarg *first, OTF_MapEntry** entries, OTF_Writer *writer,
		      OTF_Reader *reader, OTF_HandlerArray *handlers, OTF_FileManager *manager,
		      OTF_MasterControl *master, OTF_MasterControl *new_master) {

	if(ch_i != NULL) {
		delete[] ch_i;
		ch_i = NULL;
	}

	if(first != NULL) {
		delete first;
		first = NULL;
	}

	if(entries != NULL) {
		entries = NULL;
	}

	/* new_master is free'd by writer instance */
	if(writer != NULL) {
		OTF_Writer_close(writer);
		writer = NULL;
		new_master = NULL;
	}

	if(reader != NULL) {
		OTF_Reader_close(reader);
		reader = NULL;
	}


	if(handlers != NULL) {
		OTF_HandlerArray_close(handlers);
		handlers = NULL;
	}

	if(manager != NULL) {
		OTF_FileManager_close(manager);
		manager = NULL;
	}

	if(master != NULL) {
		OTF_MasterControl_close(master);
		master = NULL;
	}

	if(new_master != NULL) {
		OTF_MasterControl_close(new_master);
		new_master = NULL;
	}
}

