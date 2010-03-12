/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <iostream>
#include <string>

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

#include "OTF_Platform.h"

#include "Handler.h"

#define MAX_L	4
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
"      -i <file>     specify the input trace file                  \n" \
"      -o <file>     specify the output file                       \n" \
"      -l <list>     a space-separated list of processes           \n" \
"                    to show,                                      \n" \
"                    e.g. -l 1 2 3-4 8-5                           \n" \
"      -v <list>     a space-separated list of processes           \n" \
"                    NOT to show,                                  \n" \
"                    see -l for exact syntax                       \n" \
"      -s <mode>     display all selected processes,               \n" \
"                    no files are created (simulation mode),       \n" \
"                    modes: (l)ist, (r)ange or (t)able             \n" \
"                    defaut: range                                 \n" \
"                                                                  \n" \

map<int, bool> cpuMap;

int write_master(string input, string output, bool invers, bool show, int sim_mode);
int display_processes(firstarg *first, int sim_mode);
void free_all_pointers(char *ch_i, firstarg *first, OTF_MapEntry *entries, OTF_Writer *writer,
		      OTF_Reader *reader, OTF_HandlerArray *handlers, OTF_FileManager *manager,
		      OTF_MasterControl *master, OTF_MasterControl *new_master);

int main (int argc, char* argv[]) {
	
	char *pwd = NULL;
	bool enable = true;
	bool invers_mode = true;
	bool mode_set = false;
	bool simulation = false;
	int sim_mode = RANGE_MODE;
	size_t found;

	string arg = "-l";
	string input_path;
	string input_file;
	string input_folder;
	string output_file;
	string output_folder;
	string output_path;

	if ( argc <= 1 ) {
		cout << HELPTEXT << endl;
		return 0;
	}

	/* check for parameter list */
	for ( int i = 1; i < argc; i++ ) {
		if ( 0 == strcmp("-h", argv[i]) || 0 == strcmp("--help", argv[i]) ) {
			cout << HELPTEXT << endl;
			return 0;

		} else if ( 0 == strcmp("-l", argv[i]) ) {
			bool is_hyphen = false;
			char min[MAX_L + 1];
			char max[MAX_L + 1];
			int count;
			int left;
			int right;

			if ( ((i+1) >= argc) || ( argv[i+1][0] == '-') ) {
				cerr << "At least one argument expected after " << arg << endl;
				return 1;
			}

			while ( (i+1) < argc ) {
				if ( argv[i+1][0] == '-' ) break;
				i++;

				memset(max, '\0', MAX_L + 1);
				memset(min, '\0', MAX_L + 1);
				count = 0;
				is_hyphen = false;
				for (uint32_t j = 0; j < strlen(argv[i]); j++) {
					if ( argv[i][j] > 47 && argv[i][j] < 58 ) {
						if (count >= MAX_L ) {
							cerr << "Error: CPU can be " << MAX_L << "-digit at most." << endl;
							return 1;
						}
						if ( is_hyphen ) {
							max[count] =argv[i][j];
						} else {
							min[count] =argv[i][j];
						}
						count++;

					} else if ( (argv[i][j] == '-') && (is_hyphen == false) && (argv[i][j+1] != '\0') ) {
						is_hyphen = true;
						count = 0;

					} else {
						cerr << "Error: Wrong argument after " << arg << endl;
						return 1;
					}
				}

				left = atoi(min);
				right = atoi(max);

				if ( ! is_hyphen ) {
					right = left;
				}

				if ( left > right ) {
					int tmp = left;
					left = right;
					right = tmp;
				}

				for ( int k = left; k <= right; k++ ) {
					cpuMap[k] = enable;
				}
			}

			enable = true;
			arg = "-l";
			if ( ! mode_set ) {
				invers_mode = false;
				mode_set = true;
			}

		} else if ( 0 == strcmp("-v", argv[i]) ) {
			enable = false;
			if ( ! mode_set ) {
				invers_mode = true;
				mode_set = true;
			}
			arg = "-v";
			strcpy(argv[i], "-l");
			i--;

		} else if ( 0 == strcmp("-i", argv[i]) ) {
			if (i+1 >= argc) {
				cerr << "Error: Option " << argv[i] << " expect exactly 1 argument." << endl;
				return 1;
			}
			if ( argv[i+1][0] == '-' ) {
				cerr << "Error: No argument given after " << argv[i] << endl;
				return 1;
			}
			
			i++;
			input_path = argv[i];

		} else if ( 0 == strcmp("-o", argv[i]) ) {
			if (i+1 >= argc) {
				cerr << "Error: Option " << argv[i] << " expect exactly 1 argument." << endl;
				return 1;
			}
			if ( argv[i+1][0] == '-' ) {
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

	/* string operations to handle input and output path */
	/* check if -i was set */
	if ( input_path.empty() ) {
		cerr << "Error: No input file given." << endl;
		return 1;
	}

	/* get current working directory */
  	pwd = new char[OTF_PATH_MAX];
  	*pwd = '\0';
	pwd = getcwd(pwd, OTF_PATH_MAX);
	if ( pwd == NULL) {
		cerr << "Error: Path length greater than the maximum." << endl;
		delete[] pwd;
		return 1;
	}
	
	/* make absolute path - necessary to create a symbolic link later on */
	if (input_path[0] != '/') {
		input_path = pwd + string("/") + input_path;
	}

	if (output_path[0] != '/') {
		output_path = pwd + string("/") + output_path;
	}

	delete[] pwd;

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
	}

	/* check if output directory exists */
	if ( access(output_folder.c_str(), F_OK) ) {
		cerr << "Error: Directory " << output_folder << " does not exist!" << endl;
		return 1;
	}

	/* check if input and output path were identical */
	if ( input_path == output_path ) {
		cerr << "Error: The input and output file cannot be indentical." << endl;
		return 1;
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
	return write_master(input_path ,output_path, invers_mode, simulation, sim_mode);
}

int write_master(string input, string output, bool invers, bool show, int sim_mode) {

	/* create symbolic links, definiton file and master file */

	uint32_t num_args;
	bool append = false;
	char *ch_i = new char [MAX_L + 1];
	firstarg *first = new firstarg;
	uint64_t read;

	string file;
	string s_link;
	
	string file_suffix[4] = {".events", ".snaps", ".stats", ".marker"};

	OTF_MapEntry *entries = NULL;
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
		return 2;
	}
	
	/* modifies the MapEntries of otf masterfile according to entries in cpuMap*/
	entries = new OTF_MapEntry[num_args];
	for(uint32_t i = 0; i < num_args; i++) {
		entries[i] = *(OTF_MasterControl_getEntry(master, i+1));
		
		for(uint32_t j = 0; j < entries[i].n; j++) {
			if (  cpuMap.end() == cpuMap.find(entries[i].values[j]) ) {
				if ( ! invers ) {
					entries[i].values[j] = 0;
				}
			} else {
				if ( cpuMap[entries[i].values[j]] == 0 ) {
					entries[i].values[j] = 0;
				}
			}
		}
	}

	/* create new empty master and symbolic links */
	new_master = OTF_MasterControl_new(manager);
	for(uint32_t i = 0; i < num_args; i++) {
		append = false;
		for(uint32_t j = 0; j < entries[i].n; j++) {
			if(entries[i].values[j] > 0) {
				OTF_MasterControl_append(new_master, i+1, entries[i].values[j]);
				first->procMap[ entries[i].values[j] ] = true;
				append = true;
			}
		}

		/* if there is nothing to append or show_mode is active, don't create symbolic link */
		if ( ( ! append) || (show) ) {
			continue;
		}

		/* create symbolic links */
		snprintf(ch_i, MAX_L, "%x", i+1);
		
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

	/* kind of copyhandler which replicate all definitons */
	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefinitionComment, OTF_DEFINITIONCOMMENT_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFINITIONCOMMENT_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefTimerResolution, OTF_DEFTIMERRESOLUTION_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFTIMERRESOLUTION_RECORD);
	
	/* this definition is not copied but modified */
	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefProcess, OTF_DEFPROCESS_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) first, OTF_DEFPROCESS_RECORD);

	/* this definition is not copied but modified */
	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefProcessGroup, OTF_DEFPROCESSGROUP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) first, OTF_DEFPROCESSGROUP_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefFunction, OTF_DEFFUNCTION_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFFUNCTION_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefFunctionGroup, OTF_DEFFUNCTIONGROUP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFFUNCTIONGROUP_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefCollectiveOperation, OTF_DEFCOLLOP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFCOLLOP_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefCounter, OTF_DEFCOUNTER_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFCOUNTER_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefCounterGroup, OTF_DEFCOUNTERGROUP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFCOUNTERGROUP_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefScl, OTF_DEFSCL_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFSCL_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefSclFile, OTF_DEFSCLFILE_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFSCLFILE_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefCreator, OTF_DEFCREATOR_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFCREATOR_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefVersion, OTF_DEFVERSION_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFVERSION_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefFile, OTF_DEFFILE_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFFILE_RECORD);

	OTF_HandlerArray_setHandler(handlers, 
		(OTF_FunctionPointer*) handleDefFileGroup, OTF_DEFFILEGROUP_RECORD);
	OTF_HandlerArray_setFirstHandlerArg(handlers, 
	(void*) writer, OTF_DEFFILEGROUP_RECORD);

	read = OTF_Reader_readDefinitions (reader, handlers);
	if( read == OTF_READ_ERROR ) {
		cerr << "An error occurred while reading the tracefile. It seems to be damaged. Abort." << endl;
		CLEAR_EVERYTHING
		return 1;
	}

	/* set the writer's master to the modified master instance */
	/* closing the writer at the end writes the new master file to harddisk */
	OTF_Writer_setMasterControl(writer, new_master);

	/* clear everything */
	CLEAR_EVERYTHING

	return 0;
}

int display_processes(firstarg *first, int sim_mode) {

	map<int, bool>::iterator it;

	/* cout << "Note: You are in simulation mode at the moment. No files are created.\n" << endl; */
	if ( first->procMap.size() < 1) {
		cout << "You exclude all processes!" << endl;

		return 0;
	}

	cout << "You choose the following processes: " << endl;
	if ( sim_mode == LIST_MODE ) {
		for (it = first->procMap.begin(); it != first->procMap.end(); ++it) {
			cout << it->first << endl;
		}
	
	} else if ( sim_mode == RANGE_MODE ) {
		it = first->procMap.begin();
		int cur = it->first;
		int start = cur;
		int end = 0;
		for (it = ++it ; it != first->procMap.end(); ++it) {
			if ( (cur + 1) == it->first) {
				cur++;
			} else {
				end = cur;
				if (end > start) {
					cout << start << " - " << end << endl;
				} else {
					cout << start << endl;
				}
				cur = it->first;
				start = cur;
				end = 0;
			}
		}
		if (end == 0) {
			cout << start << " - " << cur << endl;
		}

	} else if ( sim_mode == TABLE_MODE ) {
		int i = 0;
		for (it = first->procMap.begin(); it != first->procMap.end(); ++it) {
			cout << it->first << "\t";
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

void free_all_pointers(char *ch_i, firstarg *first, OTF_MapEntry *entries, OTF_Writer *writer,
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
		delete[] entries;
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

