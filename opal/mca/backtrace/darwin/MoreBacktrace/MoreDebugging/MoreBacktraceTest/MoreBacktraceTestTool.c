/*
	File:		MoreBacktraceTestTool.c

	Contains:	Command line tool to test MoreBacktrace.

	Written by:	DTS

	Copyright:	Copyright (c) 2006 by Apple Computer, Inc., All Rights Reserved.

	Disclaimer:	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc.
				("Apple") in consideration of your agreement to the following terms, and your
				use, installation, modification or redistribution of this Apple software
				constitutes acceptance of these terms.  If you do not agree with these terms,
				please do not use, install, modify or redistribute this Apple software.

				In consideration of your agreement to abide by the following terms, and subject
				to these terms, Apple grants you a personal, non-exclusive license, under Apple’s
				copyrights in this original Apple software (the "Apple Software"), to use,
				reproduce, modify and redistribute the Apple Software, with or without
				modifications, in source and/or binary forms; provided that if you redistribute
				the Apple Software in its entirety and without modifications, you must retain
				this notice and the following text and disclaimers in all such redistributions of
				the Apple Software.  Neither the name, trademarks, service marks or logos of
				Apple Computer, Inc. may be used to endorse or promote products derived from the
				Apple Software without specific prior written permission from Apple.  Except as
				expressly stated in this notice, no other rights or licenses, express or implied,
				are granted by Apple herein, including but not limited to any patent rights that
				may be infringed by your derivative works or by other works in which the Apple
				Software may be incorporated.

				The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
				WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
				WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
				PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
				COMBINATION WITH YOUR PRODUCTS.

				IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
				CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
				GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
				ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
				OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT
				(INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN
				ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Change History (most recent first):

*/

/////////////////////////////////////////////////////////////////

// System interfaces

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <TargetConditionals.h>

// MIB interfaces

#include "MoreBacktraceTestCommon.h"

/////////////////////////////////////////////////////////////////

static void PrintUsage(const char *argv0)
    // Print the program's usage.
{
    const char *command;
    
    command = strrchr(argv0, '/');
    if (command == NULL) {
        command = argv0;
    } else {
        command += 1;
    }
    fprintf(stderr, "usage: %s command...\n", command);
    fprintf(stderr, "       commands: -self\n");
    fprintf(stderr, "       commands: -signal\n");
    fprintf(stderr, "       commands: -signal-nonleaf\n");
    fprintf(stderr, "       commands: -pid <pid>\n");
    fprintf(stderr, "       commands: -smash\n");
}


int main(int argc, char **argv)
{
	int				err;
	int				retVal;
	const char *	builtArch;
	bool			printUsage;
	int				argIndex;
	pid_t			pid;

	// Just for convenience's sake, print out the architecture of the 
	// code we're actually running.  In the world of universal binaries, 
	// Rosseta, and PowerPC 32- and 64-bit, it's often hard to remember 
	// what's what.
	
	#if TARGET_CPU_PPC
		builtArch = "ppc";
	#elif TARGET_CPU_PPC64
		builtArch = "ppc64";
	#elif TARGET_CPU_X86
		builtArch = "i386";
	#else
		#error What are you building?
	#endif
	fprintf(stderr, "Built for %s.\n", builtArch);

	// Process command line arguments.
	
	retVal = EXIT_SUCCESS;
	printUsage = false;
	if (argc < 2) {
		printUsage = true;
	} else {
		argIndex = 1;
		while (argIndex < argc) {
			char *			bt;

			err = 0;
			bt = NULL;
			if ( strcmp(argv[argIndex], "-self") == 0 ) {
				argIndex += 1;
				err = TestMoreBacktraceMachSelf(&bt);
			} else if ( strcmp(argv[argIndex], "-pid") == 0 ) {
				argIndex += 1;
				if (argIndex < argc) {
					pid = (pid_t) atol(argv[argIndex]);
					argIndex += 1;
					
					err = TestMoreBacktraceMachThread(pid, &bt);
				} else {
					printUsage = true;
				}
			} else if ( strcmp(argv[argIndex], "-signal") == 0 ) {
				argIndex += 1;
				err = TestMoreBacktraceMachSelfSignal(false, &bt);
			} else if ( strcmp(argv[argIndex], "-signal-nonleaf") == 0 ) {
				argIndex += 1;
				err = TestMoreBacktraceMachSelfSignal(true, &bt);
			} else if ( strcmp(argv[argIndex], "-smash") == 0 ) {
				argIndex += 1;
				err = TestMoreBacktraceMachThreadStackSmash(&bt);
				if (err == 0) {
					fprintf(stderr, "Post-smash backtrace:\n");
				}
			} else {
				argIndex = argc;
				printUsage = true;
			}
			
			if ( (err == 0) && (bt != NULL) ) {
				fprintf(stdout, "%s\n", bt);
			} else {
				fprintf(stderr, "Failed to backtrace (error %d).\n", err);
			}
			free(bt);
		}
	}

	if (printUsage) {
		PrintUsage(argv[0]);
		retVal = EXIT_FAILURE;
	}

	return retVal;
}
