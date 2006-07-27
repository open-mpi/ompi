/*
	File:		MoreBacktraceTestCommon.c

	Contains:	Common code for testing MoreBacktrace.

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

$Log: MoreBacktraceTestCommon.c,v $
Revision 1.5  2006/01/23 19:26:07  eskimo1
Spin on a volatile int because the compiler doesn't like spinning an a volatile pointer (even after I declare it correctly).

Revision 1.1  2006/01/22 22:47:49  eskimo1
A new module that abstracts the test code out of the GUI test app so that it can also be used by the command line tool.


*/

/////////////////////////////////////////////////////////////////

// Our Prototypes

#include "MoreBacktraceTestCommon.h"

// Mac OS Interfaces

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

// MIB Interfaces

#include "MoreAddrToSym.h"

/////////////////////////////////////////////////////////////////

#pragma mark ***** Backtrace To Text

// TestMoreBacktraceFramesToText uses a memory based (FILE *) (created using 
// fwopen) to collect its results.  WriteCallback is the routine that handles 
// writes for this (FILE *).  WriteCallbackContext holds its state.

struct WriteCallbackContext {
	size_t		dataSize;				// current data size
	char *		data;					// current data
};
typedef struct WriteCallbackContext WriteCallbackContext;

static int WriteCallback(void *cookie, const char *data, int dataSize)
	// Passed as a write callback to fwopen to create a memory-based 
	// (FILE *).  See <x-man-page://3/fwopen> for details.
{
	int						err;
	int						result;
	char *					tmp;
	WriteCallbackContext *	context;

	// Recover our context from the cookie.
	
	context = (WriteCallbackContext *) cookie;
	
	// Allocate a new buffer to accommodate the old data and the new.
	
	err = 0;
	tmp = malloc(context->dataSize + dataSize);
	if (tmp == NULL) {
		err = ENOMEM;
	}
	
	// Fill it in.
	
	if (err == 0) {
		memcpy(tmp,                     context->data, context->dataSize);	// old data
		memcpy(tmp + context->dataSize, data,          dataSize);			// new data
		
		free(context->data);
		context->data = tmp;
		context->dataSize += dataSize;
	}

	// Clean up.
	
	if (err == 0) {
		result = dataSize;
	} else {
		errno = err;
		result = -1;
	}
	return result;
}

extern int TestMoreBacktraceFramesToText(
	const MoreBTFrame	frames[], 
	size_t				frameCount, 
	bool				lookupSymbolNames, 
	char **				btPtr
)
	// See comments in header.
{
	int						err;
	int						junk;
	WriteCallbackContext	resultContext;
	FILE *					result;
	size_t					frameIndex;
	MoreAToSSymInfo *		symbols;

	assert(frames != NULL);
	assert(btPtr != NULL);
	assert(*btPtr == NULL);
	
	result = NULL;
	resultContext.dataSize = 0;
	resultContext.data     = NULL;
	symbols = NULL;
	
	// Create an array of NULL CFStringRefs to hold the symbol pointers.
	
	err = MoreAToSCreate(frameCount, &symbols);
	
	// If we've been asked to look up the symbols, do so.
	
	if ( (err == 0) && lookupSymbolNames) {
		MoreAToSAddr *addresses;
		
		// Allocate an array for the addresses and fill it in.
		
		addresses = (MoreAToSAddr *) malloc(frameCount * sizeof(*addresses));
		if (addresses == NULL) {
			err = ENOMEM;
		}
		if (err == 0) {	
			for (frameIndex = 0; frameIndex < frameCount; frameIndex++) {
				if (frames[frameIndex].flags & kMoreBTPCBadMask) {
					addresses[frameIndex] = 0;
				} else {
					addresses[frameIndex] = frames[frameIndex].pc;
				}
			}
		}
		
		// Call MoreAddrToSym module to do the job.
		
		if (err == 0) {
			err = MoreAToSCopySymbolNamesUsingDyld(frameCount, addresses, symbols);
		}
		
		free(addresses);
	}

	// Create the output string, starting with an empty string and then 
	// appending an entry for each frame.  This uses the little known 
	// ability of BSD to create a (FILE *) with custom I/O routines.  In this 
	// case, I have a write routine that outputs to a memory buffer (the 
	// details of which are held in resultContext).
	
	if (err == 0) {
		result = fwopen(&resultContext, WriteCallback);
		if (result == NULL) {
			err = errno;
		}
	}
	if (err == 0) {
		int		ptrWidth;
		
		// Determine whether we can treat the backtrace is 32-bit or whether we have to 
		// output the full 64-bits of each pointer.
		
		ptrWidth = 8;
		for (frameIndex = 0; frameIndex < frameCount; frameIndex++) {
			const MoreBTFrame *	thisFrame;
			
			thisFrame = &frames[frameIndex];
			
			if ( !(thisFrame->flags & kMoreBTPCBadMask)    && (thisFrame->pc & 0xFFFFFFFF00000000LL) ) {
				ptrWidth = 16;
			}
			if ( !(thisFrame->flags & kMoreBTFrameBadMask) && (thisFrame->fp & 0xFFFFFFFF00000000LL) ) {
				ptrWidth = 16;
			}
		}
		
		// Now output each entry.
		
		for (frameIndex = 0; frameIndex < frameCount; frameIndex++) {
			const char *thisSymbol;
			
			// First the frame number and the flags.
			
			junk = fprintf(result, "%2zu %c%c%c ",
				 frameIndex,
				 (frames[frameIndex].flags & kMoreBTFrameBadMask)      ? 'F' : 'f',
				 (frames[frameIndex].flags & kMoreBTPCBadMask)         ? 'P' : 'p',
				 (frames[frameIndex].flags & kMoreBTSignalHandlerMask) ? 'S' : 's'
			);
			assert(junk > 0);
			
			// Then the frame pointer.
			
			if (frames[frameIndex].flags & kMoreBTFrameBadMask) {
				fprintf(result, "%*s ", ptrWidth, "");
			} else {
				fprintf(result, "%*llx ", ptrWidth, frames[frameIndex].fp);
			}
			
			// Then the PC.
			
			if (frames[frameIndex].flags & kMoreBTPCBadMask) {
				fprintf(result, "%*s ", ptrWidth, "");
			} else {
				fprintf(result, "%*llx ", ptrWidth, frames[frameIndex].pc);
			}
			
			// Finally the symbolic name, if present.
								 					 
			thisSymbol = symbols[frameIndex].symbolName;
			if (thisSymbol != NULL) {
				junk = fprintf(result, " %s+%04llx", thisSymbol, symbols[frameIndex].symbolOffset);
				assert(junk > 0);
			}
			
			junk = fprintf(result, "\n");
			assert(junk > 0);
		}
	}
	
	// Null terminate the string.
	
	if (err == 0) {
		junk = fputc(0, result);
		assert(junk >= 0);
	}
	
	// Clean up.

	if (result != NULL) {
		junk = fclose(result);
		assert(junk == 0);
	}
	if (err == 0) {
		*btPtr = resultContext.data;	// all good, return resultContext.data to the client
	} else {
		free(resultContext.data);		// all bad, free up resultContext.data and leave *btPtr as NULL
	}
	MoreAToSDestroy(frameCount, symbols);

	assert( (err == 0) == (*btPtr != NULL) );
			
	return err;
}

#pragma mark ***** Self Test

// To simplify the test code, I receive the backtrace into a fixed size array. 
// The number of elements in this array, 70, is the number that I can reasonably 
// display in the window of the GUI app.  I've carried it forward into the common 
// code because I don't see any pressing need to change it.

enum {
	kFrameCount = 70
};

extern int TestMoreBacktraceMachSelf(char **btPtr)
	// See comments in header.
{
	int				err;
	MoreBTFrame 	frames[kFrameCount];
	size_t			frameCount;
	size_t			validFrames;
	
	assert( btPtr != NULL);
	assert(*btPtr == NULL);

	frameCount = sizeof(frames) / sizeof(*frames);
	err = MoreBacktraceMachSelf(0, 0, frames, frameCount, &validFrames);
	if (err == 0) {
		if (validFrames > frameCount) {
			validFrames = frameCount;
		}
		err = TestMoreBacktraceFramesToText(frames, validFrames, true, btPtr);
	}
	
	assert( (err == 0) == (*btPtr != NULL) );

	return err;
}

#pragma mark ***** Other Task Test

extern int TestMoreBacktraceMachThread(pid_t pid, char **btPtr)
	// See comments in header.
{
	int						err;
	int						junk;
	MoreBTFrame				frames[kFrameCount];
	size_t					frameCount;
	size_t					validFrames;
	task_t					targetTask;
	bool					didSuspend;
	thread_array_t			threadList;
	mach_msg_type_number_t	threadCount;

	assert(pid > 0);
	assert(btPtr != NULL);
	assert(*btPtr == NULL);

	targetTask = MACH_PORT_NULL;
	threadList = NULL;
	didSuspend = false;
	
	frameCount = 0;			// just to quieten a warning
	
	// Convert pid to the Mach task control port.
	
	err = task_for_pid(mach_task_self(), pid, &targetTask);
	if (err == 0) {
		if (targetTask == mach_task_self()) {
			err = -1;			// this won't go well
		}
	}
	
	// Suspend the task while we sample it.  Otherwise the 
	// list of threads might change.
	
	if (err == 0) {
		err = task_suspend(targetTask);
		didSuspend = (err == 0);
	}
	if (err == 0) {
		err = task_threads(targetTask, &threadList, &threadCount);
	}
	if (err == 0) {
		// A task without any threads makes no sense.
		
		assert(threadCount > 0);
		
		// We always sample the first thread.  This has no real 
		// significance because Mach doesn't guarantee to return 
		// the threads in any particular order.  In a real tool 
		// you'd iterate over all of the threads and sample each, 
		// but I have no way of displaying the results in my 
		// test framework.
		
		frameCount = sizeof(frames) / sizeof(*frames);
		err = MoreBacktraceMachThread(targetTask, threadList[0], 0, 0, frames, frameCount, &validFrames);
	}
	
	// Resume the task as quickly as possibly after the backtrace.
	
	if (didSuspend) {
		junk = task_resume(targetTask);
		assert(junk == 0);
	}
	
	// Create the text output without symbols.
	
	if (err == 0) {
		if (validFrames > frameCount) {
			validFrames = frameCount;
		}
		err = TestMoreBacktraceFramesToText(frames, validFrames, false, btPtr);
	}
	
	// Clean up.
	
	junk = mach_port_deallocate(mach_task_self(), targetTask); 
	assert(junk == 0);
	if (threadList != NULL) {
		mach_msg_type_number_t thisThread;
		
		for (thisThread = 0; thisThread < threadCount; thisThread++) {
			junk = mach_port_deallocate(mach_task_self(), threadList[thisThread]);
			assert(junk == 0);
		}
		junk = vm_deallocate(mach_task_self(), (vm_address_t) threadList, threadCount * sizeof(*threadList));
		assert(junk == 0);
	}

	assert( (err == 0) == (*btPtr != NULL) );

	return err;
}

#pragma mark ***** Signal Test

// The following are set by the signal handler when it runs.

static volatile int		gError = -1;
static char volatile *	gBacktrace = NULL;

static void MySIGUSR1Handler(int signal)
	// Handle SIGUSR1.  Note that we call many functions 
	// here that aren't "signal safe".  However, we know 
	// that this signal isn't happening asynchronously, 
	// we're sending it to ourselves via a call to "pthread_kill", 
	// so most of the unsafeness is moot.  Still, if this 
	// wasn't a just test program I'd figure out a better way.
{
	int		err;
	char *	tmp;
	assert(signal == SIGUSR1);

	fprintf(stderr, "MySIGUSR1Handler calls TestMoreBacktraceMachSelf.\n");
	tmp = (char *) gBacktrace;
	err = TestMoreBacktraceMachSelf(&tmp);
	gBacktrace = tmp;			// Because the code in TestSignalBacktraceNested 
	gError = err;				// might be spinning on gError, assign that last.
}

static int TestSignalBacktraceNested(bool nonLeaf)
	// Send a SIGUSR1 to ourselves.
{
	int err;
	
	if ( ! nonLeaf ) {

		// This is the standard branch.  We use pthread_kill to send a signal 
		// to ourself.

		err = pthread_kill(pthread_self(), SIGUSR1);
	} else {

		// Use this branch of the code, in tandem with sending the SIGUSR1 from 
		// the command line, to test the case where a signal interrupts a non-leaf 
		// routine.  I added this because pthread_kill (well, actually __pthread_kill) 
		// is a system call leaf routine, so the main test only checks the 
		// in leaf routine case.
		//
		// Also, this is a hard spin loop because I can't 
		// add a delay call because then it's likely that the signal would come 
		// in while we're blocked in the delay, and then we're back to the leaf 
		// routine test again.
		
		fprintf(stderr, "Signal this process using:\n");
		fprintf(stderr, "\n");
		fprintf(stderr, "$ kill -USR1 %ld\n", (long) getpid());

		while (gError == -1) {
			// do nothing
		}
		err = 0;
	}
	return err;
}

extern int TestMoreBacktraceMachSelfSignal(bool nonLeaf, char **btPtr)
	// See comments in header.
{
	int					err;
	int					junk;
	struct sigaction 	oldSig;
	struct sigaction 	newSig;

	assert(btPtr != NULL);
	assert(*btPtr == NULL);

	assert(gBacktrace == NULL);
	assert(gError == -1);

	// Install MySIGUSR1Handler as the SIGUSR1 handler.
	
	memset(&newSig, 0, sizeof(newSig));
	newSig.sa_handler = &MySIGUSR1Handler;
	err = sigaction(SIGUSR1, &newSig, &oldSig);
	if (err < 0) {
		err = errno;
	}
	
	if (err == 0) {
	
		// Call a routine that sends a signal to us.
		
		err = TestSignalBacktraceNested(nonLeaf);
		
		// Restore the old signal handler.
		
		junk = sigaction(SIGUSR1, &oldSig, NULL);
		assert(junk == 0);
		
		if (err == 0) {
			err = gError;				// get the error from the signal handler
			gError = -1;				// just for tidiness
		}
	}
	
	if (err == 0) {
		*btPtr = (char *) gBacktrace;	// get the backtrace from the signal handler
		gBacktrace = NULL;				// just for tidiness
	}
	
	assert( (err == 0) == (*btPtr != NULL) );

	return err;
}

#pragma mark ***** Stack Smash Test

// This test works by create a dummy thread that blocks forever deep in nested 
// routines, then smashing its stack.

static pthread_t				gDeadThread;
	// The dummy thread itself.

static pthread_mutex_t			gDeadThreadReadyMutex;
static pthread_cond_t 			gDeadThreadReadyCond;
	// A condition variable that protects gDeadThreadReady and gDeadThreadID.

static volatile bool			gDeadThreadReady = false;
	// True if the dead thread is ready to be smashed.
	
static volatile thread_t		gDeadThreadID = MACH_PORT_NULL;
	// The Mach thread ID of the thread.

static void DeadThreadNested4(void)
	// The victim routine itself.
{
	int			junk;
	
	// Set our condition to true.
	
	junk = pthread_mutex_lock(&gDeadThreadReadyMutex);
	assert(junk == 0);
	
	gDeadThreadReady = true;
	gDeadThreadID    = mach_thread_self();

	junk = pthread_mutex_unlock(&gDeadThreadReadyMutex);
	assert(junk == 0);
	
	// Signal the main thread who is waiting on our condition.
	
	junk = pthread_cond_signal(&gDeadThreadReadyCond);
	assert(junk == 0);
	
	// Loop forever.
	
	do {
		(void) pause();
	} while (true);
}

static void DeadThreadNested3(void)
	// A few nested procedures so you get an interesting trace.
{
	DeadThreadNested4();
}

static void DeadThreadNested2(void)
	// A few nested procedures so you get an interesting trace.
{
	DeadThreadNested3();
}

static void DeadThreadNested1(void)
	// A few nested procedures so you get an interesting trace.
{
	DeadThreadNested2();
}

static void * DeadThread(void *param)
	// The thread entry point for the victim thread.
{
	#pragma unused(param)
	DeadThreadNested1();
	return NULL;
}

static int StartDeadThread(void)
	// Start a victim thread whose stack we intend to smash 
	// in order to test our handling of bad pointers in a 
	// stack crawl.
	//
	// Returns an errno-style error.
{
	int			err;
	int			junk;
	thread_t	threadID;
	MoreBTFrame	frames[10];
	size_t		frameCount;
	size_t		validFrames;

	// Init resources required for condition variable.
	
	err = pthread_mutex_init(&gDeadThreadReadyMutex, NULL);
	if (err == 0) {
		err = pthread_cond_init(&gDeadThreadReadyCond, NULL);
	}

	// Start the thread.
	
	if (err == 0) {
		err = pthread_create(&gDeadThread, NULL, DeadThread, NULL);
	}
	if (err == 0) {
		junk = pthread_detach(gDeadThread);
		assert(junk == 0);
	}
	
	// Wait for the condition.
	
	if (err == 0) {
		threadID = MACH_PORT_NULL;
		do {
			junk = pthread_mutex_lock(&gDeadThreadReadyMutex);
			assert(junk == 0);
			
			if (gDeadThreadReady) {
				threadID = gDeadThreadID;
			}
			
			if (threadID == MACH_PORT_NULL) {
				junk = pthread_cond_wait(&gDeadThreadReadyCond, &gDeadThreadReadyMutex);
				assert(junk == 0);
			}
			
			junk = pthread_mutex_unlock(&gDeadThreadReadyMutex);
			assert(junk == 0);
		} while ( threadID == MACH_PORT_NULL );
	}
	
	// Take a backtrace.
	
	if (err == 0) {
		frameCount = sizeof(frames) / sizeof(frames[0]);
		
		err = MoreBacktraceMachThread(
			mach_task_self(),
			threadID,
			0,
			0,
			frames,
			frameCount,
			&validFrames
		);
	}
	if (err == 0) {
		if (validFrames < frameCount) {
			err = -1;
		} else if (validFrames > frameCount) {
			validFrames = frameCount;
		}
	}
	if ( (err == 0) && true) {
		char *	bt;
		
		bt = NULL;
		
		err = TestMoreBacktraceFramesToText(frames, validFrames, true, &bt);
		if (err == 0) {
			fprintf(stderr, "Pre-smash backtrace:\n");
			fprintf(stdout, "%s\n", bt);
		}
		
		free(bt);

		err = 0;
	}
	
	// Now let's use that backtrace to vandalise the stack!
	
	if (err == 0) {
		int		i;
		char *	busErrorAddress;
		char **	fp;

		// I don't want to use 0 for the Mac OS X bus error value, because 
		// it's a little obvious.  Instead I use a value that's in the bottom 
		// page (ie is less than 0x1000) and is easily recognisable.

		busErrorAddress = (char *) (intptr_t) 0xfec;

		// 1. We go down 4 frames on the stack and smash the next 
		//    frame pointer.

		fp = (char **) (intptr_t) frames[4].fp;
		fp[0] = busErrorAddress;
		
		// 2. We go down 2 frames on the stack and smash the 
		//    return address.  As its location is architecture dependent, 
		//    we just smash 8 words and hope that we get it (-:

		fp = (char **) (intptr_t) frames[2].fp;
		for (i = 0; i < 8; i++) {
			fp[i] = busErrorAddress;
		}
	}
	
	return err;
}

extern int TestMoreBacktraceMachThreadStackSmash(char **btPtr)
	// See comments in header.
{
	int						err;
	MoreBTFrame 			frames[kFrameCount];
	size_t					frameCount;
	size_t					validFrames;

	assert( btPtr != NULL);
	assert(*btPtr == NULL);
	
	// Start a thread that just blocks indefinitely.  In the 
	// process, smash its stack.
		
	err = 0;
	if ( ! gDeadThreadReady ) {
		err = StartDeadThread();
	}
	assert( (err != 0) || (gDeadThreadReady && (gDeadThreadID != MACH_PORT_NULL)) );
	
	// Take a backtrace of that thread and return it.
	
	if (err == 0) {
		frameCount = sizeof(frames) / sizeof(*frames);
		err = MoreBacktraceMachThread(mach_task_self(), gDeadThreadID, 0, 0, frames, frameCount, &validFrames);
	}
	if (err == 0) {
		if (validFrames > frameCount) {
			validFrames = frameCount;
		}
		err = TestMoreBacktraceFramesToText(frames, validFrames, true, btPtr);
	}

	assert( (err == 0) == (*btPtr != NULL) );

	return err;
}
