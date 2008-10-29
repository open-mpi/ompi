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

#include "vt_unify_tkfac.h"
#include "vt_unify.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <assert.h>

int
Handle_Enter( OTF_WStream* wstream,
	      uint64_t time, uint32_t statetoken, uint32_t cpuid,
	      uint32_t scltoken )
{
   uint32_t mcpuid = cpuid % 65536;

   TokenFactory_DefFunction * p_tkfac_deffunction =
      static_cast<TokenFactory_DefFunction*>(theTokenFactory[TKFAC__DEF_FUNCTION]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_statetoken =
      p_tkfac_deffunction->translateLocalToken( mcpuid, statetoken );
   assert( global_statetoken != 0 );
   
   uint32_t global_scltoken = scltoken;

   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( mcpuid, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( mcpuid, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeEnter( wstream,
				      time,
				      global_statetoken,
				      cpuid,
				      global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}
		  
int
Handle_Leave( OTF_WStream* wstream,
	      uint64_t time, uint32_t statetoken, uint32_t cpuid,
	      uint32_t scltoken )
{
   uint32_t mcpuid = cpuid % 65536;

   TokenFactory_DefFunction * p_tkfac_deffunction =
      static_cast<TokenFactory_DefFunction*>(theTokenFactory[TKFAC__DEF_FUNCTION]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_statetoken = statetoken;

   if( statetoken != 0 )
   {
      global_statetoken =
	 p_tkfac_deffunction->translateLocalToken( mcpuid, statetoken );
      assert( global_statetoken != 0 );
   }
   
   uint32_t global_scltoken = scltoken;

   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( mcpuid, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( mcpuid, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeLeave( wstream,
				      time,
				      global_statetoken,
				      cpuid,
				      global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_FileOperation( OTF_WStream* wstream,
		      uint64_t time, uint32_t filetoken, uint32_t cpuid,
		      uint64_t handleid, uint32_t operation, uint64_t bytes,
		      uint64_t duration, uint32_t scltoken )
{
   uint32_t mcpuid = cpuid % 65536;

   TokenFactory_DefFile * p_tkfac_deffile =
      static_cast<TokenFactory_DefFile*>(theTokenFactory[TKFAC__DEF_FILE]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_filetoken =
      p_tkfac_deffile->translateLocalToken( mcpuid, filetoken );
   assert( global_filetoken != 0 );
   
   uint32_t global_scltoken = scltoken;

   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( mcpuid, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( mcpuid, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeFileOperation( wstream,
					      time,
					      global_filetoken,
					      cpuid,
					      handleid,
					      operation,
					      bytes,
					      duration,
					      global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_SendMsg( OTF_WStream* wstream,
		uint64_t time, uint32_t sender, uint32_t receiver,
		uint32_t communicator, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken )
{
   uint32_t msender = sender % 65536;

   TokenFactory_DefProcessGroup * p_tkfac_defprocessgroup =
      static_cast<TokenFactory_DefProcessGroup*>(theTokenFactory[TKFAC__DEF_PROCESS_GROUP]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_communicator = 
      p_tkfac_defprocessgroup->translateLocalToken( msender, communicator );
   assert( global_communicator != 0 );

   uint32_t global_scltoken = scltoken;

   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( msender, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( msender, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeSendMsg( wstream,
					time, sender, receiver,
					global_communicator, msgtag,
					msglength, global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_RecvMsg( OTF_WStream* wstream,
		uint64_t time, uint32_t receiver, uint32_t sender,
		uint32_t communicator, uint32_t msgtag,
		uint32_t msglength, uint32_t scltoken )
{
   uint32_t mreceiver = receiver % 65536;

   TokenFactory_DefProcessGroup * p_tkfac_defprocessgroup =
      static_cast<TokenFactory_DefProcessGroup*>(theTokenFactory[TKFAC__DEF_PROCESS_GROUP]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_communicator = 
      p_tkfac_defprocessgroup->translateLocalToken( mreceiver, communicator );
   assert( global_communicator != 0 );

   uint32_t global_scltoken = scltoken;

   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( mreceiver, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( mreceiver, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeRecvMsg( wstream,
					time, receiver, sender,
					global_communicator, msgtag,
					msglength, global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_CollectiveOperation( OTF_WStream* wstream,
			    uint64_t time, uint32_t process,
			    uint32_t functionToken, uint32_t communicator,
			    uint32_t rootprocess, uint32_t sent,
			    uint32_t received, uint64_t duration,
			    uint32_t scltoken )
{
   uint32_t mprocess = process % 65536;

   TokenFactory_DefCollectiveOperation * p_tkfac_defcollop =
      static_cast<TokenFactory_DefCollectiveOperation*>(theTokenFactory[TKFAC__DEF_COLL_OP]);

   TokenFactory_DefProcessGroup * p_tkfac_defprocessgroup =
      static_cast<TokenFactory_DefProcessGroup*>(theTokenFactory[TKFAC__DEF_PROCESS_GROUP]);

   TokenFactory_DefScl * p_tkfac_defscl = 
      static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

   uint32_t global_functionToken =
      p_tkfac_defcollop->translateLocalToken( mprocess, functionToken );
   assert( global_functionToken != 0 );

   uint32_t global_communicator =
      p_tkfac_defprocessgroup->translateLocalToken( mprocess, communicator );
   assert( global_communicator != 0 );

   uint32_t global_scltoken = scltoken;
   
   if( scltoken != 0 )
   {
      global_scltoken =
	 p_tkfac_defscl->translateLocalToken( mprocess, scltoken );
      assert( global_scltoken != 0 );
   }

   time = CorrectTime( mprocess, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeCollectiveOperation( wstream,
						    time, process,
						    global_functionToken,
						    global_communicator,
						    rootprocess, sent,
						    received, duration,
						    global_scltoken );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_Counter( OTF_WStream* wstream,
		uint64_t time, uint32_t process, uint32_t counter_token,
		uint64_t value )
{
   uint32_t mprocess = process % 65536;

   TokenFactory_DefCounter * p_tkfac_defcounter = 
      static_cast<TokenFactory_DefCounter*>(theTokenFactory[TKFAC__DEF_COUNTER]);

   uint32_t global_counter_token =
      p_tkfac_defcounter->translateLocalToken( mprocess, counter_token );
   assert( global_counter_token != 0 );

   time = CorrectTime( mprocess, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeCounter( wstream,
					time, process, global_counter_token,
					value );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}

int
Handle_EventComment( OTF_WStream* wstream,
		     uint64_t time, uint32_t process, const char* comment )
{
   uint32_t mprocess = process % 65536;

   time = CorrectTime( mprocess, time ) - g_uMinStartTime;

   int wrrc = OTF_WStream_writeEventComment( wstream,
					     time, process, comment );
   return wrrc == 1 ? OTF_RETURN_OK : OTF_RETURN_ABORT;
}
