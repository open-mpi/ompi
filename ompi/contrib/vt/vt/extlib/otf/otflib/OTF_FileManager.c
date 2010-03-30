/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "OTF_Platform.h"
#include "OTF_FileManager.h"
#include "OTF_File.h"
#include "OTF_Errno.h"


/* *** structs *** */


/** entry of doubly linked list, used by struct struct_OTF_FileManager below. */
struct struct_OTF_FileList {


	OTF_File* file;
	struct struct_OTF_FileList* prev;
	struct struct_OTF_FileList* next;
};
typedef struct struct_OTF_FileList OTF_FileList;


/** file handles management structure */
struct struct_OTF_FileManager {

	/** number of files currently opened */
	uint32_t count;

	/** number of files allow to be opened simultaneously */
	uint32_t number;

	/** list of objects of type OTF_RBuffer or OTF_WBuffer */
	OTF_FileList* list;
};


/* *** headers *** */

void OTF_FileManager_init( OTF_FileManager* fh );
void OTF_FileManager_finalize( OTF_FileManager* fh );

int OTF_FileManager_listInsertAtHead( OTF_FileList** list, OTF_File* entry );
int OTF_FileManager_listUnlinkAtHead( OTF_FileList** list, OTF_File* entry );
int OTF_FileManager_listUnlinkAtTail( OTF_FileList** list, OTF_File* entry );
void OTF_FileManager_listPrint( OTF_FileList** list );


/* *** implementation *** */


void OTF_FileManager_init( OTF_FileManager* fh ) {


	fh->count= 0;
	fh->number= 10;

	fh->list= NULL;
}


void OTF_FileManager_finalize( OTF_FileManager* manager ) {


	OTF_FileList* pos;
	OTF_FileList* next;


#	ifdef OTF_DEBUG
		if ( 0 < manager->count ) {
		
			OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
					"open file remaining.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif /* OTF_DEBUG */

	if ( NULL != manager->list ) {
	
		pos= manager->list;

		pos->prev->next= NULL;

		while ( NULL != pos ) {

			next = pos->next;
			free( pos );
			pos = next;

		}
	}

	manager->list= NULL;
}


OTF_FileManager* OTF_FileManager_open( uint32_t number ) {



	OTF_FileManager* ret= (OTF_FileManager*) malloc( sizeof(OTF_FileManager) );
	if( NULL == ret ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_FileManager_init( ret );

	OTF_FileManager_setNumber( ret, number );

	return ret;
}


void OTF_FileManager_close( OTF_FileManager* fh ) {


	OTF_FileManager_finalize( fh );

	free( fh );
	fh = NULL;

}


uint32_t OTF_FileManager_getCount( OTF_FileManager* fh ) { 


	return fh->count; 
}


uint32_t OTF_FileManager_getNumber( OTF_FileManager* fh ) {


	return fh->number; 
}


uint32_t OTF_FileManager_setNumber( OTF_FileManager* fh, uint32_t number ) {


	if ( 0 == number ) {
	
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"illegal value 0 ignored.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return fh->number;
	}

	fh->number= number;

	return fh->number;
}


/** ensure there is a free file handle available after this call. 
return 1 on success, 0 otherwise (which is not supposed to happen) */
int OTF_FileManager_guaranteeFile( OTF_FileManager* m ) {


	/*
	OTF_fprintf( stderr, "OTF_FileManager_guaranteeFile()\n" );
	*/

	if ( m->count < m->number ) {

		/* free file handles available */

		/*
		OTF_fprintf( stderr, "    OTF_FileManager_guaranteeFile() free handles left\n" );
		*/

		return 1;
	}

	/*	
	OTF_fprintf( stderr, "    OTF_FileManager_guaranteeFile() need new handles\n" );
	*/

	/* suspend last entry in list */
	if ( 0 == OTF_FileManager_suspendFile( m, m->list->file ) ) {
	
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_FileManager_suspendFile() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return 0;
	}

	/*
	OTF_fprintf( stderr, "post suspend %u / %u\n", m->count, m->number );
	*/

	return 1;
}


/** registers the 'file' as open. return 1 on success, 0 otherwise. */
int OTF_FileManager_registerFile( OTF_FileManager* m, OTF_File* file ) {


	/*
	OTF_fprintf( stderr, "OTF_FileManager_registerFile()\n" );
	*/

	if ( OTF_FILESTATUS_ACTIVE != OTF_File_status( file ) ) {
	
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"file not open.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	if ( m->count >= m->number ) {
	
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"cannot register new file because limit %u exceeded, call "
						"'OTF_FileManager_guaranteeFile()' before.\n",
				__FUNCTION__, __FILE__, __LINE__, m->number );
			
		return 0;
	}

	OTF_FileManager_listInsertAtHead( &(m->list), file );

	/*
	OTF_fprintf( stderr, " c++ %u -> %u\n", m->count, m->count+1 );
	*/
	m->count++;

	return 1;
}


/** marks currently opened 'file' as used which is important for the 
scheduling strategy, i.e. the internal decision which file to suspend next.
return 1 on success or 0 for an suspended file. */
int OTF_FileManager_touchFile( OTF_FileManager* m, OTF_File* file ) {


	/*
	OTF_fprintf( stderr, "OTF_FileManager_touchFile()\n" );
	*/

	if ( OTF_FILESTATUS_ACTIVE != OTF_File_status( file ) ) {
	
		return 0;
	}

	/* unlink 'file' from somewhere in the list and put it to head */

	OTF_FileManager_listUnlinkAtHead( &(m->list), file );
	OTF_FileManager_listInsertAtHead( &(m->list), file );

	return 1;
}


/** suspend an open file explicitly. this may be called externaly or 
internally. return 1 on success, 0 otherwise. */
int OTF_FileManager_suspendFile( OTF_FileManager* m, OTF_File* file ) {


	/*
	OTF_fprintf( stderr, "OTF_FileManager_suspendFile()\n" );
	*/

	if ( OTF_FILESTATUS_ACTIVE != OTF_File_status( file ) ) {
	
		/* file not open, so cannot be suspended */

		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"file to be suspended is not open.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	if ( 0 == OTF_FileManager_listUnlinkAtTail( &(m->list), file ) ) {
	
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"could not unlink this entry.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	};

	/*
	OTF_fprintf( stderr, " c-- %u -> %u\n", m->count, m->count-1 );
	*/
	m->count--;

	OTF_File_suspend( file );

	return 1;
}


int OTF_FileManager_listInsertAtHead( OTF_FileList** list, OTF_File* entry ) {
	

	OTF_FileList* newentry= (OTF_FileList*) malloc( sizeof(OTF_FileList) );
	if( NULL == newentry ) {
		
		OTF_fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	if ( NULL != (*list) ) {

		/*
		OTF_fprintf( stderr, "pre insert : %p --> %p  (%p)\n", 
			*list, (*list)->next, (*list)->file );
		*/

		newentry->file= entry;
		newentry->prev= (*list)->prev;
		newentry->next= (*list);

		/*
		OTF_fprintf( stderr, "new entry  : %p --> %p  (%p)\n", 
			newentry, newentry->next, newentry->file );
		*/

		(*list)->prev->next= newentry;
		(*list)->prev= newentry;

		*list= newentry;

		/*
		OTF_fprintf( stderr, "post insert: %p --> %p  (%p)\n", 
			*list, (*list)->next, (*list)->file );
		*/

	} else {

		/* empty list */

		newentry->file= entry;
		newentry->prev= newentry;
		newentry->next= newentry;

		*list= newentry;
	}

	/*
	OTF_fprintf( stderr, "after OTF_FileManager_listInsertAtHead():\n" );
	*/
	/*
	OTF_FileManager_listPrint( list );
	*/

	return 0;
}


int OTF_FileManager_listUnlinkAtHead( OTF_FileList** list, OTF_File* file ) {


	OTF_FileList* pos;


	if ( NULL == *list ) {
	
		return 0;
	}

	pos= *list;
	while ( ( pos != (*list)->prev ) && ( pos->file != file ) ) {

		pos= pos->next;
	}

	if ( pos->file == file ) {

		/* found, unlink here */

		pos->prev->next= pos->next;
		pos->next->prev= pos->prev;

		if ( pos->next == pos ) {

			/* list gets empty! */
			*list= NULL;

		} else if ( (*list) == pos ) {
		
			/* removed current head */
			*list= (*list)->next;
		}

		free( pos );
		pos = NULL;

		/* 
		OTF_fprintf( stderr, "after OTF_FileManager_listUnlinkAtHead() %p found:\n", file );
		*/
		/*
		OTF_FileManager_listPrint( listHead, listTail );
		*/

		return 1;
	}
	
	/* not found */
	/* 
	OTF_fprintf( stderr, "after OTF_FileManager_listUnlinkAtHead() %p not found:\n", file );
	*/
	/* 
	OTF_FileManager_listPrint( listHead, listTail ); 
	*/

	return 0;
}


int OTF_FileManager_listUnlinkAtTail( OTF_FileList** list, OTF_File* file ) {


	OTF_FileList* pos;


	if ( NULL == *list ) {
	
		return 0;
	}

	pos= (*list)->prev;
	while ( ( pos != (*list) ) && ( pos->file != file ) ) {

		pos= pos->prev;
	}

	if ( pos->file == file ) {

		/* found, unlink here */

		pos->prev->next= pos->next;
		pos->next->prev= pos->prev;

		if ( pos->next == pos ) {

			/* list gets empty! */
			*list= NULL;

		} else if ( (*list) == pos ) {
		
			/* removed current head */
			*list= (*list)->next;
		}

		free( pos );
		pos = NULL;

		/* 
		OTF_fprintf( stderr, "after OTF_FileManager_listUnlinkAtHead() %p found:\n", file ); 
		*/
		/*
		OTF_FileManager_listPrint( listHead, listTail );
		*/

		return 1;
	}

	/* not found */
	/*
	OTF_fprintf( stderr, "after OTF_FileManager_listUnlinkAtTail():\n" );
	*/
	/*
	OTF_FileManager_listPrint( listHead, listTail );
	*/

	return 0;
}


void OTF_FileManager_listPrint( OTF_FileList** list ) {


	OTF_FileList* pos;


	if ( NULL == *list ) {
	
		OTF_fprintf( stderr, "empty list\n ----- \n" );
	
		return;
	}

	pos= *list;

	OTF_fprintf( stderr, "head: %p --> %p  (%p %u)\n", (void*)pos,
		(void*)pos->next, (void*)pos->file,
		OTF_File_status( pos->file ) );
	while ( pos != (*list)->prev ) {

		pos= pos->next;

		OTF_fprintf( stderr, "      %p --> %p  (%p %u)\n", (void*)pos,
			(void*)pos->next, (void*)pos->file,
			OTF_File_status( pos->file ) );
	}

	OTF_fprintf( stderr, " ----- \n" );
}

