#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

#include "OTF_Definitions.h"
#include "OTF_Errno.h"

char otf_strerr[OTF_ERR_LEN] = "No errors occurred.";
int otf_errno = OTF_NO_ERROR;

#ifdef OTF_VERBOSE

	void OTF_fprintf( FILE* stream, const char* format, ... ) {
		va_list ap;
		va_start(ap, format);

		vsnprintf( otf_strerr, OTF_ERR_LEN, format, ap );
		otf_errno = OTF_ERROR;
		vfprintf( stream, format, ap);

		va_end(ap);
	}

#else

	void OTF_fprintf( FILE* stream, const char* format, ... ) {
		va_list ap;
		va_start(ap, format);

		vsnprintf( otf_strerr, OTF_ERR_LEN, format, ap );
		otf_errno = OTF_ERROR;

		va_end(ap);
	}

#endif
