/*
 * Copyright (c) 2011 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <logging.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>

#include "backtrace.h"

typedef struct _xc_log_buffer {
        int   level;
        char *name;
} xc_log_buffer;

/* Only 0-7 loglevels are supported. Others
 * should be one of these initial 8 loglevels */
static const xc_log_buffer xc_log_strings[] = {
        [XC_LOG_EMERG].level=LOG_EMERG,     [XC_LOG_EMERG].name="Emergency",
        [XC_LOG_ALERT].level=LOG_ALERT,     [XC_LOG_ALERT].name="Alert",
        [XC_LOG_CRIT].level=LOG_CRIT,       [XC_LOG_CRIT].name="Critical",
        [XC_LOG_ERR].level=LOG_ERR,         [XC_LOG_ERR].name="Error",
        [XC_LOG_WARNING].level=LOG_WARNING, [XC_LOG_WARNING].name="Warning",
        [XC_LOG_NOTICE].level=LOG_NOTICE,   [XC_LOG_NOTICE].name="Notice",
        [XC_LOG_INFO].level=LOG_INFO,       [XC_LOG_INFO].name="Info",
        [XC_LOG_DEBUG].level=LOG_DEBUG,     [XC_LOG_DEBUG].name="Debug",
        [XC_LOG_FATAL].level=LOG_ERR,       [XC_LOG_FATAL].name="Fatal",
        [XC_LOG_SENTINAL].level=-1,         [XC_LOG_SENTINAL].name=NULL
};

void
xc_log_init (const char* prefix)
{
	int extra_opts = 0;
	if (isatty(STDERR_FILENO))
		extra_opts = LOG_PERROR;

	openlog(prefix, LOG_CONS | LOG_NOWAIT | extra_opts | LOG_PID, \
		LOG_USER);

}

int
xc_log_set_mask (int mask)
{
	return setlogmask(mask);
}

void
xc_log (int flags, const char *file, const char *function, int line, const char *fmt, ...)
{
	if ( flags >= XC_LOG_SENTINAL || flags < 0 )
		return;
	if ( xc_log_strings[flags].level < LOG_EMERG || xc_log_strings[flags].level > LOG_DEBUG )
		return;

	int     fatal  = ( flags == XC_LOG_FATAL );
	size_t  size   = 1 + strlen(fmt) + strlen(xc_log_strings[flags].name) + 3;
	char   *format = (char *)malloc(size * sizeof(char));
	if (!format) {
		syslog (LOG_ERR, "malloc failed");
		return;
	}
	va_list ap;

	sprintf (format, "[%s] %s", xc_log_strings[flags].name, fmt);

	/* Syslog */
	syslog (LOG_DEBUG, "[%s] %s:%s:%d", xc_log_strings[flags].name, file, function, line);

	va_start (ap, fmt);
	vsyslog (LOG_USER | xc_log_strings[flags].level, format, ap);
	va_end (ap);

	free(format);

	if ( fatal ) {
		dump_backtrace ();
		abort ();
	}

	return;
}

void
xc_log_exit (void)
{
	closelog();
	return;
}
