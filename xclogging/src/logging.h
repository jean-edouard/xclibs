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

#ifndef LOGGING_H
#define LOGGING_H

#include <errno.h>
#include <stdarg.h>
#include <syslog.h>
#include <string.h>

typedef enum _xc_log_level {
	XC_LOG_EMERG = 0,
	XC_LOG_ALERT,
	XC_LOG_CRIT,
	XC_LOG_ERR,
	XC_LOG_WARNING,
	XC_LOG_NOTICE,
	XC_LOG_INFO,
	XC_LOG_DEBUG,
	XC_LOG_FATAL,
	XC_LOG_SENTINAL,
} xc_log_level;

#define XCLOG_DEBUG(format, ...)     xc_log(XC_LOG_DEBUG,   __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_INFO(format, ...)      xc_log(XC_LOG_INFO,    __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_NOTICE(format, ...)    xc_log(XC_LOG_NOTICE,  __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_WARNING(format, ...)   xc_log(XC_LOG_WARNING, __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_ERROR(format, ...)     xc_log(XC_LOG_ERR,     __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_CRITICAL(format, ...)  xc_log(XC_LOG_CRIT,    __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_ALERT(format, ...)     xc_log(XC_LOG_ALERT,   __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)
#define XCLOG_EMERGENCY(format, ...) xc_log(XC_LOG_EMERG,   __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)

#define XCLOG_FATAL(format, ...)     xc_log(XC_LOG_FATAL,   __FILE__, __PRETTY_FUNCTION__, __LINE__, format, ##__VA_ARGS__)

/* Not multithread-safe */
#define XCLOG_PERROR(format, ...)						\
	do {									\
		char *XCLOG_perror = strerror(errno);				\
		xc_log(XC_LOG_ERR, __FILE__, __PRETTY_FUNCTION__, __LINE__,     \
			format ": %s", ##__VA_ARGS__, XCLOG_perror); 		\
	} while(0)

# define loglevel_to_logmask(loglevel) (( 1UL << ( loglevel + 1 )) - 1 )
int  xc_log_set_mask (int logmask);
void xc_log_init     (const char* prefix);
void xc_log_exit     (void);
void xc_log          (int flags, const char *file, const char *function, int line, const char *fmt, ...)
	__attribute__((format (printf, 5, 6)));

#endif /* LOGGING_H */
