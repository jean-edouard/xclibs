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

/* Example to demonstrate use of xclogging lib */

/* Logs messages upto loglevel input by the user
   usage: ./testlog --loglevel [0-7]           
   Defaut loglevel = 7, ie all the logs are permitted */

#include <logging.h>
#include <unistd.h>
#include <getopt.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <config.h>

#define LOG_LEVEL 7

typedef struct _ctx_cfg {
	int loglevel;
	int logmask;
} ctx_cfg;

int
parse_args(int argc, char * const argv[], ctx_cfg *opt_values)
{
	int errno;

	typedef enum {
		OPT_LOG_LEVEL = 1000,
	} opts;

	struct option option_flags[] = {
		{
			.name     = "loglevel",
			.has_arg  = 1,
			.flag     = NULL,
			.val      = OPT_LOG_LEVEL,
		},
		{0, }
	};

	int matching_option;

	while(1) {
		int c;
		c = getopt_long(argc, argv, "l:",
				option_flags,
				&matching_option);
		switch(c) {
			case -1:
				goto finish;
			case 'l':
			case OPT_LOG_LEVEL:
				{
					errno=0;
					if (!isdigit(optarg[0])) {
						fprintf(stderr, "Invalid loglevel specified: '%s'\n",
								optarg);
						return -1;
					}
					long int intarg = strtol(optarg, NULL, 10);
					if (errno || intarg < 0 || intarg > 7) {
						fprintf(stderr, "Invalid loglevel specified: '%s'\n",
								optarg);
						return -1;
					}
#if HAVE_SYSLOG_H
					opt_values->logmask  = LOG_UPTO(intarg);
#else
					opt_values->logmask  = (1UL << (intarg + 1)) - 1;
#endif
					opt_values->loglevel = intarg;
					break;
				}
			default:
				fprintf(stderr, "getopt returned character code 0%o\n", c);
				return -1;
		}
	} /* while */
finish:
	return 0;
}
/* parse_args */

int main(int argc, char **argv)
{
	ctx_cfg config;
	config.loglevel = LOG_LEVEL; /* Default loglevel */
#if HAVE_SYSLOG_H
	config.logmask  = LOG_UPTO(config.loglevel);
#else
	config.logmask  = (1UL << (config.loglevel + 1)) - 1;
#endif
	if(parse_args(argc, argv, &config))
		return 1;

	xc_log_init("xclogging-test");
	xc_log_set_mask(config.logmask);

	/* Test log messages */
	XCLOG_INFO ("Loglevel %d", config.loglevel);

	XCLOG_DEBUG     ("testing debug");
	XCLOG_INFO      ("testing info");
	XCLOG_NOTICE    ("testing notice");
	XCLOG_WARNING   ("testing warning");
	XCLOG_ERROR     ("testing error");
	XCLOG_CRITICAL  ("testing critical");
	XCLOG_ALERT     ("testing alert");
	XCLOG_EMERGENCY ("testing emergency");

	/* Testing Perror */
	FILE *fp = fopen("abc.xyz", "rb");
	if ( !fp )
		XCLOG_PERROR("fopen()");

#if 0
        /* make check will fail, if this is enabled */
	XCLOG_FATAL     ("testing fatal, program will abort!");
#endif
	xc_log_exit();

	return 0;
}
/* main */
