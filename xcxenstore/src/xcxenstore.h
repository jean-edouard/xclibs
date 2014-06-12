/*
 * Copyright (c) 2013 Citrix Systems, Inc.
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

#ifndef XENSTORE_H_
# define XENSTORE_H_

#include <stdbool.h>

/* This XenStore API can only works with string data */

typedef void (*xenstore_watch_cb_t)(const char *path, void *);

int xenstore_init (void);
void xenstore_release (void);

/* Nested transaction is disallow for the moment */
bool xenstore_transaction_start (void);
bool xenstore_transaction_end (bool abort);

char *xenstore_read (const char *fmt, ...)
    __attribute__ ((format (printf, 1, 2)));
char *xenstore_dom_read (unsigned int domid, const char *fmt, ...)
    __attribute__ ((format (printf, 2, 3)));

bool xenstore_write (const char *data, const char *fmt, ...)
    __attribute__ ((format (printf, 2, 3)));
bool xenstore_write_int (int data, const char *fmt, ...)
    __attribute__ ((format (printf, 2, 3)));
bool xenstore_dom_write (unsigned int domid, const char *data,
                         const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));
bool xenstore_dom_write_int (unsigned int domid, int data,
                             const char *fmt, ...)
    __attribute__ ((format (printf, 3, 4)));

bool xenstore_rm (const char *fmt, ...)
    __attribute__ ((format (printf, 1, 2)));

char **xenstore_ls (unsigned int *num, const char *format, ...)
    __attribute__ ((format (printf, 2, 3)));

bool xenstore_watch (xenstore_watch_cb_t cb, void *opaque, const char *fmt,
                     ...)
    __attribute__ ((format (printf, 3, 4)));

bool xenstore_watch_dir (xenstore_watch_cb_t cb, void *opaque, const char *fmt,
                         ...)
    __attribute__ ((format (printf, 3, 4)));

bool xenstore_dom_watch (unsigned int domid, xenstore_watch_cb_t cb,
                         void *opaque, const char *format, ...)
    __attribute__ ((format (printf, 4, 5)));

bool xenstore_mkdir (const char *fmt, ...)
    __attribute__ ((format (printf, 1, 2)));

bool xenstore_chmod (const char *perms, int nperm, const char *format, ...)
    __attribute__ ((format (printf, 3, 4)));

bool xenstore_dom_chmod (unsigned int domid, const char *perms, int nperm,
                         const char *fmt, ...)
    __attribute__ ((format (printf, 4, 5)));

char *xenstore_get_domain_path (unsigned int domid);

#endif /* !XENSTORE_H_ */
