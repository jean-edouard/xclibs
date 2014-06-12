/*
 * Copyright (c) 2012 Citrix Systems, Inc.
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

/* A test exe to check the various apis
   of libxcxenstore
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <event.h>
#include <xs.h>

static struct event xenstore_event;

//Call back function registered with a xenstore path using the watch
//This will be called inside xenstore_watches_scan 
void xs_watch_cb(const char * path, void * opaque)
{
	printf("Call back function triggered for device model!!\n");
}

void xs_watch_cb2(int path, void * opaque)
{
	printf("Call back function triggered for focus!!\n\n");
}

int main()
{
	pid_t pid;
	int ret;

	char * path = "/local/domain/0/device-model/1";
	char * path2 = "/local/domain/0/switcher/focus";

	//Initializing event
	event_init();

	//Initializing xenstore
	xenstore_init();

	if ((pid = fork()) > 0) {
		//Setting xenstore watch
		printf("Watch set for device model.\n");
		xenstore_watch(xs_watch_cb, NULL, path);

		//Starting event loop
		event_dispatch();
	}
	else {
		sleep(5);
		printf("After sleep. Writing to xs path.\n");

		//Writing to xenstore path
		ret = xenstore_write("0",path);
		if (!ret)
			printf("xs write failed.\n");
	}
}
