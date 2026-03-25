/* Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
 *               root of the project or at https://github.com/pdidev/paraconf
 * 
 * SPDX-License-Identifier: MIT
 */

#ifndef STATUS_H__
#define STATUS_H__

#include "paraconf.h"

#define PC_handle_err(callstatus, free_stamp)                                                                                                        \
	do {                                                                                                                                             \
		status = callstatus;                                                                                                                         \
		if (status) goto free_stamp;                                                                                                                 \
	} while (0)

#define PC_handle_tree(free_stamp)                                                                                                                   \
	do {                                                                                                                                             \
		if (PC_status(restree)) goto free_stamp;                                                                                                     \
	} while (0)

#define PC_handle_err_tree(callstatus, free_stamp)                                                                                                   \
	do {                                                                                                                                             \
		restree.status = callstatus;                                                                                                                 \
		if (PC_status(restree)) goto free_stamp;                                                                                                     \
	} while (0)

#define PC_handle_tree_err(calltree, free_stamp)                                                                                                     \
	do {                                                                                                                                             \
		status = calltree.status;                                                                                                                    \
		if (status) goto free_stamp;                                                                                                                 \
	} while (0)

PC_status_t PC_make_err(PC_status_t status, const char* message, ...);

#endif // STATUS_H__
