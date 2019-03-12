// Includes the fca header.
#include "fca.h"

////////////////////////////////////////

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

////////////////////////////////////////

void
err(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
}

int
main()
{
	// Declares and  initializes a FlowVR module  with an input  port named “in”
	// and  an output  port named  “out”. Both  ports are  created  with default
	// parameters.
	fca_module mod = fca_new_module("in", fca_IN, 0,
	                                "out", fca_OUT, 0,
	                                NULL);

	if (mod == NULL)
	{
		err("The module initialization failed.");

		return EXIT_FAILURE;
	}

	// Gets the port from which we want to send our message.
	fca_port out = fca_get_port(mod, "out");

	// It should have succeed because we know such a port exist.
	assert(out != NULL);

	fca_stamp stamp = fca_register_stamp(out, "my_stamp",
	                                     fca_BINARY, 123);

	printf("%u - %s\n", fca_get_stamp_type(stamp), fca_get_stamp_name(stamp));

	// Allocates a new message for the message we want to send.
	fca_message msg = fca_new_message(mod, sizeof(float));

	// If the allocation failed, terminates the module properly and exits.
	if (msg == NULL)
	{
		fca_free(mod);

		err("The message allocation failed.");

		return EXIT_FAILURE;
	}

	// Puts the message in the message.
	{
		// Gets the data field of the message.
		float *p = fca_get_write_access(msg, 0);

		// Fills it with our message.
		*p = 3;
	}

	// “Sends” the message.
	bool success = fca_put(out, msg);

	// Frees the message.
	fca_free(msg);

	// Terminates the module.
	fca_free(mod);

	if (!success)
	{
		err("The message sending failed.");

		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
