# FlowVR C API #

## Presentation ##

FCA is  an attempt to provide  an easy-to-use but  nevertheless complete library
for writing FlowVR modules in C (compatible with the C89 standard).

At the moment, this API is far from complete and the API (obviously the ABI too)
is subject to change.


## Compilation ##

The compilation is done with cmake and should be pretty straightforward:

	# The FlowVR environment variables must be correctly defined!
    . $FLOWVR_PREFIX/bin/flowvr-suite-config.sh

	mkdir build/
	cd build/
	cmake ..
	make
	make install


## Usage ##

To use  this library in your program,  just include the “fca.h”  header and link
with the “fca” library (“-lfca” for most compilers).

For more information just look at the example.
