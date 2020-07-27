# FlowVR

# Overview

[http://flowvr.sourceforge.net/FlowVRDoc.html]

FlowVR is a middleware library that eases development and execution of high performance interactive applications requiring to harness the power of computing nodes distributed in a cluster or grid. 
FlowVR has been used for virtual reality application, telepresence, multi-camera  real-time 3D    modeling, in situ visualization, 
steering of parallel molecular dynamics applications, etc.     FlowVR  reuses  and  extends the  data  flow  paradigm commonly   used  for   scientific   visualization  environments.    An application  is  seen  as   a  set  of  possibly  distributed  modules exchanging  data. Different  module assemblies can be  specified  without modification or recompilation of the modules. The application developper does not have to 
bother about networking issues. That's the job of FlowVR to manage and optimize data exchanges between modules.  FlowVR supports parallel modules, enabling to 
harness existing MPI  codes for instance. This  feature is commonly used for in-situ visualization or computational steering. 


# Getting started with FlowVR

## 0) Systems supported

  - Linux 32 and 64 bits.


## 1)  Code Access

Flowvr- code is accessible to all  from  the INRIA gitlab: 

https://gitlab.inria.fr/flowvr/flowvr-ex


To get the code: 

```console
git clone https://gitlab.inria.fr/flowvr/flowvr-ex.git
```

## 2) Dependencies
  
We provide a [flowvr-dep-install.sh](https://gitlab.inria.fr/flowvr/flowvr-ex/-/blob/master/flowvr-dep-install.sh) to install all needed dependencies on Ubuntu / Debian. You will find it at the root of the FlowVR source directory.
Here is the bare minimum you need:
{{{
build-essential gcc g++ python-dev openssh-server openssh-client make cmake cmake-curses-gui libhwloc-dev
}}}

## 3) Compiling

We rely on CMake to configure and compile all components of the FlowVR-Suite. In a first step, CMake probes your environment and defines the components that can be safely compiled, then it generates makefiles. 

First, create a BUILD directory and call 'ccmake' from it, specifying an install path as parameter:
```console
mkdir BUILD
cd BUILD
ccmake /path/to/SOURCE/dir -DCMAKE_INSTALL_PREFIX:PATH=/where/you/want/flowvr/installed
```

CMake launches a dialog inside your terminal. From this dialog, press 'c' for a first pass of 'configure'. CMake will display a list of options that can be customized. The main options are shown directly. If you need to fine-tune, press 't' to reveal a set of advanced options.

Once you're happy with your settings, press 'c' for a second pass of 'configure'. If everything went well, you may press 'g' to generate makefiles.

You can now compile from the BUILD directory. The '-j' option specifies a number of threads to compile with.
```console
make -j 4
make install
```

## 4) Setting the environment variables

For temporarily setting the environment variables required to compile and execute FlowVR applications, run:

```console
source  [path_to_your_flowvr_installation]/bin/flowvr-suite-config.sh
```

For a permanent installation, append it to your ~/.bashrc configuration file.

## 5) Changing the maximum shared memory segment size

(Optional step)
The FlowVR deamon uses shared memory segments to store messages and the default maximum segment size is a tight fit for FlowVR applications.

To set the maximum segment size at 1GB, follow these steps :


Execute this command as super user:


```console
sysctl -w kernel.shmmax=1073741824
```

You may also use:
```console
flowvr-setup-shmem
```

The maximum segment size is now set, but it will revert to its default value upon reboot.

If you want to make this change permanent, edit the /etc/sysctl.conf file and include:

```console
kernel.shmmax=1073741824
```

A reboot is needed to make the /etc/sysctl.conf configuration effective

  
Check the parameters with:
```console
  sysctl -a kern.sysv
```

View allocated segments with:
```conosle
ipcs
```
Suppress an allocated segment with
```console
ipcrm -s shmid
```

## 6) Running the tests

You can run your first FlowVR application :

```console
flowvr-demo-tictac.sh
```

This script starts a simple FlowVR application consisting of two modules, 'put' and 'get'. 
'put' sends  "tic" and "tac"  messages (as strings) to the 'get' module.   

If everything went well, the following messages should appear on the standard output.

```console
...
Sent tic (it=0)
Received tic(it=0) from /put:text
Sent tac (it=1)
Received tac(it=1) from /put:text
Sent tic (it=2)
Received tic(it=2) from /put:text
Sent tac (it=3)
....
```

If they do not, you might have missed a command in step 4) or 5).

You can stop the application by typing _stop_ and pressing [ENTER].


### Fluid

If both GLUT and OpenGL are accounted for, a second test application can be launched using :
```console
flowvr-demo-fluid.sh
```

This is an interactive fluid simulation with an OpenGL visualization.
The basic interaction is described as follows:

 * Move the mouse to interact with the fluid. 
 * Click to add more fluid. 
 * Press the 'v' key to switch between density and  velocity field views.


As usual, you can stop the application by typing _stop_.

### Primes, simulator

These are additional FlowVR application examples : 

```console
flowvr-demo-primes.sh
```

```console
flowvr-demo-simulator.sh
```


### Fiddling with the examples

You will find sample applications in "INSTALL_DIR/share/flowvr/examples".

```console
cd tictac
./make-app.sh
python tictac.py

flowvrd -s 128M &
flowvr tictac

flowvr-kill
killall flowvrd

flowvr-glgraph tictac.net.xml
```



## 7) Next hop

The recommended next hop is the [flowvr-appy tutorial](https://gitlab.inria.fr/flowvr/flowvr-ex/-/blob/master/flowvr/flowvr-appy/README.md).

Other sources of documentation are the manual ([PDF version](http://flowvr.sourceforge.net/flowvr-manual-pdf.pdf)




