##################################
# Install  FlowVR dependencies   #
##################################

#!/bin/bash




usage() {
  cat << EOT
Usage: flowvr-dep-install
       Probe OS and install dependencies (should be executed as root)
       Works with Debian, Ubuntu and Mac OS X. Modify the script to fit your needs. 
EOT
  exit 0
}



dependencies() {

    #GET the OS name
    OS=`uname`
    
    echo "Operating System: $OS"
    
    # Linux
    if [[ $OS == 'Linux' ]]; then
        
        echo ""
        echo "Debian/Ubuntu packages  - contrib repository required."
        echo ""
	sudo  apt-get update

	echo ""
	echo "Installing core FlowVR dependencies. Make sure you thoroughly check aptitude's solution before accepting it."
	echo ""
	# FlowVR core dependencies
	sudo apt-get install build-essential gcc g++ make cmake cmake-curses-gui python-dev openssh-server openssh-client

	sudo apt-get install swig # python module API
	sudo apt-get install python3 python3-dev # for some distribs

        
        sudo apt-get install openmpi-bin openmpi-common openmpi-doc libopenmpi-dev #openmpi. required for .netMPI plugin (high performance network support)

        sudo apt-get install hwloc libhwloc-dev #needed to control mapping of flowvr sharedmemory segments, daemons and modules. 

	echo ""
	echo "Installing graphics dependencies (tools and examples)."
	echo ""
	# Graphics (fluid, primes examples..)
	sudo apt-get install libxi-dev
	sudo apt-get install libxmu-dev
	sudo apt-get install freeglut3
	sudo apt-get install mesa-common-dev
	sudo apt-get install freeglut3-dev

       	echo ""
       	echo "Installing flowvr-glgraph dependencies."
       	echo ""
        # FlowVR-glgraph dependencies (Qt4, graphviz)
        sudo apt-get install libxml2-dev libxml2 libxslt1.1 libxslt1-dev libgraphviz-dev graphviz
	sudo apt-get install qt4-default
        
        # Misc dependencies
        #sudo aptitude install doxygen
        
        # VTK-FlowVR dependencies
        #sudo aptitude install libvtk5-dev python-vtk tcl-vtk
        
        # MPI (used by fluidmpi, mpi daemon)
        #sudo aptitude libopenmpi-dev libopenmpi1 mpi-default-bin
        
        #VRPN (used by contrib/flowvr-vrpn)
        #echo "VRPN 07_30 - Needs CMake >=  2.8.3"
        #wget ftp://ftp.cs.unc.edu/pub/packages/GRIP/vrpn/vrpn_07_30.zip
        #mkdir vrpn_07_30
        #cd vrpn_07_30
        #unzip ../vrpn_07_30.zip
        
        #cd vrpn
        #mkdir build
        #cd build
        #cmake ..
        #sudo make install
        
    fi
    
    #MAC:
    if [[ $OS == 'Darwin' ]]; then
        
        # Darwin port
        if !(which port &>/dev/null) then
            echo "Error: Install Darwin port first (http://darwinports.com/)"
            exit 0;
        fi
        
        echo "Install multiple flowvr dependencies through  darwin port"

        # wget
        sudo port install wget

        # cmake
        sudo port install cmake

        # GLEW
        sudo port install glew
        
        # graphviz
        sudo port install graphviz-devel
        
        # MPI 
        sudo port install openmpi
        
        # Doxygen
        sudo port install doxygen
        
        
        # swig
        sudo port install swig-python
        
        #QT
        echo "QT4"
        sudo port install qt4-mac
        
        #        wget http://releases.qt-project.org/qt4/source/qt-mac-opensource-4.8.4.dmg
        #        hdiutil mount qt-mac-opensource-4.8.4.dmg
        #        sudo  installer  -pkg /Volumes/Qt\ 4.8.4/Qt.mpkg -target /
        #        hdiutil unmount /Volumes/Qt\ 4.8.4


        
        # glui (required for flowvr-vrpn)      
        sudo port install glui
        
        #VRPN  (required for flowvr-vrpn)
        sudo port install vrpn
        # echo "VRPN"
        # wget ftp://ftp.cs.unc.edu/pub/packages/GRIP/vrpn/vrpn_07_26.zip
        # mkdir vrpn_07_26
        # cd vrpn_07_26
        # unzip ../vrpn_07_26.zip
        # cd quat
        # sudo make install HW_OS=universal_macosx 
        # cd ../vrpn
        # sudo  make install HW_OS=universal_macosx


        #python: carefull with conflicts between native python and macport  ones/ Force to use  the native one (to see list of available options: port select --list python)
        
        sudo port select --set python python27-apple
      
    fi
    echo ""
    echo "FlowVR dependencies installed. You can now compile and install FlowVR"
}


#main

finish=0

while [[ $finish != 1 ]]; do
    if [[ $1 == "-h" ]] ||  [[ $1 == "--help" ]]  ||  [[ $1 == "-help" ]]; then
        usage;
        exit 0;
    else
        dependencies;
        finish=1
    fi
done

