# FlowVR plugin examples

## **Environment setup**

### **PDI is not installed and FlowVR is embedded**

In case you have not installed PDI:

1. Build PDI with `-DBUILD_TESTING=ON` cmake flag

2. From your build directory, go to `build/FLOWVR_PLUGIN/src/FLOWVR_PLUGIN_pkg-build/examples`

3. Call `source flowvr-config.sh`

4. Your environment is ready to run examples

In case you want to run `flowvr-config.sh` from different directory:

1. Export PDI_STAGING with a path to `staging` directory in build folder: `export PDI_STAGING="/home/user123/pdi/build/staging"`

2. Call `source this/is/a/path/to/flowvr-config.sh`

3. Your environment is ready to run examples

### **PDI and Flowvr is installed**

1. Make sure you have run the `flowvr-config.sh` from `flowvr_installation_path/bin/flowvr-config.sh` (most likely `/usr/local/`)

2. If PDI installation path is different than FlowVR call also: `export PYTHONPATH=pdi_installation_path/flowvr/python`

3. Your environment is ready to run examples.

## **Running the examples**

1. For each example you have run the flowvrd. This can be done by calling `flowvrd --top` in separate shell (remember to setup environment)

2. In each example directory you can find `flowvr-origin` folder. There you can find pure flowvr source codes to compare them to source codes with PDI used

3. Enter the example directiory. Run `python example_name.py`

4. Call `flowvr example_name`.
