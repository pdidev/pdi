# Appelscript to open a new terminal window and launch the fluid_insitu_freq module
# Need to be started from the fluid_insitu directory
tell application "Terminal"
   set fp to system attribute "FLOWVR_PREFIX" 
   set cp to system attribute "PWD" 
   do script "source " & fp&"/bin/flowvr-suite-config.sh; cd " & cp &"; python bin/fluid_insitu_freq.py 50"
end tell     
