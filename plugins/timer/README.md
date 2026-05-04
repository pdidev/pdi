data:
  ...

plugins:
  timer:
    - recording: [decl_hdf5_start_timer, decl_hdf5_end_timer] # decl_hdf5_start/end_timer are events in PDI, not visible to users
    # - recording: [json_start_timer, json_end_timer] 
  decl_hdf5:
    - file: output.h5
      write: data
  json:
    - file: out.json
      write: [var1, var2]