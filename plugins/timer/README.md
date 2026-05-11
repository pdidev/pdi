data:
  ...

plugins:
  timer: 
    - timer_AAA: {start: "decl_hdf5_start_timer", stop: "decl_hdf5_stop_timer"}
    - timer_BBB: "decl_hdf5"
    - timer_CCC: [decl_hdf5, event_toto]
    - timer_DDD: 
        start: "event_toto_start_timer"
        stop: "event_toto_stop_timer"

# timer=0 if plugin is not activated
# using regex: .*_start_timer
