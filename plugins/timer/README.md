# Timer plugin {#timer_plugin}

The timer plugin allows measuring time spent by PDI and its plugins.

## Configuration {#timer_configuration}

Simple plugin build:
```yaml
plugins:
  timer: 
    - timer_A: {start: "decl_hdf5_start_timer", stop: "decl_hdf5_stop_timer"}
    - timer_B: "decl_hdf5"
    - timer_C: [toto, titi]
    - timer_D: 
        start: "begin_timing"
        stop: "end_timing"
```

The timer plugin configuration contains a list of timer names (e.g. `- timer_A`, `- timer_B`, etc). Each timer will record the time spent between the `start` and `stop` events.

`timer_A` uses a map-styled definition where both keys `start` and `stop` are mandatory.

`timer_B` uses a scalar-styled definition where only the prefix of the timer events is provided. The prefix can be either a user-defined name, or the name of the PDI plugins, such as `decl_hdf5`, `decl_netcdf`, `pycall`, etc. If the prefix is the PDI plugin, internal events named with `prefix_start_timer` and `prefix_stop_timer` will be emitted inside the plugin (except `set_value`). In the above example, `timer_B` is equivalent to `timer_A`.

`timer_C` uses a list-styled definition where a list of prefix is provided. If the name is not one of the PDI plugins' name, then it is the user's responsibility to emit the `prefix_start_timer` and `prefix_stop_timer` events. In this example, `timer_C` will record the time spent between `toto_start_timer`, `toto_stop_timer`, and between `titi_start_timer`, `titi_stop_timer`.

It is also possible to have different names for start and stop events such as `timer_D`. 

