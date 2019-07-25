\page Philosophy Philosophy of %PDI

Similarly to 
[aspect-oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming),
%PDI distinguishes between core concerns and cross-cutting concerns.

[Core concerns](https://en.wikipedia.org/wiki/Core_concern) are the aspects
of the code that fulfill its main goal.
We consider as core concerns of a simulation code the aspects handled in the
main loop that generate elements needed as input of the next iteration of the
loop.

[Cross-cutting concerns](https://en.wikipedia.org/wiki/Cross-cutting_concern)
are the aspect of the code that are not core concerns.
We consider as cross-cutting concerns of a simulation code:
* the aspects handled before the simulation main loop such as parameters
  reading, data initialization, etc,
* the aspects handled after the simulation main loop, such as result
  post-processing, storage to disk, visualization, etc,
* the aspects handled during the simulation main loop, **but** whose execution
  is not required for the next iteration of the loop, such as fault tolerance,
  logging, etc.

The simulation could run with none of the cross-cutting concerns implemented
(even if this would be completely useless with no result ever saved).
The deactivation of a core concern on the other hand would lead to a failure of
the simulation.

%PDI supports calling libraries from the 
\ref Specification_tree "specification tree" instead of from the code.
This is well suited for cross-cutting concerns but means that the code has no
control over what and how these aspects are handled which does not fit the needs
of core concerns.
