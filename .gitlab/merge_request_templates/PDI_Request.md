# List of things to check before making a MR

Before merging your code, please check the following:

* you have checked your code format:
  - [ ] you have checked that you respect all conventions specified in CONTRIBUTING.md;
  - [ ] you have checked that the indentation and formating conforms to the docs/formating.astyle;
  - [ ] you have documented with doxygen any new or changed function / class;
* you have correctly updated the copyright headers:
  - [ ] your institution is in the copyright header of every file you (substantially) modified;
  - [ ] you have checked that the end-year of the copyright there is the current one;
* you have updated the AUTHORS file:
  - [ ] you have added yourself to the AUTHORS file;
  - [ ] if this is a new contribution, you have added it to the AUTHORS file;
* [ ] you have added a line describing your changes to the Changelog;
* you have added everything to the user documentation:
  - [ ] any new CMake configuration option;
  - [ ] any change in the yaml config;
  - [ ] any change to the public or plugin API;
  - [ ] any other new or changed user-facing feature;
* [ ] you have added any new or changed yaml config format to the validation script;
* [ ] you have added unit tests for any new or improved feature;
* you have correctly linked your MR to one or more issues:
  - [ ] your MR solves an identified issue;
  - [ ] your commit contain the `Fix #issue` keyword to autoclose the issue when merged;
* [ ] you have added to this file any other check that was missing.
