<!-- describe your changes here -->

<!-- add any context explaining how you designed your changes, why you made certain choices, so as to help review work -->

## List of things to check before making a PR

Before moving my pull request out of draft, I have self-assessed the following.

* My PR passes all tests in the CI.
* [ ] I have read `CONTRIBUTING.md` and checked my PR respects all conventions specified in it.
  - [ ] I have added unit tests for any new or improved feature.
  - [ ] I am listed in the `AUTHORS` file, with a mention of the work of this PR.
  - [ ] My PR solves (with `Fix #issue` in the commit message) or is related to (with `part of #issue`) an identified issue.
  - [ ] My institution is in the copyright header of every (substantially) modified file with the correct end-year.
  - [ ] I have documented all my code with Doxygen comments.

### Changes noticeable by a user or plugin developer

Either:

* [ ] My change is absolutely not noticeable by either a user, or a plugin developer, here's why:

<!-- explain here why your changes are not noticeable by users or plugin developers -->

or:

* [ ] I have added a line describing my changes to the Changelog, as described in the [Changelog](https://github.com/pdidev/pdi/blob/main/CONTRIBUTING.md#changelog) section of `CONTRIBUTING.md`.
* I have documented any user- or plugin developer-visible change in the documentation:
  - [ ] any new CMake configuration option;
  - [ ] any change in the YAML specification tree;
  - [ ] any change to the public or plugin API;
  - [ ] any other new or changed user-facing feature.

### Further changes

Some changes require further action, described in `CONTRIBUTING.md`.

* [ ] I did not change (add, remove, update) the required version of any dependency, or
  - [ ] I have followed the [Changing the dependencies](https://github.com/pdidev/pdi/blob/main/CONTRIBUTING.md#changing-the-dependencies) section of `CONTRIBUTING.md`.
* [ ] I did not change the user API in `pdi.h`, `pdi/__init__.py` or `pdi.f90.zpp`, or
  - [ ] I have followed the [Changing the user API](https://github.com/pdidev/pdi/blob/main/CONTRIBUTING.md#changing-the-user-api) section of `CONTRIBUTING.md`.
* [ ] I did not create any new plugin, or
  - [ ] I have followed the [Creating a plugin](https://github.com/pdidev/pdi/blob/main/CONTRIBUTING.md#creating-a-plugin) section of `CONTRIBUTING.md`.
