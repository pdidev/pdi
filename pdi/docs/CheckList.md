# Checklists

## New release

When making a new release.

* change the version in `pdi/VERSION`
* change the version in `pdi/docs/Installation.md`

## Dependency change

When changing the list of dependencies or just the version of one dependency.

* update `pdi/docs/Installation.md`,
* update PDI distribution CMakeLists.txt as well as all CMakeLists.txt actually using it,
* update the dockerfiles used for tests,
* prepare the update of the spack, deb & RPM packages for the next version.

