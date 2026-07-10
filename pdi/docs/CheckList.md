# Checklists

## New release

To publish a new release:
* use `bin/release`
* describe the release https://github.com/pdidev/pdi/releases
* publish new packages https://github.com/pdidev/pkgs
* publish new recipes  https://github.com/pdidev/spack

## Dependency change

When changing the list of dependencies or just the version of one dependency:
* update `pdi/docs/Source_installation.md`,
* update `spack.yaml`,
* update PDI distribution CMakeLists.txt as well as all CMakeLists.txt actually
  using it,
* update the dockerfiles used for tests,
* prepare the update of the spack, deb & RPM packages for the next version,
* change `.github/workflows/*.yml` to use the new docker test images,
* if the new minimum compiler versions support an updated version of C / C++ /
  Fortran, change this info in `pdi/docs/Source_installation.md` and
  `pdi/CONTRIBUTING.md` as well as every `c[xx]_std_??` in cmake.
