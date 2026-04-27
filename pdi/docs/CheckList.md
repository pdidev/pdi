# Checklists

## New patch release

To publish a small fix as a patch release:
* make your changes based on the version branch (`v${X}.${Y}`)
* change the version in `pdi/VERSION`
* change the version in `pdi/docs/Source_installation.md`
* change the version in `mock_pdi/PDIConfigVersion.cmake`
* commit (or better, make a MR) in the version branch
* merge the version branch into main
* in the merge commit:
  - keep the content of `pdi/VERSION` from main (next with `-alpha` suffix)
  - keep the content `pdi/docs/Source_installation.md` from main except
    with the version updated to the just released one
* tag the new release: `git tag -m "PDI release ${VERSION}" -s "${VERSION}"`
* push all that
* describe the release https://github.com/pdidev/pdi/releases
* publish new packages https://github.com/pdidev/pkgs
* publish new recipes  https://github.com/pdidev/spack

## New minor or major release

To publish a new minor or major release, you should use `bin/release`

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
