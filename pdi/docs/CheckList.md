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

To publish a new minor or major release:
* start from the main branch
* change the version in `pdi/VERSION`
* change the version in `pdi/docs/Source_installation.md`
* change the version in `mock_pdi/PDIConfigVersion.cmake`
* go over all `CHANGELOG.md` files and mark the just released version
* commit these changes into a new version branch (`v${X}.${Y}`)
* tag the new release: `git tag -m "PDI release ${X}.${Y}.0" -s "${X}.${Y}.0"`
* merge the version branch into main `git merge --no-ff "v${X}.${Y}"`
* in the merge commit:
  - change the version in `pdi/VERSION` by increasing the minor and add the
    `-alpha` suffix
  - add an `[Unreleased]` section at the top of all `CHANGELOG.md` files
* push all that
* describe the release https://github.com/pdidev/pdi/releases
* publish new packages https://github.com/pdidev/pkgs
* publish new recipes  https://github.com/pdidev/spack

## Dependency change

When changing the list of dependencies or just the version of one dependency:
* update `pdi/docs/Source_installation.md`,
* update `spack.yaml`,
* update PDI distribution CMakeLists.txt as well as all CMakeLists.txt actually using it,
* update the dockerfiles used for tests,
* prepare the update of the spack, deb & RPM packages for the next version,
* change `.github/workflows/*.yml` to use the new docker test images.
