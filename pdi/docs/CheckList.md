\page Checklists Checklists

# New patch release

To publish a small fix as a patch release:
* make your changes based on the version branch (`v${X}.${Y}`)
* change the version in `pdi/VERSION`
* change the version in `pdi/docs/Installation.md`
* change the version in `README.md`
* go over all `CHANGELOG.md` files and mark the just released version
* commit (or better, make a MR) in the version branch
* tag the new release: `git tag -m "PDI release ${VERSION}" -s "${VERSION}"`
* merge the version branch into master
* in the merge commit:
  - keep the content of `pdi/VERSION` from master (next with `-git` suffix)
  - keep the content of `pdi/docs/Installation.md` from master except with the
    version updated to the just released one
* push all that

# New minor or major release

To publish a new minor or major release:
* start from the master branch
* change the version in `pdi/VERSION`
* change the version in `pdi/docs/Installation.md`
* go over all `CHANGELOG.md` files and mark the just released version
* commit these changes into a new version branch (`v${X}.${Y}`)
* tag the new release: `git tag -m "PDI release ${VERSION}" -s "${VERSION}"`
* merge the version branch into master
* in the merge commit:
  - change the version in `pdi/VERSION` to the next one with the `-alpha` suffix
  - change the version in `pdi/docs/Installation.md` to the just released one
* push all that

# Dependency change

When changing the list of dependencies or just the version of one dependency:
* update `pdi/docs/Installation.md`,
* update PDI distribution CMakeLists.txt as well as all CMakeLists.txt actually using it,
* update the dockerfiles used for tests,
* prepare the update of the spack, deb & RPM packages for the next version.
