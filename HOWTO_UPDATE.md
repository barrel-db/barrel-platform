# HOWTO update the sources

Platform integrate all barrel sources and its dependencies in one repository. When a 
new release happen the followig process should be done:


1. run `./support/import.sh`, this script import all deps and make the necessary cleaning.
2. create a release using the command line `make rel` and test according the readme
3. add missing files from lib: `git add lib/`
4. Tag the release: `git tag X.Y.Z` 
