# Perform package.yaml and ChangeLog.ms checks
# If all checks passed, build Haddock and upload a library to Hackage

# Perform whole-package checks: package version and up-to-date ChangeLog 
PACKAGE_VERSION=$( cat package.yaml | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' )
if [ -z $PACKAGE_VERSION ] ; then
    echo -e "\u001b[31mYou must specify 4-digit package version in package.yaml!\u001b[0m"
    echo -e "\u001b[31me.g. version: 1.2.3.4\u001b[0m"
    exit 1
fi
CHANGELOG_VERSION=$( cat ChangeLog.md | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n 1 )
if [ $PACKAGE_VERSION != $CHANGELOG_VERSION ] ; then
    echo -e "\u001b[31mChangeLog.md does not contain description for current package version!\u001b[0m"
    exit 1
else
    echo -e "Package version and ChangeLog.md up-to-date!"
fi

# If all checks passed, build Haddock and upload a library to Hackage
./test.sh && 
echo -e "\nBuilding doc..." && 
stack haddock --coverage && 
echo -e "\nUploading to Hackage..." && 
stack sdist && 
stack upload .
