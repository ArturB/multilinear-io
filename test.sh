# Re-build, perform all tests and generate coverage report
# If all tests succeeded, copy coverage report to test/coverage
# and make a git commit&push

# Get package name
PACKAGE_NAME=$( cat package.yaml | grep '^name:' | grep -o '[a-zA-z0-9\-]\+$' )
# Generate build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="$( date +%y%j ).$TODAY_SEC"

echo -e "Checking $PACKAGE_NAME, build $BUILD_ID...\n"

# Perform library tests and push changes to git if all tests passed
if stack test --coverage ; then
    echo -e "\u001b[32mAll tests passed!\u001b[0m"
    echo -e "Pushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "All done!"
    exit 0
else
    echo -e "\u001b[31mSome tests didn't pass!\u001b[0m"
    exit 1
fi
