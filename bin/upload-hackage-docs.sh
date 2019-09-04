#!/bin/bash

# Adapted from
# <https://begriffs.com/posts/2014-10-25-creating-package-hackage.html>,
# originally written by Edward Kmett

set -eo pipefail

if [ "$#" -ne 1 ]; then
    echo "Usage: bin/upload-hackage-docs.sh <HACKAGE-USER>"
    exit 1
fi

user=$1

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
    echo "Run this script in the top-level package directory"
    exit 1
fi

pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
    echo "Unable to determine package name"
    exit 1
fi

if [ -z "$ver" ]; then
    echo "Unable to determine package version"
    exit 1
fi

echo "Detected package: $pkg-$ver"

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

# Original command:
#   cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

# Using stack's cabal:
#   /Users/eschnett/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_2.2.0.1_ghc-8.4.3 haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

# This command was actually used by stack:
#   /Users/eschnett/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_2.2.0.1_ghc-8.4.3 --builddir=.stack-work/dist/x86_64-osx/Cabal-2.2.0.1 haddock --html --hoogle --html-location=../$pkg-$version/ --haddock-option=--hyperlinked-source

# # Combining the two above, and updating version numbers:
# /Users/eschnett/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_2.2.0.1_ghc-8.4.4 --builddir=.stack-work/dist/x86_64-osx/Cabal-2.2.0.1 haddock --html --hoogle --haddock-option=--hyperlinked-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

# Combining the two above, and updating version numbers:
/Users/eschnett/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_2.4.0.1_ghc-8.6.5 --builddir=$(stack path --dist-dir) haddock --html --hoogle --haddock-option=--hyperlinked-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

# cp -R "dist/doc/html/$pkg/" "$dir/$pkg-$ver-docs"
cp -R "./$(stack path --dist-dir)/doc/html/$pkg/" "$dir/$pkg-$ver-docs"

tar cvz -C "$dir" --format=ustar -f "$dir/$pkg-$ver-docs.tar.gz" "$pkg-$ver-docs"

curl -X PUT                                                     \
     -H 'Content-Type: application/x-tar'                       \
     -H 'Content-Encoding: gzip'                                \
     -u "$user"                                                 \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz"                \
     "https://hackage.haskell.org/package/$pkg-$ver/docs"
