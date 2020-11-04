
stack install
spacemonkey-make >&2

echo "\nMoving Elm files...\n" >&2
mv -v CodeGen/*.elm ~/Dropbox/CS/Repos/spacemonkey/client/src/CodeGen/ >&2
rm -rfv CodeGen/ >&2

