
stack install
spacemonkey-make >&2

echo "\nDeleting old Elm files...\n" >&2
rm -fv ../client/src/CodeGen/*.elm >&2

echo "\nMoving Elm files...\n" >&2
mv -v CodeGen/*.elm ../client/src/CodeGen/ >&2

echo "\nDeleting Elm files...\n" >&2
rm -rfv CodeGen/*.elm >&2

