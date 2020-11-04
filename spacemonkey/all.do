
stack install
spacemonkey-make >&2

echo "\nDeleting old Elm files from client dir...\n" >&2
rm -fv ../client/src/CodeGen/*.elm >&2

echo "\nMoving new Elm files to client dir...\n" >&2
mv -v CodeGen/*.elm ../client/src/CodeGen/ >&2

echo "\nDeleting Elm files from server dir...\n" >&2
rm -rfv CodeGen/*.elm >&2

