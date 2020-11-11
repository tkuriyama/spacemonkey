echo "\nRemoving old database...\n" >&2
rm -vrf ../dbsqlite/spacemonkey.db >&2

echo "\nPopulating default values...\n" >&2
stack runghc -- scripts/Initialize.hs "../dbsqlite/spacemonkey.db" >&2
