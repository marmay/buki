#!/bin/sh

PORT=9000

if test ! -d data; then
  echo "Initializing database directory, because it does not exist yet."
  initdb -U postgres -D ./data
fi

dbdir=$(readlink -f ./data)
echo "Database directory will be $dbdir"
echo "Starting database server on port $PORT"
postgres -D $dbdir -k $dbdir -p $PORT

