
```sh
# install postgres

# become postgres user
$ sudo -i -u postgres

# as postgres user, initialize the database
# (maybe only needed on arch linux?)
$ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

# As the postgres user, comment out all lines in the following file and add the
# following line.  This disables local trust-based authentication (any local
# user can login as any database user without a password), and enables
# password-based authentication (but only from localhost).
$ echo 'host all all 127.0.0.1/32 md5' >> /var/lib/postgres/data/pg_hba.conf

# create the goatass user for developement and testing
$ sudo -u postgres -- psql --command "CREATE ROLE mydbuser NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'mydbpass'"

# create the goatass database for developement
$ sudo -u postgres -- createdb mydb

# grant access to both dev db and testing db for goatass user
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE mydb TO mydbuser"

# restart postgres service
$ sudo systemctl restart postgresql

# as normal user, try accessing database
$ psql -U mydbuser -d mydb -h 127.0.0.1
```

note: (this is what happens when the database does not exist)
stack exec -- servant-on-heroku-api
servant-on-heroku-api: libpq: failed (FATAL:  role "mydbuser" does not exist
)
make: *** [Makefile:31: run] Error 1


here is some curl:

curl --verbose --request POST --header 'Content-Type: application/json' --data '{"author": "DG", "text": "Pretty good"}' 'http://localhost:8080/add-comment' | jq '.'
