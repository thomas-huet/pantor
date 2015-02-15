# Pantor

A searchable index of all public torrents.

## Install

Install [PostgreSQL](http://www.postgresql.org/), create a database named `pantor` and set the environment variable `$PGDATABASE` to `pantor`.
Use `init.sql` to create the required table.

Install and run the crawler and wait for it to fill the database.
If the crawler and the database are not on the same machine, use the environment variables `$PGUSER`, `$PGPASSWORD`, `$PGHOST` and `$PGPORT` to tell the crawler how to connect to the database.

## How it works

Pantor scans the Bittorrent DHT to discover torrents and indexes them.

The crawler collects metadata about torrents and puts them in a PostgreSQL database.
The data are indexed by PostgreSQL and can be accessed via a web interface.

It has been inspired by this paper: [Crawling BitTorrent DHTs for Fun and Profit](https://jhalderm.com/pub/papers/dht-woot10.pdf)

## Author

[Thomas HUET](https://thomash.fr)
