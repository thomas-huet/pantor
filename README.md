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

## License

Copyright (C) 2015  [Thomas HUET](https://thomash.fr)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).
