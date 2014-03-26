# erldoc

Source code of [erldoc.info](http://erldoc.info)

## Installation

**erldoc** use [PostgreSQL](http://www.postgresql.org/). So first, install Postres ;)

Then, create a user and database, and import the schema :

```sql
=# create user erldoc with password 'erldoc';
=# create database erldoc owner erldoc;
=# \c erldoc erldoc
=> \i <erldoc>/sql/schema.sql
```

Now, clone this repo :

```
git clone https://github.com/glejeune/erldoc
cd erldoc
```

Edit the `config/erldoc.config` file and change the database configuration (in the `docsrv` section) :

```erlang
...
{docsrv, [
  {db_host, "localhost"},
  {db_port, 5432},
  {db_database, "erldoc"},
  {db_username, "erldoc"},
  {db_password, "erldoc"}  %%% !!! please change this !!!
]},
...
```

OK, you can compile and start the server :

```
make
./erldoc -p
``` 

Enjoy !

## Authors

* Gregoire Lejeune : <gregoire.lejeune@free.fr>

## Licence

erldoc is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Grégoire Lejeune <<gregoire.lejeune@free.fr>>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

