# mysql-not-so-simple

[![Hackage](https://img.shields.io/hackage/v/mysql-not-so-simple.svg)](https://hackage.haskell.org/package/mysql-not-so-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/mysql-not-so-simple/badge/lts)](http://stackage.org/lts/package/mysql-not-so-simple)
[![Stackage Nightly](http://stackage.org/package/mysql-not-so-simple/badge/nightly)](http://stackage.org/nightly/package/mysql-not-so-simple)

MySQL interface

## For developers

### How to run tests?

Ensure, that you've installed `mysql`:

```shell
$ brew install mysql
```

Run `mysql` server:

```shell
$ mysql.server start
```

Create `test_db`:

```shell
$ mysql -u root
mysql> CREATE DATABASE test_db;
```

Create password with name `password` for the `root` user:

```shell
mysql> ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'password';
```

Now you're finally ready to run tests!
