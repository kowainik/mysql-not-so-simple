# mysql-not-so-simple

[![Hackage](https://img.shields.io/hackage/v/mysql-not-so-simple.svg)](https://hackage.haskell.org/package/mysql-not-so-simple)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/mysql-not-so-simple/badge/lts)](http://stackage.org/lts/package/mysql-not-so-simple)
[![Stackage Nightly](http://stackage.org/package/mysql-not-so-simple/badge/nightly)](http://stackage.org/nightly/package/mysql-not-so-simple)

MySQL interface

## For developers

### How to run tests?

Run the following command before running tests:

```shell
$ docker run --name test-mysql --rm -e MYSQL_ROOT_PASSWORD=password -e MYSQL_DATABASE=test_db -p 3306:3306 mysql:5.7.20
```
