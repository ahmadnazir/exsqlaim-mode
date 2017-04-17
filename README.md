# exsqlaim-mode

[![Build Status](https://travis-ci.org/ahmadnazir/exsqlaim-mode.svg?branch=master)](https://travis-ci.org/ahmadnazir/exsqlaim-mode)

Emacs minor mode for supporting the use of variables inside sql queries.

This project is inspired from [restclient-mode][restclient-mode]

## Enabling exsqlaim-mode

`exsqlaim-mode` is a minor mode for `sql-mode`. You can enable it automatically, when `sql-mode` is enabled:

```
(add-hook 'sql-mode-hook 'exsqlaim-mode)
```

## Example

Variables can be defined as follows:

```
@db   = `test`
@id   =  1234
@name = "John Doe"
```

The defined variables can be used inside queries:

```
SELECT * FROM @db.users WHERE id = @id OR name = @name;
```

## Keybindings

`C-c C-c` : Execute / Send the query to the sql process
`C-c C-i` : Update the query so that the variables are replaced by the values


[restclient-mode]: https://github.com/pashky/restclient.el
