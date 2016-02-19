[![Build Status](https://travis-ci.org/powerman/perl-DBIx-SecureCGI.svg?branch=master)](https://travis-ci.org/powerman/perl-DBIx-SecureCGI)
[![Coverage Status](https://coveralls.io/repos/powerman/perl-DBIx-SecureCGI/badge.svg?branch=master)](https://coveralls.io/r/powerman/perl-DBIx-SecureCGI?branch=master)

# NAME

DBIx::SecureCGI - Secure conversion of CGI params hash to SQL

# VERSION

This document describes DBIx::SecureCGI version v2.0.7

# SYNOPSIS

    #--- sync

    use DBIx::SecureCGI;

    $row   = $dbh->Select('Table',             \%Q);
    @rows  = $dbh->Select(['Table1','Table2'], {%Q, id_user=>$id});
    $count = $dbh->Count('Table',        {age__gt=>25});
    $id    = $dbh->ID('Table',           {login=>$login, pass=>$pass});
    @id    = $dbh->ID('Table',           {age__gt=>25});
    $newid = $dbh->Insert('Table',       \%Q);
    $rv    = $dbh->InsertIgnore('Table', \%Q);
    $rv    = $dbh->Update('Table',       \%Q);
    $rv    = $dbh->Replace('Table',      \%Q);
    $rv    = $dbh->Delete('Table',       \%Q);
    $rv    = $dbh->Delete(undef,         {id_user=>$id});

    @rows  = $dbh->All('SELECT * FROM Table WHERE id_user=?', $id);
    $row   = $dbh->Row('SELECT * FROM Table WHERE id_user=?', $id);
    @col   = $dbh->Col('SELECT id_user FROM Table');

    $SQL   = $dbh->GetSQL(['Table1','Table2'], \%Q);
    $cache = $dbh->TableInfo(['Table1','Table2']);
    $desc  = $dbh->ColumnInfo('Table');


    #--- async

    use AnyEvent::DBI::MySQL;
    use DBIx::SecureCGI;

    $dbh->Select(…,       sub { my (@rows)        = @_; … });
    $dbh->Count(…,        sub { my ($count)       = @_; … });
    $dbh->ID(…,           sub { my (@id)          = @_; … });
    $dbh->Insert(…,       sub { my ($newid, $dbh) = @_; … });
    $dbh->InsertIgnore(…, sub { my ($rv, $dbh)    = @_; … });
    $dbh->Update(…,       sub { my ($rv, $dbh)    = @_; … });
    $dbh->Replace(…,      sub { my ($rv, $dbh)    = @_; … });
    $dbh->Delete(…,       sub { my ($rv, $dbh)    = @_; … });

    $dbh->All(…, sub { my (@rows) = @_; … });
    $dbh->Row(…, sub { my ($row)  = @_; … });
    $dbh->Col(…, sub { my (@col)  = @_; … });

    $dbh->GetSQL(…,     sub { my ($SQL)   = @_; … });
    $dbh->TableInfo(…,  sub { my ($cache) = @_; … });
    $dbh->ColumnInfo(…, sub { my ($desc)  = @_; … });


    #--- setup

    DBIx::SecureCGI::DefineFunc( $name, '%s op %s' )
    DBIx::SecureCGI::DefineFunc( $name, [ qr/regexp/, '%s op %s' ] )
    DBIx::SecureCGI::DefineFunc( $name, sub { … } )

    $cache = $dbh->SecureCGICache();
    $dbh->SecureCGICache($new_cache);

# DESCRIPTION

This module let you use **hash with CGI params** to make (or just generate)
SQL queries to MySQL database in **easy and secure** way. To make this
magic possible there are some limitations and requirements:

- Your app and db scheme must conform to these ["CONVENTIONS"](#conventions)
- Small speed penalty/extra queries to load scheme from db
- No support for advanced SQL, only basic queries

Example: if all CGI params (including unrelated to db table 'Table') are
in `%Q`, then:

    @rows = $dbh->Select('Table', \%Q);

will execute any simple `SELECT` query from the table `Table` (defined
by user-supplied parameters in `%Q`); and this:

    @user_rows = $dbh->Select('Table', {%Q, id_user=>$id});

will make any similar query limited to records with `id_user` column
value `$id` (thus allowing user to fetch any or **his own** records).

The module is intended for use only with a fairly simple tables and simple
SQL queries. More advanced queries usually can be generated manually with
help of ["GetSQL"](#getsql) or you can just use plain [DBI](https://metacpan.org/pod/DBI) methods.

Also it support **non-blocking SQL queries** using [AnyEvent::DBI::MySQL](https://metacpan.org/pod/AnyEvent::DBI::MySQL)
and thus can be effectively used with event-based CGI frameworks like
[Mojolicious](https://metacpan.org/pod/Mojolicious) or with event-based FastCGI servers like [FCGI::EV](https://metacpan.org/pod/FCGI::EV).

Finally, it can be used in non-CGI environment, as simplified interface to
[DBI](https://metacpan.org/pod/DBI).

## SECURITY OVERVIEW

At a glance, generating SQL queries based on untrusted parameters sent by
user to your CGI looks very unsafe. But interface of this module designed
to make it safe - while you conform to some ["CONVENTIONS"](#conventions) and follow
some simple guidelines.

- **User have no control over query type (SELECT/INSERT/…)**

    It's defined by method name you call.

- **User have no control over tables involved in SQL query**

    It's defined by separate (first) parameter in all methods, unrelated to
    hash with CGI parameters.

- **User have no direct control over SQL query**

    All values from hash are either quoted before inserting into SQL, or
    checked using very strict regular expressions if it's impossible to quote
    them (like for date/time `INTERVAL` values).

- **You can block/control access to "secure" fields in all tables**

    Name all such fields in some special way (like beginning with "`_`") and
    when receiving CGI parameters immediately **delete all keys** in hash which
    match these fields (i.e. all keys beginning with "`_`"). Later you can
    analyse user's request and manually add to hash keys for these fields
    before call method to execute SQL query.

- **You can limit user's access to some subset of records**

    Just instead of using plain `\%Q` as parameter for methods use
    something like `{ %Q, id_user => $id }` - this way user will be
    limited to records with `$id` value in `id_user` column.

Within these security limitations user can do anything - select records
with custom `WHERE`, `GROUP BY`, `ORDER BY`, `LIMIT`; set any values
(allowed by table scheme, of course) for any fields on `INSERT` or
`UPDATE`; etc. without any single line of your code - exclusively by
using different CGI parameters.

# HOW IT WORKS

Each CGI parameter belongs to one of three categories:

- **related to some table's field in db:** `fieldname`,
`fieldname__funcname`
- **control command:** `__commandname`
- **your app's parameter**

It's recommended to name fields in db beginning with **lowercase** letter
or **underscore**, and name your app's parameters beginning with
**Uppercase** letter to avoid occasional clash with field name.

To protect some fields (like "`balance`" or "`privileges`") from
uncontrolled access you can use simple convention: name these fields in db
beginning with "`_`"; when receiving CGI params just
**delete all with names beginning with** "`_`" - thus it won't be possible
to access these fields from CGI params. This module doesn't know about
these protected fields and handle them just as usual fields. So, you
should later add needed keys for these fields into hash before calling
methods to execute SQL query. This way all operations on these fields will
be controlled by your app.

You can use any other similar naming scheme which won't conflict with
["CONVENTIONS"](#conventions) below - DBIx::SecureCGI will analyse db scheme (and
cache it for speed) to detect which keys match field names.

CGI params may have several values. In hash, keys for such params must
have `ARRAYREF` value. DBIx::SecureCGI support this only for keys which
contain "`__`" (double underscore). Depending on used CGI framework you
may need to convert existing CGI parameters into this format.

Error handling: all unknown keys will be silently ignored, all other
errors (unable to detect key for joining table, field without
"`__funcname`" have `ARRAYREF` value, unknown "`__funcname`" function, etc.)
will return usual DBI errors (or throw exceptions when `{RaiseError=>1}`.

## CONVENTIONS

- Each table's **first field** must be a `PRIMARY KEY`.

    >     MOTIVATION: This module use simplified analyse of db scheme and suppose
    >     first field in every table is a `PRIMARY KEY`. To add support for complex
    >     primary keys or tables without primary keys we should first define how
    >     ["ID"](#id) should handle them and how to automatically join such tables.

- Two tables are always `JOIN`ed using field which must be `PRIMARY KEY`
at least in one of them and have **same name in both tables**.

    >     So, don't name your primary key "`id`" if you plan to join this table with
    >     another - name it like "`id_thistable`" or "`thistableId`".

    If both tables have field corresponding to `PRIMARY KEY` in other table,
    then key field of **right table** (in order defined when you make array of
    tables in first param of method) will be used.

    If more than two tables `JOIN`ed, then each table starting from second
    one will try to join to each of the previous tables (starting at first
    table) until it find table with suitable field. If it wasn't found
    DBI error will be returned.

    >     MOTIVATION: Let this module automatically join tables.

- Field names must not contain "`__`" (two adjoined underscore).

    >     MOTIVATION: Distinguish special commands for this module from field names.
    >     Also, some methods sometimes create aliases for fields and their names
    >     begins with "`__`".

- Hash with CGI params may contain several values (as `ARRAYREF`) only for key
names containing "`__`" (keys unrelated to fields may have any values).

    >     MOTIVATION: Allowing `{ field => \@values }` introduce many
    >     ambiguities and in fact same as `{ field__eq => \@values }`,
    >     so it's safer to deny it.

## Hash to SQL conversion rules

### \_\_commandname

Keys beginning with "`__`" are control commands. Supported commands are:

- **\_\_order**

    Define value for `ORDER BY`. Valid values are:

        'field_name'
        'field_name ASC'
        'field_name DESC'

    Multiple values can be given as `ARRAYREF`.

- **\_\_group**

    Define value for `GROUP BY`. Valid values are same as for **\_\_order**.

- **\_\_limit**

    Can have up to two numeric values (when it's `ARRAYREF`), set `LIMIT`.

- **\_\_force**

    If the value of **\_\_force** key is true, then it's allowed to run
    ["Update"](#update) and ["Delete"](#delete) with an empty `WHERE`. (This isn't a security
    feature, it's just for convenience to protect against occasional damage on
    database while playing with CGI parameters.)

Examples:

    my @rows = $dbh->Select('Table', {
       age__ge => 20,
       age__lt => 30,
       __group => 'age',
       __order => ['age DESC', 'fname'],
       __limit => 5,
    });
    $dbh->Delete('Table', { __force => 1 });

### fieldname\_\_funcname

If the key contains a "`__`" then it is treated as applying function
"`funcname`" to field "`fieldname`".
If the there is no field with such name in database, this key is ignored.
A valid key value - string/number or a reference to an array of
strings/numbers.
A list of available functions in this version is shown below.

Unless special behavior mentioned functions handle `ARRAYREF` value by
applying itself to each value in array and joining with `AND`.

Example:

    { html__like => ['%<P>%', '%<BR>%'] }

will be transformed in SQL to

    html LIKE '%<P>%' AND html LIKE '%<BR>%'

Typically, such keys are used in `WHERE`, except when "`funcname`" begins
with "`set_`" - such keys will be used in `SET`.

### fieldname

Other keys are treated as names of fields in database.
If there is no field with such name, then key is ignored.
A valid value for these keys - scalar.

Example:

    { name => 'Alex' }
    

will be transformed in SQL to

    name = 'Alex'

Typically, such keys are used in part `SET`, except for `PRIMARY KEY`
field in ["Update"](#update) - it will be used in `WHERE`.

# INTERFACE

## Functions

### DefineFunc

    DBIx::SecureCGI::DefineFunc( $name, '%s op %s' );
    DBIx::SecureCGI::DefineFunc( $name, [ qr/regexp/, '%s op %s' ] );
    DBIx::SecureCGI::DefineFunc( $name, sub { … } );

Define new or replace existing function applied to fields after "`__`"
delimiter.

SQL expression for that function will be generated in different ways,
depending on how you defined that function - using string, regexp+string
or code:

    $expr = sprintf '%s op %s', $field, $dbh->quote($value);
    $expr = $value =~ /regexp/ && sprintf '%s op %s', $field, $value;
    $expr = $code->($dbh, $field, $value);

If `$expr` will be false DBI error will be returned.
Here is example of code implementation:

    sub {
        my ($dbh, $f, $v) = @_;
        if (… value ok …) {
            return sprintf '…', $f, $dbh->quote($v);
        }
        return;     # wrong value
    }

## Methods injected into DBI

### GetSQL

    $SQL = $dbh->GetSQL( $table,   \%Q );
           $dbh->GetSQL( $table,   \%Q, sub { my ($SQL) = @_; … } );
    $SQL = $dbh->GetSQL( \@tables, \%Q );
           $dbh->GetSQL( \@tables, \%Q, sub { my ($SQL) = @_; … } );

This is helper function which will analyse (cached) database scheme for
given tables and generate elements of SQL query for given keys in `%Q`.
You may use it to write own methods like ["Select"](#select) or ["Insert"](#insert).

In `%Q` keys which doesn't match field names in `$table` / `@tables`
are ignored.

Names of tables and fields in all keys (except `{Table}` and `{ID}`)
are already quoted, field names qualified with table name (so they're
ready for inserting into SQL query). Values of `{Table}` and `{ID}`
should be escaped with `$dbh->quote_identifier()` before using in SQL
query.

Returns `HASHREF` with keys:

    {Table}        first of the used tables
    {ID}           name of PRIMARY KEY field in {Table}
    {Select}       list of all field names which should be returned by
                   'SELECT *' excluding duplicated fields (when field with
                   same name exist in many tables only field from first table
                   will be returned); field names in {Select} are joined with ","
    {From}         all tables joined using chosen JOIN type (INNER by default)
    {Set}          string like "field=value, field2=value2" for all simple
                   "fieldname" keys in %Q
    {Where}        a-la {Set}, except fields joined using "AND" and added
                   "field__function" fields; if there are no fields it will
                   be set to string "1"
    {UpdateWhere}  a-la {Where}, except it uses only "field__function" keys
                   plus one PRIMARY KEY "fieldname" key (if it exists in %Q)
    {Order}        string like "field1 ASC, field2 DESC" or empty string
    {Group}        a-la {Order}
    {Limit}        set to value of __limit if it contain one number
    {SelectLimit}  set to value of __limit if it contain one number,
                   or to values of __limit joined with "," if it contain
                   two numbers

Example :

    CREATE TABLE A (
       id_a    INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
       i       INT NOT NULL
    );
    CREATE TABLE B (
       id_b    INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
       id_a    INT NOT NULL,
       s       VARCHAR(255) NOT NULL
    );

    $SQL = $dbh->GetSQL(['A', 'B LEFT'], {
       id_a        => 3,
       i           => 10,
       s           => 'str',
       id_b__gt    => 5,
       __group     => 'i',
       __order     => ['s DESC', 'i'],
       __limit     => [50,10],
    });

    # now %$SQL have these values:
    # (backticks added by $dbh->quote_identifier() around all table/field
    # names omitted for readability)
    Table       => 'A'
    ID          => 'id_a'
    Select      => 'A.id_a, A.i, B.id_b, B.s'
    From        => 'A LEFT JOIN B ON (B.id_a = A.id_a)'
    Set         => 'B.s = "str",    A.id_a = 3,    A.i = 10'
    Where       => 'B.s = "str" AND A.id_a = 3 AND A.i = 10 AND B.id_b > 5'
    UpdateWhere => '                A.id_a = 3              AND B.id_b > 5'
    Group       => 'A.i'
    Order       => 'B.s DESC, A.i'
    Limit       => ''
    SelectLimit => '50,10'

### Insert

    $newid = $dbh->Insert( $table, \%Q );
             $dbh->Insert( $table, \%Q, sub { my ($newid, $dbh) = @_; … } );

Execute SQL query:

    INSERT INTO {Table} SET {Set}

Return `$dbh->{mysql_insertid}` on success or `undef` on error.

It's **strongly recommended** to always use

    $dbh->Insert( …, { %Q, …, primary_key_name=>undef }, … )

because if you didn't force `primary_key` field to be `NULL` in SQL (and
thus use `AUTO_INCREMENT` value) then user may send CGI parameter to set
it to `-1` or `4294967295` and this will result in **DoS** because no
more records can be added using `AUTO_INCREMENT` into this table.

### InsertIgnore

    $rv = $dbh->InsertIgnore( $table, \%Q );
          $dbh->InsertIgnore( $table, \%Q, sub { my ($rv, $dbh) = @_; … } );

Execute SQL query:

    INSERT IGNORE INTO {Table} SET {Set}

Return `$rv` (true on success or `undef` on error).

### Update

    $rv = $dbh->Update( $table, \%Q );
          $dbh->Update( $table, \%Q, sub { my ($rv, $dbh) = @_; … } );

Execute SQL query:

    UPDATE {Table} SET {Set} WHERE {UpdateWhere} [LIMIT {Limit}]

Uses in `SET` part all fields given as "`fieldname`", in `WHERE` part all
fields given as "`fieldname__funcname`" plus `PRIMARY KEY` field if it was
given as "`fieldname`".

Return `$rv` (amount of modified records on success or `undef` on error).

To use with empty `WHERE` part require `{__force=>1}` in `%Q`.

### Replace

    $rv = $dbh->Replace( $table, \%Q );
          $dbh->Replace( $table, \%Q, sub { my ($rv, $dbh) = @_; … } );

Execute SQL query:

    REPLACE INTO {Table} SET {Set}

Uses in `SET` part all fields given as "`fieldname`".

Return `$rv` (true on success or `undef` on error).

### Delete

    $rv = $dbh->Delete( $table,   \%Q );
          $dbh->Delete( $table,   \%Q, sub { my ($rv, $dbh) = @_; … } );
    $rv = $dbh->Delete( \@tables, \%Q );
          $dbh->Delete( \@tables, \%Q, sub { my ($rv, $dbh) = @_; … } );
    $rv = $dbh->Delete( undef,    \%Q );
          $dbh->Delete( undef,    \%Q, sub { my ($rv, $dbh) = @_; … } );

Execute SQL query:

    DELETE FROM {Table} WHERE {Where} [LIMIT {Limit}]

Delete records from `$table` or (one-by-one) from each table in
`@tables`. If `undef` given, then delete records from **ALL** tables
(except `TEMPORARY`) which have **ALL** fields mentioned in `%Q`.

To use with empty `WHERE` part require `{__force=>1}` in `%Q`.

Return `$rv` (amount of deleted records or `undef` on error).
If used to delete records from more than one table - return `$rv`
for last table. If error happens it will be immediately returned,
so some tables may not be processed in this case.

### ID

    $id = $dbh->ID( $table,   \%Q );
    @id = $dbh->ID( $table,   \%Q );
          $dbh->ID( $table,   \%Q, sub { my (@id) = @_; … } );
    $id = $dbh->ID( \@tables, \%Q );
    @id = $dbh->ID( \@tables, \%Q );
          $dbh->ID( \@tables, \%Q, sub { my (@id) = @_; … } );

Return result of executing this SQL query using ["Col"](#col):

    SELECT {ID} FROM {From} WHERE {Where}
        [ORDER BY {Order}] [LIMIT {SelectLimit}]

### Count

    $count = $dbh->Count( $table,   \%Q );
             $dbh->Count( $table,   \%Q, sub { my ($count) = @_; … } );
    $count = $dbh->Count( \@tables, \%Q );
             $dbh->Count( \@tables, \%Q, sub { my ($count) = @_; … } );

Return result of executing this SQL query using ["Col"](#col):

    SELECT count(*) __count FROM {From} WHERE {Where}

### Select

    $row  = $dbh->Select( $table,   \%Q );
    @rows = $dbh->Select( $table,   \%Q );
            $dbh->Select( $table,   \%Q, sub { my (@rows) = @_; … } );
    $row  = $dbh->Select( \@tables, \%Q );
    @rows = $dbh->Select( \@tables, \%Q );
            $dbh->Select( \@tables, \%Q, sub { my (@rows) = @_; … } );

Execute one of these SQL queries (depending on using `__group` command):

    SELECT * FROM {From} WHERE {Where}
        [ORDER BY {Order}] [LIMIT {SelectLimit}]
    SELECT *, count(*) __count FROM {From} WHERE {Where} GROUP BY {Group}
        [ORDER BY {Order}] [LIMIT {SelectLimit}]

Instead of `SELECT *` it uses enumeration of all fields qualified using
table name; if same field found in several tables it's included only
one - from first table having that field.

In `@tables` you can append `' LEFT'` or `' INNER'` to table name to
choose `JOIN` variant (by default `INNER JOIN` will be used):

    $dbh->Select(['TableA', 'TableB LEFT', 'TableC'], …)

Return result of executing SQL query using ["All"](#all) when called in list
context or ["Row"](#row) when called in scalar context.

### All

    @rows = $dbh->All( $sql, @bind )
            $dbh->All( $sql, @bind, sub { my (@rows) = @_; … } );

Shortcut for this ugly but very useful snippet:

    @{ $dbh->selectall_arrayref($sql, {Slice=>{}}, @bind) }

### Row

    $row = $dbh->Row( $sql, @bind );
           $dbh->Row( $sql, @bind, sub { my ($row) = @_; … } );

Shortcut for:

    $dbh->selectrow_hashref($sql, undef, @bind)

If you wonder why it exists, the answer is simple: it was added circa
2002, when there was no `$dbh->selectrow_hashref()` and now it
continue to exists for compatibility and to complement ["All"](#all)
and ["Col"](#col).

### Col

    $col = $dbh->Col( $sql, @bind );
    @col = $dbh->Col( $sql, @bind );
           $dbh->Col( $sql, @bind, sub { my (@col) = @_; … } );

Shortcut for:

    $col = $dbh->selectcol_arrayref($sql, undef, @bind)->[0];
    @col = @{ $dbh->selectcol_arrayref($sql, undef, @bind) };

### SecureCGICache

    $cache = $dbh->SecureCGICache();
    $cache = $dbh->SecureCGICache( $new_cache );

Fetch (or set when `$new_cache` given) `HASHREF` with cached results of
"`DESC tablename`" SQL queries for all tables used previous in any methods.

You may need to reset cache (by using `{}` as `$new_cache` value) if
you've changed scheme for tables already accessed by any method or if you
changed current database.

Also in some environments when many different `$dbh` used simultaneously,
connected to same database (like in event-based environments) it may make
sense to share same cache for all `$dbh`.

### TableInfo

    $cache = $dbh->TableInfo( $table );
             $dbh->TableInfo( $table,   sub { my ($cache) = @_; … } );
    $cache = $dbh->TableInfo( \@tables );
             $dbh->TableInfo( \@tables, sub { my ($cache) = @_; … } );

Ensure "`DESC tablename`" for all `$table` / `@tables` is cached.

Return same as ["SecureCGICache"](#securecgicache) on success or `undef` on error.

### ColumnInfo

    $desc = $dbh->ColumnInfo( $table );
            $dbh->ColumnInfo( $table, sub { my ($desc) = @_; … } );

Ensure "`DESC $table`" is cached.

Return result of `$dbh->All("DESC $table")` on success or `undef` on
error.

## \_\_funcname functions for fields

These functions can be added and replaced using ["DefineFunc"](#definefunc).

Functions which can be used in `%Q` as "`fieldname_funcname`":

### eq, ne, lt, gt, le, ge

    field =  value     field IS NULL
    field != value     field IS NOT NULL
    field <  value
    field >  value
    field <= value
    field >= value

For functions **eq** or **ne**:

    eq []            - NOT 1
    ne []            - NOT 0
    eq only    undef - name IS NULL
    ne only    undef - name IS NOT NULL
    eq without undef - name IN (...)
    ne without undef - (name IS NULL OR name NOT IN (...))
    eq with    undef - (name IS NULL OR name IN (...))
    ne with    undef - name NOT IN (...)

where

    "[]"           : name__func=>[]
    "only    undef": name__func=>undef    or name__func=>[undef]
    "without undef": name__func=>$defined or name__func=>[@defined]
    "with    undef": name__func=>[@defined_and_not_defined]

### like, not\_like

    field LIKE value
    field NOT LIKE value

### date\_eq, date\_ne, date\_lt, date\_gt, date\_le, date\_ge

    field =  DATE_ADD(NOW(), INTERVAL value)
    field != DATE_ADD(NOW(), INTERVAL value)
    field <  DATE_ADD(NOW(), INTERVAL value)
    field >  DATE_ADD(NOW(), INTERVAL value)
    field <= DATE_ADD(NOW(), INTERVAL value)
    field >= DATE_ADD(NOW(), INTERVAL value)

value must match:

    /^-?\d+ (?:SECOND|MINUTE|HOUR|DAY|MONTH|YEAR)$/

### set\_add

    field = field + value

When used in ["Update"](#update) it will be in `SET` instead of `WHERE`.
It doesn't make sense to use this function with ["Insert"](#insert),
["InsertIgnore"](#insertignore) or ["Replace"](#replace).

### set\_date

    field = NOW()
    field = DATE_ADD(NOW(), INTERVAL value)

If it's value is (case-insensitive) string `'NOW'` then it'll use
`NOW()` else it will use `DATE_ADD(…)`.

When used in ["Insert"](#insert), ["InsertIgnore"](#insertignore), ["Update"](#update) and ["Replace"](#replace) it
will be in `SET`.

# LIMITATIONS

Only MySQL supported.

It's impossible to change `PRIMARY KEY` using ["Update"](#update) with:

    { id => $new_id, id__eq => $old_id }

because both "`id`" and "`id__eq`" will be in `WHERE` part:

    SET id = $new_id WHERE id = $new_id AND id = $old_id

and if we won't add `'id => $new_id'` in `WHERE` part if we have
` 'id__eq' `, then we'll have do use this

    $dbh->Func($table, {%Q, id_user=>$S{id_user}, id_user__eq=>$S{id_user})

in **all** CGI requests to protect against attempt to read someone else's
records or change own records's id\_user field by using `'id_user'`
or `'id_user__eq'` CGI params.

# SUPPORT

## Bugs / Feature Requests

Please report any bugs or feature requests through the issue tracker
at [https://github.com/powerman/perl-DBIx-SecureCGI/issues](https://github.com/powerman/perl-DBIx-SecureCGI/issues).
You will be notified automatically of any progress on your issue.

## Source Code

This is open source software. The code repository is available for
public review and contribution under the terms of the license.
Feel free to fork the repository and submit pull requests.

[https://github.com/powerman/perl-DBIx-SecureCGI](https://github.com/powerman/perl-DBIx-SecureCGI)

    git clone https://github.com/powerman/perl-DBIx-SecureCGI.git

## Resources

- MetaCPAN Search

    [https://metacpan.org/search?q=DBIx-SecureCGI](https://metacpan.org/search?q=DBIx-SecureCGI)

- CPAN Ratings

    [http://cpanratings.perl.org/dist/DBIx-SecureCGI](http://cpanratings.perl.org/dist/DBIx-SecureCGI)

- AnnoCPAN: Annotated CPAN documentation

    [http://annocpan.org/dist/DBIx-SecureCGI](http://annocpan.org/dist/DBIx-SecureCGI)

- CPAN Testers Matrix

    [http://matrix.cpantesters.org/?dist=DBIx-SecureCGI](http://matrix.cpantesters.org/?dist=DBIx-SecureCGI)

- CPANTS: A CPAN Testing Service (Kwalitee)

    [http://cpants.cpanauthors.org/dist/DBIx-SecureCGI](http://cpants.cpanauthors.org/dist/DBIx-SecureCGI)

# AUTHORS

Alex Efros &lt;powerman@cpan.org>

Nikita Savin &lt;asdfgroup@gmail.com>

# COPYRIGHT AND LICENSE

This software is Copyright (c) 2002-2014 by Alex Efros &lt;powerman@cpan.org>.

This is free software, licensed under:

    The MIT (X11) License
