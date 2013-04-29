package DBIx::SecureCGI;

use warnings;
use strict;
use utf8;
use feature ':5.10';
use Carp;

use version; our $VERSION = qv('2.0.0');    # REMINDER: update Changes

# REMINDER: update dependencies in Build.PL
use DBI;


## no critic (ProhibitPostfixControls Capitalization ProhibitEnumeratedClasses)

my $PRIVATE = 'private_' . __PACKAGE__;
my $INT     = qr/\A-?\d+\s+(?:SECOND|MINUTE|HOUR|DAY|MONTH|YEAR)\z/msi;
my $IDENT   = qr/((?!__)\w[a-zA-Z0-9]*(?:_(?!_)[a-zA-Z0-9]*)*)/ms;
my %Func    = ();


DefineFunc(eq => sub {
    my ($dbh, $f, $v) = @_;
    my (@val, $null, @expr);
    @val  = ref $v ? @{$v} : $v;
    $null = grep {!defined} @val;
    @val  = grep {defined} @val;
    push @expr, sprintf '%s IS NULL', $f                    if $null;
    push @expr, sprintf '%s = %s', $f, $dbh->quote($val[0]) if @val==1;
    push @expr, sprintf '%s IN (%s)',
        $f, join q{,}, map { $dbh->quote($_) } @val         if @val>1;
    push @expr, 'NOT 1'                                     if !@expr;
    return @expr==1 ? $expr[0] : '('.join(' OR ', @expr).')';
});
DefineFunc(ne => sub {
    my ($dbh, $f, $v) = @_;
    my (@val, $null, @expr);
    @val  = ref $v ? @{$v} : $v;
    $null = grep {!defined} @val;
    @val  = grep {defined} @val;
    push @expr, sprintf '%s IS NOT NULL', $f                if $null && !@val;
    push @expr, sprintf '%s IS NULL', $f                    if !$null && @val;
    push @expr, sprintf '%s != %s', $f,$dbh->quote($val[0]) if @val==1;
    push @expr, sprintf '%s NOT IN (%s)', $f,
        join q{,}, map { $dbh->quote($_) } @val             if @val>1;
    push @expr, 'NOT 0'                                     if !@expr;
    return @expr==1 ? $expr[0] : '('.join(' OR ', @expr).')';
});
DefineFunc(lt       => '%s <  %s');
DefineFunc(gt       => '%s >  %s');
DefineFunc(le       => '%s <= %s');
DefineFunc(ge       => '%s >= %s');
DefineFunc(like     => '%s LIKE %s');
DefineFunc(not_like => '%s NOT LIKE %s');
DefineFunc(date_eq  => [$INT, '%s =  DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(date_ne  => [$INT, '%s != DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(date_lt  => [$INT, '%s <  DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(date_gt  => [$INT, '%s >  DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(date_le  => [$INT, '%s <= DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(date_ge  => [$INT, '%s >= DATE_ADD(NOW(), INTERVAL %s)']);
DefineFunc(set_date => sub {
    my ($dbh, $f, $v) = @_;
    if (uc $v eq 'NOW') {
        return sprintf '%s = NOW()', $f;
    } elsif ($v =~ /$INT/mso) {
        return sprintf '%s = DATE_ADD(NOW(), INTERVAL %s)', $f, $dbh->quote($v),
    }
    return;
});
DefineFunc(set_add  => sub {
    my ($dbh, $f, $v) = @_;
    return sprintf '%s = %s + %s', $f, $f, $dbh->quote($v);
});


sub DefineFunc {
    my ($func, $cmd) = @_;
    if (!$func || ref $func || $func !~ /\A[A-Za-z]\w*\z/ms) {
        croak "bad function name: $func";
    }
    if (!ref $cmd) {
        if (2 != (() = $cmd =~ /%s/msg)) {
            croak "bad function: $cmd";
        }
    } elsif (ref $cmd eq 'ARRAY') {
        if (2 != @{$cmd}
                || ref $cmd->[0] ne 'Regexp'
                || (ref $cmd->[1] || 2 != (() = $cmd->[1] =~ /%s/msg))) {
            croak "bad function: [@$cmd]";
        }
    } elsif (ref $cmd ne 'CODE') {
        croak 'bad function';
    }
    $Func{$func} = $cmd;
    return;
}

sub _ret { ## no critic (RequireArgUnpacking)
    my $cb = shift;
    if ($cb) {
        return $cb->(@_);
    } else {
        return wantarray ? @_ : $_[0];
    }
}

sub _ret1 {
    my ($cb, $ret, $h) = @_;
    if ($cb) {
        return $cb->($ret, $h);
    } else {
        return $ret;
    }
}

sub _retdo {
    my ($dbh, $sql, $cb) = @_;
    if (!$cb) {
        return $dbh->do($sql);
    }
    return $dbh->do($sql, undef, $cb);
}

# Set cache to given HASHREF, if any.
# Initialize cache, if needed.
# Return current cache.
sub DBI::db::SecureCGICache {
    my ($dbh, $cache) = @_;
    if ($cache && ref $cache eq 'HASH') {
        $dbh->{$PRIVATE} = $cache;
    } else {
        $dbh->{$PRIVATE} //= {};
    }
    return $dbh->{$PRIVATE};
}

# Ensure $dbh->All("DESC $table") is cached.
# Return cached $dbh->All("DESC $table").
# On error set $dbh->err and return nothing.
sub DBI::db::ColumnInfo {
    my ($dbh, $table, $cb) = @_;
    my $cache = $dbh->SecureCGICache();
    if ($cache->{$table}) {
        return _ret($cb, $cache->{$table});
    }

    if (!$cb) {
        my @desc = $dbh->All('DESC '.$dbh->quote_identifier($table));
        return _set_column_info($dbh, $cache, $table, undef, @desc);
    }
    return $dbh->All('DESC '.$dbh->quote_identifier($table), sub {
        my @desc = @_;
        return _set_column_info($dbh, $cache, $table, $cb, @desc);
    });
}

sub _set_column_info {
    my ($dbh, $cache, $table, $cb, @desc) = @_;
    if (@desc) {
        my @pk = grep {$desc[$_]{Key} eq 'PRI'} 0 .. $#desc;
        if (1 != @pk || $pk[0] != 0) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "first field must be primary key: $table\n", undef, 'ColumnInfo'));
        }
        $cache->{$table} = \@desc;
    }
    return _ret($cb, $cache->{$table});
}

# Ensure DESC for all $tables cached.
# Return $dbh->SecureCGICache().
# On error set $dbh->err and return nothing.
sub DBI::db::TableInfo {
    my ($dbh, $tables, $cb) = @_;
    my @tables = ref $tables eq 'ARRAY' ? @{$tables} : ($tables);
    if (!@tables || grep {/\A\z|\s/ms} @tables) {
        return _ret($cb, $dbh->set_err($DBI::stderr, "bad tables: [@tables]\n", undef, 'TableInfo'));
    }

    if (!$cb) {
        while (@tables) {
            my $desc = $dbh->ColumnInfo(shift @tables);
            if (!$desc) {
                return;
            }
        }
        return $dbh->SecureCGICache();
    }
    my $code; $code = sub {
        my ($desc) = @_;
        if (!$desc) {
            undef $code;
            return $cb->();
        }
        if (@tables) {
            return $dbh->ColumnInfo(shift @tables, $code);
        }
        undef $code;
        return $cb->( $dbh->SecureCGICache() );
    };
    return $dbh->ColumnInfo(shift @tables, $code);
}

sub DBI::db::GetSQL {
    my ($dbh, $tables, $P, $cb) = @_;
    # remove possible JOIN info from table names for TableInfo()
    my @tables = map {my $s=$_;$s=~s/\s.*//ms;$s} ref $tables ? @{$tables} : $tables; ## no critic
    if (!$cb) {
        my $cache = $dbh->TableInfo(\@tables);
        return _get_sql($dbh, $cache, $tables, $P);
    }
    return $dbh->TableInfo(\@tables, sub {
        my $cache = shift;
        return _get_sql($dbh, $cache, $tables, $P, $cb);
    });
}

sub _get_sql { ## no critic (ProhibitExcessComplexity)
    my ($dbh, $cache, $tables, $P, $cb) = @_;
    if (!$cache) {
        return _ret($cb);
    }

    # Extract JOIN type info from table names
    my (@tables, @jointype);
    for (ref $tables eq 'ARRAY' ? @{$tables} : $tables) {
        if (!/\A(\S+)(?:\s+(LEFT|INNER))?\s*\z/msi) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "unknown join type: $_\n", undef, 'GetSQL'));
        }
        push @tables, $1;
        push @jointype, $2 // 'INNER';
    }

    my %SQL = (
        Table       => $tables[0],
        ID          => $cache->{ $tables[0] }[0]{Field},
        Select      => q{},
        From        => q{},
        Set         => q{},
        Where       => q{},
        UpdateWhere => q{},
        Order       => q{},
        Group       => q{},
        Limit       => q{},
        SelectLimit => q{},
    );

    # Detect keys which should be used for JOINing tables
    $SQL{From} = $dbh->quote_identifier($tables[0]);
    my @field = map {{ map {$_->{Field}=>1} @{ $cache->{$_} } }} @tables; ## no critic
TABLE:
    for my $right (1..$#tables) {
        ## no critic (ProhibitAmbiguousNames)
        my $rkey = $cache->{ $tables[$right] }[0]{Field};
        for my $left (0..$right-1) {
            my $lkey = $cache->{ $tables[$left] }[0]{Field};
            my $key = $field[$left]{$rkey}  ? $rkey :
                      $field[$right]{$lkey} ? $lkey : next;
            $SQL{From} .= sprintf ' %s JOIN %s ON (%s.%s = %s.%s)',
                $jointype[$right],
                map { $dbh->quote_identifier($_) }
                $tables[$right], $tables[$right], $key, $tables[$left], $key;
            next TABLE;
        }
        return _ret($cb, $dbh->set_err($DBI::stderr, "can't join table: $tables[$right]\n", undef, 'GetSQL'));
    }

    # Set $SQL{Select} using qualified field names and without duplicates
    my %qualify;
    for my $t (@tables) {
        for my $f (map {$_->{Field}} @{ $cache->{$t} }) {
            next if $qualify{$f};
            $qualify{$f} = sprintf '%s.%s',
                map { $dbh->quote_identifier($_) } $t, $f;
            $SQL{Select} .= ', '.$qualify{$f};
        }
    }
    $SQL{Select} =~ s/\A, //ms;

    # Set $SQL{Set}, $SQL{Where}, $SQL{UpdateWhere}
    for my $k (keys %{$P}) {
        $k =~ /\A$IDENT(?:__(?!_)$IDENT)?\z/ms or next; # ignore non-field keys
        my $f   = $qualify{$1} or next;                 # ignore non-field keys
        my $func= $2 // q{};
        my $cmd = $Func{$func || 'eq'};
        if (!$cmd) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "unknown function: $k\n", undef, 'GetSQL'));
        }
        if (!$func && ref $P->{$k}) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "ARRAYREF without function: $k\n", undef, 'GetSQL'));
        }
        # WARNING functions `eq' and `ne' must process value array themselves:
        my $is_list = ref $P->{$k} && $func ne 'eq' && $func ne 'ne';
        for my $v ($is_list ? @{$P->{$k}} : $P->{$k}) {
            my $expr
                = ref $cmd eq 'CODE'    ? $cmd->($dbh, $f, $v)
                : ref $cmd eq 'ARRAY'   ? ($v =~ /$cmd->[0]/ms && sprintf $cmd->[1], $f, $v)
                :                         sprintf $cmd, $f, $dbh->quote($v);
            if (!$expr) {
                return _ret($cb, $dbh->set_err($DBI::stderr, "bad value for $k: $v\n", undef, 'GetSQL'));
            }
            $SQL{Set}         .= ", $expr"    if !$func || $func =~ /\Aset_/ms;
            $SQL{Where}       .= " AND $expr" if           $func !~ /\Aset_/ms;
            $SQL{UpdateWhere} .= " AND $expr" if $func &&  $func !~ /\Aset_/ms;
            $SQL{UpdateWhere} .= " AND $expr" if $k eq $SQL{ID};
        }
    }
    $SQL{Set}         =~ s/\A, //ms;
    $SQL{Where}       =~ s/\A AND //ms;
    $SQL{UpdateWhere} =~ s/\A AND //ms;
    $SQL{Set}         =~ s/\s+IS\s+NULL/ = NULL/msg;
    $SQL{Where}       ||= '1';
    $SQL{UpdateWhere} ||= '1';

    # Set $SQL{Order} and $SQL{Group}
    for my $order (ref $P->{__order} ? @{$P->{__order}} : $P->{__order}) {
        next if !defined $order;
        if ($order !~ /\A(\w+)\s*( ASC| DESC|)\z/ms || !$qualify{$1}) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "bad __order value: $order\n", undef, 'GetSQL'));
        }
        $SQL{Order} .= ", $qualify{$1}$2";
    }
    for my $group (ref $P->{__group} ? @{$P->{__group}} : $P->{__group}) {
        next if !defined $group;
        if ($group !~ /\A(\w+)\s*( ASC| DESC|)\z/ms || !$qualify{$1}) {
            return _ret($cb, $dbh->set_err($DBI::stderr, "bad __group value: $group\n", undef, 'GetSQL'));
        }
        $SQL{Group} .= ", $qualify{$1}$2";
    }
    $SQL{Order} =~ s/\A, //ms;
    $SQL{Group} =~ s/\A, //ms;

    # Set $SQL{Limit}, $SQL{SelectLimit}
    my @limit = ref $P->{__limit} ? @{$P->{__limit}} : $P->{__limit} // ();
    for (grep {!m/\A\d+\z/ms} @limit) {
        return _ret($cb, $dbh->set_err($DBI::stderr, "bad __limit value: $_\n", undef, 'GetSQL'));
    }
    if (@limit == 1) {
        $SQL{Limit}       = " $limit[0]"; # make __limit=>0 true value
        $SQL{SelectLimit} = " $limit[0]"; # make __limit=>0 true value
    }
    elsif (@limit == 2) {
        $SQL{SelectLimit} = join q{,}, @limit;
    }
    elsif (@limit > 2) {
        return _ret($cb, $dbh->set_err($DBI::stderr, "too many __limit values: [@limit]\n", undef, 'GetSQL'));
    }

    return _ret($cb, \%SQL);
}

sub DBI::db::Insert {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'INSERT INTO %s SET %s',
        $dbh->quote_identifier($SQL->{Table}), $SQL->{Set};

    if (!$cb) {
        return $dbh->do($sql) ? $dbh->{mysql_insertid} : undef;
    }
    return $dbh->do($sql, undef, sub {
        my ($rv, $dbh) = @_;    ## no critic (ProhibitReusedNames)
        return $cb->(($rv ? $dbh->{mysql_insertid} : undef), $dbh);
    });
}

sub DBI::db::InsertIgnore {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'INSERT IGNORE INTO %s SET %s',
        $dbh->quote_identifier($SQL->{Table}), $SQL->{Set};
    return _retdo($dbh, $sql, $cb);
}

sub DBI::db::Update {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);
    if ($SQL->{UpdateWhere} eq '1' && !$P->{__force}) {
        return _ret1($cb, $dbh->set_err($DBI::stderr, "empty WHERE require {__force=>1}\n", undef, 'Update'), $dbh);
    }

    my $sql = sprintf 'UPDATE %s SET %s WHERE %s' . ($SQL->{Limit} ? ' LIMIT %s' : q{}),
        $dbh->quote_identifier($SQL->{Table}), $SQL->{Set}, $SQL->{UpdateWhere},
        $SQL->{Limit} || ();
    return _retdo($dbh, $sql, $cb);
}

sub DBI::db::Replace {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'REPLACE INTO %s SET %s',
        $dbh->quote_identifier($SQL->{Table}), $SQL->{Set};
    return _retdo($dbh, $sql, $cb);
}

sub _find_tables_for_delete {
    my ($dbh, $fields, $tables, $P, $cb) = @_;
    if (!@{$tables}) {
        return _ret1($cb, undef, $dbh);
    }

    my $found = [];
    if (!$cb) {
        for my $t (@{$tables}) {
            my $desc = $dbh->ColumnInfo($t);
            if ($desc) {
                my @columns = map {$_->{Field}} @{$desc};
                my %seen;
                if (@{$fields} == grep {++$seen{$_}==2} @{$fields}, @columns) {
                    push @{$found}, $t;
                }
            }
        }
        return $dbh->Delete($found, $P);
    }
    my $code; $code = sub {
        my ($desc) = @_;
        my $t = shift @{$tables};
        if ($desc) {
            my @columns = map {$_->{Field}} @{$desc};
            my %seen;
            if (@{$fields} == grep {++$seen{$_}==2} @{$fields}, @columns) {
                push @{$found}, $t;
            }
        }
        if (@{$tables}) {
            return $dbh->ColumnInfo($tables->[0], $code);
        }
        undef $code;
        return $dbh->Delete($found, $P, $cb);
    };
    return $dbh->ColumnInfo($tables->[0], $code);
}

sub DBI::db::Delete { ## no critic (ProhibitExcessComplexity)
    my ($dbh, $table, $P, $cb) = @_;

    if (!defined $table) {
        my %fields = map {/\A$IDENT(?:__(?!_)$IDENT)?\z/ms ? ($1=>1) : ()} keys %{$P};
        my @fields = keys %fields;
        if (!@fields) {
            return _ret1($cb, $dbh->set_err($DBI::stderr, "table undefined, require params\n", undef, 'Delete'), $dbh);
        }
        if (!$cb) {
            return _find_tables_for_delete($dbh, \@fields, [$dbh->Col('SHOW TABLES')], $P);
        }
        return $dbh->Col('SHOW TABLES', sub {
            my (@tables) = @_;
            return _find_tables_for_delete($dbh, \@fields, \@tables, $P, $cb);
        });
    }

    my @tables = ref $table ? @{$table} : $table;
    if (!$cb) {
        my $res;
        for my $t (@tables) {
            my $SQL = $dbh->GetSQL($t, $P) or return;
            if ($SQL->{Where} eq '1' && !$P->{__force}) {
                return $dbh->set_err($DBI::stderr, "empty WHERE require {__force=>1}\n", undef, 'Delete');
            }
            my $sql = sprintf 'DELETE FROM %s WHERE %s' . ($SQL->{Limit} ? ' LIMIT %s' : q{}),
                $dbh->quote_identifier($SQL->{Table}), $SQL->{Where}, $SQL->{Limit} || ();
            $res = $dbh->do($sql) or return;
        }
        return $res;
    }
    my $code; $code = sub {
        my ($SQL) = @_;
        my $t = shift @tables;
        if (!$SQL) {
            undef $code;
            return $cb->(undef, $dbh);
        }
        if ($SQL->{Where} eq '1' && !$P->{__force}) {
            undef $code;
            return $cb->($dbh->set_err($DBI::stderr, "empty WHERE require {__force=>1}\n", undef, 'Delete'), $dbh);
        }
        my $sql = sprintf 'DELETE FROM %s WHERE %s' . ($SQL->{Limit} ? ' LIMIT %s' : q{}),
            $dbh->quote_identifier($SQL->{Table}), $SQL->{Where}, $SQL->{Limit} || ();
        $dbh->do($sql, sub {
            my ($res, $dbh) = @_;   ## no critic (ProhibitReusedNames)
            if ($res && @tables) {
                return $dbh->GetSQL($tables[0], $P, $code);
            }
            undef $code;
            return $cb->($res, $dbh);
        });
    };
    return $dbh->GetSQL($tables[0], $P, $code);
}

sub DBI::db::ID {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'SELECT %s.%s FROM %s WHERE %s'
        . ($SQL->{Order}        ? ' ORDER BY %s' : q{})
        . ($SQL->{SelectLimit}  ? ' LIMIT %s' : q{}),
        (map { $dbh->quote_identifier($_) } $SQL->{Table}, $SQL->{ID}),
        $SQL->{From}, $SQL->{Where}, $SQL->{Order} || (), $SQL->{SelectLimit} || ();
    return $dbh->Col($sql, $cb // ());
}

sub DBI::db::Count {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'SELECT count(*) __count FROM %s WHERE %s',
        $SQL->{From}, $SQL->{Where};
    return $dbh->Col($sql, $cb // ());
}

sub DBI::db::Select {
    my ($dbh, $table, $P, $cb) = @_;
    my $SQL = $dbh->GetSQL($table, $P) or return _ret1($cb, undef, $dbh);

    my $sql = sprintf 'SELECT %s'
        . ($SQL->{Group} ? ', count(*) __count' : q{})
        . ' FROM %s WHERE %s'
        . ($SQL->{Group}        ? ' GROUP BY %s' : q{})
        . ($SQL->{Order}        ? ' ORDER BY %s' : q{})
        . ($SQL->{SelectLimit}  ? ' LIMIT %s'    : q{}),
        $SQL->{Select}, $SQL->{From}, $SQL->{Where},
        $SQL->{Group} || (), $SQL->{Order} || (), $SQL->{SelectLimit} || ();
    if (!$cb) {
        return wantarray ? $dbh->All($sql) : $dbh->Row($sql);
    }
    return $dbh->All($sql, $cb);
}

sub _is_cb {
    my $cb = shift;
    my $ref = ref $cb;
    return $ref eq 'CODE' || $ref eq 'AnyEvent::CondVar';
}

sub DBI::db::All {
    my ($dbh, $sql, @bind) = @_;
    my $cb = @bind && _is_cb($bind[-1]) ? pop @bind : undef;
    if (!$cb) {
        (my $sth = $dbh->prepare($sql, {async=>0}))->execute(@bind) or return;
        return @{ $sth->fetchall_arrayref({}) };
    }
    return $dbh->prepare($sql)->execute(@bind, sub {
        my ($rv, $sth) = @_;
        return $cb->(!$rv ? () : @{ $sth->fetchall_arrayref({}) });
    });
}

sub DBI::db::Row {
    my ($dbh, $sql, @bind) = @_;
    return $dbh->selectrow_hashref($sql, undef, @bind);
}

sub DBI::db::Col {
    my ($dbh, $sql, @bind) = @_;
    my $cb = @bind && _is_cb($bind[-1]) ? pop @bind : undef;
    if (!$cb) {
        my @res = @{ $dbh->selectcol_arrayref($sql, undef, @bind) || [] };
        return wantarray ? @res : $res[0];
    }
    return $dbh->selectcol_arrayref($sql, undef, @bind, sub {
        my ($ary_ref) = @_;
        return $cb->($ary_ref ? @{ $ary_ref } : ());
    });
}


1; # Magic true value required at end of module
__END__

=encoding utf8

=head1 NAME

DBIx::SecureCGI - Secure conversion of CGI params hash to SQL


=head1 SYNOPSIS

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


=head1 DESCRIPTION

This module let you use unmodified hash with CGI params to make (or just
generate) SQL queries to MySQL database in B<easy and secure> way. To make
this magic possible there are some limitations and requirements:

=over

=item * Your app and db scheme must conform to these L</"CONVENTIONS">

=item * Small speed penalty/extra queries to load scheme from db

=item * No support for advanced SQL, only basic queries

=back

Example: if all CGI params (including unrelated to db table 'Table') are
in C< %Q >, then C<< $dbh->Select('Table', \%Q); >> will execute any
simple C<SELECT> query from the table C<Table> (defined by user-supplied
parameters in C< %Q >) and C<< $dbh->Select('Table', {%Q, id_user=>$id}); >>
will make any similar query limited to records with C<id_user> column
value C< $id > (thus allowing user to fetch any or B<his own> records).

The module is intended for use only with a fairly simple tables and simple
SQL queries. More advanced queries usually can be generated manually with
help of C<< $dbh->GetSQL() >> or you can just use plain DBI methods.

Also is support non-blocking SQL queries using L<AnyEvent::DBI::MySQL> and
thus can be effectively used with event-based CGI frameworks like
L<Mojolicious> or with event-based FASTCGI servers like C<FCGI::EV>.

Finally, it can be used in non-CGI environment, just to make ease using DBI.

=head2 SECURITY OVERVIEW

At a glance, generating SQL queries based on untrusted parameters sent by
user to your CGI looks very unsafe. But interface of this module designed
to make it safe - while you conform to some L</"CONVENTIONS"> and follow
some simple guidelines.

=over

=item * User have to control over query type (SELECT/INSERT/…).

It's defined by method name you call.

=item * User have no control over tables involved in SQL query.

It's defined by separate (first) parameter in all methods, unrelated to
hash with CGI parameters.

=item * User have no direct control over SQL query.

All values from hash are either quoted before inserting into SQL, or
checked using very strict regular expressions if it's impossible to quote
them (like for date/time C<INTERVAL> values).

=item * You can block/control access to "secure" fields in all tables.

Name all such fields in some special way (like beginning with C<_>) and
when receiving CGI parameters immediately B<delete all> keys in hash which
match these fields (i.e. all keys beginning with C<_>). Later you can
analyse user's request and manually add to hash keys for these fields
before call method to execute SQL query.

=item * You can limit user's access to some subset of records.

Just instead of using plain C< \%Q > as parameter for methods use
something like C<< { %Q, id_user => $id } >> - this way user will be
limited to records with C< $id > value in C< id_user > column.

=back

Within these security limitations user can do anything - select records
with custom C<WHERE>, C<GROUP BY>, C<ORDER BY>, C<LIMIT>; set any values
(allowed by table scheme, of course) for any fields on C<INSERT> or
C<UPDATE>; etc. without any single line of your code - exclusively by
using different CGI parameters.


=head1 HOW IT WORKS

Each CGI parameter belongs to one of three categories:

=over

=item * related to some table's field in db

C<fieldname> AND C<fieldname__funcname>

=item * control command for DBIx::SecureCGI

C<__command>

=item * your app's parameter

=back

It's recommended to name fields in db beginning with B<lowercase> letter
or B<underscore>, and name your app's parameters beginning with
B<Uppercase> letter to avoid occasional clash with field name.

To protect some fields (like C<balance> or C<privileges>) from
uncontrolled access you can use simple convention: name these fields in db
beginning with C<_>; when receiving CGI params just B<delete all with
names beginning with> C<_> - thus it won't be possible to access these
fields from CGI params. DBIx::SecureCGI doesn't know about this and
handle these fields as usual fields, so you should later add needed keys
for these fields into hash you'll give to DBIx::SecureCGI methods.
This way all operations on these fields will be controlled by your app.

You can use any other similar naming scheme which won't conflict with
L</"CONVENTIONS"> below - DBIx::SecureCGI will analyse db scheme (and
cache it for speed) to detect which keys match field names.

CGI params may have several values. In hash, keys for such params must
have ARRAYREF value. DBIx::SecureCGI support this only for keys which
contain C<__>. Depending on used CGI framework you may need to convert
existing CGI parameters in this format.

Error handling: all unknown keys will be silently ignored, all other
errors (unable to detect key for joining table, field without
C<__funcname> have ARRAYREF value, unknown C<__funcname> function, etc.)
will return usual DBI errors (or throw exceptions when C<< {RaiseError=>1} >>.

=head2 CONVENTIONS

=over

=item *

Each table's B<first field> must be a C<PRIMARY KEY>.

=over

MOTIVATION: DBIx::SecureCGI uses simplified analyse of db scheme and
suppose first field in every table is a C<PRIMARY KEY>. To add support for
complex primary keys or tables without primary keys we should first define
how C<< $dbh->ID() >> should handle them and how to automatically join
such tables.

=back

=item *

Two tables are always C<JOIN>ed using field which must be C<PRIMARY KEY>
at least in one of them and have B<same name in both tables>.

=over

So, don't name your primary key C<id> if you plan to join this table with
another - name it like C<id_thistable> or C<thistableId>.

=back

If both tables have field corresponding to C<PRIMARY KEY> in other table,
then key field of B<right table> (in order defined when you make array of
tables in first param of method) will be used.

If more than two tables C<JOIN>ed, then each table starting from second
one will try to join to each of the previous tables (starting at first
table) until it find table with suitable field. If it wasn't found
DBI error will be returned.

=over

MOTIVATION: Let DBIx::SecureCGI automatically join tables.

=back

=item *

Field names must not contain C<__> (two adjoined underscore).

=over

MOTIVATION: Distinguish special params for DBIx::SecureCGI from field
names. Also, DBIx::SecureCGI sometimes create aliases for fields
and their names begins with C<__>.

=back

=item *

Hash with CGI params may contain several values (as ARRAYREF) only for key
names containing C<__> (keys unrelated to fields may have any values).

=over

MOTIVATION: Allowing C<< { field => \@values } >> introduce many
ambiguities and in fact same as C<< { field__eq => \@values } >>,
so it's safer to deny it.

=back

=back

=head2 Hash to SQL convertion rules

=over

=item * __something

Keys beginning with C<__> are control keys. Supported keys are:

=over

=item B<__order>

Define value for C<ORDER BY>. Valid values are C<"field_name"> or
C<"field_name ASC"> or C<"field_name DESC">. Multiple values can be
given as ARRAYREF.

=item B<__group>

Define value for C<GROUP BY>. Valid values are same as for B<__order>.

=item B<__limit>

Can have up to two numeric values (when it's ARRAYREF), set C<LIMIT>.

=item B<__force>

If the value of B<__force> key is true, then it's allowed to run
C< $dbh->Update() > and C< $dbh->Delete() > with an empty C<WHERE>. (This
isn't a security feature, it's just for convenience to protect against
occasional damage on database while playing with CGI parameters.)

=back

=item * fieldname__funcname

If the key contains a C<__> then it is treated as a C<field__function>.
If the there is no field with this name in database, this key is ignored.
A valid key value - scalar or a reference to an array of scalars.
A list of available functions in this version is shown below.

Unless special behavior mentioned functions handle ARRAYREF value by
applying itself to each value in array and joining with C<AND>.

Example: C<< { html__like => ["%<P>%", "%<BR>%"] } >> will be transformed
to C<< "html LIKE '%<P>%' AND html LIKE '%<BR>%'" >>.

Typically, such keys are used in C<WHERE>, except when C<funcname> begins
with C<set_> - such keys will be used in C<SET>.

=item * fieldname

Other keys are treated as names of fields in database.
If there is no field with such name, then key is ignored.
A valid value for these keys - scalar.

Example: C<< { name => "Alex" } >> will be transformed to
C<< "name = 'Alex'" >> in SQL.

Typically, such keys are used in part C<SET>, except for C<PRIMARY KEY>
field in C<< $dbh->Update() >> - it will be placed in C<WHERE>.

=back


=head1 INTERFACE

=over

=item B<DefineFunc>( $name, '%s op %s' )

=item B<DefineFunc>( $name, [ qr/regexp/, '%s op %s' ] )

=item B<DefineFunc>( $name, sub { … } )

Define new or replace existing function applied to fields after C<__>
delimiter.

SQL expression for that function will be generated in different ways,
depending on how you defined that function - using string, regexp+string
or code:

    $expr = sprintf '%s op %s', $field, $dbh->quote($value);
    $expr = $value =~ /regexp/ && sprintf '%s op %s', $field, $value;
    $expr = $code->($dbh, $field, $value);

If C<$expr> will be false DBI error will be returned.
Here is example of code implementation:

    sub {
        my ($dbh, $f, $v) = @_;
        if (… value ok …) {
            return sprintf '…', $f, $dbh->quote($v);
        }
        return;     # wrong value
    }

=back


=head1 INTERFACE injected into DBI

=over

=item B<GetSQL>( $table, \%Q )

=item B<GetSQL>( $table, \%Q, sub { my ($SQL) = @_; … })

=item B<GetSQL>( \@tables, \%Q )

=item B<GetSQL>( \@tables, \%Q, sub { my ($SQL) = @_; … })

This is helper function which will analyse (cached) database scheme for
given tables and generate elements of SQL query for given keys in C<%Q>.
You may use it to write own methods like C<< $dbh->Select() >> or 
C<< $dbh->Insert() >>.

In C< %Q > keys which doesn't match field names in C< $table / @tables >
are ignored.

Names of tables and fields in all keys (except C<{Table}> and C<{ID}>)
are escaped, field names qualified with table name (so they're ready for
inserting into SQL query). Values of C<{Table}> and C<{ID}> should be
escaped with C<< $dbh->quote_identifier() >> before using in SQL query.

Returns HASHREF with keys:

=over

=item {Table}

first of the used tables

=item {ID}

name of C<PRIMARY KEY> field in {Table}

=item {Select}

list of all field names which should be returned by C<SELECT *> excluding
duplicated fields: if C<SELECT> used on more than one table with same
field "fieldname" then C<{Select}> will include only name from first table
"tablename.fieldname"; field names in C<{Select}> are joined with ","

=item {From}

all tables joined using chosen JOIN type (INNER by default)

=item {Set}

string like C< "field=value, field2=value2" > for all simple
"field" keys in C< %Q >

=item {Where}

a-la {Set}, except fields joined using C<AND> and added
"field__function" fields; if there are no fields it will be set to string
C<"1">

=item {UpdateWhere}

a-la {Where}, except it uses only "field__function" keys plus
one C<PRIMARY KEY> "field" key (if it exists in C< %Q >)

=item {Order}

string like "field1 ASC, field2 DESC" or empty string

=item {Group}

a-la {Order}

=item {Limit}

=item {SelectLimit}

Both set to values of C<__limit> if it contain one number;
if C<__limit> contain two numbers, then C<{Limit}> will be empty,
and C<{SelectLimit}> will contain both numbers joined with ","

=back


=item B<Insert>( $table, \%Q )

=item B<Insert>( $table, \%Q, sub { my ($newid, $dbh) = @_; … }) )

    INSERT INTO {Table} SET {Set}

Return C<< $dbh->{mysql_insertid} >> on success or undef() on error.

It's B<strongly recommended> to always use
C<< {%Q, …, primary_key=>undef} >>, because if you didn't force
C<primary_key> field to be C<NULL> (and thus use C<AUTO_INCREMENT>)
then user may send CGI parameter to set it to C<-1> or C<4294967295> and
this will result in B<DoS> because no more records can be added using
C<AUTO_INCREMENT> into this table.


=item B<InsertIgnore>( $table, \%Q )

=item B<InsertIgnore>( $table, \%Q, sub { my ($rv, $dbh) = @_; … })

    INSERT IGNORE INTO {Table} SET {Set}

Return C<$rv> (true on success or undef() on error).


=item B<Update>( $table, \%Q )

=item B<Update>( $table, \%Q, sub { my ($rv, $dbh) = @_; … })

    UPDATE {Table} SET {Set} WHERE {UpdateWhere} [LIMIT {Limit}]

Uses in C<SET> part all fields given as C<field>, in C<WHERE> part all
fields given as C<field__function> plus C<PRIMARY KEY> field if it was
given as C<field>.

Return C<$rv> (amount of modified records on success or undef() on error).

To use with empty C<WHERE> part require C<< {__force=>1} >> in C<%Q>.


=item B<Replace>( $table, \%Q )

=item B<Replace>( $table, \%Q, sub { my ($rv, $dbh) = @_; … })

    REPLACE INTO {Table} SET {Set}

Uses in C<SET> part all fields given as C<field>.

Return C<$rv> (true on success or undef() on error).


=item B<Delete>( $table, \%Q )

=item B<Delete>( $table, \%Q, sub { my ($rv, $dbh) = @_; … })

=item B<Delete>( \@tables, \%Q )

=item B<Delete>( \@tables, \%Q, sub { my ($rv, $dbh) = @_; … })

=item B<Delete>( undef, \%Q )

=item B<Delete>( undef, \%Q, sub { my ($rv, $dbh) = @_; … })

    DELETE FROM {Table} WHERE {Where} [LIMIT {Limit}]

Delete records from C<$table> or (one-by-one) from each table in
C<@tables>. If undef() given, then delete records from B<ALL> tables
(except C<TEMPORARY>) which have B<ALL> fields mentioned in C<%Q>.

To use with empty C<WHERE> part require C<< {__force=>1} >> in C<%Q>.

Return C<$rv> (amount of deleted records or undef() on error).
If used to delete records from more than one table - return C<$rv>
for last table. If error happens it will be immediately returned,
so some tables may not be processed in this case.


=item B<ID>( $table, \%Q )

=item B<ID>( $table, \%Q, sub { my (@id) = @_; … })

=item B<ID>( \@tables, \%Q )

=item B<ID>( \@tables, \%Q, sub { my (@id) = @_; … })

    SELECT {ID} FROM {From} WHERE {Where}
    [ORDER BY {Order}] [LIMIT {SelectLimit}]

Execute SQL query using C<< $dbh->Col() >>.


=item B<Count>( $table, \%Q )

=item B<Count>( $table, \%Q, sub { my ($count) = @_; … })

=item B<Count>( \@tables, \%Q )

=item B<Count>( \@tables, \%Q, sub { my ($count) = @_; … })

    SELECT count(*) __count FROM {From} WHERE {Where}

Execute SQL query using C<< $dbh->Col() >>.


=item B<Select>( $table, \%Q )

=item B<Select>( $table, \%Q, sub { my (@rows) = @_; … })

=item B<Select>( \@tables, \%Q )

=item B<Select>( \@tables, \%Q, sub { my (@rows) = @_; … })

    SELECT * FROM {From} WHERE {Where}
    [ORDER BY {Order}] [LIMIT {SelectLimit}]

    SELECT *, count(*) __count FROM {From} WHERE {Where} GROUP BY {Group}
    [ORDER BY {Order}] [LIMIT {SelectLimit}]

Instead of C<SELECT *> it uses enumeration of all fields qualified using
table name; if same field found in several tables it's included only
one - from first table having that field.

In C<@tables> you can append C<" LEFT"> or C<" INNER"> to table name to
choose C<JOIN> variant (by default C<INNER JOIN> will be used):

    $dbh->Select(['TableA', 'TableB LEFT', 'TableC'], …)

Execute request using C<< $dbh->All() >> when called in list context or
using C<< $dbh->Row() >> when called in scalar context.


=item B<All>( $sql, @bind )

=item B<All>( $sql, @bind, sub { my (@rows) = @_; … })

Shortcut for this ugly but very useful snippet:

    @{ $dbh->selectall_arrayref($sql, {Slice=>{}}, @bind) }


=item B<Row>( $sql, @bind )

=item B<Row>( $sql, @bind, sub { my ($row) = @_; … })

Shortcut for:

    $dbh->selectrow_hashref($sql, undef, @bind)

If you wonder why it exists, the answer is simple: it was added circa
2002, when there was no C<< $dbh->selectrow_hashref() >> yet and now it
continue to exists for compatibility and to complement C<< $dbh->All() >>
and C<< $dbh->Col() >>.


=item B<Col>( $sql, @bind )

=item B<Col>( $sql, @bind, sub { my (@col) = @_; … })

Shortcut for:

    $scalar = $dbh->selectcol_arrayref($sql, undef, @bind)->[0]
    @array  = @{ $dbh->selectcol_arrayref($sql, undef, @bind) }


=item C<SecureCGICache>()

=item C<SecureCGICache>( $new_cache )

Fetch (or set when C<$new_cache> given) HASHREF with cached results of
C<DESC tablename> SQL queries for all tables used previous in any methods.

You may need to reset cache (by using C<{}> as C<$new_cache> value) if
you've changed scheme for tables already accessed by any method or if you
changed current database.

Also in some environments when many different C<$dbh> used simultaneously,
connected to same database (like in event-based environments) it may make
sense to share same cache for all C<$dbh>.


=item B<TableInfo>( $table, sub { my ($cache) = @_; … })

=item B<TableInfo>( \@tables, sub { my ($cache) = @_; … })

Ensure C<DESC tablename> for all C<$table / @tables> is cached.

Return C<< $dbh->SecureCGICache() >> on success or undef() on error.


=item B<ColumnInfo>( $table, sub { my ($desc) = @_; … })

Ensure C<DESC $table> is cached.

Return C<< $dbh->All("DESC $table") >> on success or undef() on error.


=back


=head1 __FUNCTIONS for fields

These functions can be added and replaced using
C<< DBIx::SecureCGI::DefineFunc() >>.

Functions which can be used in C<%Q> as C<fieldname_funcname>:

=over

=item B<eq>, B<ne>, B<lt>, B<gt>, B<le>, B<ge>

 field =  value     field IS NULL
 field != value     field IS NOT NULL
 field <  value
 field >  value
 field <= value
 field >= value

For functions B<eq> or B<ne>:

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

=item B<like>, B<not_like>

 field LIKE value
 field NOT LIKE value

=item B<date_eq>, B<date_ne>, B<date_lt>, B<date_gt>, B<date_le>, B<date_ge>

 field =  DATE_ADD(NOW(), INTERVAL value)
 field != DATE_ADD(NOW(), INTERVAL value)
 field <  DATE_ADD(NOW(), INTERVAL value)
 field >  DATE_ADD(NOW(), INTERVAL value)
 field <= DATE_ADD(NOW(), INTERVAL value)
 field >= DATE_ADD(NOW(), INTERVAL value)

value must match C</^-?\d+ (?:SECOND|MINUTE|HOUR|DAY|MONTH|YEAR)$/>

=item B<set_add>

 field = field + value

When used in C<< $dbh->Update() >> it will be in C<SET> instead of C<WHERE>.
It doesn't make sense to use this function with C<< $dbh->Insert() >>,
C<< $dbh->InsertIgnore() >> or C<< $dbh->Replace() >>.

=item B<set_date>

 field = NOW()
 field = DATE_ADD(NOW(), INTERVAL value)

If it's value is (case-insensitive) string C<"NOW"> then it'll use
C<NOW()> else it will use C<DATE_ADD(…)>.

When used in C<< $dbh->Insert() >>, C<< $dbh->InsertIgnore() >>,
C<< $dbh->Update() >> and C<< $dbh->Replace() >> it will be in C<SET>.

=back


=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Only MySQL supported.

It's impossible to change C<PRIMARY KEY> using C<< $dbh->Update() >> with:

 { id => $new_id, id__eq => $old_id }

because both C<id> and C<id__eq> will be in C<WHERE> part:

 SET id = $new_id WHERE id = $new_id AND id = $old_id

and if we won't add C<< 'id => $new_id' >> in C<WHERE> part if we have
C< 'id__eq' >, then we'll have do use this
C<< '($table, {%Q, id_user=>$S{id_user}, id_user__eq=>$S{id_user})' >>
in B<all> CGI requests to protect against attempt to read someone else's
records or change own records's id_user field by using C< 'id_user' >
or C< 'id_user__eq' > CGI params.


=head1 SUPPORT

Please report any bugs or feature requests through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=DBIx-SecureCGI>.
I will be notified, and then you'll automatically be notified of progress
on your bug as I make changes.

You can also look for information at:

=over

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=DBIx-SecureCGI>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/DBIx-SecureCGI>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/DBIx-SecureCGI>

=item * Search CPAN

L<http://search.cpan.org/dist/DBIx-SecureCGI/>

=back


=head1 AUTHORS

Alex Efros  C<< <powerman@cpan.org> >>

Nikita Savin  C<< <nikita@asdfGroup.com> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2002-2013 Alex Efros <powerman@cpan.org>.

This program is distributed under the MIT (X11) License:
L<http://www.opensource.org/licenses/mit-license.php>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

