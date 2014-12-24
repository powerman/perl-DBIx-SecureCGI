use 5.010;
use strict;
use warnings;
use Module::Build;

my $class = Module::Build->subclass(
    class => 'Module::Build::Custom',
    code => <<'SUBCLASS' );

# Better compatibility with Gentoo ebuilds.
sub dist_version {
    my $self = shift;
    my $v = $self->SUPER::dist_version;
    $v =~ s/\Av//ms;
    return $v;
}
SUBCLASS

my $builder = $class->new(
    module_name         => 'DBIx::SecureCGI',
    license             => 'mit',
    dist_author         => 'Alex Efros <powerman@cpan.org>',
    dist_version_from   => 'lib/DBIx/SecureCGI.pm',
    release_status      => 'stable',
    configure_requires => {
        'Module::Build'         => 0.4004, # for test_requires
    },
    requires => {
        'perl'                  => 5.010,
        'version'               => '0.77',
        'DBI'                   => 0,
        'DBD::mysql'            => 0,
        'AnyEvent'              => 0,
        'AnyEvent::DBI::MySQL'  => '1.0.2',
    },
    test_requires => {
        'Test::More'            => 0,
        'Test::Exception'       => 0,
        'Test::CheckManifest'   => '0.9',
        'Test::Perl::Critic'    => 0,
        'Test::Pod'             => '1.22',
        'Test::Pod::Coverage'   => '1.08',
        'Pod::Coverage'         => '0.18',
    },
#     add_to_cleanup     => [ 'DBIx-SecureCGI-*' ],
);

$builder->create_build_script();

my $default_db = q{test};
if (`echo "show variables" | mysql -u test \Q$default_db\E 2>&1` !~ /Variable_name/ms) {
    $default_db = q{};
}
my ($db, $login, $pass) = (q{}, q{}, q{});
$db = $builder->prompt("\nEnter database name (empty/space to skip test):", $default_db);
$db =~ s/\s+//msg;
if ($db ne q{}) {
    $login = $builder->prompt("Enter login for database '$db':", 'test');
    $pass  = $builder->prompt("Enter pass for login '$login':", '');
}
open my $f, '> t/.answers' or die "open: $!";
printf {$f} "%s\n%s\n%s\n", $db, $login, $pass;
close $f or die "close: $!";
