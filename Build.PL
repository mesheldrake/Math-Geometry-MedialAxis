use 5.006;
use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Math::Geometry::MedialAxis',
    license             => 'artistic_2',
    dist_author         => q{Michael E. Sheldrake <mike.sheldrake@gmail.com>},
    dist_version_from   => 'lib/Math/Geometry/MedialAxis.pm',
    configure_requires => {
        'Module::Build' => 0,
    },
    build_requires => {
        'Test::More' => 0,
    },
    requires => {
        'Boost::Polygon::Voronoi' => 0.01,
    },
    add_to_cleanup     => [ 'Math-Geometry-MedialAxis-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
