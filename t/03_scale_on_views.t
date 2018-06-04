#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 18;
use Math::Geometry::MedialAxis;

{

    my $x1=0;
    my $y1=0;
    my $r1=1;
    my $x2=2;
    my $y2=0;
    my $r2=2;
    my $x3=1;
    my $y3=1;
    my $r3=3;

    # similar triangle to test 01_basic.t
    my $e1 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e3 = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    my $e1t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e3t = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    my $v1 = Math::Geometry::MedialAxis::VertexView->new($x1,$y1,$r1);
    my $v2 = Math::Geometry::MedialAxis::VertexView->new($x2,$y2,$r2);
    my $v3 = Math::Geometry::MedialAxis::VertexView->new($x3,$y3,$r3);
    
    $v1->set_incident_edge($e1);
    $v2->set_incident_edge($e2);
    $v3->set_incident_edge($e3);

    $e1->set_vertex0($v1);
    $e2->set_vertex0($v2);
    $e3->set_vertex0($v3);

    $e1t->set_vertex0($v2);
    $e2t->set_vertex0($v3);
    $e3t->set_vertex0($v1);

    $e1->set_twin($e1t);
    $e1t->set_twin($e1);
    $e2->set_twin($e2t);
    $e2t->set_twin($e2);
    $e3->set_twin($e3t);
    $e3t->set_twin($e3);

    $e1->set_next($e2);
    $e2->set_next($e3);
    $e3->set_next($e1);

    $e1->set_prev($e3);
    $e2->set_prev($e1);
    $e3->set_prev($e2);

    $e1t->set_next($e3t);
    $e2t->set_next($e1t);
    $e3t->set_next($e2t);

    $e1t->set_prev($e3t);
    $e2t->set_prev($e1t);
    $e3t->set_prev($e2t);
        
    ok( $v1->incident_edge() == $e1,
        "vertex->incident_edge() returns same object as edge it was set to" );
    ok( $e1->vertex0() == $v1,
        "edge->vertex0() returns same object as vertex it was set to" );

    my $stock = [[$x1,$y1,$r1],[$x2,$y2,$r2],[$x3,$y3,$r3]];

    {
    is_deeply( vert_coords()             , $stock, "initial vertex coords" );
    is_deeply( vert_coords_from_edge_v0(), $stock, "initial v0 coords" );
    is_deeply( vert_coords_from_edge_v1(), $stock, "initial v1 coords" );

    ok($v1->scale() == 1 && $v2->scale() == 1 && $v3->scale() == 1,
        "undefined scale has default scale 1");
    }

    {
    my $scale=10000;
    my $stock_10000 = [map {[map $_ * $scale, @$_]} @$stock];
    $v1->scale($scale);
    $v2->scale($scale);
    $v3->scale($scale);
    ok($v1->scale() == $scale && $v2->scale() == $scale && $v3->scale() == $scale,
        "set scale $scale on vertices");
    is_deeply( vert_coords()             , $stock_10000, "scale $scale vertex coords" );
    is_deeply( vert_coords_from_edge_v0(), $stock_10000, "scale $scale v0 coords" );
    is_deeply( vert_coords_from_edge_v1(), $stock_10000, "scale $scale v1 coords" );
    }

    {
    my $scale=4444;
    my $stock_4444 = [map {[map $_ * $scale, @$_]} @$stock];
    $v1->scale($scale);
    $v2->scale($scale);
    $v3->scale($scale);
    ok($v1->scale() == $scale && $v2->scale() == $scale && $v3->scale() == $scale,
        "set scale $scale on vertices");
    is_deeply( vert_coords()             , $stock_4444, "scale $scale vertex coords" );
    is_deeply( vert_coords_from_edge_v0(), $stock_4444, "scale $scale v0 coords" );
    is_deeply( vert_coords_from_edge_v1(), $stock_4444, "scale $scale v1 coords" );
    }

    {
    my $scale=undef;
    $v1->scale($scale);
    $v2->scale($scale);
    $v3->scale($scale);
    ok($v1->scale() == 1 && $v2->scale() == 1 && $v3->scale() == 1,
        "set scale to undef on vertices");
    is_deeply( vert_coords()             , $stock, "scale undef vertex coords" );
    is_deeply( vert_coords_from_edge_v0(), $stock, "scale undef v0 coords" );
    is_deeply( vert_coords_from_edge_v1(), $stock, "scale undef v1 coords" );
    }



    # exercise simple get on vertices
    sub vert_coords {
        return [[$v1->x(),$v1->y(),$v1->r()],
                [$v2->x(),$v2->y(),$v2->r()],
                [$v3->x(),$v3->y(),$v3->r()]];
    }

    # demonstrate that vertex object returned from edge->vertex0(),
    # is the same as the original vertex set on the edge, and that it will work
    # as a key for the Hash::Util::FieldHash::fieldhash  %scale hash
    # where it's scale value is stored.
    sub vert_coords_from_edge_v0 {
        return [[$e1->vertex0()->x(),$e1->vertex0()->y(),$e1->vertex0()->r()],
                [$e2->vertex0()->x(),$e2->vertex0()->y(),$e2->vertex0()->r()],
                [$e3->vertex0()->x(),$e3->vertex0()->y(),$e3->vertex0()->r()]];
    }

    # demonstrate that vertex objects returned from an edge->twin()->vertex0()
    # are still our original vertex objects, that can retrieve their
    # sclae setting from our Hash::Util::FieldHash::fieldhash %scale hash
    sub vert_coords_from_edge_v1 {
        return [[$e3->vertex1()->x(),$e3->vertex1()->y(),$e3->vertex1()->r()],
                [$e1->vertex1()->x(),$e1->vertex1()->y(),$e1->vertex1()->r()],
                [$e2->vertex1()->x(),$e2->vertex1()->y(),$e2->vertex1()->r()]];
    }

}


__END__
