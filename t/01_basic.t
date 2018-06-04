#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 24;
use Math::Geometry::MedialAxis;

{
    # See that our next(), prev(), twin(), rot_next() and rot_prev() methods
    # consistently return Math::Geometry::MedialAxis::Edge objects
    # and not the parent class Boost::Polygon::MedialAxis::Edge.

    my $e1 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e3 = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    my $e1t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e3t = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    # not necessarily valid Voronoi geometry, but doesn't matter here
    my $v1 = Math::Geometry::MedialAxis::VertexView->new(1.1,1.1,0.5);
    my $v2 = Math::Geometry::MedialAxis::VertexView->new(2.1,1.1,0.5);
    my $v3 = Math::Geometry::MedialAxis::VertexView->new(0.55,1/sqrt(2),0.5);
    
    $v1->set_incident_edge($e1);
    $v2->set_incident_edge($e2);
    $v3->set_incident_edge($e3);

    $e1->set_vertex0($v1);
    $e2->set_vertex0($v2);
    $e3->set_vertex0($v3);

    $e1t->set_vertex0($v2);
    $e2t->set_vertex0($v3);
    $e3t->set_vertex0($v1);
    
    ok( $e1->isa("Math::Geometry::MedialAxis::EdgeView"),
        "new() returns correct class for Edge" );
    ok( $v1->isa("Math::Geometry::MedialAxis::VertexView"),
        "new() returns correct class for Vertex" );

    ok( $v1->incident_edge()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "vertex->incident_edge() returns correct class for Edge" );
    ok( $e1->vertex0()->isa("Math::Geometry::MedialAxis::VertexView"),
        "edge->vertex0() returns correct class for Vertex" );

    ok( $v1->incident_edge() == $e1,
        "vertex->incident_edge() returns same object as edge it was set to" );
    ok( $e1->vertex0() == $v1,
        "edge->vertex0() returns same object as vertex it was set to" );
        

    # set up half-edge graph of a triangle
    
    # note we are passing Math::Geometry::MedialAxis::Edge objects
    # into C++, but the typemap we're using ignores the Perl class
    # and sends in instead a pointer with the right C++ type. We hope.
    
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
    
    # twin()
    ok($e1->twin()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->twin() returns object of correct class");
    ok($e1->twin() == $e1t, 
        "edge->twin() returns correct object, and '==' overload works");
    ok($e1->twin()->twin()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->twin()->twin() returns object of correct class");
    ok($e1->twin()->twin() == $e1, 
        "edge->twin()->twin() is same object as edge");

    # next()
    ok($e1->next()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->next() returns object of correct class");
    ok($e1->next() == $e2, 
        "edge->next() returns correct object, and '==' overload works");
    ok($e1->next()->next()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->next()->next() returns object of correct class");
    ok($e1->next()->next() == $e3, 
        "edge->next()->next() is correct object");
    ok($e1->next()->next()->next() == $e1,
        "edge->next()->next()->next() is same object as edge");
    
    # prev()
    ok($e1->prev()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->prev() returns object of correct class");
    ok($e1->prev() == $e3, 
        "edge->prev() returns correct object, and '==' overload works");
    ok($e1->prev()->prev()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->prev()->prev() returns object of correct class");
    ok($e1->prev()->prev() == $e2, 
        "edge->prev()->prev() is correct object");
    ok($e1->prev()->prev()->prev() == $e1,
        "edge->prev()->prev()->prev() is same object as edge");

    # rot_next()
    ok($e1->rot_next()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->rot_next() returns object of correct class");
    ok($e1->rot_next() == $e3t, 
        "edge->rot_next() returns correct object, and '==' overload works");

    # rot_prev()
    ok($e2->rot_prev()->isa("Math::Geometry::MedialAxis::EdgeView"),
        "edge->rot_prev() returns object of correct class");
    ok($e2->rot_prev() == $e1t, 
        "edge->rot_prev() returns correct object, and '==' overload works");

}


__END__
