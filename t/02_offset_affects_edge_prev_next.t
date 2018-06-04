#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Math::Geometry::MedialAxis;

{
    # See that EdgeView->next() and EdgeView->prev()
    # return twin() instead when the edge's vertex0 radius
    # is less than the offset set on the Edge.
    
    # We'll need a quadrilateral that's like a triangle with one tip cut off.
    my $e0 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e1 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e3 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e4 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e5 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e6 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e7 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e8 = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e9 = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    # one vertical end is 200 high, elevated to y=100
    # the other vertical end is 400 high, resting on y=0
    # 1000 long
    my $v0 = Math::Geometry::MedialAxis::VertexView->new(0,100,0);
    my $v1 = Math::Geometry::MedialAxis::VertexView->new(1000,0,0);
    my $v2 = Math::Geometry::MedialAxis::VertexView->new(1000,400,0);
    my $v3 = Math::Geometry::MedialAxis::VertexView->new(0,300,0);

    # Voronoi-like vertices inside the shape
    my $yl = ($v3->y() - $v0->y())/2;
    my $al = atan2(-10,1)/2;
    my $hl = $yl/cos($al);
    my $xl = sqrt($hl**2 - $yl**2);
    my $v4 = Math::Geometry::MedialAxis::VertexView->new($xl, $v0->y() + $yl, $xl);
    #print join(",",$v4->x(),$v4->y(),$v4->r());
    
    my $yr = ($v2->y() - $v1->y())/2;
    my $ar = atan2(10,1)/2;
    my $hr = $yr/cos($ar);
    my $xr = sqrt($hr**2 - $yr**2);
    my $v5 = Math::Geometry::MedialAxis::VertexView->new(1000 - $xr, $v1->y() + $yr, $xr);
    #print join(",",$v5->x(),$v5->y(),$v5->r());
    
    $e0->set_vertex0($v3);
    $e1->set_vertex0($v4);
    $e2->set_vertex0($v0);
    $e3->set_vertex0($v4);
    $e4->set_vertex0($v5);
    $e5->set_vertex0($v1);
    $e6->set_vertex0($v5);
    $e7->set_vertex0($v2);
    $e8->set_vertex0($v5);
    $e9->set_vertex0($v4);

    $e0->set_twin($e9);
    $e1->set_twin($e2);
    $e2->set_twin($e1);
    $e3->set_twin($e8);
    $e4->set_twin($e5);
    $e5->set_twin($e4);
    $e6->set_twin($e7);
    $e7->set_twin($e6);
    $e8->set_twin($e3);
    $e9->set_twin($e0);
    
    eval('$e'.$_)->set_next(eval('$e'.($_ + 1))) for (0..8);
    $e9->set_next($e0);
    eval('$e'.$_)->set_prev(eval('$e'.($_ - 1))) for (1..9);
    $e0->set_next($e9);
    
    # With that setup, with no offset, or a small offset
    # less than the smallest radius of those internal vertices, the 
    # next() and prev() methods should return just the same underlying next()
    # and prev() from the parent class.

    # try it with offset not even set
    ok($e3->twin() == $e8, "normal twins linked 1");
    ok($e8->twin() == $e3, "normal twins linked 2");
    ok($e3->prev() == $e2, "normal prev");
    ok($e8->next() == $e9, "normal next");

    # now, with offset > v4's radius, e3->prev() 
    # and e8->next() should return twin() instead 
    eval('$e'.$_)->offset($v4->r() + 1) for (0..9);

    ok($e3->prev() == $e8, "radius < offset makes prev() go to twin() instead");
    ok($e8->next() == $e3, "radius < offset makes next() go to twin() instead");

    # this should go back to normal behavior
    eval('$e'.$_)->offset($v4->r() - 1) for (0..9);
    ok($e3->prev() == $e2, "back to normal prev");
    ok($e8->next() == $e9, "back to normal next");

}


__END__
