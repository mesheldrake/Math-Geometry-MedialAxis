#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;
use Math::Geometry::MedialAxis;

{
    
    # Quadrilateral that's like a triangle with one tip cut off.
    # same as test 02_*.t's shape
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

    {
    my $pi = $e3->point_i();
    ok($pi, "got something for point_i()");
    }

    {
    my $p0 = $e3->point0();
    my $pi = $e3->point_i($v4->r());
    is_deeply([map eval(sprintf("%.14f",$_)), @$pi],
              [map eval(sprintf("%.14f",$_)), @$p0],
          "got a point_i() same as point0()");
    }

    {
    my $p1 = $e3->point1();
    my $pi = $e3->point_i($v5->r());
    is_deeply([map eval(sprintf("%.14f",$_)), @$pi],
              [map eval(sprintf("%.14f",$_)), @$p1],
          "gota a point_i() same as point1()");
    }

    {
    my $p0 = $e3->point0();
    my $p1 = $e3->point1();
    is($e3->point0()->[2], $v4->r(), "expected radius for point0");
    is($e3->point1()->[2], $v5->r(), "expected radius for point1");
    my $pm = [($p0->[0] + $p1->[0])/2, ($p0->[1] + $p1->[1])/2, ($p0->[2] + $p1->[2])/2];
    my $pi = $e3->point_i(($v4->r() + $v5->r())/2);
    is_deeply([map eval(sprintf("%.14f",$_)), @$pi],
              [map eval(sprintf("%.14f",$_)), @$pm],
          "got point_i() that corresponds to average of point0() and point1()");
    }

    {
    # with offset set and no args to point_i() should get the
    # the intersection with the edge,
    # which for our test shape will be at y==200
    $e3->offset( ($v4->r() + $v5->r())/2 );
    my $pi0 = $e3->point_i();
    $e3->offset( ($v4->r() + $v5->r())/2 - 1);
    my $pi1 = $e3->point_i();
    $e3->offset( ($v4->r() + $v5->r())/2 + 1);
    my $pi2 = $e3->point_i();
    is($pi0->[1], $e3->vertex0()->y(),
        "with offset preset, point_i() gives edge intersect point");
    ok($pi1->[0] < $pi0->[0], "intersect shifts  left when offset decreased");
    ok($pi2->[0] > $pi0->[0], "intersect shifts right when offset increased");
    }
    

}


__END__
