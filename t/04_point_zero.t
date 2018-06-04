#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 7;
use Math::Geometry::MedialAxis;

{

    # two simply rectangular consecutive edges
    my $x1=0;
    my $y1=0;
    my $r1=4;

    my $x2=4;
    my $y2=0;
    my $r2=4;
        
    my $foot1x = $x1;
    my $foot1y = $r1;

    my $foot2x = $x2;
    my $foot2y = $r2;

    my $theta = 0;
    my $phi = 2 * atan2(1,1);

    my $e1  = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e1t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2  = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    $e1->set_twin($e1t); # source of vertex1() for e1
    $e1t->set_twin($e1);
    $e1->set_next($e2);
    $e2->set_twin($e2t); # source of vertex1() for e2 - because is_infinite()
    $e2t->set_twin($e2); # will be called, and that references twin


    my $v1 = Math::Geometry::MedialAxis::VertexView->new($x1,$y1,$r1);
    my $v2 = Math::Geometry::MedialAxis::VertexView->new($x2,$y2,$r2);
    
    $v1->set_incident_edge($e1);
    $v2->set_incident_edge($e2);

    $e1->set_vertex0($v1);
    $e1t->set_vertex0($v2);
    $e2->set_vertex0($v2);
    $e2t->set_vertex0($v2); # shouldn't matter that it's the same, only need
                            # needs to exist for an is_infinte() test

    $e1->set_foot($foot1x, $foot1y);
    $e2->set_foot($foot2x, $foot2y);

    $e1->set_theta($theta);
    $e1->set_phi($phi);
    $e2->set_theta($theta);
    $e2->set_phi($phi);

    {
    my $p = $e1->point0();
    ok($p, "got something for point0");

    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,$foot1y,$r1)],
              "no offset, point0 x, y and r correct");
    }

    {
    my $p = $e1->point0(2.1);
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,1.9,$r1)],
              "offset in point request, point0 x, y and r correct");

    }

    $e1->offset(2.1);
    $e2->offset(2.1);

    {
    my $p = $e1->point0();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,1.9,$r1)],
              "offsets set on edges, point0 x, y and r correct");

    }

    $e1->offset(0.4);
    $e2->offset(1.9);

    {
    my $p = $e1->point0();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,3.6,$r1)],
              "different offsets set on edges, point0 x, y and r correct");

    }

    $e1->offset(4.1);
    $e2->offset(1.0);

    {
    my $p = $e1->point0();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($x1,$y1,$r1)],
              "offset greater than radius, point0 x, y and r correct");
    }

    {
    my $p = $e1->point0(undef,'raw');
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($x1,-0.1,$r1)],
              "offset greater than radius, raw, point0 x, y and r correct");
    }

}


__END__
