#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 7;
use Math::Geometry::MedialAxis;

{

    # two simply rectangular consecutive edges

    my $x0=-4.0;
    my $y0=0.0;
    my $r0=4.0;

    my $x1=0.0;
    my $y1=0.0;
    my $r1=4.0;

    my $x2=4.0;
    my $y2=0.0;
    my $r2=4.0;
        
    my $foot0x = $x0;
    my $foot0y = $r0;

    my $foot1x = $x1;
    my $foot1y = $r1;

    my $foot2x = $x2;
    my $foot2y = $r2;

    my $theta = 0.0;
    my $phi = 2.0 * atan2(1,1);

    my $e0  = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e0t = Math::Geometry::MedialAxis::EdgeView->new(1,1);

    my $e1  = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e1t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2  = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    my $e2t = Math::Geometry::MedialAxis::EdgeView->new(1,1);
    $e1->set_twin($e1t); # source of vertex1() for e1
    $e1t->set_twin($e1);
    $e2->set_twin($e2t); # source of vertex1() for e2 - because is_infinite()
    $e2t->set_twin($e2); # will be called, and that references twin
    $e0->set_twin($e0t);
    $e0t->set_twin($e0);
    
    $e0->set_next($e1);
    $e1->set_next($e2);
    $e2t->set_next($e1t);
    $e1t->set_next($e0t);

    my $v0 = Math::Geometry::MedialAxis::VertexView->new($x0,$y0,$r0);

    my $v1 = Math::Geometry::MedialAxis::VertexView->new($x1,$y1,$r1);
    my $v2 = Math::Geometry::MedialAxis::VertexView->new($x2,$y2,$r2);
    
    $v0->set_incident_edge($e0);

    $v1->set_incident_edge($e1);
    $v2->set_incident_edge($e2);

    $e0->set_vertex0($v0);
    $e0t->set_vertex0($v1);

    $e1->set_vertex0($v1);
    $e1t->set_vertex0($v2);
    $e2->set_vertex0($v2);
    $e2t->set_vertex0($v2); # shouldn't matter that it's the same, only need
                            # needs to exist for an is_infinte() test

    $e0->set_foot($foot0x, $foot0y);

    $e1->set_foot($foot1x, $foot1y);
    $e2->set_foot($foot2x, $foot2y);

    $e0->set_theta($theta);
    $e0->set_phi($phi);
    $e1->set_theta($theta);
    $e1->set_phi($phi);
    $e2->set_theta($theta);
    $e2->set_phi($phi);
    $e0t->set_theta($theta);
    $e0t->set_phi($phi);
    $e1t->set_theta($theta);
    $e1t->set_phi($phi);
    $e2t->set_theta($theta);
    $e2t->set_phi($phi);

    {
    my $p = $e0->point1();
    ok($p, "got something for point1");

    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,$foot1y,$r1)],
              "no offset, point1 x, y and r correct");
    }

    {
    my $p = $e0->point1(2.1);
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,1.9,$r1)],
              "offset in point request, point1 x, y and r correct");

    }

    $e0->offset(2.1);
    $e1->offset(2.1);
    $e2->offset(2.1);

    {
    my $p = $e0->point1();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,1.9,$r1)],
              "offsets set on edges, point1 x, y and r correct");

    }

    $e0->offset(1.9); # this is the offset applied, even at point1
    $e1->offset(0.4); # but if you asked for e1->point0 (same point, sort of) this offset would apply
    $e2->offset(1.9);

    {
    my $p = $e0->point1();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($foot1x,2.1,$r1)],
              "different offsets set on edges, point1 x, y and r correct");

    }

    $e0->offset(4.1);
    $e1->offset(4.1);
    $e2->offset(4.1);
    # Here we end up getting a next() that's really a twin
    # because of how our next() override does that when offset is greater
    # than radius. So for this test we offsets set on the twin too. Which
    # twin? Just $e1t really.
    $e0t->offset(4.1);
    $e1t->offset(4.1);
    $e2t->offset(4.1);

    {
    my $p = $e1->point1();
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($x2,$y2,$r2)],
              "offset greater than radius, point1 x, y and r correct");
    }

    {
    # This result might not make sense at first, but it's correct, for now.
    # See comment in reflecting section in edgeview->point1().
    my $p = $e1->point1(undef,'raw');
    is_deeply([map eval(sprintf("%.14f",$_)), @$p],
              [map eval(sprintf("%.14f",$_)), ($x2,0.1,$r2)],
              "offset greater than radius, raw, point1 x, y and r correct");
    }

}


__END__
