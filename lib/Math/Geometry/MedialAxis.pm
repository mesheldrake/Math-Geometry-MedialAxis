package Math::Geometry::MedialAxis;

use 5.006;
use strict;
use warnings;

=head1 NAME

Math::Geometry::MedialAxis

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Math::Geometry::MedialAxis;

    my $foo = Math::Geometry::MedialAxis->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 SUBROUTINES/METHODS

=head2 function1


=head2 function2


=head1 AUTHOR

Michael E. Sheldrake

=head1 BUGS

Probably.


=head1 SUPPORT


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2018 Michael E. Sheldrake.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut


{

use Boost::Polygon::Voronoi;
use XML::DOM::XPath;

use strict;
use warnings;

my $builder = Boost::Polygon::Voronoi::builder->new();
my $parser = XML::DOM::Parser->new('KeepCDATA' => 1);

sub new {
    my $class = shift;
    my $self = {
      ma => undef,
      cells => undef,
      edges => undef,
      vertices => undef,
      svg_url => '',
      last_svg_xpath => '',
      bpv_scale => 1 # need to set this based on input geometry and use before/after B:P:V runs
    };
    bless $self, $class;
    return $self;
}

sub cells {
    my $self = shift;
    if ($self->{ma}) {
        $self->{cells} = $self->{ma}->cells() if !$self->{cells}; # cache it
        return $self->{cells};
    } else { return []; }
}
sub edges {
    my $self = shift;
    if ($self->{ma}) {
        if (!$self->{edges}) {
            $self->{edges} = Math::Geometry::MedialAxis::EdgeCollection->new($self->{ma}->edges());
            foreach my $e (@{$self->{edges}}) {
                $e->set_foot($e->foot()->x()/$self->{bpv_scale},
                             $e->foot()->y()/$self->{bpv_scale});

            }
        }
        bless $_, 'Math::Geometry::MedialAxis::EdgeView' for @{$self->{edges}};
        $_->scale(1/$self->{bpv_scale}) for @{$self->{edges}};
        return $self->{edges};
    } else { return []; }
}
sub vertices {
    my $self = shift;
    if ($self->{ma}) {
        if (!$self->{vertices}) {
            $self->{vertices} = $self->{ma}->vertices() if !$self->{vertices};
            bless $_, 'Math::Geometry::MedialAxis::VertexView' for @{$self->{vertices}};
            $_->scale(1/$self->{bpv_scale}) for @{$self->{vertices}};
        }
        return $self->{vertices};
    } else { return []; }
}

sub to_svg {
    my $self = shift;
    my $outfn = shift;
    if (!$self->{svg_url}) { warn "Use from_svg() to set svg url before calling to_svg()"; return; }

    my $doc = $parser->parsefile($self->{svg_url});
    $doc->setXMLDecl($doc->createXMLDecl('1.0', 'UTF-8', 'yes')) if !$doc->getXMLDecl();
    
    my $svg_doc_frag = $self->edges->to_svg_dom($doc);
    
    my @nodes = $doc->findnodes($self->{last_svg_xpath});
    if ($nodes[-1]->getParentNode->getLastChild() == $nodes[-1]) { 
        $nodes[-1]->getParentNode->appendChild($svg_doc_frag);
    }
    else {
        $nodes[-1]->getParentNode->insertBefore($svg_doc_frag,$nodes[-1]->getNextSibling);
    }

    $doc->printToFile($outfn);

}

sub from_svg {
    my $self  = shift;
    my $url   = shift;
    my $xpath = shift;

    if ($xpath && !ref $xpath) {$xpath = [$xpath];}

    $self->{svg_url} = $url;


    ### need to generate this based on geometry read and integer limits of B:P:V
    $self->{bpv_scale} = 10000;
    ###


    my $doc = $parser->parsefile($url);
    
    $self->{last_svg_xpath} = $xpath->[-1];
    my %node_groups;
    $node_groups{$_} = [($doc->findnodes($_))] for @$xpath;
    
    foreach my $k (@$xpath) {
        print " $k : ",scalar(@{$node_groups{$k}})," \n";
        my $nodes = $node_groups{$k};
        print $nodes,"\n";
        print $_->getAttribute('d'),"\n" for @$nodes;
        my @segs = map _path_to_segments($_->getAttribute('d')), @$nodes;
        print "sl :",scalar(@segs),"\n";
        $self->add_segments([map {@$_} @segs]);
    }
    warn "doing... with $builder ...\n";
    $self->{ma} = $builder->medial_axis();
    warn "done\n",scalar(@{$self->{ma}->edges()}),"\n";
    
}

sub add_segments {
    my $self = shift;
    my $segs = shift;
    my $bad_seg_report='';
    my $insert_last_index;
    print "sl now: ",scalar(@$segs),"\n";
    for (my $i=0;$i<@$segs;$i++) {
        eval {
            $insert_last_index = $builder->insert_segment(map {$_ * $self->{bpv_scale}} @{$segs->[$i]});
            #print "\n[",join(",",map {$_ * $self->{bpv_scale}} @{$segs->[$i]}),"]\n";
        };
        if ($@) { 
            $bad_seg_report = 'trouble inserting this segment: ['.
                              join(',',@{$segs->[$i]}).
                              ']'.$@;
            last;
        }
    }
    if ($bad_seg_report) { die $bad_seg_report; }

}

sub _path_to_segments {
    my $path = shift;

    # skip e in split because sometimes it's in numbers like 1.0e-12
    my @segs = split(/([a-df-zA-DF-Z])/, $path);
    shift @segs if $segs[0] =~ /^\s*$/;
    # give any Z commands coordinates from preceeding M
    my $curr_m_coords = $segs[1];
    for (my $i=0;$i<@segs;$i+=2) {
        if ($segs[$i]=~/M/) { $curr_m_coords = $segs[$i+1]; }
        elsif ($segs[$i]=~/Z/) { $segs[$i+1] = $curr_m_coords; }
    }
    # check that path is all absolute linear moves
    if (scalar(@segs) != scalar((grep {($_ % 2 == 0 ? ($segs[$_]=~/[MLHVZ]/) : ($segs[$_]=~/[0-9eE\.\- ,]+/))} (0..$#segs)))) {
        #$bad_path_report = join('.',map {$_ % 2 == 0 ? $segs[$_].'['.($segs[$_]=~/[MLHVZ]/ ? 'ok':'bad').']' : $segs[$_].'['.($segs[$_]=~/[0-9eE\.\- ,]+/ ? 'ok':'bad').']'} (0..$#segs));
        warn "Bad path or failure to parse correctly. path: $path";
        last;
    }
    # turn path into segments ready for builder->insert_segment()
    my @these_builder_path_segs;
    my $last_x;
    my $last_y;
    for (my $i=0;$i<@segs;$i+=2) {
        my $cmd = $segs[$i];
        my $coords_string=$segs[$i+1];
        $coords_string=~s/^\s*(.*?)\s*$/$1/;
        my @coords = map {eval($_) + 0} split(/[ ,]+/, $coords_string);
        if ($cmd eq 'M') {($last_x,$last_y)=($coords[0],$coords[1]);}
        if ($cmd eq 'L' || $cmd eq 'Z') {
            for (my $j=0;$j<@coords;$j+=2) {
                push @these_builder_path_segs, [$last_x,$last_y,$coords[$j],$coords[$j+1]];
                ($last_x,$last_y) = ($coords[$j],$coords[$j+1]);
            }
        }
        if ($cmd eq 'H') {
            for (my $j=0;$j<@coords;$+=1) {
                push @these_builder_path_segs, [$last_x,$last_y,$coords[$j],$last_y];
                $last_x = $coords[$j];
            }
        }
        if ($cmd eq 'V') {
            for (my $j=0;$j<@coords;$+=1) {
                push @these_builder_path_segs, [$last_x,$last_y,$last_x,$coords[$j]];
                $last_x = $coords[$j];
            }
        }
    }
    
    return \@these_builder_path_segs;
}



# EdgeCollection

# Collection of Edges or EdgeViews
# Edge sequences that are subsets of the complete edge collection of the 
# half-edge graph, or sequences that omit some contiguous edges of the orignal 
# graph, will likely need special handling when returning lists of points.
# We don't want to clutter Edge or EdgeView objects with logic regarding their 
# position in arbitrary sequences. We'll put that sort of logic here.

package Math::Geometry::MedialAxis::EdgeCollection;

use strict;

sub new {
    my $class = shift;
    my $self = $_[0] // [];
    bless $self, $class;
    return $self;
}

sub is_loop {
    my $self = shift;
    print "is LOOP\n\n" if $self->[-1]->vertex1 == $self->[0]->vertex0;
    return $self->[-1]->vertex1 == $self->[0]->vertex0;
}

sub start_point {
    my $self = shift;
    return @{$self} ? $self->[0]->start_point(@_) : (); 
}

sub end_point   {
    my $self = shift;
    return @{$self} ? $self->[-1]->end_point(@_)  : ();
}
    
sub mid_points {
    return () if !@{$_[0]};
    my ($self, $resolution, $tool_diameter,$offset) = @_;
    my $start = $self->[0];
    my @start_mid = ();
    # special handling of first point in light of how interpolate is handled
    # for start and mid for each edge
    if (!($start->interpolate && $start->interpolate < $start->radius)) {
        push @start_mid, $start->mid_points($resolution, $tool_diameter);
        if ($#$self && $self->[1]->edge != $start->edge->next) {
            push @start_mid, $start->end_point($resolution, $tool_diameter);
        }
    }
    my @ret = grep $_, 
        (  (
            @start_mid,
            (map {($self->[$_]->start_point($resolution, $tool_diameter), 
                  ($self->[$_]->mid_points($resolution, $tool_diameter)),
                  # if the edge collection contains consecutive EdgeViews
                  # whose underlying Edges do not have the next() prev()
                  # relationship, include the end point of this edge, since 
                  # it will not be a duplicate of the next start point. 
                  # (same done above for @start_mid setup)
                  ($self->[$_ + 1]->edge != $self->[$_]->edge->next && !$self->[$_]->twinturn
                   ? $self->[$_]->end_point($resolution, $tool_diameter)
                   : ()
                  )
                  )} (1..$#$self - 1) ),
           $self->[-1]->mid_points($resolution, $tool_diameter)
           )
        );
    return @ret;
}

sub points {
    my ($self, $resolution, $tool_diameter) = @_;

    return grep $_, ($self->start_point($resolution, $tool_diameter),
                     $self->mid_points($resolution, $tool_diameter), 
                     $self->end_point($resolution, $tool_diameter)
                    );
}

sub points_edges_polygon {
    my ($self, $resolution, $tool_diameter) = @_;
    my @points = grep $_, ($self->start_point($resolution, $tool_diameter),
                     $self->mid_points($resolution, $tool_diameter), 
                     $self->end_point($resolution, $tool_diameter)
                    );
    my @epoints;

    if ($points[0]->[3]->isa('Math::Geometry::MedialAxis::EdgeView') && $points[0]->[3]->interpolate) {
        my $r1 = $points[0]->[3]->radius;
        my $r2 = $points[0]->[3]->next->radius;
        my $factor = abs(($points[0]->[3]->interpolate - ($r1 < $r2 ? $r1 : $r2)) / ($r1 - $r2));
        $factor = 1 - $factor if $r1 > $r2;
        
        #push @epoints, $points[0]->[3]->vertex_i($factor);
        #trying to make this first along-edge point cross the dge a little
        # so there's some cross over with another polygon made the same way that
        # we want to union with - otherwise this point is on that poly's line and
        # the point-on-line isn't always detected for the union intersection.
        push @epoints, $points[0]->[3]->point_i($points[0]->[3]->interpolate,$points[0]->[3]->interpolate + 2,1);
    }

    push @epoints , map [@{
        $_->[3]->edge->vertex0
        #$_->[3]->point0($_->[3]->radius + 2,1)
        }], grep $_->[3]->radius, @points[1..$#points];

    if ($points[-1]->[3]->isa('Math::Geometry::MedialAxis::EdgeView') && $points[-1]->[3]->interpolate) {
        my $r1 = $points[-1]->[3]->radius;
        my $r2 = $points[-1]->[3]->next->radius;
        my $factor = abs(($points[-1]->[3]->interpolate - ($r1 < $r2 ? $r1 : $r2)) / ($r1 - $r2));
        $factor = 1 - $factor if $r1 > $r2;
        
        #push @epoints, $points[-1]->[3]->vertex_i($factor);
        push @epoints, $points[-1]->[3]->point_i($points[-1]->[3]->interpolate,$points[-1]->[3]->interpolate + 2,1);
    }
    $_->[2] = 0 for @epoints[1..$#epoints-1]; # set radius to zero to mark the on-medial-axis points
    $epoints[0]->[2] = 0;
    $epoints[-1]->[2] = 0;
    return @points, reverse @epoints;
}

sub points_edges_twins_polygon {
    my ($self, $o2, $resolution, $tool_diameter) = @_;
    
    my @ret;

    my @points = grep !$_->[3]->vertex0->visited,
                   ($self->start_point($resolution, $tool_diameter),
                    $self->mid_points($resolution, $tool_diameter), 
                    $self->end_point($resolution, $tool_diameter)
                   );

    $_->[3]->vertex0->visited(1) for @points;

    my @edges;
    my $e = $self->[0];
    my $pi = 0;
    while ($e != $self->[-1]) {
        push @edges, $e if (!@edges || $e != $edges[-1]) && $e->twin->visited != 1;
        $e = $self->[$pi++];
    }
    push @edges, $e if (!@edges || $e != $edges[-1]) && $e->twin->visited != 1;
    
     my $twin_ec = Math::Geometry::MedialAxis::EdgeCollection->new([
        map {
            my $ne = Math::Geometry::MedialAxis::EdgeView->new(
                edge => $_->edge->twin,
                # zero, or else you'll get colinear points that Clipper will 
                # trim - and your left-right marker points are likely the ones
                # that will get lost, making for bad left-right splits as the
                # split will happen at a neighboring intersection that's not 
                # ideal or even correct.
                offset => 0,#$o2, 
                interpolate => $_->interpolate,
                intersect => $_->intersect,
                twinturn => $_->twinturn,
            );
            #Slic3r::MedialAxis::EdgeView::edges_svg([$_->twin,$_]);
            #Slic3r::MedialAxis::EdgeView::edge_svg($_);
            #Slic3r::MedialAxis::EdgeView::edge_svg($_->edge->twin);
            #Slic3r::MedialAxis::EdgeView::edges_svg([$_,$ne]);
            #Slic3r::MedialAxis::EdgeView::edge_svg($ne);

            $ne;
        } reverse @edges
    ]);
        
    $_->twin->edge->vertex0->visited(1) for @{$twin_ec};
    
    # Fix next<->prev refs for new sequence of twins.
    # mid_points() and end_point() need these to be right.
    for (my $i = 1; $i < @{$twin_ec}; $i++) {
        if ($twin_ec->[$i]->prev != $twin_ec->[$i - 1]) {
            $twin_ec->[$i]->prev($twin_ec->[$i - 1]);
        }
        if ($twin_ec->[$i - 1]->next != $twin_ec->[$i]) {
            $twin_ec->[$i - 1]->next($twin_ec->[$i]);
        }
    }

    my @epoints = map [@{$_}[0,1,2]], ( # drop edge refs
                     $twin_ec->start_point($resolution, $tool_diameter),
                     $twin_ec->mid_points($resolution, $tool_diameter),
                     $twin_ec->end_point($resolution, $tool_diameter)
                    );

    @points = map [@{$_}[0,1,2]], @points; # drop edge refs

    # mark the two sides

    $_->[2] = 2 for @points;
    $_->[2] = 3 for @epoints;

    # remove near duplicates

    for (@points, @epoints) { $_->[0] = int($_->[0]); $_->[1] = int($_->[1]); }

    for (my $i = $#points - 1; $i > -2; $i--) {
        if (   abs($points[$i]->[0] - $points[$i + 1]->[0]) < 2
            && abs($points[$i]->[1] - $points[$i + 1]->[1]) < 2) {
            splice @points, $i, 1;
        }
    }
    
    for (my $i = $#epoints - 1; $i > -2; $i--) {
        if (   abs($epoints[$i]->[0] - $epoints[$i + 1]->[0]) < 2 
            && abs($epoints[$i]->[1] - $epoints[$i + 1]->[1]) < 2) {
            splice @epoints, $i, 1;
        }
    }

    # Make sure @epoints' end points are not duplicates of @points' endpoints -
    # otherwise the marked splice points at the ends of @points might be 
    # masked/ignored by Clipper.

    if (@points) {
        shift @epoints while @epoints && abs($epoints[0]->[0] - $points[-1]->[0]) < 2 && abs($epoints[0]->[1] - $points[-1]->[1]) < 2;
        pop   @epoints while @epoints && abs($epoints[-1]->[0] - $points[0]->[0]) < 2 && abs($epoints[-1]->[1] - $points[0]->[1]) < 2;

        # return polygon
        if ($self->is_loop && @points) {
            $points[0]->[2] = (2 << 32) + 2;
            @ret = (\@points);
            if (@epoints) {
                $epoints[0]->[2] = (3 << 32) + 3;
                if (_is_counter_clockwise(\@points)) {
                    unshift @ret, [reverse @epoints];
                    @{$ret[1]} = reverse @{$ret[1]};
                } else {
                    push @ret, [reverse @epoints];
                    @{$ret[0]} = reverse @{$ret[0]};
                }
            }
        }
        # return polyline
         else {
            # mark the splice points
            $points[0]->[2]  = (2 << 32) + 3;
            $points[-1]->[2] = (3 << 32) + 2;

            @ret = ([@points, @epoints]);
        }
        @$_ = reverse @$_, for @ret; # if using twins, ends up clockwise, so reverse
    }
    
    return @ret;
}

sub _is_counter_clockwise {
	# Taken straight from Paul's site and a more
	# recent version, I think, of the page
	# as of 8/3/2009.
	# A different approach that he explicitly
	# says is for concave polygons.
	# http://local.wasp.uwa.edu.au/~pbourke/geometry/clockwise/index.html
	# Signed area of the polygon: plus for ccw, minus for cw.
	my $p=shift;
	my $area = 0;
	if (scalar(@{$p}) < 3) {return 0;}
	my $firstind=0;
	# in this case, we want the first and last point of the tested polygon to be the same
	# if the polygon isn't that way, "close" it yourself by starting the loop at index -1 
	# (then the last item of the array and the first get processed on first run of the for loop)
	if ( $p->[0]->[0] ne $p->[scalar(@{$p}) - 1]->[0] && $p->[0]->[1] ne $p->[scalar(@{$p}) - 1]->[1]) {$firstind=-1;}
	for (my $i=$firstind;$i<scalar(@{$p}) - 1;$i++) {$area+=0.5 * (($p->[$i]->[0] * $p->[$i+1]->[1]) - ($p->[$i+1]->[0] * $p->[$i]->[1]));}
	#print "ccw test area:$area\n";

	#if ($area > 0) { return 1; }
	#elsif ($area < 0) { return -1; }
	#else {return(0);}

    return $area >= 0 ? 1 : 0;

	}

sub to_svg_dom {
    my $ec = shift;
    my $doc = shift;
    
    my $dfrag = $doc->createDocumentFragment();

    my $sw = 0.3;

    my $s = $dfrag->appendChild($doc->createElement('style'));
    $s->appendChild($doc->createCDATASection("\n"
        .'.ev path {stroke-width:'.$sw.'}'."\n"
        .'.ev .ei  {stroke:blue;  stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"
        .'.ev .eo  {stroke:red;   stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"
        .'.ev .si  {stroke:green; stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"
        .'.ev .so  {stroke:orange;stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"
        .'.ev .f   {stroke:gray;  stroke-opacity:0.6;fill:none;stroke-dasharry:3 3;marker-end:url(#foot_square)}'."\n"
        .'.ev .sg  {stroke:gray;  stroke-opacity:0.6;fill:none;}'."\n"

        .'.ev .oi  {stroke:aqua; stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"
        .'.ev .oo  {stroke:aqua;stroke-opacity:0.6;fill:none;marker-end:url(#edge_arrow)}'."\n"

        .'.ev .ci  {fill:blue;    stroke:none;fill-opacity:0.2;}'."\n"
        .'.ev .coi {fill:blue;    stroke:none;fill-opacity:0.5;}'."\n"
        .'.ev .co  {fill:red;     stroke:none;fill-opacity:0.3;}'."\n"
        .'.ev .coo {fill:red;     stroke:none;fill-opacity:0.5;}'."\n"
    ));
    
    $_->to_svg_dom($dfrag) for @$ec;

    return $dfrag;
}

}

package Math::Geometry::MedialAxis::EdgeView;
{
#use Math::Geometry::MedialAxis::VertexView;
use Boost::Polygon::Voronoi;
use Hash::Util::FieldHash;

# This child class of Boost::Polygon::MedialAxis::Edge
# which is defined in Boost::Polygon::Voronoi.
# It's an "inside-out" class. This lets us add data fields to Edge objects
# even though though we really can't add data fields inside of Edge objects,
# since they are scalar references to C++ objects.
#
# The idea here is that you can (probably) re-bless those Edge objects as
# EdgeViews and they'll still work the old way AND not only have the added 
# methods here, but also the added data fields.

# EdgeViews are still references to pointers to Edges in C++.
# To do equality tests from Perl though, we need to overload
# to do the dereference before comparison.
use overload
    '==' => sub { ${$_[0]} == ${$_[1]} },
    '!=' => sub { ${$_[0]} != ${$_[1]} },
    bool => sub { defined $_[0] }
;

Hash::Util::FieldHash::fieldhash my %offset;
Hash::Util::FieldHash::fieldhash my %scale;
Hash::Util::FieldHash::fieldhash my %visited;
Hash::Util::FieldHash::fieldhash my %interpolate;
Hash::Util::FieldHash::fieldhash my %intersect;
Hash::Util::FieldHash::fieldhash my %twinturn;

push @Math::Geometry::MedialAxis::EdgeView::ISA, 'Boost::Polygon::MedialAxis::Edge';

sub new {
    my $class = shift;
    return bless Boost::Polygon::MedialAxis::Edge->new(@_), $class;
}

sub offset {
    my ($self, $offset) = @_;
    if (defined $offset) {$offset{$$self} = $offset;}
    else {return defined $offset{$$self} ? $offset{$$self} : 0;}
}
sub scale {
    my ($self, $scale) = @_;
    if (defined $scale) {$scale{$$self} = $scale;}
    else {return defined $scale{$$self} ? $scale{$$self} : 1;}
}
sub visited {
    my ($self, $visited) = @_;
    if (defined $visited) {$visited{$$self} = $visited;}
    else {return defined $visited{$$self} ? $visited{$$self} : 0;}
}
sub interpolate {
    my ($self, $interpolate) = @_;
    if (defined $interpolate) {$interpolate{$$self} = $interpolate;}
    else {return defined $interpolate{$$self} ? $interpolate{$$self} : 0;}
}
sub intersect {
    my ($self, $intersect) = @_;
    if (defined $intersect) {$intersect{$$self} = $intersect;}
    else {return defined $intersect{$$self} ? $intersect{$$self} : 0;}
}
sub twinturn {
    my ($self, $twinturn) = @_;
    if (defined $twinturn) {$twinturn{$$self} = $twinturn;}
    else {return defined $twinturn{$$self} ? $twinturn{$$self} : 1;}        #####  experiment with always on maybe? for moment
}

sub radius {
    my $self = shift;
    if ($self->is_infinite() && $self->vertex1()) {return undef;}
    return $self->vertex0()->r();
}

# something like these overrides accomplishes
# basic edge filtering for a given offset
sub next {
    my ($edge,$assign_edge) = @_;
    if ($assign_edge) {
        die "Attempt to assign next(edge) with wrong edge object class (",ref($assign_edge),")" 
            if ! (   $assign_edge->isa('Math::Geometry::MedialAxis::EdgeView')
                  || $assign_edge->isa('Boost::Polygon::MedialAxis::Edge'));
        $edge->SUPER::next($assign_edge);
    }
    if (defined $offset{$$edge} && $offset{$$edge} > $edge->SUPER::next()->vertex0()->r()) {
        return bless $edge->SUPER::twin(), ref($edge);
    }
    else {return bless $edge->SUPER::next(), ref($edge);}
}
sub prev {
    my ($edge,$assign_edge) = @_;
    if ($assign_edge) {
        die "Attempt to assign prev(edge) with wrong edge object class" 
            if ! (   $assign_edge->isa('Math::Geometry::MedialAxis::EdgeView')
                  || $assign_edge->isa('Boost::Polygon::MedialAxis::Edge'));
        $edge->SUPER::prev($assign_edge);
    }
    if (defined $offset{$$edge} && $offset{$$edge} > $edge->vertex0()->r()) {
        return bless $edge->SUPER::twin(), ref($edge);
    }
    else {return bless $edge->SUPER::prev(), ref($edge);}
}
sub twin {
    my ($edge,$assign_edge) = @_;
    if ($assign_edge) {
        die "Attempt to assign twin(edge) with wrong edge object class" 
            if ! (   $assign_edge->isa('Math::Geometry::MedialAxis::EdgeView')
                  || $assign_edge->isa('Boost::Polygon::MedialAxis::Edge'));
        $edge->SUPER::twin($assign_edge);
    }
    return bless $edge->SUPER::twin(), ref($edge);
}
sub rot_next {
    return bless $_[0]->SUPER::twin()->next_vd(), ref($_[0]);
}
sub rot_prev {
    return bless $_[0]->SUPER::prev_vd()->twin(), ref($_[0]);
}


sub vertex0 {
    # Might make sense to cache these on perl side if they get refered to often,
    # instead of importing from C++ side every time this is called on SUPER.
    my $edge = shift;
    return undef if !$edge->SUPER::vertex0();
    my $v = bless $edge->SUPER::vertex0(), 'Math::Geometry::MedialAxis::VertexView';
    # maybe setting scale on edge is wrong?
    # since edge should be maybe more topological than geometric?
    # anyway, if vertex already has scale set, how do you coordinate that with
    # the edge's scale setting? edge scale could apply if vertex doesn't have scale.
    # but what to do if they both have scale settings?
    # maybe keep all the scale stuff on the objects that actually have geometric coordinate data
    #$v->scale($scale{$$edge}) if defined $v;
    return $v;
}
sub vertex1 {
    my $edge = shift;
    my $v = $edge->SUPER::vertex1();
    return undef if ! defined $v;
    bless $v, 'Math::Geometry::MedialAxis::VertexView';
    #$v->scale($scale{$$edge}) if defined $v;
    return $v;
}
sub vertex0_inf {
    my $edge = shift;
    my $length = shift;
    my $v1 = $edge->SUPER::vertex1();
    my $a=$edge->twin()->theta();
    my $xinf = $v1->x() + ($length) * cos($a);
    my $yinf = $v1->y() + ($length) * sin($a);
    my $v = Math::Geometry::MedialAxis::VertexView->new($xinf,$yinf,$length);
#    $v->scale($scale{$$edge}) if defined $v;
    return $v;
}
sub vertex1_inf {
    my $edge = shift;
    my $length = shift;
    my $v0 = $edge->SUPER::vertex0();
    my $a=$edge->theta();
    my $xinf = $v0->x() + ($length) * cos($a);
    my $yinf = $v0->y() + ($length) * sin($a);
    my $v = Math::Geometry::MedialAxis::VertexView->new($xinf,$yinf,$length);
#    $v->scale($scale{$$edge}) if defined $v;
    return $v;
}

sub to_svg_dom {
    my $edge = shift;
    my $d = shift;
    my $doc = $d->getOwnerDocument();
    
    my $g = $d->appendChild($doc->createElement('g'));
    $g->setAttribute('id',${$edge});
    $g->setAttribute('class','ev');

    my $e;
    my $escale = $edge->scale();
    
    if ($edge->is_infinite()) {
        $e = $g->appendChild($doc->createElement('path'));
        my $v0 = $edge->vertex0() ? $edge->vertex0() : $edge->vertex0_inf(90000);
        my $v1 = $edge->vertex1() ? $edge->vertex1() : $edge->vertex1_inf(90000);
        $e->setAttribute('class',$edge->is_primary() ? ($edge->is_internal() ? 'ei' : 'eo') : ($edge->is_internal() ? 'si' : 'so'));
        $e->setAttribute('d','M'.($v0->x() * $escale).','.($v0->y() * $escale)
                            .'L'.($v1->x() * $escale).','.($v1->y() * $escale));
    }
    elsif ($edge->is_primary()) {
        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class', $edge->is_internal() ? 'ci' : 'co' );
        $e->setAttribute('d','M'.($edge->vertex0()->x() * $escale).','.($edge->vertex0()->y() * $escale)
                            .( $edge->is_linear() ? 'L' : 'Q' .($edge->Q()->x() * $escale).','.($edge->Q()->y() * $escale).',')
                            .    ($edge->vertex1()->x() * $escale).','.($edge->vertex1()->y() * $escale)
                            .'L'.($edge->SUPER::next()->foot()->x() ).','.($edge->SUPER::next()->foot()->y() )
                            .'L'.($edge->foot()->x() )   .','.($edge->foot()->y() )
                            .'Z'
        );
        # make this the offset inside region
        #$e = $g->append($doc->createElement('path'));
        #$e->setAttribute('class',($edge->is_internal()?'coi':'coo'));
        #$e->setAttribute('d','M'.$vertex0()->x().','.$vertex0()->y()
        #                    .'L'.$vertex1()->x().','.$vertex1()->y()
        #                    .'L'.$edge->next(1)->foot()->x().','.$edge->next(1)->foot()->y()
        #                    .'L'.$edge->foot()->x().','.$edge->foot()->y()
        #                    .'Z'
        #);

        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class',$edge->is_internal() ? 'ei' : 'eo');
        $e->setAttribute('d','M'.($edge->vertex0()->x() * $escale).','.($edge->vertex0()->y() * $escale)
                            .( $edge->is_linear() ? 'L' : 'Q' .($edge->Q()->x() * $escale).','.($edge->Q()->y() * $escale).',')
                            .    ($edge->vertex1()->x() * $escale).','.($edge->vertex1()->y() * $escale));

        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class','f');
        $e->setAttribute('d','M'.($edge->vertex0()->x() * $escale).','.($edge->vertex0()->y() * $escale)
                            .'L'.$edge->foot()->x().','.$edge->foot()->y());

        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class','sg');
        $e->setAttribute('d','M'.$edge->foot()->x().','.$edge->foot()->y()
                            .'L'.$edge->SUPER::next()->foot()->x().','.$edge->SUPER::next()->foot()->y());

        # the offset line

        my @off_points = map {[$_->[0] * $escale, $_->[1] * $escale]} $edge->points();

        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class',($edge->is_internal() ?'oi':'oo'));
        $e->setAttribute('d','M'.join('L',map {$_->[0].','.$_->[1]} @off_points));

    }
    elsif ($edge->is_secondary()) {
        $e = $g->appendChild($doc->createElement('path'));
        $e->setAttribute('class',$edge->is_internal() ? 'si' : 'so');
        $e->setAttribute('d','M'.($edge->vertex0()->x() * $escale).','.($edge->vertex0()->y() * $escale)
                            .'L'.($edge->vertex1()->x() * $escale).','.($edge->vertex1()->y() * $escale));

    }
    
}


# point0, point_i and point1 return points on, or offset from, 
# the original polygon.
# point0 is the location of the edge's foot on the polygon.
# point1 is the location of the next edge's foot on the polygon.
# The segment between those two points lies on one of the
# original polygon segments, or is offset and parallel to it.

sub point0 {
    my ($self, $off, $raw) = @_;

    $off //= $self->offset;
    
    my $thetaphi = $self->theta + $self->phi;
    
    if (   !$raw
        && $off > $self->radius
        && $off > $self->next->radius) {
        return [$self->vertex0->x(), $self->vertex0->y(), $self->vertex0->r()];
    }

    # Normally positive offset is from the polygon inward.
    # For now, if it's negative we'll offset from the medial axis edge outward.
    # But this convention might change with the evolution of the Edge and 
    # EdgeView models.
#    my $radius =  defined($off) && $off > 0 ? $self->radius - $off : -$off;

    # Now allowing zero offset from polygon - because to get zero offset from
    # medial axis edge is just a matter of setting offset > radius, handled
    # above - so now whe can get zero offset for both.
    my $radius =                   $off >= 0 ? $self->radius - $off : -$off;

    my $pointx = $radius * cos($thetaphi) + $self->vertex0->x();
    my $pointy = $radius * sin($thetaphi) + $self->vertex0->y();

    #$DB::svg->appendRaw("<!-- [$pointx, $pointy] selfoff: ".$self->offset." off: $off, thetaphi: $thetaphi = (".$self->theta."+".$self->phi.") radius: $radius -->") if $DB::svg;

    # watch for any cases of offset distance coming out with too much error
    if (0) {
        my $too_much_error; # e.g.: to watch for > 10% error, set to 0.1
        my  $calc_off = sqrt(($self->foot->x() - $pointx)**2 + ($self->foot->y() - $pointy)**2);
        if ($self->offset && abs($self->offset - $calc_off)/$self->offset > $too_much_error) {
            print "too much offset error: ",
                ($self->offset ? $self->offset : 'undef'),
                " vs ",$calc_off,"\n";
        }
    }
    return [$pointx, $pointy, $self->vertex0->r()];
    # how do we want to handle radius? Should it just always be edge radius.
    # or might we return the edge radius - offset, or even just offset (from polygon)
    # as partial radius more relevant to the the point returned?
    # with this partial radius distance from polygon value it easier to use that
    # with flow_width to do dynamic flow adjustmets (E or xy feedrate)
    }

sub point1 { 
    my ($self, $off, $raw) = @_;
    my $p;
    
    $off //= $self->offset;

    $p = $self->next->point0($off, $raw);

    if (   $self->SUPER::next() == $self->SUPER::twin() 
        || $self->next() == $self->twin() 
       ) {
        # (note we're checking underlying Edges, not EdgeViews)
        # account for phi not being the same for consecutive edges
        # in corners, where the medial axis touches the polygon,
        # or where a similar "twinturn" happens

        # old comment above
        # new comment below
        
        # maybe you wouldn't expect point1 to return this point
        # but this made sense back then. it evolved into the 
        # sensible, functioning approach. So don't undo it unless
        # you really know what you're undoing, and even then, consider
        # redoing it as an option.
        
        # ... kinda remembering ... yeah corners, and maybe negative
        # offsets in corners ... negative meaning offset out from
        # edge, not from polygon segment in ... maybe
        
        $p = [@{_reflect($p, 
                         [$self->vertex0()->x(),$self->vertex0()->y()], 
                         [$self->vertex1()->x(),$self->vertex1()->y()]
        )}[0,1], $p->[2]];
    }

    return $p;
    }

# point_i is an interpolated point between point0 and point1.
# $target is a target radius
# The idea is to find the point between point0 and point1 where the
# interpolated radius matches target. So this is only meant to work when
# $target is within the radius range of the edge.
# By default, this will return the interpolated point where the offset (from
# the polygon) segment intersects this medial axis edge (assuming it does).
# But other radius targets can be used for getting an interpolated point at, for
# example, some minimum or maximum radius threshold, if the edge stradles
# that threshold.

sub point_i {
    my ($self, $target, $offset) = @_;

    $offset //= $self->offset;
    $target //= $offset;
    
    my ($p1, $p2);
    if ($target == $offset) {
        $p1 = [$self->vertex0->x(),$self->vertex0->y(),$self->vertex0->r()];
        $p2 = [$self->vertex1->x(),$self->vertex1->y(),$self->vertex1->r()];
    } else {
        $p1 = $self->point0($offset, 1);
        $p2 = $self->point1($offset, 1);
    }

    my $r1 = $self->radius;
    my $r2 = $self->next->radius;
    
    my $factor = abs(($target - ($r1 < $r2 ? $r1 : $r2)) / ($r1 - $r2));
    $factor = 1 - $factor if $r1 > $r2;

    return [$p1->[0] + $factor * ($p2->[0] - $p1->[0]),
            $p1->[1] + $factor * ($p2->[1] - $p1->[1]),
            $p1->[2] + $factor * ($p2->[2] - $p1->[2])
           ];
    }

# vertex0, and vertex1 are the edge's start and end points,
# while vertex_i is an interpolated point between them
sub vertex_i {
    my ($self, $factor) = @_;
    my ($p1, $p2) = ($self->vertex0, $self->vertex1);
    return [$p1->x() + $factor * ($p2->x() - $p1->x()),
            $p1->y() + $factor * ($p2->y() - $p1->y()),
            $p1->r() + $factor * ($p2->r() - $p1->r()),
            # this was $self->vertex0()->[4] # which for a vertex in the old system was the visited flag
            $self->vertex0()->visited(), # misleading because it may not be the same edge as $self
           ];
}

# start_point(), end_point() and mid_points() for an EdgeView take into account
# radius, offset, interpolate and twinturn options.
# Use these three methods to get points expressing the intended 
# "view" of an EdgeView.

sub start_point {
    my $self = shift;
    my $ret;
    if ($self->interpolate && $self->interpolate < $self->radius) {
        $ret = $self->point_i($self->interpolate);
    } elsif ($self->radius < $self->offset && $self->next->radius > $self->offset) {
        # the medial axis edge-intersecting point
        if ($self->twinturn) { $ret = $self->point_i;}
        else { $ret = [$self->vertex0()->x(),$self->vertex0()->y(),$self->vertex0()->r()]; }
    } elsif ($self->radius < $self->offset && $self->next->radius < $self->offset) {
        $ret = [$self->vertex0()->x(),$self->vertex0()->y(),$self->vertex0()->r()];
    } else {
        $ret = $self->point0;
    }
    #return $ret;
    return @$ret < 4 ? [@$ret,$self] : $ret;
}

sub end_point {
    my $self = shift;
    my $ret;
    if ($self->interpolate && $self->interpolate > $self->radius) {
        $ret = $self->point_i($self->interpolate);
    } elsif ($self->radius > $self->offset && $self->next->radius < $self->offset) {
        # the medial axis edge-intersecting point
        if ($self->twinturn) { $ret= $self->point_i;}
        else { $ret = [$self->vertex1()->x(),$self->vertex1()->y(),$self->vertex1()->r(), $self->next]; }
    } elsif ($self->radius < $self->offset && $self->next->radius < $self->offset) {
        $ret = [$self->vertex1()->x(),$self->vertex1()->y(),$self->vertex1()->r(), $self->next];
    } else {
        $ret = [@{$self->point1}[0,1,2] , $self->next];
    }
    #return $ret;
    return @$ret < 4 ? [@$ret,$self] : $ret;
}

sub mid_points {
    my ($self, $resolution, $tool_diameter) = @_;
    my @ret;
    if ($self->interpolate && 
       ($self->radius > $self->offset && $self->next->radius > $self->offset)
       && ($self->interpolate < $self->radius || $self->interpolate < $self->next->radius)
       && ($self->interpolate > $self->radius || $self->interpolate > $self->next->radius)
       ) {
        # new - on probation - probably gives duplicates
        # interpolate in cases where it's _not_ because of the offset exceeding radius threshold
        # would use in conjunction with non-uniform offsets between edges, and an offset change within in the span of an edge
        # note that if $e->[INTERPOLATE] is an array ref, $e->interpolate is an array of (radius target, offset to use)
        # so we can set up special interpolate points that don't use the edge's current offset value.
        # That might also help with doing samples for curves below. 
        push @ret, $self->point_i($self->interpolate);
    } elsif ( 
        !$self->twinturn &&
          (  ($self->radius > $self->offset && $self->next->radius < $self->offset)
          || ($self->radius < $self->offset && $self->next->radius > $self->offset)
          )
        ) {
        # the medial axis edge-intersecting point
        push @ret, $self->point_i;
    } elsif (
        0 && 
        $self->curved
        ) {
        # TODO: curve samples at given resolution
        # push @ret, $self->bezier_samples($resolution);
    } else {
        @ret = ();
    }
    for (@ret) {$_ = [@$_,$self] if @$_ < 4; }
    return @ret;

}

sub points {
    my $self = shift;
    
    return grep $_, (
             $self->start_point, 
             $self->mid_points, 
             $self->end_point
             );
}


sub _rotate_2d {
    my ($p1, $theta, $origin) = @_;
    return [(  ($p1->[0] - $origin->[0]) * CORE::cos($theta) 
             - ($p1->[1] - $origin->[1]) * CORE::sin($theta)
            ) + $origin->[0],
            (  ($p1->[1] - $origin->[1]) * CORE::cos($theta) 
             + ($p1->[0] - $origin->[0]) * CORE::sin($theta)
            ) + $origin->[1]
           ];
}

sub _reflect {
    my ($p1, $p2, $p3) = @_;
    my $dy = $p3->[1] - $p2->[1];
    my $dx = $p3->[0] - $p2->[0];
    if ($dy == 0 && $dx == 0) {
        # warn "Can't reflect about a point\n";
        return $p1;
        }
    my $theta = atan2($dy, $dx);
    $p1 = _rotate_2d($p1, -$theta, $p2);
    $p1->[1] -= $p2->[1];
    $p1->[1] *= -1.0;
    $p1->[1] += $p2->[1];
    return _rotate_2d($p1, $theta, $p2);
}

}


package Math::Geometry::MedialAxis::VertexView;
{
use Boost::Polygon::Voronoi;
use Hash::Util::FieldHash;

use overload
    '==' => sub { ${$_[0]} == ${$_[1]} },
    '!=' => sub { ${$_[0]} != ${$_[1]} },
    bool => sub { defined $_[0] }
;

# This child class of Boost::Polygon::Voronoi::Edge
# An "inside-out" class inheriting from Boost::Polygon::Voronoi::Vertex
# where we're able to add our own additional data fields to the objects. 

Hash::Util::FieldHash::fieldhash my %scale;
Hash::Util::FieldHash::fieldhash my %visited;

push @Math::Geometry::MedialAxis::VertexView::ISA, 'Boost::Polygon::MedialAxis::Vertex';

sub scale {
    my ($self, $scale) = @_;
    if (@_ == 2) {
        if (defined $scale) {$scale{$$self} = $scale;}
        else {delete $scale{$$self};}
    }
    else {return defined $scale{$$self} ? $scale{$$self} : 1;}
}
sub visited {
    my ($self, $visited) = @_;
    if (defined $visited) {$visited{$$self} = $visited;}
    else {return defined $visited{$$self} ? $visited{$$self} : 0;}
}

sub x { return ($_[0]->SUPER::x() * $_[0]->scale()); }

sub y { return ($_[0]->SUPER::y() * $_[0]->scale()); }

sub r { return ($_[0]->SUPER::r() * $_[0]->scale()); }

sub incident_edge { return bless $_[0]->SUPER::incident_edge(),
                                 'Math::Geometry::MedialAxis::EdgeView'; }

}



1;
