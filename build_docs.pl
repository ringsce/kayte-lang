#!/usr/bin/env perl
use strict;
use warnings;
use File::Find;
use File::Spec;
use File::Path qw(make_path);

# --- CONFIGURATION ---
my $SOURCE_DIR = './source';         # Where your .pas or .pp files are
my $DOCS_DIR   = './docs';        # Parent directory for documentation output
my $XML_DIR    = "$DOCS_DIR/xml"; # Directory to store generated skeleton files
my $HTML_DIR   = "$DOCS_DIR/html";# Directory where the final website is built
my $PACKAGE    = 'MyProject';     # FPDoc package grouping identifier

# Ensure output directories exist
make_path($XML_DIR, $HTML_DIR) unless -d $XML_DIR && -d $HTML_DIR;

print "🔍 Scanning '$SOURCE_DIR' for Free Pascal units...\n";

my @pascal_files;

# Find all Pascal source files recursively
find(sub {
    if (-f && /\.(pas|pp)$/i) {
        push @pascal_files, $File::Find::name;
    }
}, $SOURCE_DIR);

if (!@pascal_files) {
    die "❌ No .pas or .pp files found in $SOURCE_DIR\n";
}

print "📦 Found " . scalar(@pascal_files) . " source files. Beginning generation...\n\n";

foreach my $src_file (@pascal_files) {
    # Extract file base name (e.g., mathutils from ./src/mathutils.pas)
    my (undef, undef, $filename) = File::Spec->splitpath($src_file);
    my ($unit_name) = $filename =~ /^([^.]+)/;

    my $xml_file = File::Spec->catfile($XML_DIR, "$unit_name.xml");

    # Step 1: Generate XML Skeleton via 'makeskel' if it doesn't exist
    if (!-e $xml_file) {
        print "⚙️ [SKEL] Missing XML for '$unit_name'. Creating skeleton...\n";
        
        # Build makeskel command line structure
        my $skel_cmd = "makeskel --package=$PACKAGE --input=$src_file --output=$xml_file";
        system($skel_cmd) == 0 or warn "⚠️ Warning: Failed to create skeleton for $unit_name\n";
    } else {
        print "💾 [SKEL] Found existing XML description for '$unit_name'. Skipping skeleton creation.\n";
    }

    # Step 2: Compile HTML Documentation via 'fpdoc'
    print "🖥️ [FPDOC] Compiling HTML reference for '$unit_name'...\n";
    my $fpdoc_cmd = "fpdoc --package=$PACKAGE --input=$src_file --descr=$xml_file --format=html --output=$HTML_DIR";
    
    if (system($fpdoc_cmd) == 0) {
        print "✅ Successfully processed '$unit_name'.\n\n";
    } else {
        print "❌ Error compiling documentation for '$unit_name'.\n\n";
    }
}

print "🎉 Done! Final documentation website built inside: $HTML_DIR\n";
