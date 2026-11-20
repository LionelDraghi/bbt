#!/usr/bin/perl
# Minimal RFC6455 websocket client to poke gnoga's server on Windows.
use strict; use warnings;
use IO::Socket::INET;
use MIME::Base64;

my $port = shift @ARGV || 8081;
my $sock = IO::Socket::INET->new(PeerAddr => '127.0.0.1', PeerPort => $port,
                                 Proto => 'tcp', Timeout => 10)
  or die "connect: $!";
$sock->autoflush(1);

my $key = encode_base64("SIMULATED" x 2, ''); # 16 bytes of junk
my $hs = "GET /gnoga HTTP/1.1\r\nHost: 127.0.0.1:$port\r\n"
       . "Upgrade: websocket\r\nConnection: Upgrade\r\n"
       . "Sec-WebSocket-Key: $key\r\nSec-WebSocket-Version: 13\r\n\r\n";
print $sock $hs;

my $resp = '';
while ($resp !~ /\r\n\r\n/) {
    my $c; $sock->recv($c, 4096);
    die "handshake closed" unless defined $c && length $c;
    $resp .= $c;
}
print "---- handshake reply ----\n$resp\n---- frames ----\n";

sub read_frame {
    my ($data) = @_;
    $sock->recv($data, 2);
    return 0 unless defined $data && length $data == 2;
    my ($b0, $b1) = unpack("C2", $data);
    my $len = $b1 & 0x7f;
    if ($len == 126) { $sock->recv($data, 2); $len = unpack("n", $data); }
    elsif ($len == 127) { $sock->recv($data, 8); $len = unpack("Q>", $data); }
    my $payload = '';
    while (length($payload) < $len) {
        my $c; $sock->recv($c, $len - length($payload));
        return 0 unless defined $c && length $c;
        $payload .= $c;
    }
    my $op = $b0 & 0x0f;
    my $fin = ($b0 & 0x80) ? 1 : 0;
    return undef if $op == 8;                      # close
    if ($op == 9) {                                # ping -> pong
        print $sock pack("C2", 0x8A, 0x00);
        print "-- sent pong for ping\n";
        return 1;
    }
    print "op=$op fin=$fin len=$len : $payload\n";
    return 1;
}

my $n = 0;
while ($n < 40) {
    my $r = read_frame();
    last unless defined $r && $r == 1;
    $n++;
}

# find the Step button id in the received scripts, and click it
my $timeout_at = time() + 15;
while (time() < $timeout_at) {
    my $r;
    eval { $r = read_frame(); 1 } or last;
    last unless defined $r && $r == 1;
    # look for a fresh state for 'salut' or the run having ended
    select(undef, undef, undef, 0.2);
}

print "---- done reading; now clicking Step repeatedly\n";
# Click message: <id>|click|x|y|screenX|screenY|which|alt|ctrl|shift|meta
# We do not know the button id yet; send one click per plausible gnoga id 1..40
for my $id (1 .. 40) {
    my $msg = "$id|click|0|0|0|0|0|false|false|false|false";
    my $mask = pack("C4", map { int(rand(256)) } 1 .. 4);
    my $masked = $msg ^ $mask x length($msg);
    my $frame = pack("CC", 0x81, 0x80 | length($masked)) . $mask . $masked;
    print $sock $frame;
    select(undef, undef, undef, 0.15);
}
my $end = time() + 8;
while (time() < $end) {
    my $r;
    eval { $r = read_frame(); 1 } or last;
    last unless defined $r && $r == 1;
}
print $sock pack("C2", 0x88, 0x00);  # close
print "---- closed\n";
