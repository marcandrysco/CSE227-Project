use strict;

use Net::HTTPServer;
use String::Random;

my $server = new Net::HTTPServer();

$server->RegisterURL("/mp3", \&gen_mp3);
$server->RegisterURL("/corrupt-mp3", \&corrupt_mp3);

sub gen_mp3
{
    my $req = shift;             # Net::HTTPServer::Request object
    my $res = $req->Response();  # Net::HTTPServer::Response object

    my $seed = $req->Env("seed");
    srand($seed);

    my @types = ("audio/mpeg", "audio/MPA", "audio/mpa-robust");
    $res->Header("Content-Type", $types[rand @types]);

    $res->Print(pack "H4", "FFFB");     # mp3 sync word + version id
    $res->Print(pack "H4", "A040");     # other stuff, see http://en.wikipedia.org/wiki/MP3#File_structure
    # $res->Print(pack "H4", rand_hex(4)); # other stuff, see http://en.wikipedia.org/wiki/MP3#File_structure

    my $len = int(rand 500*1024); # number of bytes
    for (my $i=0; $i < $len; $i++) {
      $res->Print(pack("W",int(rand 255)));
    }

    return $res;
}

sub corrupt_mp3
{
    my $req = shift;
    my $res = $req->Response();
    
    my $seed = $req->Env("seed");
    srand($seed);

    my @types = ("audio/mpeg", "audio/MPA", "audio/mpa-robust");
    $res->Header("Content-Type", $types[rand @types]);

    open(my $fh, "<:raw", "dat/test.mp3");
    my ($n, $data);
    while(($n = read $fh, $data, 2048) != 0) {
        my $off = int(rand($n));
        my $len = int(rand($n - $off));
        my $rep = rand_hex($len);
        substr($data, $off, $len) = pack ("H".$len), rand_hex($len);
        $res->Print($data);
    }
    close $fh;

    return $res;
}

sub rand_hex
{
    my $len = shift;
    my $str = String::Random->new;
    return $str->randregex("[0-9A-F]{$len}");
}

$server->Start();

$server->Process();
