% Fuzzing HTML5
% Marc Andrysco and Eric Seidel

# Abstract

This is the abstract. It convinces people to read the report.

# Introduction

While more and more applications transition from the desktop to the web,
many of the security and privacy features provided that were once taken for
granted by the operating system are lagging behind. Recent additions to web
browsers include support for: WebGL, canvas, video and audio tags, WebSockets,
CSS3, and Local Storage. These features are motivated by the needs of
application developer needs without necessarily thoroughly scrutinized for
security. With the rapid pace of development of web standards, these
technologies have not been given the time to find a correct the existing bugs.

Prior work to find bugs in web technologies has already yielded a fair number
of tools. In particular, fuzz testing frameworks attempt find unexpected
behavior by randomly generating unexpected input vectors. The Chrome
developers have written the fuzzing tool [ClusterFuzz] for continuous
integration of the Chrome browser, searching for regressions as they occur.
The [Javascript-fuzz] tool generates a wide range of random Javascript values
covering the ECMAScript 5.1 standard. The [JSFunFuzz] tool was written by the
Mozzila developers for fuzz testing the Firefox javascript engine.

These have focused on previously untested portions of web standard, especially
on Javascript, which rapidly became the core programming language of the web.
Our tool provides a framework for fuzz testing media content with a focus on
the audio and video tags. Unlike generalized fuzz testing tools, our tool
targets the audio and video tags with randomized data, both generated
structurally from the media format specifications and by corrupted otherwise
valid, properly handled media files. Using our techniques, we are able to
exercise critical bugs, allowing a malicious website to crash the browser and
sometimes even crash the entire operating system. These bugs warrant further
investigation as they may be attack vectors for exploits.

# A Media Fuzzing Web Page and Server

Our goal is to uncover bugs in web browser implementations of media
decoders and players, thus we must generate both random media files and
random pages that embed the files. To this end we built a web server
that serves random media files, and a javascript application the creates
random page layouts.

Reproducibility is paramount for diagnosing the source of any uncovered
bugs, thus all of the randomness in our system is actually
deterministically simulated based on a user-configurable seed
parameter. All future references to randomly generated data in this
section refer to "random" generation based on a fixed seed.

User-land crashes are detected and analyzed by running the browser under
a debugger.

## Random Page Layout

In order to stress the web rendering engine, web pages are constructed with a
random layout, nesting elements appropriately, generating on the order of one
hundred video and audio tags for a single page. After an interval of twenty
seconds, the page is updated, randomly selecting elements for removing and
replacing them with new content containing additional media elements. This
process is repeated until the page is terminated or a bug causes the page to
crash.

The pages are generated with style and DOM attributes randomly set for each
element. Media tags are generated to be overlapping with a variety of
position, opacity, and styling attributes. Creating elements in this way
allows the web page to stress the rendering engine in hopes to discover issues
arising from media content layed out in an unexpected fashion.

## Structured Random Audio Media

We generate random MP3 audio files from a simple description of the
format as a Haskell datatype, using the [QuickCheck] library. For each
metadata field, we randomly choose valid or invalid values, in order to
exercise a variety of code paths. We generate 250-500KB of random data
for the actual audio stream. A simple web server then listens for
requests from the fuzzing page and creates random MP3s on demand.

## Corrupted Audio and Video Media

In addition to structurally fuzzing MP3 files, we randomly corrupt valid
files from a range of other media standards, including MPEG-4, Ogg
Vorbis, Flash video, etc. We gathered a single sample file per format
and randomly overwrite between 10 and 10,000 individual bytes on demand,
via the web server.

This approach scales much better than structured fuzzing as we do not
need to model the myriad media formats, we only need valid sample files.

# Evaluation

Using our framework for fuzzing data inputs, we were able to trigger a wide
range of crashing bugs. In particular, we noticed that MPEG-4 video files
caused the largest number of bugs, likely due to the video format's
complexity. Firefox was especially vulnerable to crashing when parsing corrupt
video data on all platforms. After filtering out videos that expose crashes,
our framework caused Firefox to quickly consume all available memory. Crashes
were less reliable on Chrome; typically, the isolated tab would crash,
although there were a few instances where the entire browser was taken down.
On the other hand, our testing was never able to cause crashes or memory leaks
in Internet Explorer.

## Firefox Metadata Segfault

Specific types of corruption of MPEG-4 video files cause an immediate crash
of Firefox. Our investigation of the bug revealed that an uninitialized
pointer is passed to the free function, causing a segmentation fault. In our
limited testing, we were unable to determine if an attacker could control the
unitialized pointer value in order to perform a remote exploit. Even without
the ability to control the pointer value, an malicious website can easily
crash the Firefox, closing all open windows and tabs.

We traced the bug down to the following code.

```c
GError* error;
gchar* debug;
gst_message_parse_error(message, &error, &debug);
//...
g_error_free(error);
g_free(debug);
```

In particular, the debug pointer is uninitialized before the call to
`gst_message_parse_error`. The documentation states that "the values
returned in the output arguments are copies; the caller must free them
when done." However, calling `gst_message_parse_error` on the specially
crafted MPEG-4 file prevents the call from ever writing to debug and the
unitialized value is then passed to `g_free`. The allocator attempts to
free the value, causing the segmentation fault. Interestingly, the sample code
provideed by GStreamer sets `debug` to `NULL` before the function call and
explicitly checks for `NULL` afterwards. While this suggests that that the
caller code should handle `NULL` values, this requirement is not present in
the documentation. We are therefore uncertain whether the bug belongs to
Firefox or GStreamer.

## Mac OS X Kernel Panic

Running our framework using Firefox in Mac OS X causes a kernel panic
that forces the user to reboot the entire system. The panic occurs while
executing privileged code in a sandboxed process dedicated to decoding
video. Our analysis found that the panic only occurs when attempting to
play hundreds of corrupt video files simultaneously. This leads us to
believe that the bug is caused by a race condition that occurs when
subjecting the sandboxed process to a heavy load.

Upon inspecting the resulting kernel panic, we noticed that error code
varied between a general protection fault and a page fault. Typically,
the instruction pointer was set to either the value `0x0000000000000000`
or `0xFFFFFFFFFFFFFFFF` indicating an invalid jump or return. While the
error was very quick to reproduce, only requiring a few seconds, we were
unable to find a reliable method of producing identical kernel
panics. Because this crash affect privileged code, we find the bug
particularly worrisome and plan to investigate further.

# Related Work

In contrast to our approach, which treats the browser as a black-box,
white-box fuzzing symbolically executes the program it is testing in
order to group equivalent inputs. [SAGE] instruments an initial run of a
program on a valid input and uses a generational search algorithm to
enumerate further inputs that are very similar to the initial input. It
has been used to find exploits in several undisclosed media formats as
well as a Microsoft Office application. [AEG] combines the [KLEE]
symbolic executor with a binary analysis to automatically generate
working exploits, and found exploits in a variety of UNIX
applications. Web browsers, however, are substantially larger than the
programs tested by white-box approaches, and to the best of our
knowledge nobody has successfully applied symbolic execution to a
browser.

The fuzzing tool [ClusterFuzz] performs fuzz tests for continuous integration
of Chrome. The [Javascript-fuzz] tool is used for generating random Javascript
values. Mozzila has developed the [JSFunFuzz] tool for testing the Javascript
engine of Firefox. Peach Fuzzer provides a general method of creating random
data based on a XML specification.

# Future Work

Our work showns that fuzzing inputs to the browser is a viable method of
finding severe bugs, many of which have the potention to become exploits.
This leaves room for additional, more comprehensive fuzzing tools that can
test a wider range of the web standards.

Based on the fact that a majority of bugs were discovered by performing random
perturbations on valid inputs, we believe that there is significant potential
for building tools to crawl existing websites using a proxy that can perform
deterministic corruption of data. Such a method allows for simplified fuzz
testing of all web technologies currently in use, and provides the added
benifit that future standards will be automatically tested as they become used
by the public.

# Conclusion

Fuzz testing is not a new idea, and media formats are a well-known
source of security vulnerabilities. However, our experience suggests
that browser vendors do not test their software in a sufficiently
adversarial manner. We have shown that it is both easy and effective to
fuzz test the media players shipped with modern browsers, and recommend
the vendors do so as well.

[ClusterFuzz]: https://code.google.com/p/clusterfuzz/
[Javascript-fuzz]: https://github.com/NodeGuy/JavaScript-fuzz
[JSFunFuzz]: https://code.google.com/p/google-caja/source/browse/trunk/src/third_party/js/jsfunfuzz/jsfunfuzz.js?r=1767
[CREST]: http://jburnim.github.io/crest/
[KLEE]: https://klee.github.io
[AEG]: http://security.ece.cmu.edu/aeg/
[SAGE]: http://research.microsoft.com/en-us/projects/atg/ndss2008.pdf
[paper]: http://dl.acm.org/citation.cfm?id=2541977
[V8]: https://code.google.com/p/v8/
[address disclosure vulnerabilities]: http://blog.beyondtrust.com/zd_threat/internet-explorer-9-memory-disclosure
[arbitrary code execution]: https://bugzilla.mozilla.org/show_bug.cgi?id=796866
[cross-site scripting]: http://net-security.org/dl/articles/WHXSSThreats.pdf
[denial-of-service bugs]: https://www.evilfingers.com/advisory/Google_Chrome_Browser_0.2.149.27_in_chrome_dll.php
[QuickCheck]: http://hackage.haskell.org/package/QuickCheck
