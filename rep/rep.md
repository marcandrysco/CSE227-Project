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
Our tool provides a framework for fuzz testing media content with focus on the
audio and video tags. Unlike generalized fuzz testing tools, our tool targets
the audio and video tags with randomized data, both generated structurally
from the media format specifications and by corrupted otherwise valid,
properly handed media files. Using our techniques, we are able to exercise
critical bugs, allowing a malicious website to crash the browser and sometimes
even crash the entire operating system. These bugs warrant further
investigation as they may be attack vectors for exploits.

# Overview
This is the overview. It describes at a high level what we did.

# What do we call this section?
This is the technical section. It goes into detail.

All pages are deterministically generated via a seed parameter that can be
passed to the page. The seed allows the user to reproduce crashes given a
fixed seed; however, crashes due to race conditions or timing bugs are
frequently not deterministic create hard to reproduce bugs. All references to
randomly generated data refer to random generation based on this fixed seed.

User-land crashes are detected and analyzed by running the browser under a
debugger.

## Random Page Layout

In order to stress the web rendering engine, web pages are constructed with a
random layout, nesting elements appropriately, generating on the order of one
hundred video and audio tags for a single page. After an interval of twenty
seconds, the page is updated, randomly selecting elements for removing and
replacing them with new content containing additional media elements. This
process is repeated until the page is terminated or a bug causes the page to
crash.

The pages are generated with numerous random properties set for every element,
generating media tags to be overlapping with a variety of position, opacity,
and styling attributes. Creating elements in this way allows the web page to
stress the rendering engine and hopes to discover issues arising from media
content layed out in an unexpected fashion.

## Structured Random Audio Media


## Corrupted Audio and Video Media

Corrupted media is generated based on valid media files to generate introduce
subtle errors in what would otherwise be a valid file. 

# Evaluation

This is the evaluation. It explains how we tested our ideas / implementation, and presents interesting results.

By randomly corrupting MPEG-4 video files, we were able to trigger a segfault in Firefox's custom memory allocator.

# Related Work
This is the related work. It talks about stuff other people have done that is similar to what we did.

# Conclusion
This is the conclusion. It reminds people of what we just said.

[ClusterFuzz]: https://code.google.com/p/clusterfuzz/
[Javascript-fuzz]: https://github.com/NodeGuy/JavaScript-fuzz
[JSFunFuzz]: https://code.google.com/p/google-caja/source/browse/trunk/src/third_party/js/jsfunfuzz/jsfunfuzz.js?r=1767
[CREST]: http://jburnim.github.io/crest/
[KLEE]: https://klee.github.io
[AEG]: http://security.ece.cmu.edu/aeg/
[paper]: http://dl.acm.org/citation.cfm?id=2541977
[V8]: https://code.google.com/p/v8/
[address disclosure vulnerabilities]: http://blog.beyondtrust.com/zd_threat/internet-explorer-9-memory-disclosure
[arbitrary code execution]: https://bugzilla.mozilla.org/show_bug.cgi?id=796866
[cross-site scripting]: http://net-security.org/dl/articles/WHXSSThreats.pdf
[denial-of-service bugs]: https://www.evilfingers.com/advisory/Google_Chrome_Browser_0.2.149.27_in_chrome_dll.php
