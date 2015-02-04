Project Proposal
================

## Motivation

While more and more applications transition from the desktop to the web,
many of the security and privacy features provided that were once taken for
granted by the operating system are lagging behind. Recent additions to web
browsers include support for:

 * WebGL
 * Canvas
 * Video and Audio Tags
 * Template Tags
 * WebSockets
 * CSS3
 * Local Storage

These features are motivated by the needs of application developer needs
without necessarily thoroughly scrutinized for security. With the rapid pace
of development of web standard, we believe that these technologies are prime
candidates for bugs, vulnerabilities, and exploits. For example, prior
exploits include address desclosure vulnerabilities [CITE], arbitrary code
execution [CITE], cross-site scripting [CITE], and denial-of-service bugs
[CITE].

## Prior Work

Fuzz testing frameworks attempt find unexpected behavior by randomly
generating unexpected input vectors. The Chrome developers have written the
fuzzing tool [ClusterFuzz] for continuous integration of the Chrome browser,
searching for regressions as they occur. The [Javascript-fuzz] tool generates
a wide range of random Javascript values covering the ECMAScript 5.1 standard.
The [JSFunFuzz] tool was written by the Mozzila developers for fuzz testing
the Firefox javascript engine.

--symbolic execution--

## Proposed Work

We proprose constructing a framework for structurally fuzz testing new web
technologies, including new HTML5, CSS3, and Javascript features. Unlike the
generalized fuzz testing tools mentioned above, we will construct tools for
creating valid HTML documents and javascript expressions that will target the
new APIs. We will additionally construct unstructured data as input to these
APIs, e.g. random bit vectors posing as AVI files. Although it may be
difficult or impossible to symbollically execute the entire web broswers, we
may explore using targeted symbolic execution (similar to AEG) for focusing on
specific APIs. Using these techniques, we hope to uncovered bugs and
vulnerabilites.

[ClusterFuzz]: https://code.google.com/p/clusterfuzz/
[Javascript-fuzz]: https://github.com/NodeGuy/JavaScript-fuzz
[JSFunFuzz]: https://code.google.com/p/google-caja/source/browse/trunk/src/third_party/js/jsfunfuzz/jsfunfuzz.js?r=1767
