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
exploits include address disclosure vulnerabilities [CITE], arbitrary code
execution [CITE], cross-site scripting [CITE], and denial-of-service bugs
[CITE].

## Prior Work

### Fuzz Testing

Fuzz testing frameworks attempt find unexpected behavior by randomly
generating unexpected input vectors. The Chrome developers have written the
fuzzing tool [ClusterFuzz] for continuous integration of the Chrome browser,
searching for regressions as they occur. The [Javascript-fuzz] tool generates
a wide range of random Javascript values covering the ECMAScript 5.1 standard.
The [JSFunFuzz] tool was written by the Mozzila developers for fuzz testing
the Firefox javascript engine.

### Dynamic Symbolic Execution

Dynamic-symbolic execution tools like [CREST] and [KLEE] combine symbolic
execution with concrete testing in order to quickly explore as many paths
through the program as possible. While often focused on pure bug finding,
[AEG] shows that the approach can also be extended to discovering paths that
are actually exploitable. These tools are often limited by the path explosion
in larger programs like a web browser, which renders the search space too large
for path enumeration. However, we may be able to reduce the search space by
slicing out program paths that relate to the specific APIs we want to target.
Furthermore, a recent [paper] describes generating symbolic executors for
interpreted languages by symbolically executing the interpreter itself, which
suggests that we may be able to scale an existing tool to, e.g. the [V8]
Javascript engine.

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
[CREST]: http://jburnim.github.io/crest/
[KLEE]: https://klee.github.io
[AEG]: http://security.ece.cmu.edu/aeg/
[paper]: http://dl.acm.org/citation.cfm?id=2541977
[V8]: https://code.google.com/p/v8/
