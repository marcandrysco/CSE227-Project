(function() {
  "use strict";

  /* create a random seed or retrieve it from the url */
  var seed = Math.floor(Math.random() * Math.pow(2, 32));
  var str = document.location.search.substring(1).split("&");
  var query = new Object();
  for(var i = 0; i < str.length; i++) {
    var kv = str[i].split("=");
    query[kv[0]] = kv[1];
  }
  if(query["seed"]) { seed = query["seed"]; }

  /* random number "class" */
  var Rand = {
    state: { x: 123456789, y: 362436069, z: 521288629, w: seed },
    /* retrieve the next pseudo-random number */
    next: function() {
      var t = (Rand.state.x ^ (Rand.state.x << 11)) >>> 0;
      Rand.state.x = Rand.state.y;
      Rand.state.y = Rand.state.z;
      Rand.state.z = Rand.state.w;
      Rand.state.w = (Rand.state.w ^ (Rand.state.w >>> 19) ^ (t ^ (t >>> 8))) >>> 0;
      return Rand.state.w;
    },
    /* create a random number in the interval [min,max] */
    range: function(min, max) {
      if(min < 0) { min = 0; }
      if(max < 0) { max = 0; }
      return min + Rand.next() % (max - min + 1);
    },
    /* create a random number in the interval [0,lim) */
    index: function(limit) {
      return Rand.next() % limit;
    },
    /* retrieve a random element from the array */
    element: function(arr) {
      return arr[Rand.index(arr.length)]
    },
    /* create a random floating point number in the interval [0,1) */
    float: function() {
      return Rand.next() / Math.pow(2,32);
    },
    /* create a weighted random boolean */
    bool: function(weight)
    {
      if(weight === undefined) { weight = 0.5; }
      return Rand.float() < weight;
    }
  };

  /* element generator "class" */
  var Gen = {
    attr: function(style) {
    },
    /* div element, random attributes */
    div: function() {
      var el = document.createElement("div");
      if(Rand.bool()) {
        el.style.display = "inline-block";
      }
      if(Rand.bool(0.2)) {
        el.style.position = ["static", "relative", "absolute", "fixed", "sticky"][Rand.index(5)];
      }
      if(Rand.bool()) {
        el.style.width = Rand.range(0, 100);
        el.style.height = Rand.range(0, 100);
      }
      if(Rand.bool()) {
        el.style.backgroundColor = "rgb(" + Rand.index(256) + "," + Rand.index(256) + "," + Rand.index(256) + ")";
      }
      if(Rand.bool()) {
        el.style.opacity = Rand.float();
      }
      return el;
    },
    /* span element */
    span: function() {
      return document.createElement("span");
    },
    /* audio element */
    audio: function() {
      var el = document.createElement("audio");
      var types = ["mpeg", "corrupt-mpeg", "corrupt-ogg", "corrupt-wav", "corrupt-wma"];
      if(Rand.bool(0.2)) { el.style.position = ["static", "relative", "absolute", "fixed", "sticky"][Rand.index(5)]; }
      if(Rand.bool()) { el.style.width = Rand.range(0, 100); el.style.height = Rand.range(0, 100); }
      if(Rand.bool()) { el.style.backgroundColor = "rgb(" + Rand.index(256) + "," + Rand.index(256) + "," + Rand.index(256) + ")"; }
      if(Rand.bool()) { el.style.opacity = Rand.float(); }
      if(Rand.bool()) { el.controls = true; }
      if(Rand.bool()) { el.loop = true; }
      if(Rand.bool()) { el.autoplay = true; }
      if(Rand.bool()) { el.muted = true; }
      if(Rand.bool(0.8)) { el.src = "http://localhost:9000/" + Rand.element(types) + "?seed=" + Rand.next() }
      return el;
    },
    /* audio element */
    video: function() {
      var el = document.createElement("video");
      var types = ["corrupt-mp4", "corrupt-ogv", "corrupt-webm", "corrupt-3gp", "corrupt-flv"];
      if(Rand.bool(0.2)) { el.style.position = ["static", "relative", "absolute", "fixed", "sticky"][Rand.index(5)]; }
      if(Rand.bool()) { el.style.width = Rand.range(0, 100); el.style.height = Rand.range(0, 100); }
      if(Rand.bool()) { el.style.backgroundColor = "rgb(" + Rand.index(256) + "," + Rand.index(256) + "," + Rand.index(256) + ")"; }
      if(Rand.bool()) { el.style.opacity = Rand.float(); }
      if(Rand.bool()) { el.controls = true; }
      if(Rand.bool()) { el.loop = true; }
      if(Rand.bool()) { el.autoplay = true; }
      if(Rand.bool()) { el.muted = Rand.bool(); }
      if(Rand.bool(0.8)) { el.src = "http://localhost:9000/" + Rand.element(types) + "?seed=" + Rand.next() }
      return el;
    }
  };

  /* finder "class" */
  var Find = {
    /* find a random element */
    element: function(el, depth) {
      if(!el.hasChildNodes()) { return el; }
      if(!Rand.bool(1 / Math.max(depth - 2, 1))) { return el; }
      return Find.element(Rand.element(el.childNodes), depth + 1);
    }
  };

  var options = {
    depthMin: 3,
    depthMax: 8,
    depthSpeed: 0.3
  };

  var tagList = [
    [ Gen.div ],
    [ Gen.span ],
    [ Gen.audio, Gen.video ]
  ];

  /* generate random data at a given depth */
  function gen(depth, tag) {
    while(Rand.bool(options.depthSpeed) && (tag < (tagList.length - 1))) { tag++; }
    var el = Rand.element(tagList[tag])();
    var n = Rand.range(options.depthMin-depth, options.depthMax-depth);
    for(var i = 0; i < n; i++) {
      var child = el.appendChild(gen(depth + 1, tag));
    }
    el.depth = depth;
    el.tag = tag;
    return el;
  }

  window.onload = function() {
    document.body.replaceChild(gen(0, 0), document.body.childNodes[0]);
    var shuffle = function() {
      var n = Rand.range(100,200);
      for(var i = 0; i < n; i++) {
        var el = Find.element(document.body, 0, document.body);
        el.parentNode.replaceChild(gen(el.depth, el.tag), el);
      }
    };
    for(var i = 1; i < 10; i++) {
      setTimeout(shuffle, i*20000);
    }
  };
})();
