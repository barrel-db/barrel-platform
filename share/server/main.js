/*
    http://www.JSON.org/json2.js
    2010-03-20

    Public Domain.

    NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.

    See http://www.JSON.org/js.html


    This code should be minified before deployment.
    See http://javascript.crockford.com/jsmin.html

    USE YOUR OWN COPY. IT IS EXTREMELY UNWISE TO LOAD CODE FROM SERVERS YOU DO
    NOT CONTROL.


    This file creates a global JSON object containing two methods: stringify
    and parse.

        JSON.stringify(value, replacer, space)
            value       any JavaScript value, usually an object or array.

            replacer    an optional parameter that determines how object
                        values are stringified for objects. It can be a
                        function or an array of strings.

            space       an optional parameter that specifies the indentation
                        of nested structures. If it is omitted, the text will
                        be packed without extra whitespace. If it is a number,
                        it will specify the number of spaces to indent at each
                        level. If it is a string (such as '\t' or '&nbsp;'),
                        it contains the characters used to indent at each level.

            This method produces a JSON text from a JavaScript value.

            When an object value is found, if the object contains a toJSON
            method, its toJSON method will be called and the result will be
            stringified. A toJSON method does not serialize: it returns the
            value represented by the name/value pair that should be serialized,
            or undefined if nothing should be serialized. The toJSON method
            will be passed the key associated with the value, and this will be
            bound to the value

            For example, this would serialize Dates as ISO strings.

                Date.prototype.toJSON = function (key) {
                    function f(n) {
                        // Format integers to have at least two digits.
                        return n < 10 ? '0' + n : n;
                    }

                    return this.getUTCFullYear()   + '-' +
                         f(this.getUTCMonth() + 1) + '-' +
                         f(this.getUTCDate())      + 'T' +
                         f(this.getUTCHours())     + ':' +
                         f(this.getUTCMinutes())   + ':' +
                         f(this.getUTCSeconds())   + 'Z';
                };

            You can provide an optional replacer method. It will be passed the
            key and value of each member, with this bound to the containing
            object. The value that is returned from your method will be
            serialized. If your method returns undefined, then the member will
            be excluded from the serialization.

            If the replacer parameter is an array of strings, then it will be
            used to select the members to be serialized. It filters the results
            such that only members with keys listed in the replacer array are
            stringified.

            Values that do not have JSON representations, such as undefined or
            functions, will not be serialized. Such values in objects will be
            dropped; in arrays they will be replaced with null. You can use
            a replacer function to replace those with JSON values.
            JSON.stringify(undefined) returns undefined.

            The optional space parameter produces a stringification of the
            value that is filled with line breaks and indentation to make it
            easier to read.

            If the space parameter is a non-empty string, then that string will
            be used for indentation. If the space parameter is a number, then
            the indentation will be that many spaces.

            Example:

            text = JSON.stringify(['e', {pluribus: 'unum'}]);
            // text is '["e",{"pluribus":"unum"}]'


            text = JSON.stringify(['e', {pluribus: 'unum'}], null, '\t');
            // text is '[\n\t"e",\n\t{\n\t\t"pluribus": "unum"\n\t}\n]'

            text = JSON.stringify([new Date()], function (key, value) {
                return this[key] instanceof Date ?
                    'Date(' + this[key] + ')' : value;
            });
            // text is '["Date(---current time---)"]'


        JSON.parse(text, reviver)
            This method parses a JSON text to produce an object or array.
            It can throw a SyntaxError exception.

            The optional reviver parameter is a function that can filter and
            transform the results. It receives each of the keys and values,
            and its return value is used instead of the original value.
            If it returns what it received, then the structure is not modified.
            If it returns undefined then the member is deleted.

            Example:

            // Parse the text. Values that look like ISO date strings will
            // be converted to Date objects.

            myData = JSON.parse(text, function (key, value) {
                var a;
                if (typeof value === 'string') {
                    a =
/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*)?)Z$/.exec(value);
                    if (a) {
                        return new Date(Date.UTC(+a[1], +a[2] - 1, +a[3], +a[4],
                            +a[5], +a[6]));
                    }
                }
                return value;
            });

            myData = JSON.parse('["Date(09/09/2001)"]', function (key, value) {
                var d;
                if (typeof value === 'string' &&
                        value.slice(0, 5) === 'Date(' &&
                        value.slice(-1) === ')') {
                    d = new Date(value.slice(5, -1));
                    if (d) {
                        return d;
                    }
                }
                return value;
            });


    This is a reference implementation. You are free to copy, modify, or
    redistribute.
*/

/*jslint evil: true, strict: false */

/*members "", "\b", "\t", "\n", "\f", "\r", "\"", JSON, "\\", apply,
    call, charCodeAt, getUTCDate, getUTCFullYear, getUTCHours,
    getUTCMinutes, getUTCMonth, getUTCSeconds, hasOwnProperty, join,
    lastIndex, length, parse, prototype, push, replace, slice, stringify,
    test, toJSON, toString, valueOf
*/


// Create a JSON object only if one does not already exist. We create the
// methods in a closure to avoid creating global variables.

if (!this.JSON) {
    this.JSON = {};
}

(function () {

    function f(n) {
        // Format integers to have at least two digits.
        return n < 10 ? '0' + n : n;
    }

    if (typeof Date.prototype.toJSON !== 'function') {

        Date.prototype.toJSON = function (key) {

            return isFinite(this.valueOf()) ?
                   this.getUTCFullYear()   + '-' +
                 f(this.getUTCMonth() + 1) + '-' +
                 f(this.getUTCDate())      + 'T' +
                 f(this.getUTCHours())     + ':' +
                 f(this.getUTCMinutes())   + ':' +
                 f(this.getUTCSeconds())   + 'Z' : null;
        };

        String.prototype.toJSON =
        Number.prototype.toJSON =
        Boolean.prototype.toJSON = function (key) {
            return this.valueOf();
        };
    }

    var cx = /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
        escapable = /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
        gap,
        indent,
        meta = {    // table of character substitutions
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        },
        rep;


    function quote(string) {

// If the string contains no control characters, no quote characters, and no
// backslash characters, then we can safely slap some quotes around it.
// Otherwise we must also replace the offending characters with safe escape
// sequences.

        escapable.lastIndex = 0;
        return escapable.test(string) ?
            '"' + string.replace(escapable, function (a) {
                var c = meta[a];
                return typeof c === 'string' ? c :
                    '\\u' + ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
            }) + '"' :
            '"' + string + '"';
    }


    function str(key, holder) {

// Produce a string from holder[key].

        var i,          // The loop counter.
            k,          // The member key.
            v,          // The member value.
            length,
            mind = gap,
            partial,
            value = holder[key];

// If the value has a toJSON method, call it to obtain a replacement value.

        if (value && typeof value === 'object' &&
                typeof value.toJSON === 'function') {
            value = value.toJSON(key);
        }

// If we were called with a replacer function, then call the replacer to
// obtain a replacement value.

        if (typeof rep === 'function') {
            value = rep.call(holder, key, value);
        }

// What happens next depends on the value's type.

        switch (typeof value) {
        case 'string':
            return quote(value);

        case 'number':

// JSON numbers must be finite. Encode non-finite numbers as null.

            return isFinite(value) ? String(value) : 'null';

        case 'boolean':
        case 'null':

// If the value is a boolean or null, convert it to a string. Note:
// typeof null does not produce 'null'. The case is included here in
// the remote chance that this gets fixed someday.

            return String(value);

// If the type is 'object', we might be dealing with an object or an array or
// null.

        case 'object':

// Due to a specification blunder in ECMAScript, typeof null is 'object',
// so watch out for that case.

            if (!value) {
                return 'null';
            }

// Make an array to hold the partial results of stringifying this object value.

            gap += indent;
            partial = [];

// Is the value an array?

            if (Object.prototype.toString.apply(value) === '[object Array]') {

// The value is an array. Stringify every element. Use null as a placeholder
// for non-JSON values.

                length = value.length;
                for (i = 0; i < length; i += 1) {
                    partial[i] = str(i, value) || 'null';
                }

// Join all of the elements together, separated with commas, and wrap them in
// brackets.

                v = partial.length === 0 ? '[]' :
                    gap ? '[\n' + gap +
                            partial.join(',\n' + gap) + '\n' +
                                mind + ']' :
                          '[' + partial.join(',') + ']';
                gap = mind;
                return v;
            }

// If the replacer is an array, use it to select the members to be stringified.

            if (rep && typeof rep === 'object') {
                length = rep.length;
                for (i = 0; i < length; i += 1) {
                    k = rep[i];
                    if (typeof k === 'string') {
                        v = str(k, value);
                        if (v) {
                            partial.push(quote(k) + (gap ? ': ' : ':') + v);
                        }
                    }
                }
            } else {

// Otherwise, iterate through all of the keys in the object.

                for (k in value) {
                    if (Object.hasOwnProperty.call(value, k)) {
                        v = str(k, value);
                        if (v) {
                            partial.push(quote(k) + (gap ? ': ' : ':') + v);
                        }
                    }
                }
            }

// Join all of the member texts together, separated with commas,
// and wrap them in braces.

            v = partial.length === 0 ? '{}' :
                gap ? '{\n' + gap + partial.join(',\n' + gap) + '\n' +
                        mind + '}' : '{' + partial.join(',') + '}';
            gap = mind;
            return v;
        }
    }

// If the JSON object does not yet have a stringify method, give it one.

    if (typeof JSON.stringify !== 'function') {
        JSON.stringify = function (value, replacer, space) {

// The stringify method takes a value and an optional replacer, and an optional
// space parameter, and returns a JSON text. The replacer can be a function
// that can replace values, or an array of strings that will select the keys.
// A default replacer method can be provided. Use of the space parameter can
// produce text that is more easily readable.

            var i;
            gap = '';
            indent = '';

// If the space parameter is a number, make an indent string containing that
// many spaces.

            if (typeof space === 'number') {
                for (i = 0; i < space; i += 1) {
                    indent += ' ';
                }

// If the space parameter is a string, it will be used as the indent string.

            } else if (typeof space === 'string') {
                indent = space;
            }

// If there is a replacer, it must be a function or an array.
// Otherwise, throw an error.

            rep = replacer;
            if (replacer && typeof replacer !== 'function' &&
                    (typeof replacer !== 'object' ||
                     typeof replacer.length !== 'number')) {
                throw new Error('JSON.stringify');
            }

// Make a fake root object containing our value under the key of ''.
// Return the result of stringifying the value.

            return str('', {'': value});
        };
    }


// If the JSON object does not yet have a parse method, give it one.

    if (typeof JSON.parse !== 'function') {
        JSON.parse = function (text, reviver) {

// The parse method takes a text and an optional reviver function, and returns
// a JavaScript value if the text is a valid JSON text.

            var j;

            function walk(holder, key) {

// The walk method is used to recursively walk the resulting structure so
// that modifications can be made.

                var k, v, value = holder[key];
                if (value && typeof value === 'object') {
                    for (k in value) {
                        if (Object.hasOwnProperty.call(value, k)) {
                            v = walk(value, k);
                            if (v !== undefined) {
                                value[k] = v;
                            } else {
                                delete value[k];
                            }
                        }
                    }
                }
                return reviver.call(holder, key, value);
            }


// Parsing happens in four stages. In the first stage, we replace certain
// Unicode characters with escape sequences. JavaScript handles many characters
// incorrectly, either silently deleting them, or treating them as line endings.

            text = String(text);
            cx.lastIndex = 0;
            if (cx.test(text)) {
                text = text.replace(cx, function (a) {
                    return '\\u' +
                        ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
                });
            }

// In the second stage, we run the text against regular expressions that look
// for non-JSON patterns. We are especially concerned with '()' and 'new'
// because they can cause invocation, and '=' because it can cause mutation.
// But just to be safe, we want to reject all unexpected forms.

// We split the second stage into 4 regexp operations in order to work around
// crippling inefficiencies in IE's and Safari's regexp engines. First we
// replace the JSON backslash pairs with '@' (a non-JSON character). Second, we
// replace all simple value tokens with ']' characters. Third, we delete all
// open brackets that follow a colon or comma or that begin the text. Finally,
// we look to see that the remaining characters are only whitespace or ']' or
// ',' or ':' or '{' or '}'. If that is so, then the text is safe for eval.

            if (/^[\],:{}\s]*$/.
test(text.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g, '@').
replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']').
replace(/(?:^|:|,)(?:\s*\[)+/g, ''))) {

// In the third stage we use the eval function to compile the text into a
// JavaScript structure. The '{' operator is subject to a syntactic ambiguity
// in JavaScript: it can begin a block or an object literal. We wrap the text
// in parens to eliminate the ambiguity.

                j = eval('(' + text + ')');

// In the optional fourth stage, we recursively walk the new structure, passing
// each name/value pair to a reviver function for possible transformation.

                return typeof reviver === 'function' ?
                    walk({'': j}, '') : j;
            }

// If the text is not JSON parseable, then a SyntaxError is thrown.

            throw new SyntaxError('JSON.parse');
        };
    }
}());
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

var Filter = (function() {

  var view_emit = false;

  return {
      emit : function(key, value) {
        view_emit = true;
      },
      filter : function(fun, ddoc, args) {
        var results = [];
        var docs = args[0];
        var req = args[1];
        for (var i=0; i < docs.length; i++) {
          results.push((fun.apply(ddoc, [docs[i], req]) && true) || false);
        };
        respond([true, results]);
      },
      filter_view : function(fun, ddoc, args) {
        // recompile
        var source = fun.toSource ? fun.toSource() : '(' + fun.toString() + ')';
        fun = evalcx(source, filter_sandbox);

        var results = [];
        var docs = args[0];
        for (var i=0; i < docs.length; i++) {
          view_emit = false;
          fun(docs[i]);
          results.push((view_emit && true) || false);
        };
        respond([true, results]);
      }
    }
})();
// mimeparse.js
//
// This module provides basic functions for handling mime-types. It can
// handle matching mime-types against a list of media-ranges. See section
// 14.1 of the HTTP specification [RFC 2616] for a complete explanation.
//
//   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
//
// A port to JavaScript of Joe Gregorio's MIME-Type Parser:
//
//   http://code.google.com/p/mimeparse/
//
// Ported by J. Chris Anderson <jchris@apache.org>, targeting the Spidermonkey runtime.
//
// To run the tests, open mimeparse-js-test.html in a browser.
// Ported from version 0.1.2
// Comments are mostly excerpted from the original.

var Mimeparse = (function() {
  // private helpers
  function strip(string) {
    return string.replace(/^\s+/, '').replace(/\s+$/, '');
  };

  function parseRanges(ranges) {
    var parsedRanges = [], rangeParts = ranges.split(",");
    for (var i=0; i < rangeParts.length; i++) {
      parsedRanges.push(publicMethods.parseMediaRange(rangeParts[i]));
    };
    return parsedRanges;
  };

  var publicMethods = {
    // Carves up a mime-type and returns an Array of the
    //  [type, subtype, params] where "params" is a Hash of all
    //  the parameters for the media range.
    //
    // For example, the media range "application/xhtml;q=0.5" would
    //  get parsed into:
    //
    // ["application", "xhtml", { "q" : "0.5" }]
    parseMimeType : function(mimeType) {
      var fullType, typeParts, params = {}, parts = mimeType.split(';');
      for (var i=0; i < parts.length; i++) {
        var p = parts[i].split('=');
        if (p.length == 2) {
          params[strip(p[0])] = strip(p[1]);
        }
      };
      fullType = parts[0].replace(/^\s+/, '').replace(/\s+$/, '');
      if (fullType == '*') fullType = '*/*';
      typeParts = fullType.split('/');
      return [typeParts[0], typeParts[1], params];
    },

    // Carves up a media range and returns an Array of the
    //  [type, subtype, params] where "params" is a Object with
    //  all the parameters for the media range.
    //
    // For example, the media range "application/*;q=0.5" would
    //  get parsed into:
    //
    // ["application", "*", { "q" : "0.5" }]
    //
    // In addition this function also guarantees that there
    //  is a value for "q" in the params dictionary, filling it
    //  in with a proper default if necessary.
    parseMediaRange : function(range) {
      var q, parsedType = this.parseMimeType(range);
      if (!parsedType[2]['q']) {
        parsedType[2]['q'] = '1';
      } else {
        q = parseFloat(parsedType[2]['q']);
        if (isNaN(q)) {
          parsedType[2]['q'] = '1';
        } else if (q > 1 || q < 0) {
          parsedType[2]['q'] = '1';
        }
      }
      return parsedType;
    },

    // Find the best match for a given mime-type against
    // a list of media_ranges that have already been
    // parsed by parseMediaRange(). Returns an array of
    // the fitness value and the value of the 'q' quality
    // parameter of the best match, or (-1, 0) if no match
    // was found. Just as for qualityParsed(), 'parsed_ranges'
    // must be a list of parsed media ranges.
    fitnessAndQualityParsed : function(mimeType, parsedRanges) {
      var bestFitness = -1, bestFitQ = 0, target = this.parseMediaRange(mimeType);
      var targetType = target[0], targetSubtype = target[1], targetParams = target[2];

      for (var i=0; i < parsedRanges.length; i++) {
        var parsed = parsedRanges[i];
        var type = parsed[0], subtype = parsed[1], params = parsed[2];
        if ((type == targetType || type == "*" || targetType == "*") &&
          (subtype == targetSubtype || subtype == "*" || targetSubtype == "*")) {
          var matchCount = 0;
          for (var param in targetParams) {
            if (param != 'q' && params[param] && params[param] == targetParams[param]) {
              matchCount += 1;
            }
          }

          var fitness = (type == targetType) ? 100 : 0;
          fitness += (subtype == targetSubtype) ? 10 : 0;
          fitness += matchCount;

          if (fitness > bestFitness) {
            bestFitness = fitness;
            bestFitQ = params["q"];
          }
        }
      };
      return [bestFitness, parseFloat(bestFitQ)];
    },

    // Find the best match for a given mime-type against
    // a list of media_ranges that have already been
    // parsed by parseMediaRange(). Returns the
    // 'q' quality parameter of the best match, 0 if no
    // match was found. This function bahaves the same as quality()
    // except that 'parsedRanges' must be a list of
    // parsed media ranges.
    qualityParsed : function(mimeType, parsedRanges) {
      return this.fitnessAndQualityParsed(mimeType, parsedRanges)[1];
    },

    // Returns the quality 'q' of a mime-type when compared
    // against the media-ranges in ranges. For example:
    //
    // >>> Mimeparse.quality('text/html','text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5')
    // 0.7
    quality : function(mimeType, ranges) {
      return this.qualityParsed(mimeType, parseRanges(ranges));
    },

    // Takes a list of supported mime-types and finds the best
    // match for all the media-ranges listed in header. The value of
    // header must be a string that conforms to the format of the
    // HTTP Accept: header. The value of 'supported' is a list of
    // mime-types.
    //
    // >>> bestMatch(['application/xbel+xml', 'text/xml'], 'text/*;q=0.5,*/*; q=0.1')
    // 'text/xml'
    bestMatch : function(supported, header) {
      var parsedHeader = parseRanges(header);
      var weighted = [];
      for (var i=0; i < supported.length; i++) {
        weighted.push([publicMethods.fitnessAndQualityParsed(supported[i], parsedHeader), i, supported[i]]);
      };
      weighted.sort();
      return weighted[weighted.length-1][0][1] ? weighted[weighted.length-1][2] : '';
    }
  };
  return publicMethods;
})();
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.


var Mime = (function() {
  // registerType(name, mime-type, mime-type, ...)
  // 
  // Available in query server sandbox. TODO: The list is cleared on reset.
  // This registers a particular name with the set of mimetypes it can handle.
  // Whoever registers last wins.
  // 
  // Example: 
  // registerType("html", "text/html; charset=utf-8");

  var mimesByKey = {};
  var keysByMime = {};
  function registerType() {
    var mimes = [], key = arguments[0];
    for (var i=1; i < arguments.length; i++) {
      mimes.push(arguments[i]);
    };
    mimesByKey[key] = mimes;
    for (var i=0; i < mimes.length; i++) {
      keysByMime[mimes[i]] = key;
    };
  }

  // Some default types
  // Ported from Ruby on Rails
  // Build list of Mime types for HTTP responses
  // http://www.iana.org/assignments/media-types/
  // http://dev.rubyonrails.org/svn/rails/trunk/actionpack/lib/action_controller/mime_types.rb

  registerType("all", "*/*");
  registerType("text", "text/plain; charset=utf-8", "txt");
  registerType("html", "text/html; charset=utf-8");
  registerType("xhtml", "application/xhtml+xml", "xhtml");
  registerType("xml", "application/xml", "text/xml", "application/x-xml");
  registerType("js", "text/javascript", "application/javascript", "application/x-javascript");
  registerType("css", "text/css");
  registerType("ics", "text/calendar");
  registerType("csv", "text/csv");
  registerType("rss", "application/rss+xml");
  registerType("atom", "application/atom+xml");
  registerType("yaml", "application/x-yaml", "text/yaml");
  // just like Rails
  registerType("multipart_form", "multipart/form-data");
  registerType("url_encoded_form", "application/x-www-form-urlencoded");
  // http://www.ietf.org/rfc/rfc4627.txt
  registerType("json", "application/json", "text/x-json");
  
  
  var mimeFuns = [];
  function provides(type, fun) {
    Mime.providesUsed = true;
    mimeFuns.push([type, fun]);
  };

  function resetProvides() {
    // set globals
    Mime.providesUsed = false;
    mimeFuns = [];
    Mime.responseContentType = null;  
  };

  function runProvides(req, ddoc) {
    var supportedMimes = [], bestFun, bestKey = null, accept = req.headers["Accept"];
    if (req.query && req.query.format) {
      bestKey = req.query.format;
      Mime.responseContentType = mimesByKey[bestKey][0];
    } else if (accept) {
      // log("using accept header: "+accept);
      mimeFuns.reverse().forEach(function(mimeFun) {
        var mimeKey = mimeFun[0];
        if (mimesByKey[mimeKey]) {
          supportedMimes = supportedMimes.concat(mimesByKey[mimeKey]);
        }
      });
      Mime.responseContentType = Mimeparse.bestMatch(supportedMimes, accept);
      bestKey = keysByMime[Mime.responseContentType];
    } else {
      // just do the first one
      bestKey = mimeFuns[0][0];
      Mime.responseContentType = mimesByKey[bestKey][0];
    }

    if (bestKey) {
      for (var i=0; i < mimeFuns.length; i++) {
        if (mimeFuns[i][0] == bestKey) {
          bestFun = mimeFuns[i][1];
          break;
        }
      };
    };

    if (bestFun) {
      return bestFun.call(ddoc);
    } else {
      var supportedTypes = mimeFuns.map(function(mf) {
        return mimesByKey[mf[0]].join(', ') || mf[0];
      });
      throw(["error","not_acceptable",
        "Content-Type "+(accept||bestKey)+" not supported, try one of: "+supportedTypes.join(', ')]);
    }
  };

  
  return {
    registerType : registerType,
    provides : provides,
    resetProvides : resetProvides,
    runProvides : runProvides
  };
})();




////
////  Render dispatcher
////
////
////
////

var Render = (function() {
  var new_header = false;
  var chunks = [];
  
  
  //  Start chunks
  var startResp = {};
  function start(resp) {
    startResp = resp || {};
    new_header = true;
  };

  function sendStart() {
    startResp = applyContentType((startResp || {}), Mime.responseContentType);
    respond(["start", chunks, startResp]);
    chunks = [];
    startResp = {};
    new_header = false;
  }

  function applyContentType(resp, responseContentType) {
    resp["headers"] = resp["headers"] || {};
    if (responseContentType) {
      resp["headers"]["Content-Type"] = resp["headers"]["Content-Type"] || responseContentType;    
    }
    return resp;
  }

  function send(chunk) {
    chunks.push(chunk.toString());
  };

  function blowChunks(label) {
    if (new_header) {
      respond([label||"chunks", chunks, startResp]);
      new_header = false;
    }
    else {
      respond([label||"chunks", chunks]);
    }
    chunks = [];
  };

  var gotRow = false, lastRow = false;
  function getRow() {
    if (lastRow) return null;
    if (!gotRow) {
      gotRow = true;
      sendStart();
    } else {
      blowChunks();
    }
    var json = JSON.parse(readline());
    if (json[0] == "list_end") {
      lastRow = true;
      return null;
    }
    if (json[0] != "list_row") {
      throw(["fatal", "list_error", "not a row '" + json[0] + "'"]);
    }
    return json[1];
  };

  
  function maybeWrapResponse(resp) {
    var type = typeof resp;
    if ((type == "string") || (type == "xml")) {
      return {body:resp};
    } else {
      return resp;
    }
  };

  // from http://javascript.crockford.com/remedial.html
  function typeOf(value) {
    var s = typeof value;
    if (s === 'object') {
      if (value) {
        if (value instanceof Array) {
          s = 'array';
        }
      } else {
        s = 'null';
      }
    }
    return s;
  };

  function isDocRequestPath(info) {
    var path = info.path;
    return path.length > 5;
  };

  function runShow(fun, ddoc, args) {
    try {
      resetList();
      Mime.resetProvides();
      var resp = fun.apply(ddoc, args) || {};
      resp = maybeWrapResponse(resp);

      // handle list() style API
      if (chunks.length && chunks.length > 0) {
        resp.headers = resp.headers || {};
        for(var header in startResp) {
          resp.headers[header] = startResp[header];
        }
        resp.body = chunks.join("") + (resp.body || "");
        resetList();
      }

      if (Mime.providesUsed) {
        var provided_resp = Mime.runProvides(args[1], ddoc) || {};
        provided_resp = maybeWrapResponse(provided_resp);
        resp.body = (resp.body || "") + chunks.join("");
        resp.body += provided_resp.body || "";
        resp = applyContentType(resp, Mime.responseContentType);
        resetList();
      }

      var type = typeOf(resp);
      if (type == 'object' || type == 'string') {
        respond(["resp", maybeWrapResponse(resp)]);
      } else {
        throw(["error", "render_error", "undefined response from show function"]);      
      }
    } catch(e) {
      if (args[0] === null && isDocRequestPath(args[1])) {
        throw(["error", "not_found", "document not found"]);
      } else {
        renderError(e, fun.toString());
      }
    }
  };

  function runUpdate(fun, ddoc, args) {
    try {
      var method = args[1].method;
      // for analytics logging applications you might want to remove the next line
      if (method == "GET") throw(["error","method_not_allowed","Update functions do not allow GET"]);
      var result = fun.apply(ddoc, args);
      var doc = result[0];
      var resp = result[1];
      var type = typeOf(resp);
      if (type == 'object' || type == 'string') {
        respond(["up", doc, maybeWrapResponse(resp)]);
      } else {
        throw(["error", "render_error", "undefined response from update function"]);      
      }
    } catch(e) {
      renderError(e, fun.toString());
    }
  };

  function resetList() {
    gotRow = false;
    lastRow = false;
    chunks = [];
    startResp = {};
    new_header = false;
  };

  function runList(listFun, ddoc, args) {
    try {
      Mime.resetProvides();
      resetList();
      var head = args[0];
      var req = args[1];
      var tail = listFun.apply(ddoc, args);

      if (Mime.providesUsed) {
        tail = Mime.runProvides(req, ddoc);
      }    
      if (!gotRow) getRow();
      if (typeof tail != "undefined") {
        chunks.push(tail);
      }
      blowChunks("end");
    } catch(e) {
      renderError(e, listFun.toString());
    }
  };

  function renderError(e, funSrc) {
    if (e.error && e.reason || e[0] == "error" || e[0] == "fatal") {
      throw(e);
    } else {
      var logMessage = "function raised error: " +
                       (e.toSource ? e.toSource() : e.toString()) + " \n" +
                       "stacktrace: " + e.stack;
      log(logMessage);
      throw(["error", "render_error", logMessage]);
    }
  };

  function escapeHTML(string) {
    return string && string.replace(/&/g, "&amp;")
                 .replace(/</g, "&lt;")
                 .replace(/>/g, "&gt;");
  };

  
  return {
    start : start,
    send : send,
    getRow : getRow,
    show : function(fun, ddoc, args) {
      // var showFun = Couch.compileFunction(funSrc);
      runShow(fun, ddoc, args);
    },
    update : function(fun, ddoc, args) {
      // var upFun = Couch.compileFunction(funSrc);
      runUpdate(fun, ddoc, args);
    },
    list : function(fun, ddoc, args) {
      runList(fun, ddoc, args);
    }
  };
})();

// send = Render.send;
// getRow = Render.getRow;
// start = Render.start;

// unused. this will be handled in the Erlang side of things.
// function htmlRenderError(e, funSrc) {
//   var msg = ["<html><body><h1>Render Error</h1>",
//     "<p>JavaScript function raised error: ",
//     e.toString(),
//     "</p><h2>Stacktrace:</h2><code><pre>",
//     escapeHTML(e.stack),
//     "</pre></code><h2>Function source:</h2><code><pre>",
//     escapeHTML(funSrc),
//     "</pre></code></body></html>"].join('');
//   return {body:msg};
// };
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

var State = {
  reset : function(config) {
    // clear the globals and run gc
    State.funs = [];
    State.lib = null;
    State.query_config = config || {};
    init_sandbox();
    gc();
    print("true"); // indicates success
  },
  addFun : function(newFun) {
    // Compile to a function and add it to funs array
    State.funs.push(Couch.compileFunction(newFun, {views : {lib : State.lib}}));
    print("true");
  },
  addLib : function(lib) {
    State.lib = lib;
    print("true");
  }
};
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

var resolveModule = function(names, mod, root) {
  if (names.length == 0) {
    if (typeof mod.current != "string") {
      throw ["error","invalid_require_path",
        'Must require a JavaScript string, not: '+(typeof mod.current)];
    }
    return {
      current : mod.current,
      parent : mod.parent,
      id : mod.id,
      exports : {}
    };
  }
  // we need to traverse the path
  var n = names.shift();
  if (n == '..') {
    if (!(mod.parent && mod.parent.parent)) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(mod.current)];
    }
    return resolveModule(names, {
      id : mod.id.slice(0, mod.id.lastIndexOf('/')),
      parent : mod.parent.parent,
      current : mod.parent.current
    });
  } else if (n == '.') {
    if (!mod.parent) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(mod.current)];
    }
    return resolveModule(names, {
      parent : mod.parent,
      current : mod.current,
      id : mod.id
    });
  } else if (root) {
    mod = {current : root};
  }
  if (mod.current[n] === undefined) {
    throw ["error", "invalid_require_path", 'Object has no property "'+n+'". '+JSON.stringify(mod.current)];
  }
  return resolveModule(names, {
    current : mod.current[n],
    parent : mod,
    id : mod.id ? mod.id + '/' + n : n
  });
};

var Couch = {
  // moving this away from global so we can move to json2.js later
  toJSON : function (val) {
    return JSON.stringify(val);
  },
  compileFunction : function(source, ddoc, name) {
    if (!source) throw(["error","not_found","missing function"]);

    var evaluate_function_source = function(source, evalFunction, sandbox) {
      sandbox = sandbox || {};
      if(typeof CoffeeScript === "undefined") {
        return evalFunction(source, sandbox, name);
      } else {
        var coffee = CoffeeScript.compile(source, {bare: true});
        return evalFunction(coffee, sandbox, name);
      }
    }

    try {
      if (sandbox) {
        if (ddoc) {
          if (!ddoc._module_cache) {
            ddoc._module_cache = {};
          }
          var require = function(name, module) {
            module = module || {};
            var newModule = resolveModule(name.split('/'), module.parent, ddoc);
            if (!ddoc._module_cache.hasOwnProperty(newModule.id)) {
              // create empty exports object before executing the module,
              // stops circular requires from filling the stack
              ddoc._module_cache[newModule.id] = {};
              var s = "function (module, exports, require) { " + newModule.current + "\n }";
              try {
                var func = sandbox ? evalcx(s, sandbox, newModule.id) : eval(s);
                func.apply(sandbox, [newModule, newModule.exports, function(name) {
                  return require(name, newModule);
                }]);
              } catch(e) { 
                throw [
                  "error",
                  "compilation_error",
                  "Module require('" +name+ "') raised error " +
                  (e.toSource ? e.toSource() : e.stack)
                ];
              }
              ddoc._module_cache[newModule.id] = newModule.exports;
            }
            return ddoc._module_cache[newModule.id];
          };
          sandbox.require = require;
        }
        var functionObject = evaluate_function_source(source, evalcx, sandbox);
      } else {
        var functionObject = evaluate_function_source(source, eval);
      }
    } catch (err) {
      throw([
        "error",
        "compilation_error",
        (err.toSource ? err.toSource() : err.stack) + " (" + source + ")"
      ]);
    };
    if (typeof(functionObject) == "function") {
      return functionObject;
    } else {
      throw(["error","compilation_error",
        "Expression does not eval to a function. (" + source.toString() + ")"]);
    };
  },
  recursivelySeal : function(obj) {
    // seal() is broken in current Spidermonkey
    try {
      seal(obj);
    } catch (x) {
      // Sealing of arrays broken in some SpiderMonkey versions.
      // https://bugzilla.mozilla.org/show_bug.cgi?id=449657
    }
    for (var propname in obj) {
      if (typeof obj[propname] == "object") {
        arguments.callee(obj[propname]);
      }
    }
  }
};

// prints the object as JSON, and rescues and logs any toJSON() related errors
function respond(obj) {
  try {
    print(Couch.toJSON(obj));
  } catch(e) {
    log("Error converting object to JSON: " + e.toString());
    log("error on obj: "+ (obj.toSource ? obj.toSource() : obj.toString()));
  }
};

function log(message) {
  // idea: query_server_config option for log level
  if (typeof message == "xml") {
    message = message.toXMLString();
  } else if (typeof message != "string") {
    message = Couch.toJSON(message);
  }
  respond(["log", String(message)]);
};

function isArray(obj) {
  return toString.call(obj) === "[object Array]";
}
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

var Validate = {
  validate : function(fun, ddoc, args) {
    try {
      fun.apply(ddoc, args);
      respond(1);
    } catch (error) {
      if (error.name && error.stack) {
        throw error;
      }
      respond(error);
    }
  }
};
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.



var Views = (function() {

  var map_results = []; // holds temporary emitted values during doc map

  function runReduce(reduceFuns, keys, values, rereduce) {
    for (var i in reduceFuns) {
      reduceFuns[i] = Couch.compileFunction(reduceFuns[i]);
    };
    var reductions = new Array(reduceFuns.length);
    for(var i = 0; i < reduceFuns.length; i++) {
      try {
        reductions[i] = reduceFuns[i](keys, values, rereduce);
      } catch (err) {
        handleViewError(err);
        // if the error is not fatal, ignore the results and continue
        reductions[i] = null;
      }
    };
    var reduce_line = Couch.toJSON(reductions);
    var reduce_length = reduce_line.length;
    // TODO make reduce_limit config into a number
    if (State.query_config && State.query_config.reduce_limit &&
          reduce_length > 200 && ((reduce_length * 2) > State.line_length)) {
      var reduce_preview = "Current output: '"+(reduce_line.substring(0,100) + "'... (first 100 of "+reduce_length+" bytes)");
      throw(["error", 
        "reduce_overflow_error", 
        "Reduce output must shrink more rapidly: "+reduce_preview]);
    } else {
      print("[true," + reduce_line + "]");
    }
  };

  function handleViewError(err, doc) {
    if (err == "fatal_error") {
      // Only if it's a "fatal_error" do we exit. What's a fatal error?
      // That's for the query to decide.
      //
      // This will make it possible for queries to completely error out,
      // by catching their own local exception and rethrowing a
      // fatal_error. But by default if they don't do error handling we
      // just eat the exception and carry on.
      //
      // In this case we abort map processing but don't destroy the 
      // JavaScript process. If you need to destroy the JavaScript 
      // process, throw the error form matched by the block below.
      throw(["error", "map_runtime_error", "function raised 'fatal_error'"]);
    } else if (err[0] == "fatal") {
      // Throwing errors of the form ["fatal","error_key","reason"]
      // will kill the OS process. This is not normally what you want.
      throw(err);
    }
    var message = "function raised exception " +
                  (err.toSource ? err.toSource() : err.stack);
    if (doc) message += " with doc._id " + doc._id;
    log(message);
  };

  return {
    // view helper functions
    emit : function(key, value) {
      map_results.push([key, value]);
    },
    sum : function(values) {
      var rv = 0;
      for (var i in values) {
        rv += values[i];
      }
      return rv;
    },
    reduce : function(reduceFuns, kvs) {
      var keys = new Array(kvs.length);
      var values = new Array(kvs.length);
      for(var i = 0; i < kvs.length; i++) {
          keys[i] = kvs[i][0];
          values[i] = kvs[i][1];
      }
      runReduce(reduceFuns, keys, values, false);
    },
    rereduce : function(reduceFuns, values) {
      runReduce(reduceFuns, null, values, true);
    },
    mapDoc : function(doc) {
      // Compute all the map functions against the document.
      //
      // Each function can output multiple key/value pairs for each document.
      //
      // Example output of map_doc after three functions set by add_fun cmds:
      // [
      //  [["Key","Value"]],                    <- fun 1 returned 1 key value
      //  [],                                   <- fun 2 returned 0 key values
      //  [["Key1","Value1"],["Key2","Value2"]] <- fun 3 returned 2 key values
      // ]
      //

      Couch.recursivelySeal(doc);

      var buf = [];
      for (var i = 0; i < State.funs.length; i++) {
        map_results = [];
        try {
          State.funs[i](doc);
          buf.push(Couch.toJSON(map_results));
        } catch (err) {
          handleViewError(err, doc);
          // If the error is not fatal, we treat the doc as if it
          // did not emit anything, by buffering an empty array.
          buf.push("[]");
        }
      }
      print("[" + buf.join(", ") + "]");
    }
  };
})();
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.
//
// Copyright 2015 (c) Benoît Chesneau

Script = {

	run: function(source, ddoc, args) {
		var fun = Couch.compileFunction(source, ddoc, "rewrite");
		Res = fun.apply(ddoc, args),
		respond(Res);
	}
};// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

var sandbox = null;
var filter_sandbox = null;

function init_sandbox() {
  try {
    // if possible, use evalcx (not always available)
    sandbox = evalcx('');
    sandbox.emit = Views.emit;
    sandbox.sum = Views.sum;
    sandbox.log = log;
    sandbox.toJSON = Couch.toJSON;
    sandbox.JSON = JSON;
    sandbox.provides = Mime.provides;
    sandbox.registerType = Mime.registerType;
    sandbox.start = Render.start;
    sandbox.send = Render.send;
    sandbox.getRow = Render.getRow;
    sandbox.isArray = isArray;
  } catch (e) {
    //log(e.toSource());
  }
};
init_sandbox();

function init_filter_sandbox() {
  try {
    filter_sandbox = evalcx('');
    for (var p in sandbox) {
      if (sandbox.hasOwnProperty(p)) {
        filter_sandbox[p] = sandbox[p];
      }
    }
    filter_sandbox.emit = Filter.emit;
  } catch(e) {
    log(e.toSource ? e.toSource() : e.stack);
  }
};

init_filter_sandbox();

// Commands are in the form of json arrays:
// ["commandname",..optional args...]\n
//
// Responses are json values followed by a new line ("\n")

var DDoc = (function() {
  var ddoc_dispatch = {
    "lists"     : Render.list,
    "shows"    : Render.show,
    "filters"   : Filter.filter,
    "views"     : Filter.filter_view,
    "updates"  : Render.update,
    "validate_doc_update" : Validate.validate,
    "validate_doc_read" : Validate.validate,
    "run": Script.run
  };
  var ddocs = {};
  return {
    ddoc : function() {
      var args = [];
      for (var i=0; i < arguments.length; i++) {
        args.push(arguments[i]);
      };
      var ddocId = args.shift();
      if (ddocId == "new") {
        // get the real ddocId.
        ddocId = args.shift();
        // store the ddoc, functions are lazily compiled.
        ddocs[ddocId] = args.shift();
        print("true");
      } else {
        // Couch makes sure we know this ddoc already.
        var ddoc = ddocs[ddocId];
        if (!ddoc) throw(["fatal", "query_protocol_error", "uncached design doc: "+ddocId]);
        var funPath = args.shift();
        var cmd = funPath[0];
        // the first member of the fun path determines the type of operation
        var funArgs = args.shift();
        if (ddoc_dispatch[cmd]) {

          if (cmd == "run") {
            var src = funPath[1];
            return Script.run(src, ddoc, funArgs);
          } 

          // get the function, call the command with it
          var point = ddoc;
          for (var i=0; i < funPath.length; i++) {
            if (i+1 == funPath.length) {
              var fun = point[funPath[i]];
              if (!fun) {
                throw(["error","not_found",
                       "missing " + funPath[0] + " function " + funPath[i] +
                       " on design doc " + ddocId]);
              }
              if (typeof fun != "function") {
                fun = Couch.compileFunction(fun, ddoc, funPath.join('.'));
                // cache the compiled fun on the ddoc
                point[funPath[i]] = fun;
              };
            } else {
              point = point[funPath[i]];
            }
          };

          // run the correct responder with the cmd body
          ddoc_dispatch[cmd].apply(null, [fun, ddoc, funArgs]);
        } else {
          // unknown command, quit and hope the restarted version is better
          throw(["fatal", "unknown_command", "unknown ddoc command '" + cmd + "'"]);
        }
      }
    }
  };
})();

var Loop = function() {
  var line, cmd, cmdkey, dispatch = {
    "ddoc"     : DDoc.ddoc,
    // "view"    : Views.handler,
    "reset"    : State.reset,
    "add_fun"  : State.addFun,
    "add_lib"  : State.addLib,
    "map_doc"  : Views.mapDoc,
    "reduce"   : Views.reduce,
    "rereduce" : Views.rereduce
  };
  function handleError(e) {
    var type = e[0];
    if (type == "fatal") {
      e[0] = "error"; // we tell the client it was a fatal error by dying
      respond(e);
      quit(-1);
    } else if (type == "error") {
      respond(e);
    } else if (e.error && e.reason) {
      // compatibility with old error format
      respond(["error", e.error, e.reason]);
    } else if (e.name) {
      respond(["error", e.name, e]);
    } else {
      respond(["error","unnamed_error",e.toSource ? e.toSource() : e.stack]);
    }
  };
  while (line = readline()) {
    cmd = JSON.parse(line);
    State.line_length = line.length;
    try {
      cmdkey = cmd.shift();
      if (dispatch[cmdkey]) {
        // run the correct responder with the cmd body
        dispatch[cmdkey].apply(null, cmd);
      } else {
        // unknown command, quit and hope the restarted version is better
        throw(["fatal", "unknown_command", "unknown command '" + cmdkey + "'"]);
      }
    } catch(e) {
      handleError(e);
    }
  };
};

Loop();
