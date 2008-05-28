var rfc2047 = {};
(function() {
/*
 * rfc2047.js 0.1
 * 
 * Copyright (c) 2008 Christian Haselbach (mr-co.de)
 * Licensed under the MIT license.
 * http://www.opensource.org/licenses/mit-license.php
 *
 */

    var leftPad = function(str, len, padChar) {
	if (padChar == null) padChar = ' ';
	var n = len - str.length;
	var left = "";
	for (var i = 0; i < n; i++) {
	    left += padChar;
	}
	return left + str;
    };

    var crlfsp = "\r\n ";

    var charMaxLen = function(charset) {
	switch (charset) {
	case "UTF-8":       return 4;
	case "UTF-16":      return 4;
	case "ISO-8559-1":  return 1;
	case "ISO-8559-2":  return 1;
	case "ISO-8559-3":  return 1;
	case "ISO-8559-4":  return 1;
	case "ISO-8559-5":  return 1;
	case "ISO-8559-6":  return 1;
	case "ISO-8559-7":  return 1;
	case "ISO-8559-8":  return 1;
	case "ISO-8559-9":  return 1;
	case "ISO-8559-10": return 1;
	case "ISO-8559-11": return 1;
	case "ISO-8559-12": return 1;
	case "ISO-8559-13": return 1;
	case "ISO-8559-14": return 1;
	case "ISO-8559-15": return 1;
	case "ISO-8559-16": return 1;
	case "US-ASCII":    return 1;
	case "KOI8-R":      return 1;
	default: throw 'charset ' + charset + ' unknown';
	}
    };

    var isDirectB = function(c) {
	return (48 <= c && c <= 57) || (65 <= c && c <= 90)
	|| (97 <= c && c <= 122);
    };

    var isSevenBitClean = function(str) {
	var len = str.length;
	for (var i = 0; i < len; i++) {
	    if (str.charCodeAt(i) > 127) {
		return false;
	    }
	    return true;
	}
    };

    rfc2047.encode = function(str, charset, encoding) {
	charset = (charset == null) ? "UTF-8" : charset.toUpperCase();
	encoding = (encoding == null) ? "B" : encoding.toUpperCase();
	var slen = str.length;
	var wlen = Math.floor((75 - 8 - charset.length)
			      / charMaxLen(charset));
	var output = "";
	var bEnc = function() {
	    var i = 0;
	    var n = 0;
	    while (i < slen) {
		if (n != 0) {
		    output += crlfsp;
		}
		var j = Math.min(slen, i + 4 * Math.ceil(wlen / 5));
		var substr = str.substring(i, j);
		// TODO: Add charset translation here.
		var b64str = Base64.encode(substr);
		output += "=?" + charset + "?" + encoding + "?" + b64str + "?=";
		i = j;
		n++;
	    }
	};
	var qEncPart = function(i) {
	    // TODO: Should operate on bytes according to the charset
	    var len = 0;
	    while ((i < slen) && (len < wlen)) {
		var c = str.charCodeAt(i);
		if (isDirectB(c)) {
		    output += str[i];
		    len++;
		} else {
		    if (len + 3 >= wlen) {
			break;
		    }
		    output += "=" + leftPad(c.toString(16), 2, "0");
		    len += 3;
		}
		i++;
	    }
	    return i;
	};
	var qEnc = function() {
	    var i = 0;
	    var n = 0;
	    while (i < slen) {
		if (n != 0) {
		    output += crlfsp;
		}
		output += "=?" + charset + "?" + encoding + "?";
		i = qEncPart(i, 0);
		output += "?=";
	    }
	};
	switch (encoding) {
	case "B": bEnc(); break;
	case "Q": qEnc(); break;
	default: throw "Unknown encoding " + encoding;
	}
	return output;
    };

    rfc2047.encodeIfNeeded = function(str, charset, encoding) {
	if (isSevenBitClean(str)) {
	    return str;
	}
	return rfc2047.encode(str, charset, encoding);
    };
})();
