/**
 *
   Polly v0.1
   A jQuery plugin for I18N.
   Provides the following functions:
   - loadI18NFile(url, complete, lang) loads I18N messages from url,
     where url is a URL template. This template is a string that can contain
     {0} which is replaces by the language key. E.g., if the language key
     is en_CA and the url is http://example/foo{0}.json then the function
     will try to load from the following URLs:
      + http://example/foo.json
      + http://example/foo_en.json
      + http://example/foo_en_CA.json
     The function complete is optional. It will be called after everything
     is loaded.
     The language can be specified via the parameter lang. If it is not
     defined, the browser default language is used.
   - getI18N(key, ...) translates the key and fills {0}, {1}, ... with
     the according parameter using the currently loaded dictionary.
     Example: if the key "foo" has the value "some {0} other", then the
     call getI18N("foo", "or") will have the result "some or other".
   - i18nLabel() translates every span that has a css class starting
     with i18n- or i18nx-. Note that this must be the only css class or
     strange things might happen. The rest of the css class is used as the
     key. For i18nx you can define a template as i18n value where the
     place holders ({0}, ...) are filled with the child nodes that have the
     css class i18narg.
                                                                             *
                                                                            **/

(function() {
    var i18n = {};

    var lineMatch = new RegExp("(.*){(\\d+)}$");

    var formatDo = function(str, literal, replace) {
        var strParts = str.match(/.*?({\d+}|$)/g);
        for (var i=0; i<strParts.length; i++) {
            var curPart = strParts[i];
            var s = lineMatch.exec(curPart);
            if (s) {
                literal(s[1]);
                replace(s[2]);
            } else {
                if (curPart) literal(curPart);
            }
        }
    };

    var format = function(str) {
        var result = "";
        var args = arguments[1] instanceof Array ?
            arguments[1] : [].splice.call(arguments, 1);
        var len = args.length;
        formatDo(
            str,
            function(s) { result += s; },
            function(s) {
                var i = parseInt(s);
                result += i < len ? args[i] : "{" + s + "}";
            });
        return result;
    };

    var extendFun = function(data) {
        if (data != null) {
            jQuery.extend(i18n, data);
        }
    };

    var makeLoadFun = function(url, complete) {
        return function(data) {
            jQuery.ajax({url: url, dataType: "json",
                         success: extendFun, complete: complete});
        };
    };

    var getLanguage = jQuery.i18nLang = function() {
        return navigator.language ? navigator.language : navigator.userLanguage;
    }

    var languageMatch = jQuery.i18nLangMatch = function(lang, langs) {
        var bestMatch;
        var bestScore = 0;
        var len = lang.length;
        var langParts = lang.split(/[-_]/);
        var partsLen = langParts.length;
        for (var i=0; i<langs.length; i++) {
            var curLang = langs[i];
            if (curLang == lang) {
                return lang;
            }
            var curLangParts = curLang.split(/[-_]/);
            var j = 0;
            var k = Math.min(langParts.length, partsLen);
            while (j < k && curLangParts[j] == langParts[j]) {
                j++;
            }
            if (j == partsLen) {
                return curLang;
            }
            if (j > bestScore) {
                bestScore = j;
                bestMatch = curLang;
            }
        }
        return bestMatch;
    };
    
    jQuery.loadI18NFile = function(baseUrl, complete, lang) {
        if (lang == undefined) {
            lang = getLanguage();
        }
        var langParts = lang.split(/[-_]/);
        i18n = {};
        var suffixes = [""];
        var suffix = "";
        for (var i=0; i<langParts.length; i++) {
            suffix += "_" + langParts[i];
            suffixes.push(suffix);
        }
        var loadFun = complete ? complete : function() {};
        for (var i=suffixes.length-1; i>=0; i--) {
            loadFun = makeLoadFun(format(baseUrl, suffixes[i]), loadFun);
        }
        loadFun();
    };

    jQuery.getI18N = jQuery.i18n = function(key) {
        return format(i18n[key],[].splice.call(arguments, 1)) ;
    };

    jQuery.i18nLabel = function() {
        jQuery("span[class^=i18nx-]").each(function() {
            var node = $(this);
            var value = i18n[node.attr("class").substr(6)];
            var args;
            if (value != undefined) {
                if (this.originalChildNodes == undefined) {
                    this.originalNodes = this.childNodes;
                }
                if (this.i18nargs == undefined) {
                    args = this.i18nargs = [];
                    $("span.i18narg", this).each(function () {
                        args.push(this);
                    });
                } else {
                    args = this.i18nargs;
                }
                node.empty();
                formatDo(
                    value,
                    function(s) {
                        node.append(document.createTextNode(s));
                    },
                    function(s) {
                        var i = parseInt(s);
                        node.append($(args[i]).clone());
                    });
            }
        });
        jQuery("span[class^=i18n-]").each(function() {
            var node = $(this);
            var value = i18n[node.attr("class").substr(5)];
            if (value != undefined) {
                if (this.originalText == undefined) {
                    this.originalText = node.text();
                }
                node.text(value);
            } else if (this.originalText != undefined) {
                node.text(this.originalText);
            }
        });
    };

})();
