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

    jQuery.getI18N = function(key) {
        return format(i18n[key],[].splice.call(arguments, 1)) ;
    };

    jQuery.i18nLabel = function() {
        jQuery("span[class^=i18n-]").each(function() {
            var node = $(this);
            var value = i18n[node.attr("class").substr(5)];
            if (value != undefined) {
                this.originalText = node.text();
                node.text(value);
            } else if (this.originalText != undefined) {
                node.text(this.originalText);
            }
        });
    };
})();
