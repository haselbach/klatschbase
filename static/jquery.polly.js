(function() {
    var i18n = {};
    var format = function() {
        var str = arguments[0];
        var strParts = str.match(/.*?({\d+}|$)/g);
        var lineMatch = new RegExp("(.*){(\\d+)}$");
        var result = "";
        for (var i=0; i<strParts.length; i++) {
            var s = lineMatch.exec(strParts[i]);
            if (s) {
                result += s[1];
                var j = parseInt(s[2]) + 1;
                result += j < arguments.length ? arguments[j] : s[2];
            } else {
                result += strParts[i];
            }
            
        }
        return result;
    }

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
    
    jQuery.loadI18NFile = function(baseUrl, complete, lang) {
        if (lang == undefined) {
            lang = navigator.language
                ? navigator.language : navigator.userLanguage;
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
        return i18n[key];
    };

    jQuery.i18nLabel = function() {
        jQuery("span[class^=i18n-]").each(function() {
            var node = $(this);
            var value = i18n[node.attr("class").substr(5)];
            //alert(format("{0} -> {1}", node.attr("class").substr(4), value));
            if (value != undefined) {
                this.originalText = node.text();
                node.text(value);
            } else if (this.originalText != undefined) {
                node.text(this.originalText);
            }
        });
    };
})();
