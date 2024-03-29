var klatschclient = (function() {
    var refreshMessageInterval = 2;
    var refreshListInterval = 180000;
    var refreshMessageId, refreshClientListId, refreshRoomListId,
        storePropertiesId;
    var allRooms = null;
    var subscribedRooms = {};
    var auth;
    var kc = {};
    var kb = klatschbase;
    var startkeys = {};
    var recipient;
    var defaultOptions = {
        echoMessage: true,
        loadOldMessages: true,
    };
    var options = {};
    $.extend(options, defaultOptions);
    var focusMsgInput;

    kc.setFocusMsgInput = function(f) { focusMsgInput = f; };

    var setAuth = kc.setAuth = function(loginId, password) {
        auth = [loginId, password];
    }

    var getAuth = kc.getAuth = function() {
        return auth;
    }

    var intToString = function(i, n) {
	var s = "" + i;
	var m = n - s.length;
	var t = "";
	for (var k=0; k<m; k++) {
	    t += "0";
	}
	return t + s;
    };

    var createSlider = kc.createSlider = function(selector) {
        var startHeight;
        var followSlider = function(e) {
	    $("p.chat").height(startHeight + e.pageY);
	    return false;
        }
        $(selector).mousedown(function(e) {
	    startHeight = $("p.chat").height() - e.pageY;
	    $(document).bind('mousemove', followSlider);
        });
        $(document).mouseup(function() {
	    $(document).unbind('mousemove', followSlider);
        });
    }

    var getSubscribedRooms = function() {
	var rooms = [];
	$.each(subscribedRooms, function(i,room) { rooms.push(room); });
	return rooms;
    };

    var getSubscribedRoomIds = function() {
	var rooms = [];
	$.each(subscribedRooms, function(i,room) { rooms.push(room.id); });
	return rooms;
    };

    var isSubscribed = function(name) {
        return subscribedRooms[name] != undefined
    };

    var subscribe = function(name) {
        kb.roomInfo(auth, name, function(data) {
	    if (data != null && !data.error) {
                if (options.loadOldMessages
                    && startkeys.rooms && startkeys.rooms[name]) {
                    data.startkey = startkeys.rooms[name];
                }
		subscribedRooms[name] = data;
		displayRoomList();
	    }
	});
    };

    var unsubscribe = function(name) {
        if (subscribedRooms[name]) {
            startkeys.rooms[name] = subscribedRooms[name].startkey;
        }
        subscribedRooms[name] = null;
	displayRoomList();
    };

    var toggleSubscription = function(name) {
        if (isSubscribed(name)) unsubscribe(name);
        else subscribe(name);
    };

    var postMessage = function(category, name, msgline) {
	if (msgline == "") return;
        var msg = {msgtext: msgline};
        if (category == "client") {
            msg.client = name;
        } else if (category == "room") {
            msg.room = name;
        } else {
            throw "Unknown sender category";
        }
        kb.postMessage(auth, msg, function (data) {
            addOwnMessage(msgline, data);
            refresh();
        });
    };

    var sendMessage = function(msgline) {
	if (recipient) {
            postMessage(recipient[0], recipient[1], msgline);
	}
    };

    var commandJoin = function(msgline, i, isSubscribe) {
        if (i == msgline.length) {
            alert($.i18n("error-no-room-name"));
            return;
        }
        if (msgline.charAt(i+1) == "#") i++;
        try {
            var regex = new RegExp("^" + msgline.substring(i+1) + "$");
        } catch (err) {
            alert($.i18n("error-room-regexp"));
            return;
        }
        var op = isSubscribe ? subscribe : unsubscribe;
        $.each(allRooms, function(j, room) {
            var name = room.id;
            if (name.match(regex)) {
                if (isSubscribed(name) !== isSubscribe) op(name);
            }
        });
    };

    var commandMsg = function(msgline, i) {
        if (i == msgline.length) {
            alert($.i18n("error-no-destination"));
            return;
        }
        var j = msgline.indexOf(" ", i+1);
        if (j == -1) return;
        if (msgline.charAt(i+1) == "#") {
            postMessage("room",
                             msgline.substring(i+2, j),
                             msgline.substring(j+1));
        } else {
            postMessage("client",
                             msgline.substring(i+1, j),
                             msgline.substring(j+1));
        }
    };

    var commandCd = function(msgline, i) {
        if (i == msgline.length) {
            alert($.i18n("error-no-destination"));
            return;
        }
        var category;
        if (msgline.charAt(i+1) == "#") {
            category = "room";
            i++;
        } else {
            category = "client";
        }
        changeRecipient(category, msgline.substring(i+1));
    };

    var commandStoreProperty = function(msgline, i) {
        if (i == msgline.length) {
            alert($.i18n("error-no-key"));
            return;
        }
        var value;
        var j = msgline.indexOf(" ", i+1);
        var key = msgline.substring(i+1, j);
        if (j > i) {
            try {
                value = JSON.parse(msgline.substring(j+1));
            }
            catch (err) {
                alert($.i18n("error-parsing-json-value"));
                return;
            }
        }
        kb.putClientProperty(auth, auth[0], key, value, function(data) {
            addOwnMessage($.i18n("msg-set-value-for-key") + key, data);
        })
    };

    var parseCommand = kc.parseCommand = function(msgline) {
        if (msgline.charAt(0) == "/") {
            var i = msgline.indexOf(" ");
            if (i == -1) i = msgline.length;
            var command = msgline.substring(1, i);
            switch (command) {
            case "join":  commandJoin(msgline, i, true); return;
            case "part":  commandJoin(msgline, i, false); return;
            case "msg":   commandMsg(msgline, i);  return;
            case "cd":    commandCd(msgline, i); return;
            case "store": commandStoreProperty(msgline, i); return;
            }
        }
        sendMessage(msgline);
    };

    var scrollToBottom = function() {
        $("p.chat").each(function(id, p) {
                p.scrollTop = p.scrollHeight;
            });
    };

    var addOwnMessage = function(msg, data) {
	if (!options.echoMessage) {
	    return;
	}
	var node = $(document.createElement("span"))
	.addClass(data === true ? "successEntry" : "warnEntry")
	.append($(document.createElement("span")).addClass("info")
		.text(data === true ? "Sent " :  "Failed to send "
		      + (data.error ? ("[" + data.description + "] ") : "")))
	.append($(document.createElement("span")).addClass("message")
		.text(msg));
	$("p.chat").append(node);
        scrollToBottom();
    };

    var addMessage = function(node, msg) {
	var time = msg.timestamp;
	node.append($(document.createElement("span")).addClass("sender")
		    .append(recipientLink("client", msg.sender)))
	.prepend($(document.createElement("span")).addClass("timestamp")
		 .text(intToString(time[2], 2) + ":"
		       + intToString(time[1], 2) + ":"
		       + intToString(time[0], 2)))
	.append($(document.createElement("span")).addClass("message")
		.text(msg.text));
	$("p.chat").append(node);
    };

    var addPersonalMessage = function(msg) {
	var span = $(document.createElement("span"))
        .addClass("personalEntry");
	addMessage(span, msg);
    };

    var addRoomMessage = function(roomId, msg) {
	var span = $(document.createElement("span")).addClass("roomEntry")
	.append(recipientLink("room", roomId));
	addMessage(span, msg);
    };

    var refresh;
    var startMessagePolling = function(loginId, password, startkey) {
        startkeys.client = startkey;
        var scheduleNextMessagePoll = function() {
            refreshMessageId =
                setTimeout(refresh,
                           500 * Math.pow(refreshMessageInterval, 2));
        };
        var msgListFun = function(msgsList) {
            clearTimeout(refreshMessageId);
            var count = 0;
            if (msgsList != null) {
                if (msgsList.error) return;
                $.each(msgsList, function(key, msgs) {
                    if (msgs != null && msgs.messages != null
                        && msgs.messages.length > 0) {
                        var mlist = msgs.messages;
                        var nextStartkey = mlist[mlist.length - 1].id + 1;
                        if (msgs.key.category == "room") {
                            var roomId = msgs.key.name;
                            var room = subscribedRooms[roomId];
                            if (room) {
                                startkeys.rooms[roomId] = room.startkey
                                    = nextStartkey;
                                $.each(mlist, function(id, msg) {
                                    count++;
                                    addRoomMessage(roomId, msg);
                                });
                            }
                        } else if (msgs.key.category == "client") {
                            startkeys.client = nextStartkey;
                            $.each(mlist, function(id, msg) {
                                count++;
                                addPersonalMessage(msg);
                            });
                        }
                        scrollToBottom();
                    }
                });
            }
            if (count == 0) {
                if (refreshMessageInterval < 20) refreshMessageInterval++;
            } else if (count >= 2) {
                if (refreshMessageInterval > 0)  refreshMessageInterval--;
            }
            scheduleNextMessagePoll();
        }
        refresh = kc.refresh = function() {
            kb.getMessagesList([loginId, password],
                               [{category: "client",
                                 name: loginId,
                                 startkey: startkeys.client}]
                               .concat(getSubscribedRooms()),
                               msgListFun, scheduleNextMessagePoll);
        };
        refresh();
    };

    var storeProperty = function(key, value) {
        kb.putClientProperty(auth, auth[0], key, value);
    };

    var storePreferences = function() {
        storeProperty("$startkeys", startkeys);
        storeProperty("$options", options);
        storeProperty("$subscriptions", getSubscribedRoomIds());
    };
    
    var setupListeners =
    kc.setupListeners = function(loginId, password, startkey) {
        refreshClientListId =
            setInterval(displayClientList, refreshListInterval);
	refreshRoomListId =
	    setInterval(displayRoomList, refreshListInterval);
	displayClientList();
	displayRoomList();
        startkeys.client = startkey;
        kb.getClientProperty(auth, auth[0], "$startkeys", function(data) {
            if (data && !data.error && data.client) startkeys = data;
            if (!startkeys.rooms) startkeys.rooms = {};
            startMessagePolling(loginId, password, startkey);
        });
        kb.getClientProperty(auth, auth[0], "$options", function(data) {
            if (data && !data.error) {
                $.extend(options, data);
            };
            $.each(defaultOptions, function(i, n) {
                $(document.getElementById(i))
                .click(function() { options[i] = this.checked; })
                .each(function() { this.checked = options[i]; });
            });
            document.getElementById("echoMessage");
        });
        kb.getClientProperty(auth, auth[0], "$subscriptions", function(data) {
            if (data && !data.error) {
                $.each(data, function(i, name) { subscribe(name); });
            }
        });
        storePropertiesId = setInterval(storePreferences, 10*60*1000);
    };

    var subscribeLink = function(roomId) {
	return $(document.createElement("a")).attr("href","#")
	.click(function() {
	    toggleSubscription(roomId);
            focusMsgInput();
	    return false;
	})
	.text(isSubscribed(roomId) ? "\u2611": "\u2610");
    };

    var changeRecipient = function(category, id) {
	var rcptSpan =
	    $(document.createElement("span")).addClass("recipient")
	.append($(document.createElement("span")).addClass(category)
		.text(id));
	$("span.recipient").replaceWith(rcptSpan);
	recipient = [category, id];
        focusMsgInput();
    };

    var recipientLink = function(category, id) {
	return $(document.createElement("a")).attr("href","#")
        .click(function() {
            changeRecipient(category, id);
	    return false;
	})
	.append($(document.createElement("span")).addClass(category)
		.text(id));
    };

    var displayRoomList = kc.displayRoomList = function() {
        kb.roomList(function(rooms) {
            if (rooms && !rooms.error) {
                allRooms = rooms;
                var rd =
                    $(document.createElement("ul")).addClass("roomlist");
                for (var i=0; i<rooms.length; i++) {
                    var roomId = rooms[i].id;
                    rd.append($(document.createElement("li"))
                              .append(subscribeLink(roomId))
                              .append(" ")
                              .append(recipientLink("room", roomId)));
                }
                $("ul.roomlist").replaceWith(rd);
            }
        });
    };

    var displayClientList = kc.displayClientList = function() {
        kb.clientList(function(clients) {
            if (clients && !clients.error) {
                var cd =
                    $(document.createElement("ul")).addClass("clientlist");
                clients.sort(function(c1, c2) {
                    return c2.pollActivity - c1.pollActivity;
                });
                for (var i=0; i<clients.length; i++) {
                    var act = Math.ceil(clients[i].pollActivity / 15);
                    cd.append($(document.createElement("li"))
                              .append($(document.createElement("div")).text(" ")
                                      .addClass("activity")
                                      .css({borderBottomWidth: act+"px",
                                            borderTopWidth: (18-act)+"px"}))
                              .append(recipientLink("client", clients[i].id)));
                }
                $("ul.clientlist").replaceWith(cd);
            }
        });
    };

    var logout = kc.logout = function() {
        clearTimeout(refreshMessageId);
        clearTimeout(refreshClientListId);
        clearTimeout(refreshRoomListId);
        clearTimeout(storePropertiesId);
        storePreferences();
        auth = null;
    };

    return kc;
})();

$(document).ready(function() {
    var kc = klatschclient;
    var kb = klatschbase;
    var msgInputLine = true;
    var doLogin = function() {
        var loginId = document.getElementById('login').value;
        var password = document.getElementById('password').value;
        var register = document.getElementById('registerFlag')
        .checked === true;
        var onLogin = function(data) {
            if (data) {
                if (data.error) {
                    alert("Error:" + data.description);
                } else {
                    loginId = data.id;
                    setTimeout(function() {
                        kc.setupListeners(loginId, password, data.startkey);
                    },
                               500);
                    $(".login").hide();
                    $(".inchat").show();
                    kc.setAuth(loginId, password);
                    $("#msgline").focus();
                }
            } else {
                alert("epic fail");
            }
        }
        if (register) {
            kb.register(loginId,
                        {login: loginId, password: password},
                        onLogin);
        } else {
            kb.login([loginId, password], onLogin);
        }
        return false;
    }
    $("#login").keypress(function(e) {
        if (e.which == 13) {
            $("#password").focus();
        }
    }).focus();
    $("#registerFlag").click(function() {
        $("#password").focus();
    });
    $("#password").keypress(function(e) {
        if (e.which == 13) return doLogin();
    });
    $("#loginSubmit").click(doLogin);
    $("#msgline").keypress(function(e) {
        if (e.which == 13) {
            var msgline = this.value;
            this.value = "";
            kc.parseCommand(msgline);
            return false;
        }
    });
    $("#msgSubmit").click(function() {
        $("#msgarea").focus().each(function() {
            var msgline = this.value;
            this.value = "";
            kc.parseCommand(msgline);
        });
    });
    $("#createRoom").click(function() {
        var roomName = prompt($.i18n("dialog-room-name"));
        if (roomName == undefined) return;
        kb.makeRoom(kc.getAuth(), roomName, function(data) {
            if (data != null) {
                if (data.error == null) {
                    kc.displayRoomList();
                } else {
                    alert("Error creating room " + roomName + ": "
                          + data.description);
                }
            } else {
                alert("Unknown error creating room " + roomName);
            }
        });
    });
    $("#logout").click(function() {
        kc.logout();
        $(".login").show();
        $(".inchat").hide();
    });
    kc.createSlider("div.main div.slider");
    $("div.box").prepend($(document.createElement("a"))
             .addClass("button hideOp").attr("href", "#")
             .click(function() {
                 $("div.content", this.parentNode).slideToggle("fast");
                 $(this).text($(this).text() == "\u2b06"
                              ? "\u2b07" : "\u2b06");
             })
             .text("\u2b06"));
    $.loadI18NFile("klatschbase.i18n{0}.json", function() {
        $("#languageList").val($.i18nLangMatch($.i18nLang(), ["de", "en"]));
    });
    $("#languageList").change(function() {
        $.loadI18NFile("klatschbase.i18n{0}.json", $.i18nLabel, this.value);
    });
    $("#showMsgarea").click(function() {
        msgInputLine = false;
        $(this).hide();
        $("#msgline").hide();
        $("#msgarea").parent().show();
        $("#msgarea").focus();
        $("#hideMsgarea").show();
    });
    $("#hideMsgarea").click(function() {
        msgInputLine = true;
        $(this).hide();
        $("#msgline").show().focus();
        $("#msgarea").parent().hide();
        $("#showMsgarea").show();
    });
    kc.setFocusMsgInput(function() {
        if (msgInputLine) $("#msgline").focus();
        else $("#msgarea").focus();
    });
    var preloadFinished = function() {
        $(".preload").hide();
        $(".login").show();
    };
    setTimeout(preloadFinished, 1000);
    $("a#forceLogin").click(preloadFinished);
});