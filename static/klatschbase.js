var utilities = {
    intToString: function(i, n) {
	var s = "" + i;
	var m = n - s.length;
	var t = "";
	for (var k=0; k<m; k++) {
	    t += "0";
	}
	return t + s;
    }
};

var klatschclient = {
    refreshMessageInterval: 2,
    refreshListInterval: 180000,
    subscribedRooms: {},
    getSubscribedRooms: function() {
	var rooms = [];
	$.each(this.subscribedRooms, function(i,room) { rooms.push(room); });
	return rooms;
    },
    toggleSubscription: function(name) {
	var self = this;
	if (self.subscribedRooms[name] != undefined) {
	    self.subscribedRooms[name] = undefined;
	    self.displayRoomList();
	} else {
	    klatschbase.roomInfo(self.auth, name, function(data) {
		    if (data != null && !data.error) {
			self.subscribedRooms[name] = data;
			self.displayRoomList();
		    }
		});
	}
    },
    isSubscribed: function(name) {
	return this.subscribedRooms[name] != undefined;
    },
    postMessage: function(category, name, msgline) {
        var self = this;
	if (msgline == "") return;
        var msg = {msgtext: msgline};
        if (category == "client") {
            msg.client = name;
        } else if (category == "room") {
            msg.room = name;
        } else {
            throw "Unknown sender category";
        }
        klatschbase.postMessage(this.auth, msg, function (data) {
                self.addOwnMessage(msgline, data);
                self.refresh();
            });
    },
    sendMessage: function(msgline) {
	var rcpt = this.recipient;
	if (rcpt) {
            this.postMessage(rcpt[0], rcpt[1], msgline);
	}
    },
    parseCommand: function(msgline) {
        if (msgline.charAt(0) == "/") {
            var i = msgline.indexOf(" ");
            if (i == -1) i = msgline.length;
            var command = msgline.substring(1, i);
            switch (command) {
            case "join":
                if (i == -1) {
                    alert("No room name specified");
                    return;
                }
                if (msgline.charAt(i+1) == "#") i++;
                this.toggleSubscription(msgline.substring(i+1));
                return;
            case "msg":
                if (i == -1) {
                    alert("No destination specified");
                    return;
                }
                var j = msgline.indexOf(" ", i+1);
                if (j == -1) return;
                if (msgline.charAt(i+1) == "#") {
                    this.postMessage("room",
                                     msgline.substring(i+2, j),
                                     msgline.substring(j+1));
                } else {
                    this.postMessage("client",
                                     msgline.substring(i+1, j),
                                     msgline.substring(j+1));
                }
                return;
            }
        }
        this.sendMessage(msgline);
    },
    addOwnMessage: function(msg, data) {
	if (document.getElementById('echoMessage').checked === false) {
	    return;
	}
	var node = $(document.createElement("span"))
	.addClass(data === true ? "successEntry" : "warnEntry")
	.append($(document.createElement("span")).addClass("info")
		.text(data === true ? "Sent " :  "Failed to send "
		      + (data.error ? ("[" + data.description + "] ") : "")))
	.append($(document.createElement("span")).addClass("message")
		.text(msg));
	$("p.chat").append(node)
	.each(function(id, p) {
		p.scrollTop = p.scrollHeight;
	    });
    },
    addMessage: function(node, msg) {
	var time = msg.timestamp;
	node.append($(document.createElement("span")).addClass("sender")
		    .append(this.recipientLink("client", msg.sender)))
	.prepend($(document.createElement("span")).addClass("timestamp")
		 .text(utilities.intToString(time[2], 2) + ":"
		       + utilities.intToString(time[1], 2) + ":"
		       + utilities.intToString(time[0], 2)))
	.append($(document.createElement("span")).addClass("message")
		.text(msg.text));
	$("p.chat").append(node);
    },
    addPersonalMessage: function(msg) {
	var span = $(document.createElement("span")).addClass("personalEntry");
	this.addMessage(span, msg);
    },
    addRoomMessage: function(roomId, msg) {
	var span = $(document.createElement("span")).addClass("roomEntry")
	.append(this.recipientLink("room", roomId));
	this.addMessage(span, msg);
    },
    startMessagePolling: function(loginId, password, startkey) {
        var self = this;
        this.startkey = startkey;
        var msgListFun = function(msgsList) {
            clearTimeout(self.refreshMessageId);
            var count = 0;
            if (msgsList != null) {
                if (msgsList.error) {
                    return;
                }
                $.each(msgsList, function(key, msgs) {
                        if (msgs != null && msgs.messages != null
                            && msgs.messages.length > 0) {
                            var mlist = msgs.messages;
                            var nextStartkey = mlist[mlist.length - 1].id + 1;
                            if (msgs.key.category == "room") {
                                var roomId = msgs.key.name;
                                var room = self.subscribedRooms[roomId];
                                if (room) {
                                    room.startkey = nextStartkey;
                                    $.each(mlist, function(id, msg) {
                                            count++;
                                            self.addRoomMessage(roomId, msg);
                                        });
                                }
                            } else if (msgs.key.category == "client") {
                                self.startkey = nextStartkey;
                                $.each(mlist, function(id, msg) {
                                        count++;
                                        self.addPersonalMessage(msg);
                                    });
                            }
                            $("p.chat").each(function(id, p) {
                                    p.scrollTop = p.scrollHeight;
                                });
                        }
                    });
            }
            if (count == 0) {
                if (self.refreshMessageInterval < 20) {
                    self.refreshMessageInterval++;
                }
            } else if (count > 2) {
                if (self.refreshMessageInterval > 0) {
                    self.refreshMessageInterval--;
                }
            }
            self.refreshMessageId =
            setTimeout('klatschclient.refresh()',
                       500 * Math.pow(self.refreshMessageInterval, 2));
        }
        this.refresh = function() {
            klatschbase.getMessagesList([loginId, password],
                                        [{category: "client",
                                          name: loginId,
                                          startkey: self.startkey}]
                                        .concat(self.getSubscribedRooms()),
                                        msgListFun);
        };
        this.refresh();
    },
    setupListeners: function(loginId, password, startkey) {
	this.refreshClientListId =
	setInterval('klatschclient.displayClientList()',
		    this.refreshListInterval);
	this.refreshRoomListId =
	setInterval('klatschclient.displayRoomList()',
		    this.refreshListInterval);
	this.displayClientList();
	this.displayRoomList();
        this.startMessagePolling(loginId, password, startkey);
    },
    subscribeLink: function(roomId) {
	var self = this;
	return $(document.createElement("a")).attr("href","#")
	.click(function() {
		self.toggleSubscription(roomId);
		$("#msgline").focus();
		return false;
	    })
	.text(self.isSubscribed(roomId) ? "\u2611": "\u2610");
    },
    recipientLink: function(category, id) {
	var self = this;
	var action = function() {
	    var rcptSpan =
	    $(document.createElement("span")).addClass("recipient")
	    .append($(document.createElement("span")).addClass(category)
		    .text(id));
	    $("span.recipient").replaceWith(rcptSpan);
	    self.recipient = [category, id];
	    $("#msgline").focus();
	    return false;
	};
	return $(document.createElement("a")).attr("href","#").click(action)
	.append($(document.createElement("span")).addClass(category)
		.text(id));
    },
    displayRoomList: function() {
	var self = this;
	klatschbase.roomList(function(rooms) {
		if (rooms && !rooms.error) {
		    var rd =
			$(document.createElement("ul")).addClass("roomlist");
		    for (var i=0; i<rooms.length; i++) {
			var roomId = rooms[i].id;
			rd.append($(document.createElement("li"))
				  .append(self.subscribeLink(roomId))
				  .append(" ")
				  .append(self.recipientLink("room", roomId)));
		    }
		    $("ul.roomlist").replaceWith(rd);
		}
	    });
    },
    displayClientList: function() {
	var self = this;
	klatschbase.clientList(function(clients) {
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
				  .append(self.recipientLink("client",
							     clients[i].id)));
		    }
		    $("ul.clientlist").replaceWith(cd);
		}
	    });
    }
};

$(document).ready(function() {
	var kc = klatschclient;
	var kb = klatschbase;
	var loginId;
        var password;
	var onLogin = function(data) {
	    if (data) {
		if (data.error) {
		    alert("Error:" + data.description);
		} else {
		    loginId = data.id;
                    setTimeout("klatschclient.setupListeners('"
                               + loginId + "','" + password
                               + "'," + data.startkey + ")", 500);
		    $(".login").hide();
		    $(".inchat").show();
		    kc.auth = [loginId, password];
		    $("#msgline").focus();
		}
	    } else {
		alert("epic fail");
	    }
	}
	$(".inchat").hide();
	$("#login").keypress(function(e) {
		if (e.which == 13) {
		    $("#password").focus();
		}
	    }).focus();
	$("#registerFlag").click(function() {
		$("#password").focus();
	    });
	$("#password").keypress(function(e) {
		if (e.which == 13) {
		    loginId = document.getElementById('login').value;
		    password = this.value;
		    var register = document.getElementById('registerFlag')
			.checked === true;
		    if (register) {
			kb.register(loginId,
				    {login: loginId, password: password},
				    onLogin);
		    } else {
			kb.login([loginId, password], onLogin);
		    }
		    return false;
		}
	    });
	$("#msgline").keypress(function(e) {
		if (e.which == 13) {
		    kc.parseCommand(this.value);
		    this.value = "";
		    return false;
		}
	    });
	$("#createRoom").click(function() {
		var roomName = prompt("Name of the room");
		kb.makeRoom([loginId, password], roomName, function(data) {
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
	var startHeight;
	var followSlider = function(e) {
	    $("p.chat").height(startHeight + e.pageY);
	    return false;
	}
	$("div.main div.slider")
	    .mousedown(function(e) {
		    startHeight = $("p.chat").height() - e.pageY;
		    $(document).bind('mousemove', followSlider);
		})
	    .mouseup(function() {
		    $(document).unbind('mousemove', followSlider);
		});
	$(document).mouseup(function() {
		$(document).unbind('mousemove', followSlider);
	    });
	$("div.box")
	    .prepend($(document.createElement("a"))
		     .addClass("button hideOp").attr("href", "#")
		     .click(function() {
			     $("div.content", this.parentNode).slideToggle("fast");
			     $(this).text($(this).text() == "\u2b06"
					  ? "\u2b07" : "\u2b06");
			 })
		     .text("\u2b06"))
    });