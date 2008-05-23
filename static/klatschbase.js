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
                        self.messagePolling(self.auth[0], self.auth[1],
                                            "room", name, data.startkey);
		    }
		});
	}
    },
    isSubscribed: function(name) {
	return this.subscribedRooms[name] != undefined;
    },
    sendMessage: function(msgline) {
	if (msgline == "") return;
	var self = this;
	var rcpt = this.recipient;
	if (rcpt) {
	    var msg = {msgtext: msgline};
	    if (rcpt[0] == "client") {
		msg.client = rcpt[1];
	    } else if (rcpt[0] == "room") {
		msg.room = rcpt[1];
	    } else {
		throw "Unknown sender category";
	    }
	    klatschbase.postMessage(this.auth, msg, function (data) {
		    self.addOwnMessage(msgline, data);
		    //self.refresh();
		});
	}
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
    messagePolling: function(loginId, password, category, name, startkey) {
	var self = this;
        var isRoom = category == "room";
        var refresh;
        var msgListFun = function(messages) {
	    clearTimeout(self.refreshMessageId);
	    if (messages != null) {
		if (messages.error) {
		    return;
		}
                if (messages.length > 0) {
                    var mlist = messages;
                    startkey = mlist[mlist.length - 1].id + 1;
                    if (isRoom) {
                        $.each(mlist, function(id, msg) {
                                self.addRoomMessage(name, msg);
                            });
                    } else {
                        $.each(mlist, function(id, msg) {
                                self.addPersonalMessage(msg);
                            });
                    }
                    $("p.chat").each(function(id, p) {
                            p.scrollTop = p.scrollHeight;
                        });
                }
            }
            refresh();
        };
        refresh = function() {
            //alert("Calling refresh");
            klatschbase.getMessagesWait([loginId, password],
                                        category, name, startkey,
                                        msgListFun, refresh);
            //alert("Called it");
        };
        refresh();
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
        this.messagePolling(loginId, password, "client", loginId, startkey);
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
		    kc.sendMessage(this.value);
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