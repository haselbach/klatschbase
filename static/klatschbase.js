var klatschclient = {
    refreshMessageInterval: 5000,
    refreshListInterval: 60000,
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
    sendMessage: function(msgline) {
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
	    klatschbase.postMessage(this.auth, msg);
	}
    },
    addMessage: function(node, msg) {
	node.append($(document.createElement("span")).addClass("sender")
		    .append(this.recipientLink("client", msg.sender)))
	.append($(document.createElement("span")).addClass("message")
		.text(msg.text));
	$("p.chat").append(node);
    },
    addPersonalMessage: function(msg) {
	var span = $(document.createElement("span")).addClass("personalEntry");
	klatschclient.addMessage(span, msg);
    },
    addRoomMessage: function(roomId, msg) {
	var span = $(document.createElement("span")).addClass("roomEntry")
	.append($(document.createElement("span")).addClass("room")
		.text(roomId));
	this.addMessage(span, msg);
    },
    startMessagePolling: function(loginId, password, startKey) {
	var self = this;
	this.startkey = startKey;
	var msgListFun = function(msgsList) {
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
		if (self.refreshMessageInterval < 60000) {
		    self.refreshMessageInterval += 1000;
		}
	    } else if (count > 2) {
		if (self.refreshMessageInterval > 1000) {
		    self.refreshMessageInterval -= 1000;
		}
	    }
	    self.refreshMessageId =
	    setTimeout('klatschclient.refresh()', self.refreshMessageInterval);
	};
	this.refresh = function() {
	    klatschbase.getMessagesList([loginId, password],
					[{category: "client",
					  name: loginId,
					  startkey: self.startkey}]
					.concat(self.getSubscribedRooms()),
					msgListFun);
	}
	self.refresh();
	this.refreshClientListId =
	setInterval('klatschclient.displayClientList()',
		    self.refreshListInterval);
	this.refreshRoomListId =
	setInterval('klatschclient.displayRoomList()',
		    self.refreshListInterval);
	this.displayClientList();
	this.displayRoomList();
    },
    subscribeLink: function(roomId) {
	var self=klatschclient;
	return $(document.createElement("a")).attr("href","#")
	.click(function() {
		self.toggleSubscription(roomId);
		$("#msgline").focus();
		return false;
	    })
	.text(self.isSubscribed(roomId) ? "X": "O");
    },
    recipientLink: function(category, id) {
	var self = klatschclient;
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
	return $(document.createElement("a")).attr("href","#")
	.click(action).text(id);
    },
    displayRoomList: function() {
	var self = klatschclient;
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
	var self = klatschclient;
	klatschbase.clientList(function(clients) {
		if (clients && !clients.error) {
		    var cd =
		    $(document.createElement("ul")).addClass("clientlist");
		    for (var i=0; i<clients.length; i++) {
			cd.append($(document.createElement("li"))
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
		    kc.startMessagePolling(loginId, password, data.startkey);
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
		    var loginName = document.getElementById('login').value;
		    password = this.value;
		    var userDesc = {login: loginName, password: password}
		    if (document.getElementById('registerFlag').checked) {
			kb.register(loginName, userDesc, onLogin);
		    } else {
			kb.login([loginName, password], onLogin);
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
    });

