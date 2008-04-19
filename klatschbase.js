var klatschclient = {
    refreshInterval: 20000,
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
			alert(JSON.stringify(data));
			self.subscribedRooms[name] = data;
			self.displayRoomList();
		    }
		});
	}
    },
    isSubscribed: function(name) {
	return this.subscribedRooms[name] != undefined;
    },
    parseCommand: function(msgline, user, password) {
	var msg = /([^\s]+)\s+([^\s]+)\s+(.*)/.exec(msgline);
	if (msg && msg[1] == "m") {
	    klatschbase.postMessage([user, password],
				    {client: msg[2], msgtext: msg[3]});
	} else if (msg && msg[1] == "r") {
	    klatschbase.postMessage([user, password],
                                    {room: msg[2],msgtext: msg[3]});
	} else {
	    alert("unknown command: " + msg);
	}
    },
    addMessage: function(id, msg) {
	$("p.chat").append("<span class='entry'><span class='sender'>"
			   + msg.sender
			   + "</span> "
			   + msg.text + "</span>");
    },
    startMessagePolling: function(loginId, password, startKey) {
	var self = this;
	var msgListFun = function(msgsList) {
	    if (msgsList != null) {
		if (msgsList.error) {
		    return;
		}
		$.each(msgsList, function(key, msgs) {
			if (msgs != null && msgs.messages != null) {
			    var mlist = msgs.messages;
			    $.each(mlist, self.addMessage);
			    startKey = mlist[mlist.length - 1].id + 1;
			    $("p.chat").each(function(id, p) {
				    p.scrollTop = p.scrollHeight;
				});
			}
		    });
	    }
	};
	this.refresh = function() {
	    klatschbase.getMessagesList([loginId, password],
					[{category: "client",
					  name: loginId,
					  startkey: startKey}]
					.concat(self.getSubscribedRooms()),
					msgListFun);
	}
	this.refreshId =
	setInterval('klatschclient.refresh()', klatschclient.refreshInterval);
    },
    subscribeLink: function(roomId) {
	var self=klatschclient;
	return $(document.createElement("a")).attr("href","#")
	.click(function() { self.toggleSubscription(roomId); })
	.text(self.isSubscribed(roomId) ? "X": "O");
    },
    displayRoomList: function() {
	var self = klatschclient;
	klatschbase.roomList(function(rooms) {
		if (rooms) {
		    var roomsdom =
			$(document.createElement("ul")).addClass("roomlist");
		    for (var i=0; i<rooms.length; i++) {
			var roomId = rooms[i].id;
			roomsdom.append($(document.createElement("li"))
					.append(self.subscribeLink(roomId))
					.append(" " + roomId));
		    }
		    $("ul.roomlist").replaceWith(roomsdom);
		}
	    });
    },
    displayClientList: function() {
	klatschbase.clientList(function(clients) {
		if (clients && !clients.error) {
		    var clientstr = "";
		    for (var i=0; i<clients.length; i++) {
			var clientId = clients[i].id;
			clientstr += "<li>" + clientId + "</li>";
		    }
		    $("ul.clientlist").html(clientstr);
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
		}
	    } else {
		alert("epic fail");
	    }
	}
        $("#jssubmit").click(function() {
		eval(document.getElementById('input').value);
	    });
	$(".inchat").hide();
	$("#loginsubmit").click(function() {
		var loginName = document.getElementById('login').value;
		password = document.getElementById('password').value;
		var userDesc = {login: loginName, password: password}
		kc.auth = [loginName, password];
		if (document.getElementById('registerFlag').checked) {
		    kb.register(loginName, userDesc, onLogin);
		} else {
		    kb.login([loginName, password], onLogin);
		}
	    });
	$("#msgsubmit").click(function() {
		kc.parseCommand(document.getElementById('msgline').value,
				loginId, password);
	    });
	$("a.refreshRooms").click(kc.displayRoomList);
	$("a.refreshClients").click(kc.displayClientList);
    });

