var klatschclient = {
    refreshInterval: 5000
};

function parseCommando(msgline, user) {
    var msg = /([^\s]+)\s+([^\s]+)\s+(.*)/.exec(msgline);
    if (msg && msg[1] == "m") {
	klatschbase.postMessage({user: user, client: msg[2],
		    msgtext: msg[3]});
    } else {
	alert("unknown command: " + msg);
    }
}

function addMessage(id, msg) {
    $("p.chat").append("<span class='entry'><span class='sender'>"
		       + msg.sender
		       + "</span> "
		       + msg.text + "</span>");
}

function startMessagePolling(loginId, startKey) {
    klatschclient.refresh = function() {
	klatschbase
	.getMessages("client", loginId, startKey, function(msgs) {
		if (msgs != null) {
		    $.each(msgs, addMessage);
		    if (msgs.length > 0) {
			startKey = msgs[msgs.length - 1].id + 1;
			$("p.chat").each(function(id, p) {
				p.scrollTop = p.scrollHeight;
			    });
		    }
		}
	    })};
    klatschclient.refreshId =
	setInterval('klatschclient.refresh()', klatschclient.refreshInterval);
}

function displayRoomList() {
    klatschbase.roomList(function(rooms) {
	    if (rooms) {
		var roomstr = "<span>blub</span>";
		for (var i=0; i<rooms.length; i++) {
		    roomstr += "<span>" + rooms[i].id + "</span>";
		}
		$("div.roomlist").html(roomstr);
		//alert(roomstr);
	    }
	});
}

$(document).ready(function() {
	var loginId;
	var onLogin = function(data) {
	    if (data) {
		if (data.error) {
		    alert("Error:" + data.description);
		} else {
		    loginId = data.id;
		    alert("success: " + loginId + " " + data.startkey);
		    startMessagePolling(loginId, data.startkey);
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
		klatschbase.register(loginName, null, onLogin);
	    });
	$("#msgsubmit").click(function() {
		parseCommando(document.getElementById('msgline').value,
			      loginId);
	    });
	$("a.refreshRooms").click(displayRoomList);
    });

