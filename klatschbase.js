var klatschclient = {
    refreshInterval: 30000
};

function parseCommand(msgline, user, password) {
    var msg = /([^\s]+)\s+([^\s]+)\s+(.*)/.exec(msgline);
    if (msg && msg[1] == "m") {
	klatschbase.postMessage([user, password],
				{client: msg[2], msgtext: msg[3]});
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

function startMessagePolling(loginId, password, startKey) {
    klatschclient.refresh = function() {
	klatschbase
	.getMessages([loginId, password],
		     "client", loginId, startKey,
		     function(msgs) {
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
        var password;
	var onLogin = function(data) {
	    if (data) {
		if (data.error) {
		    alert("Error:" + data.description);
		} else {
		    loginId = data.id;
		    alert("success: " + loginId + " " + data.startkey);
		    startMessagePolling(loginId, password, data.startkey);
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
		if (document.getElementById('registerFlag').checked) {
		    klatschbase.register(loginName, userDesc, onLogin);
		} else {
		    klatschbase.login([loginName, password], onLogin);
		}
	    });
	$("#msgsubmit").click(function() {
		parseCommand(document.getElementById('msgline').value,
			     loginId, password);
	    });
	$("a.refreshRooms").click(displayRoomList);
    });

