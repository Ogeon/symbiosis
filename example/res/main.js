var next_id = 10;

function load_more() {
	api_call("GET", "api/more/" + next_id, function(res) {
		var people = JSON.parse(res);
		var template = new templates.Card();
		var card_list = document.getElementById("cards");

		for (var i = 0; i < people.length; i++) {
			var person = people[i];
			next_id = person.id + 1;
			template.id = person.id;
			template.name = person.name;
			template.age = person.age;
			template.supervisor = person.supervisor;
			template.render_to(card_list);
		};
	}, function(status) {
		alert("Failed to load more people! Status: " + status)
	});
}

function show_info(id, new_state) {
	if(new_state) {
		history.pushState({id: id}, "Symbiosis Example - More Info", id);
	}
	api_call("GET", "api/person/" + id, function(res) {
		var person = JSON.parse(res);
		var template = new templates.MoreInfo();
		var more_info = document.getElementById("more_info");
		var more_info_bg = document.getElementById("more_info_bg");

		while(more_info.firstChild) {
			more_info.removeChild(more_info.firstChild);
		}

		template.name = person.name;
		template.age = person.age;
		template.supervisor = person.supervisor;
		template.projects = person.projects;
		template.render_to(more_info);
		more_info_bg.style.display = "block";
	}, function(status) {
		alert("Failed to load person " + id + "! Status: " + status)
	});
}

function hide_info(new_state) {
	if(new_state) {
		history.pushState({id: null}, "Symbiosis Example", "/");
	}
	var more_info_bg = document.getElementById("more_info_bg");
	more_info_bg.style.display = "none";
}

window.onpopstate = function(e) {
	if(e.state.id !== null) {
		show_info(e.state.id, false);
	} else {
		hide_info(false)
	}
}

function api_call(method, path, on_ok, on_err) {
    var xmlhttp;

    if (window.XMLHttpRequest) {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp = new XMLHttpRequest();
    } else {
        // code for IE6, IE5
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == 4 ) {
           if(xmlhttp.status == 200){
               on_ok(xmlhttp.responseText);
           }
           else {
               on_err(xmlhttp.status);
           }
        }
    }

    xmlhttp.open(method, path, true);
    xmlhttp.send();
}