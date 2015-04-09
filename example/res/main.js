function load_more() {
	api_call("GET", "api/more", function(res) {
		var people = JSON.parse(res);
		var template = new templates.Card();
		var card_list = document.getElementById("cards");

		for (var i = 0; i < people.length; i++) {
			var person = people[i];
			template.name = person.name;
			template.age = person.age;
			template.supervisor = person.supervisor;
			template.render_to(card_list);
		};
	}, function(status) {
		alert("Failed to load more people! Status: " + status)
	});
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