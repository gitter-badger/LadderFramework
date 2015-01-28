var ladder = (function() {
	var clickedLastArray = [];
	var recievedIdArray = [];
	return {
		post : function(event) {

			event.preventDefault();

			/* get some values from elements on the page: */
			var $form = $(event.target);
			console.log("$form: " + $form);
			var postData = $form.serialize();
			console.log("term: " + postData);
			var url = $form.attr('action');
			console.log("url: " + url);

			var lastClick = clickedLastArray.pop();
			if (lastClick.form == url) {
				postData = postData + "&" + lastClick.button + "=clicked";
			}

			function error(XMLHttpRequest, textStatus, errorThrown) {
				console.log("error: " + XMLHttpRequest + " --- " + textStatus + " --- " + errorThrown);
			};
			
			function success(data) {
				if (console && console.log) {
					console.log('Sample of data:', data);
				}
				eval(data);
			};
			
			var r = new XMLHttpRequest();
			var noCache = Math.random();
			r.open("POST", url, true);
			r.onreadystatechange = function () {
			  if (r.readyState != 4) return;
			  if(r.status != 200) {
				  error(r, r.statusText, r.responseText);
			  }else{
				  success(r.responseText);
			  }
			};
			r.send(postData);
		},

		clickedLast : function(form, event) {
			var url = $(form).attr('action');
			var button = $(event.target).attr("name");
			clickedLastArray.push({
				form : url,
				button : button
			});
		},
		
		ajax : function(url, name, event) {
			var val = $(event.target).val() || event;
			
			console.log("url: " + url + ", val: " + val + ", name: " + name);
			
			function error(XMLHttpRequest, textStatus, errorThrown) {
				console.log("error: " + XMLHttpRequest + " --- " + textStatus + " --- " + errorThrown);
			};
			
			function success(data) {
				if (console && console.log) {
					console.log('Sample of data:', data);
				}
				eval(data);
			};
			
			var r = new XMLHttpRequest();
			var noCache = Math.random();
			r.open("GET", url + "?" + name + "=" + val, true);
			r.onreadystatechange = function () {
			  if (r.readyState != 4) return;
			 if(r.status != 200) {
				  error(r, r.statusText, r.responseText);
			  }else{
				  success(r.responseText);
			  }
			};
			r.send();
		},

		push : function push(pageId) {
			var postData = '';
			if(recievedIdArray.length > 0) {
				var lastId = recievedIdArray.pop();
				postData = "lastId=" + lastId + "&";
			}
			
			function success(successData) { 
				try{
					var data = jQuery.parseJSON(successData);
					$.each(data.messages, function(index, msg){
						console.log("msg.id: " + msg.id)
						recievedIdArray.push(msg.id);
						try{
							eval(msg.message);
						}catch(err){
							// TODO error function
							console.log("error message: " + err);
						}
					});
				}catch(e){
					console.log("Problems with message: " + e);
				}
				ladder.push(pageId);
			};
			
			function error(XMLHttpRequest, textStatus, errorThrown) {
				if(textStatus != "timeout" || errorThrown != "timeout"){
					// TODO error function
					console.log("error: " + XMLHttpRequest + " --- " + textStatus + " --- " + errorThrown);
				}
				setTimeout('ladder.push("' + pageId + '")', 1000);
			};
			
			var r = new XMLHttpRequest();
			var noCache = Math.random();
			r.open("GET", "/pull/" + pageId + "/wait?" + postData + "_=" + noCache, true);
			r.onreadystatechange = function () {
			  if (r.readyState != 4) return;
			  if(r.status == 404){
				  location.reload();
			  }else if(r.status != 200) {
				  error(r, r.statusText, r.responseText);
			  }else{
				  success(r.responseText);
			  }
			};
			r.send();
		}
	};

})();