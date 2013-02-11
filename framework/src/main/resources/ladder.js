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

			$.ajax({
				url : url,
				type : 'POST',
				data : postData,
				dataType : "script",
				cache : false,
				beforeSend : function(xhr) {
					console.log("sending: " + xhr)
				},
				success : function(data) {
					if (console && console.log) {
						console.log('Sample of data:', data);
					}
				}
			});
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
			var val = $(event.target).val();
			
			console.log("url: " + url + ", val: " + val + ", name: " + name);
			$.ajax({
				url : url + "?" + name + "=" + val,
				type : 'GET',
				dataType : "script",
				cache : false,
				beforeSend : function(xhr) {
					console.log("sending: " + xhr)
				},
				success : function(data) {
					if (console && console.log) {
						console.log('Sample of data:', data);
					}
				}
			});
		},

		push : function push(pageId) {
			var postData = '';
			if(recievedIdArray.length > 0) {
				var lastId = recievedIdArray.pop();
				postData = "lastId=" + lastId
			}
			$.ajax({
				url : "/pull/" + pageId + "/wait?" + postData,
				//url: "/js/test.js",
				type : "GET",
				async : true,
				cache : false,
				timeout : 25000,
				dataType : "json",
				success : function(data) { 
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
					ladder.push(pageId);
				},
				error : function(XMLHttpRequest, textStatus, errorThrown) {
					if(textStatus != "timeout" || errorThrown != "timeout"){
						// TODO error function
						console.log("error: " + XMLHttpRequest + " --- " + textStatus + " --- " + errorThrown);
					}
					setTimeout('ladder.push("' + pageId + '")', 1000);
				}
			});
		}
	}

})();