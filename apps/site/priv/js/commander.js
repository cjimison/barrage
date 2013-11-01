var POSTCOMMANDURLS = {
						'input_GeneralName'		:		{'url' : '/commander/set_general', 'key' : 'general'},
						'input_NetworkName'		:		{'url' : '/commander/set_network', 'key' : 'network'},
						'input_NumOfGunners'	:		{'url' : '/commander/set_gunners', 'key' : 'gunners'},
						'connect'				:		{'url' : '/commander/connect'},
						'disconnect'			:		{'url' : '/commander/disconnect'}
					  };

$(document).ready(function() {
	fartscroll();
	SetInfo();
	//$("input[type=button]").button();		//Apply jquery-ui for buttons
});

function SetInfo() {
	RequestInfo("./commander/status", function(data) {
		$("#CommanderName").text(data.commander);
		$("#GeneralName").text(data.general);
		$("#input_GeneralName").val(data.general);
		$("#NetworkName").text(data.network);
		$("#input_NetworkName").val(data.network);
		$("#NumOfGunners").text(data.gunners);
		$("#input_NumOfGunners").val(data.gunners);
		
		if (data.state === "connected") {
			$("span.toggle").show();
			$("input.toggle").hide();
			$("input.showfirst").show();
		}
		else {
			$("span.toggle").hide();
			$("input.toggle").show();
			$("input.showfirst").hide();
		}
	});
}

function PostData(inputId) {
	var key = POSTCOMMANDURLS[inputId].key
	var url = POSTCOMMANDURLS[inputId].url
	
	var data = {};
	if(key === "gunners") {
		data[key] = parseInt($("#"+inputId).val());
	}
	else {
		data[key] = $("#"+inputId).val();
	}
	
	PostInfo(url, data, function(response) {
		//window.location.reload(true);
	});
}

function CommanderAction(action) {
	$.get(POSTCOMMANDURLS[action].url);
	SetInfo();
}