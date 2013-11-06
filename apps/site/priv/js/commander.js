var POSTCOMMANDURLS = {
						'input_GeneralName'		:		{'url' : '/commander/set_general', 'key' : 'general'},
						'input_NetworkName'		:		{'url' : '/commander/set_network', 'key' : 'network'},
						'input_NumOfGunners'	:		{'url' : '/commander/set_gunners', 'key' : 'gunners'},
						'connect'				:		{'url' : '/commander/connect'},
						'disconnect'			:		{'url' : '/commander/disconnect'}
					  };

$(document).ready(function() {
	SetCommanderInfo();
});

function SetCommanderInfo() {
	RequestInfo("commander/status", function(data) {
		$("#Commander #CommanderName").text(data.commander);
		$("#Commander #GeneralName").text(data.general);
		$("#Commander #input_GeneralName").val(data.general);
		$("#Commander #NetworkName").text(data.network);
		$("#Commander #input_NetworkName").val(data.network);
		$("#Commander #NumOfGunners").text(data.gunners);
		$("#Commander #input_NumOfGunners").val(data.gunners);
		
		if (data.state === "connected") {
			$("#Commander span.toggle").show();
			$("#Commander input.toggle").hide();
			$("#Commander input.showfirst").show();
		}
		else {
			$("#Commander span.toggle").hide();
			$("#Commander input.toggle").show();
			$("#Commander input.showfirst").hide();
		}
	});
}

function PostCommanderData(inputId) {
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
	SetCommanderInfo();
}