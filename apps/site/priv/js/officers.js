$(document).ready(function() {
	SetGeneralInfo();
	SetCommanderInfo();
});

function SetGeneralInfo() {
	RequestInfoOnly("general/status", function(data) {
		$("#GenGeneralName").text(data.general);
		$("#GenTargetServer").text(data.target_server);
		$("#input_GenTargetServer").val(data.target_server);
		$("#GenTargetPort").text(data.target_port);
		$("#input_GenTargetPort").val(data.target_port);
		$("#GenNetworkName").text(data.network);
		$("#input_GenNetworkName").val(data.network);
		
		$('#GeneralCommanders > tbody').empty();	//Clear the commanders first so you won't get duplicates
		var commanders = data.commanders;
		for (var i=0; i < commanders.length; ++i)
		{
			$('#GeneralCommanders > tbody:last').append('<tr>\
															<td>'+ commanders[i].name +'</td>\
															<td>'+ commanders[i].count +'<div class="glyphicon glyphicon-edit"></div></td>\
														</tr>');
		}
		
		$("#General span.toggle").show();
		$("#General input.toggle").hide();
		$("#General input.showfirst").show();
	});
}

function SetCommanderInfo() {
	RequestInfoOnly("commander/status", function(data) {
		$("#CommCommanderName").text(data.commander);
		$("#CommGeneralName").text(data.general);
		$("#input_CommGeneralName").val(data.general);
		$("#CommNetworkName").text(data.network);
		$("#input_CommNetworkName").val(data.network);
		$("#CommNumOfGunners").text(data.gunners);
		$("#input_CommNumOfGunners").val(data.gunners);
		
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

function PostOfficerData(inputId) {
	var key = POSTURLS[inputId].key
	var url = POSTURLS[inputId].url

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

function PostOfficerDataChangedOnly() {
	for (var input in POSTURLS) {
		var splitinput = input.split("_");
		if (splitinput[0] === "input") {
			if ($("#"+input).val() !== $("#"+splitinput[1]).text()) {
				if (input === "input_GenNetworkName")	{
					OfficerAction("disconnect", "dontset");
					PostOfficerData(input);
				}
				else {
					PostOfficerData(input);
				}
			}
		}
	}

	OfficerAction("connect");

	SetGeneralInfo();
	SetCommanderInfo();
}

function OfficerAction(action, set) {
	$.get(POSTURLS[action].url);

	if (typeof set === 'undefined') {
		SetCommanderInfo();
	}
	else if(set === 'general') {
		SetGeneralInfo();
	}
}