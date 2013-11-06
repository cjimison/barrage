
$(document).ready(function() {
	SetGeneralInfo();
});


function SetGeneralInfo() {
	RequestInfoOnly("general/status", function(data) {
		$("#General #GeneralName").text(data.general);
		$("#General #TargetServer").text(data.target_server);
		$("#General #input_TargetServer").val(data.target_server);
		$("#General #TargetPort").text(data.target_port);
		$("#General #input_TargetPort").val(data.target_port);
		$("#General #NetworkName").text(data.network);
		$("#General #input_NetworkName").val(data.network);
		
		$('#GeneralCommanders > tbody').empty();	//Clear the commanders first so you won't get duplicates
		var commanders = data.commanders;
		for (var i=0; i < commanders.length; ++i)
		{
			$('#GeneralCommanders > tbody:last').append('<tr>\
															<td>'+ commanders[i].name +'</td>\
															<td><span id="NumOfGunners">'+ commanders[i].count +'</span></td>\
														</tr>');
		}
		
		$("#General span.toggle").show();
		$("#General input.toggle").hide();
		$("#General input.showfirst").show();
	});
}

function PostGeneralData(inputId) {
	var key = POSTGENERALURLS[inputId].key
	var url = POSTGENERALURLS[inputId].url
	
	var data = {};
	data[key] = $("#"+inputId).val();
	
	PostInfo(url, data, function(response) {
		//window.location.reload(true);
	});
}

function PostGeneralDataAll() {
	for (var input in POSTGENERALURLS) {
		if (input.indexOf("input") == 0) {
			PostGeneralData(input);
		}
	}
}

function PostGeneralDataChangedOnly() {
	for (var input in POSTGENERALURLS) {
		var splitinput = input.split("_");
		if (splitinput[0] === "input") {
			if ($("#"+input).val() !== $("#"+splitinput[1]).text()) {
				PostGeneralData(input);
			}
		}
	}
}

function GeneralAction(action) {
	$.get(POSTGENERALURLS[action].url);
	SetGeneralInfo();
}

function SaveGeneralChanges() {
	//NEED CONFIRMATION IF NETWORK NAME CHANGES
	//if ($("#input_NetworkName").val() !== $("#NetworkName").text()) {
	//	$("#dialog-confirm").dialog("open");
	//}
	//else {
		PostGeneralDataChangedOnly();
		SetGeneralInfo();
	//}
}