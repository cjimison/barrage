var POSTURLS = ['upload_behaviors', 'upload_actions'];

var POSTGENERALURLS = {
						'input_TargetServer'	:		{'url' : '/general/set_target_server', 'key' : 'target_server'},
						'input_TargetPort'		:		{'url' : '/general/set_target_port', 'key' : 'target_port'},
						'input_NetworkName'		:		{'url' : '/general/set_network', 'key' : 'network'}
					  };
$(document).ready(function() {
	fartscroll();
	SetInfo();
	
	$("input[type=button]").button();		//Apply jquery-ui for buttons
	//Apply jquery-ui for confirmation dialog
	$( "#dialog-confirm" ).dialog({
		autoOpen: false,
		resizable: false,
		modal: true,
		buttons: {
			"OK": function() {
				$( this ).dialog( "close" );
				PostDataAll();
				SetInfo();
			},
			Cancel: function() {
				$( this ).dialog( "close" );
				$("#input_NetworkName").val($("#NetworkName").text());
			}
		}
	});
});


function SetInfo() {
	RequestInfo("./general/status", function(data) {
		$("#GeneralName").text(data.general);
		$("#TargetServer").text(data.target_server);
		$("#input_TargetServer").val(data.target_server);
		$("#TargetPort").text(data.target_port);
		$("#input_TargetPort").val(data.target_port);
		$("#NetworkName").text(data.network);
		$("#input_NetworkName").val(data.network);
		
		$('#GeneralCommanders > tbody').empty();	//Clear the commanders first so you won't get duplicates
		var commanders = data.commanders;
		for (var i=0; i < commanders.length; ++i)
		{
			$('#GeneralCommanders > tbody:last').append('<tr>\
															<td>'+ commanders[i].name +'</td>\
															<td><span id="NumOfGunners">'+ commanders[i].count +'</span></td>\
														</tr>');
		}
		
		$("input[type=button]").button();		//Apply jquery-ui for buttons  //Needs to reapply because of the RequestInfo		
		$("span.toggle").show();
		$("input.toggle").hide();
		$("input.showfirst").show();
	});
}

function PostData(inputId) {
	var key = POSTGENERALURLS[inputId].key
	var url = POSTGENERALURLS[inputId].url
	
	var data = {};
	data[key] = $("#"+inputId).val();
	
	PostInfo(url, data, function(response) {
		//window.location.reload(true);
	});
}

function PostDataAll() {
	for (var input in POSTGENERALURLS) {
		if (input.indexOf("input") == 0) {
			PostData(input);
		}
	}
}

function GeneralAction(action) {
	$.get(POSTGENERALURLS[action].url);
	SetInfo();
}

function SaveChanges() {
	//NEED CONFIRMATION IF NETWORK NAME CHANGES
	if ($("#input_NetworkName").val() !== $("#NetworkName").text()) {
		$("#dialog-confirm").dialog("open");
	}
	else {
		PostDataAll();
		SetInfo();
	}
}



//*** This is for the UPLOAD File ***//
function pd_showUploadButtons(show) {
	show = (typeof show === "undefined") ? true : false;
	$('#Upload_Buttons').remove();
	if (show)
	{
		$("#Post_Data").append('<div id="Upload_Buttons">');
		$("#Upload_Buttons").append('<input type=\"button\" value=\"Upload Behaviors\" onclick=\"pd_PostTo(\''+POSTURLS[0]+'\')\" >');
		$("#Upload_Buttons").append('<input type=\"button\" value=\"Upload Actions\" onclick=\"pd_PostTo(\''+POSTURLS[1]+'\')\" >');
		$("input[type=button]").button();		//Apply jquery-ui for buttons
	}
}

function pd_PostTo(url) {
	var files = $('#fileChoser')[0].files;
	if (!files.length) {
      console.log("No file selected");
    }
    else
    {
		var file = files[0];
		var reader = new FileReader();
		// If we use onloadend, we need to check the readyState.
		reader.onloadend = function(evt) {
			if (evt.target.readyState == FileReader.DONE) { // DONE == 2
				var data = evt.target.result;
				PostInfo(url, data, function(response) {
					window.location.reload(true);
				});
			}
		};
		reader.readAsText(file);
    }
	
	//Reset and hide the Post Data Controls
	var control = $("#fileChoser");
	control.replaceWith( control = control.val('').clone( true ) );
	pd_showUploadButtons(false);
}
//*** END This is for the UPLOAD File END ***//