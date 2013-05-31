var POSTURLS = ['upload_behaviors', 'upload_actions'];

var POSTGENERALURLS = {
						'input_NetworkName'		:		{'url' : '/general/set_network', 'key' : 'network'},
						'input_NumOfGunners'	:		{'url' : '/general/set_gunners', 'key' : 'gunners'},
						'connect'				:		{'url' : '/general/connect'},
						'disconnect'			:		{'url' : '/general/disconnect'}
					  };
$(document).ready(function() {
	fartscroll();
	SetInfo();
	
	$("input[type=button]").button();		//Apply jquery-ui for buttons
});


function SetInfo()
{
	$("#GeneralName").text("GENERALDUMMY@127.0.0.1");
	$("#NetworkName").text("TESTDUMMY");
	$("#input_NetworkName").val("TESTDUMMY");
	$('#GeneralCommanders > tbody:last').append('<tr>\
													<td><a href="/commander.html">commanderDUMMY@127.0.0.1</a></td>\
													<td>\
														<span id="NumOfGunners" class="toggle">100</span>\
														<input id="input_NumOfGunners" class="toggle" type="text" style="width:50px;" value="100"/>\
														<input type="button" class="toggle" value="set" onclick=\'PostData("input_NumOfGunners");\'/>\
													</td>\
												</tr>\
												<tr>\
													<td><a href="/commander.html">kommanderDUMMY2@127.0.0.1</a></td>\
													<td>\
														<span id="NumOfGunners" class="toggle">50</span>\
														<input id="input_NumOfGunners" class="toggle" type="text" style="width:50px;" value="50"/>\
														<input type="button" class="toggle" value="set" onclick=\'PostData("input_NumOfGunners");\'/>\
													</td>\
												 </tr>');
	
	$("span.toggle").show();
	$("input.toggle").hide();
	$("input.showfirst").show();
}

function PostData(inputId) {
	var key = POSTGENERALURLS[inputId].key
	var url = POSTGENERALURLS[inputId].url
	
	var data = {};
	data[key] = $("#"+inputId).val();
	
	PostInfo(url, data, function(response) {
		window.location.reload(true);
	});
}

function GeneralAction(action)
{
	$.get(POSTGENERALURLS[action].url);
	SetInfo();
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