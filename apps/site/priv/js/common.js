var ROOTURL = location.protocol + '//' + location.host + '/';
var POSTURLS = {
				'actions'				:	{'url' : 'upload_actions'},
				'behaviors'				:	{'url' : 'upload_behaviors'},
				'input_GenTargetServer'	:	{'url' : 'general/set_target_server', 'key' : 'target_server'},
				'input_GenTargetPort'	:	{'url' : 'general/set_target_port', 'key' : 'target_port'},
				'input_GenNetworkName'	:	{'url' : 'general/set_network', 'key' : 'network'},
				'input_CommGeneralName'	:	{'url' : 'commander/set_general', 'key' : 'general'},
				'input_CommNetworkName'	:	{'url' : 'commander/set_network', 'key' : 'network'},
				'input_CommNumOfGunners':	{'url' : 'commander/set_gunners', 'key' : 'gunners'},
				'connect'				:	{'url' : 'commander/connect'},
				'disconnect'			:	{'url' : 'commander/disconnect'}
			  };

function RequestInfo(url, callback) {
	var loadinghtml =  '<div>Requesting Data</div>\
						<div id="loadTimer">\
							<span class="hr">00</span>:<span class="min">00</span>:<span class="sec">00</span>\
						</div>';

	$.ajax({
		url: ROOTURL+url,
		async: true,
		beforeSend: function() {
			if (!gHideProgressOverlay) {
				 $.blockUI({ 
					message: loadinghtml
				});
				$('#loadTimer').stopwatch();
			}
		},
		success: function(data) {
			callback(JSON.parse(data));
			$.unblockUI();
			
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
		}
	});
}

function RequestInfoOnly(url, callback) {
    $.ajax({
        url: ROOTURL+url,
        async: false,
        success: function(data) {
            callback(JSON.parse(data));            
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
        }
    });
}

function PostInfo(url, data, callback) {
    $.ajax({
        url: ROOTURL+url,
        type: 'POST',
        data: data,
        async: true,
        success: function(data) {
            callback(JSON.parse(data));
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
        }
    });
}