function PostInfo(url, data, callback)
{
	$.ajax({
		url: url,
		type: 'POST',
		data: JSON.stringify(data),
		async: true,
		success: function(data) {
			callback(JSON.parse(data));
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
		}
	});
}

function RequestInfo(url, callback)
{
	$.ajax({
		url: url,
		async: true,
		success: function(data) {
			callback(JSON.parse(data));			
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
		}
	});
}