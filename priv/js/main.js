var gPlotInfo = {};
var gHideProgressOverlay = true;

var DEFAULTPROPERTIES =
	{
		title: "",
		seriesDefaults: {
			showMarker: true,
		},
		axesDefaults: {
			pad: 0,
			labelRenderer: $.jqplot.CanvasAxisLabelRenderer,
			tickRenderer:  $.jqplot.CanvasAxisTickRenderer,
		},
		axes: {
			xaxis: {
				label: 'Requests',
			},
			yaxis: {
				label: 'Time (ms)',
				tickOptions: {
					formatter: function (format, val) { 
									var number = val/1000;	//microseconds to milliseconds
									return number;
								},
				},
			},
		},
		highlighter: {
			show: true,
			tooltipOffset: 5,
			tooltipAxes: "y",
			formatString: "%s ms",
		},
	};

$(document).ready(function() {
	RequestInfo("orders", function(orders) {
		for (var i = 0; i < orders.length; ++i)
		{
			$("#main").append('<input class="button" type=\"button\" value=\"'+ orders[i] +'\" onclick=\"IssueOrder(\''+ orders[i] +'\');\" ><\/input>');
		}
		$("input[type=button]").button();		//Apply jquery-ui for buttons
	});
});

function RequestInfo(url, callback) {
	var startTime = new Date();
	var loadinghtml = '<div>Requesting Data</div>'
	loadinghtml += '<div id="loadTimer"><span class="hr">00</span>:<span class="min">00</span>:<span class="sec">00</span></div>'

	$.ajax({
		url: url,
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

function IssueOrder(name) {
	gHideProgressOverlay = false;			
	var url = "issue_order?order=" + encodeURIComponent(name);
	
	// Override the jqplot default formatter to
	// adjust the values from microseconds to milliseconds
	var tickFormatter = function (format, val) { 
		var number = val/1000;
		return number;
	}
	
	// Remove any custom graphoptions
	$('.ChartOptions').remove();
	// Clear the old graphs out
	for (chartName in gPlotInfo) {
		$('#'+chartName).remove();
	}
	gPlotInfo = {};

	RequestInfo(url, function(data) {            
		var idx = 0;
		for(plot in data)
		{
			var chartName = 'IDX_'+idx;
			$("#main").append('<div id="'+chartName+'"><\/div>');
			
			gPlotInfo[chartName] = {"plot": null, "data" : [], "properties" : {}};
			
			DEFAULTPROPERTIES.title = plot;
			
			gPlotInfo[chartName].data = data[plot];
			gPlotInfo[chartName].properties = jQuery.extend(true, getMediumProperty(chartName), DEFAULTPROPERTIES);
			LoadPlot(chartName);
			++idx;
		}
	
		$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Toggle Data Points\" onclick=\"co_ToggleMarkers()\"><\/input>');
		$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Toggle Medium Line\" onclick=\"co_ToggleMedium()\"><\/input>');
		$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Redraw Chart(s)\" onclick=\"co_RedrawCharts()\"><\/input>');
		//$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Save Chart(s)\" onclick=\"co_SaveCharts()\"><\/input>');
		$("input[type=button]").button();		//Apply jquery-ui for buttons
	});
	gHideProgressOverlay = true;
}

function LoadPlot(chartName) {
	var startTime = new Date();
	var data = scaleData(gPlotInfo[chartName].data, 250);
	gPlotInfo[chartName].plot = $.jqplot(chartName, [data], gPlotInfo[chartName].properties);
	console.log('jqPlot took ' + ((new Date()) - startTime) + ' ms');
}

function scaleData(data, interval) {
	var scaledData = [];
	var dlength = data.length;
	if (dlength > interval) {
		for (var i = 0; i < dlength - 1 ; i+=interval) {
			scaledData.push([i, data[i]]);
		}
		scaledData.push([dlength-1, data[dlength-1]]);
	}
	else {
		scaledData = data;
	}
	return scaledData;		
}

function getMediumProperty(chartName)
{
	var dlength = gPlotInfo[chartName].data.length;
	var sum = 0;
	for (i = 0; i < dlength; ++i) {
		sum += gPlotInfo[chartName].data[i];
	}
	
	var medium = sum/dlength;
	
	return {canvasOverlay: {
				objects: [
					{horizontalLine: {
						y: medium,
					}},
				]
			}}
			
}

function co_ToggleMarkers() {
	for (chartName in gPlotInfo) {
		gPlotInfo[chartName].properties.seriesDefaults.showMarker = !gPlotInfo[chartName].plot.series[0].showMarker;
		gPlotInfo[chartName].plot.destroy();
		LoadPlot(chartName);
	}
}

function co_ToggleMedium() {
	for (chartName in gPlotInfo) {
		gPlotInfo[chartName].properties.canvasOverlay.show = !gPlotInfo[chartName].plot.options.canvasOverlay.show;
		gPlotInfo[chartName].plot.destroy();
		LoadPlot(chartName);
	}
}

function co_RedrawCharts() {
	for (chartName in gPlotInfo) {
		gPlotInfo[chartName].properties = jQuery.extend(true, getMediumProperty(chartName), DEFAULTPROPERTIES);
		gPlotInfo[chartName].plot.destroy();
		LoadPlot(chartName);
	}
}

function co_SaveCharts() {
	for (chartName in gPlotInfo) {
		console.log("Saving " + chartName);
	}
}
