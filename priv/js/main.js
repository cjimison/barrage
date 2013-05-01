var gPlotInfo = {};
var gHideProgressOverlay = true;

var POSTURLS = ['upload_behaviors', 'upload_actions'];
var CONVERTTO = {'milliseconds' : 1/1000};

var DEFAULTPROPERTIES =
	{
		title: "",
		seriesDefaults: {
			showMarker: true
		},
		axesDefaults: {
			pad: 0,
			labelRenderer: $.jqplot.CanvasAxisLabelRenderer,
			tickRenderer:  $.jqplot.CanvasAxisTickRenderer
		},
		axes: {
			xaxis: {
				label: 'Requests'
			},
			yaxis: {
				label: 'Time (ms)',
				tickOptions: {
					formatter: function (format, val) { 
									var number = roundTo(val * CONVERTTO.milliseconds, 1000);
									return number;
								}
				}
			}
		},
		highlighter: {
			show: true,
			tooltipOffset: 5,
			tooltipAxes: "y",
			formatString: "%s ms"
		}
	};

$(document).ready(function() {	
	RequestInfo("orders", function(orders) {
		if (!$.isEmptyObject(orders))
		{
			$("#main").append('<div id="Request_Plot">');
			for (var i = 0; i < orders.length; ++i)
			{
				$("#Request_Plot").append('<input type=\"button\" value=\"'+ orders[i] +'\" onclick=\"rp_IssueOrder(\''+ orders[i] +'\');\" >');
			}
			$("input[type=button]").button();		//Apply jquery-ui for buttons
		}
	});
});

function RequestInfo(url, callback) {
	var loadinghtml = '<div>Requesting Data</div><div id="loadTimer"> \
	                  <span class="hr">00</span>:<span class="min">00</span>: \
	                  <span class="sec">00</span></div>';

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

function rp_IssueOrder(name) {
	gHideProgressOverlay = false;			
	var url = "issue_order?order=" + encodeURIComponent(name);
	
	// Remove any custom graphoptions
	$('#Chart_Options').remove();
	// Clear the old graphs out
	for (chartName in gPlotInfo) {
		var arr = chartName.split('_');
		$('#'+arr[0]).remove();
	}
	gPlotInfo = {};

	RequestInfo(url, function(data) {
		if (!$.isEmptyObject(data))
		{
			var idx = 0;
			for(plot in data)
			{
				var section = 'IDX'+idx;
				var chartName = section+'_Chart';
				$("#main").append('<div id="'+section+'">');
				$("#"+section).append('<div id="'+chartName+'">');
				
				gPlotInfo[chartName] = {"plot": null, "data" : [], "properties" : {}};
				
				DEFAULTPROPERTIES.title = plot;
				
				gPlotInfo[chartName].data = data[plot];
				gPlotInfo[chartName].properties = $.extend(true, setDefaultProperties_Calc(chartName), DEFAULTPROPERTIES);
				LoadPlot(chartName);
				
				$("#"+section).append(getSummary(plot, chartName));
				++idx;
			}
		
			$("#main").append('<div id="Chart_Options">');
			$("#Chart_Options").append('<input type=\"button\" value=\"Toggle Data Points\" onclick=\"co_ToggleMarkers()\">');
			$("#Chart_Options").append('<input type=\"button\" value=\"Toggle Mean Line\" onclick=\"co_ToggleHorizontalLine(\'mean\')\">');
			$("#Chart_Options").append('<input type=\"button\" value=\"Toggle Median Line\" onclick=\"co_ToggleHorizontalLine(\'median\')\">');
			$("#Chart_Options").append('<input type=\"button\" value=\"Redraw Chart(s)\" onclick=\"co_RedrawCharts()\">');
			//$("#Chart_Options").append('<input type=\"button\" value=\"Save Chart(s)\" onclick=\"co_SaveCharts()\">');
			$("input[type=button]").button();		//Apply jquery-ui for buttons
		}
		else
		{
			$("#main").append('<div class=\"ChartInfo\"><center><h1>No Data Returned<\/h1><\/center><\/div>');
		}
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

function setDefaultProperties_Calc(chartName)
{
	var data = gPlotInfo[chartName].data;
	var mean = calcMean(data);
	var median = calcMedian(data);
	
	var prop = {};
	prop.canvasOverlay = {
							show: true,
							objects: [
								{horizontalLine: {
									name: 'mean',
									y: mean,
									show: false
								}},
								{horizontalLine: {
									name: 'median',
									y: median,
									show: false,
									color: 'rgb(66, 98, 144)'
								}}
							]
						 };
	
	if (data.length == 1) {
		prop.axes = {
						xaxis: {
							min: 0,
							max: 2,
							tickInterval: 1
						}
					};
	}
	
	return prop;
}

function calcMean(array) {
	var sum = 0;
	var arrLength = array.length;
	for (var i = 0; i < arrLength; ++i) {
		sum += array[i];
	}

	return sum/arrLength;
}

function calcMedian(array) {
	var data = array.slice(0);		//Clones the array so we don't modify the ordering of the existing array
	var dataLength = data.length;
	var middle = Math.floor(dataLength/2);
	
	data.sort( function(a,b) {return a - b;} );
 
    if(dataLength % 2)
        return data[middle];		//ODD so return the middle of the sorted array
    else
        return (data[middle-1] + data[middle]) / 2;		//EVEN so return the average of the two middle numbers of the sorted array
}

function roundTo(number, to)
{
    return Math.round(number * to) / to;
}

function getSummary(plotName, chartName)
{
	var data = gPlotInfo[chartName].data;
	var numOfRequests = data.length;
	var max = roundTo(Math.max.apply(null, data) * CONVERTTO.milliseconds, 1000) + ' ms';
	var min = roundTo(Math.min.apply(null, data) * CONVERTTO.milliseconds, 1000) + ' ms';
	var mean = roundTo(calcMean(data) * CONVERTTO.milliseconds, 1000) + ' ms';
	var median = roundTo(calcMedian(data) * CONVERTTO.milliseconds, 1000) + ' ms';
	
	var summaryhtml = '<table class=\"ChartInfo\">';
	summaryhtml += '<caption>'+ plotName +'<\/caption>';
	summaryhtml += '<thead><tr>';
	summaryhtml +=  '<th>Total Requests<\/th>';
	summaryhtml +=  '<th>High<\/th>';
	summaryhtml +=  '<th>Low<\/th>';
	summaryhtml +=  '<th>Mean<\/th>';
	summaryhtml +=  '<th>Median<\/th>';
	summaryhtml += '<\/tr><\/thead>';
	summaryhtml += '<tbody><tr>';
	summaryhtml +=  '<td>'+ numOfRequests +'<\/td>';
	summaryhtml +=  '<td>'+ max +'<\/td>';
	summaryhtml +=  '<td>'+ min +'<\/td>';
	summaryhtml +=  '<td>'+ mean +'<\/td>';
	summaryhtml +=  '<td>'+ median +'<\/td>';
	summaryhtml += '<\/tr><\/tbody>';
	summaryhtml += '<\/table>';
	
	return summaryhtml;
}

function co_ToggleMarkers() {
	for (chartName in gPlotInfo) {
		gPlotInfo[chartName].properties.seriesDefaults.showMarker = !gPlotInfo[chartName].plot.series[0].showMarker;
		gPlotInfo[chartName].plot.destroy();
		LoadPlot(chartName);
	}
}

function co_ToggleHorizontalLine(linename) {
	for (chartName in gPlotInfo) {
		var co =  gPlotInfo[chartName].plot.plugins.canvasOverlay;
		var line = co.get(linename);
		line.options.show = !line.options.show;
		co.draw(gPlotInfo[chartName].plot);
	}
}

function co_RedrawCharts() {
	for (chartName in gPlotInfo) {
		gPlotInfo[chartName].properties = $.extend(true, setDefaultProperties_Calc(chartName), DEFAULTPROPERTIES);
		gPlotInfo[chartName].plot.destroy();
		LoadPlot(chartName);
	}
}

function co_SaveCharts() {
	for (chartName in gPlotInfo) {
		console.log("Saving " + chartName);
	}
}



function pd_showUploadButtons(hide) {
	$('#Upload_Buttons').remove();
	if (!hide)
	{
		$("#Post_Data").append('<div id="Upload_Buttons">');
		$("#Upload_Buttons").append('<input type=\"button\" value=\"Upload Behaviors\" onclick=\"pd_PostTo(\'upload_behaviors\')\" >');
		$("#Upload_Buttons").append('<input type=\"button\" value=\"Upload Actions\" onclick=\"pd_PostTo(\'upload_actions\')\" >');
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
					console.log("test" + response);
					window.location.reload(true);
				});
			}
		};
		reader.readAsText(file);
    }
	
	//Reset and hide the Post Data Controls
	var control = $("#fileChoser");
	control.replaceWith( control = control.val('').clone( true ) );
	pd_showUploadButtons(true);
}

function PostInfo(url, data, callback) {
	$.ajax({
		url: url,
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
