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
	
	// Remove any previous chart info
	$('.ChartInfo').remove();
	// Remove any custom graphoptions
	$('.ChartOptions').remove();
	// Clear the old graphs out
	for (chartName in gPlotInfo) {
		$('#'+chartName).remove();
	}
	gPlotInfo = {};

	RequestInfo(url, function(data) {
		
		if (!$.isEmptyObject(data))
		{
			var idx = 0;
			for(plot in data)
			{
				var chartName = 'IDX_'+idx;
				$("#main").append('<div id="'+chartName+'"><\/div>');
				
				gPlotInfo[chartName] = {"plot": null, "data" : [], "properties" : {}};
				
				DEFAULTPROPERTIES.title = plot;
				
				gPlotInfo[chartName].data = data[plot];
				gPlotInfo[chartName].properties = $.extend(true, setDefaultProperties_Calc(chartName), DEFAULTPROPERTIES);
				LoadPlot(chartName);
				
				$("#main").append(getSummary(plot, chartName));
				++idx;
			}
		
			$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Toggle Data Points\" onclick=\"co_ToggleMarkers()\"><\/input>');
			$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Toggle Mean Line\" onclick=\"co_ToggleHorizontalLine(\'mean\')\"><\/input>');
			$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Toggle Median Line\" onclick=\"co_ToggleHorizontalLine(\'median\')\"><\/input>');
			$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Redraw Chart(s)\" onclick=\"co_RedrawCharts()\"><\/input>');
			//$("#main").append('<input type=\"button\" class=\"ChartOptions\" value=\"Save Chart(s)\" onclick=\"co_SaveCharts()\"><\/input>');
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
									show: false,
								}},
								{horizontalLine: {
									name: 'median',
									y: median,
									show: false,
									color: 'rgb(66, 98, 144)',
								}},
							]
						 };
	
	if (data.length == 1) {
		prop.axes = {
						xaxis: {
							min: 0,
							max: 2,
							tickInterval: 1,
						}
					}
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
	var data = array.slice(0)		//Clones the array so we don't modify the ordering of the existing array
	var dataLength = data.length;
	var middle = Math.floor(dataLength/2);
	
	data.sort( function(a,b) {return a - b;} );
 
    if(dataLength % 2)
        return data[middle];		//ODD so return the middle of the sorted array
    else
        return (data[middle-1] + data[middle]) / 2;		//EVEN so return the average of the two middle numbers of the sorted array
}

function getSummary(plotName, chartName)
{
	var data = gPlotInfo[chartName].data;
	var numOfRequests = data.length;
	var max = Math.max.apply(null, data);
	var min = Math.max.apply(null, data);
	
	var summaryhtml = '<table class=\"ChartInfo\">';
	summaryhtml += '<caption>'+ plotName +'<\/caption>';
	summaryhtml += '<thead><tr>';
	summaryhtml +=  '<th>Total Requests<\/th>';
	summaryhtml +=  '<th>High<\/th>';
	summaryhtml +=  '<th>Low<\/th>';
	summaryhtml += '<\/tr><\/thead>';
	summaryhtml += '<tbody><tr>';
	summaryhtml +=  '<td>'+ numOfRequests +'<\/td>';
	summaryhtml +=  '<td>'+ max +'<\/td>';
	summaryhtml +=  '<td>'+ min +'<\/td>';	
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