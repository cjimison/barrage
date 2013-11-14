var g_websocket = null;
var gPlotInfo = {};
var gHideProgressOverlay = true;

var CONVERTTO = {'milliseconds' : 1/1000};

var DEFAULTPROPERTIES =
	{
		seriesColors: [ "#428bca", "#d9534f", "#5cb85c", "#f0ad4e", "#5bc0de", "#dddddd" ],
		title: "",
		seriesDefaults: {
			showMarker: true
		},
		axesDefaults: {
			pad: 1.1,
			labelRenderer: $.jqplot.CanvasAxisLabelRenderer,
			tickRenderer:  $.jqplot.CanvasAxisTickRenderer
		},
		axes: {
			xaxis: {
				label: 'Requests',
				min: 1,
				tickInterval: 1,
				tickOptions: {
					showGridline: false
				}
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
		grid: {
			drawBorder: false,
			shadow: false,
			background: "rgba(0,0,0,0)"
		},
		highlighter: {
			show: true,
			tooltipOffset: 5,
			tooltipAxes: "y",
			formatString: "%s ms"
		}
	};

$(document).ready(function() {

	RequestInfo("general/orders", function(orders) {
		if (!$.isEmptyObject(orders))
		{
			$("#ChartOrders").append('<ul id="Request_Plot" class="nav nav-pills">');
			for (var i = 0; i < orders.length; ++i)
			{
				$("#Request_Plot").append('<li><a data-toggle="pill" onclick=\"rp_IssueOrder(\''+ orders[i] +'\');\">'+ orders[i] +'</a></li>');
			}
		}
	});
});

function rp_IssueOrder(name) {
	gHideProgressOverlay = false;			
	var url = "general/issue_order?order=" + encodeURIComponent(name);
	
	// Remove ChartInfo and Chart_Options update Nav
	$('.ChartInfo').remove();
	$('#Chart_Options').remove();
	// Clear the old graphs out
	for (chartName in gPlotInfo) {
		var arr = chartName.split('_');
		$('#'+arr[0]).remove();
	}

	//This is the REALTIME graph
	if ( $('input:radio[name=liveChart]:checked').val() === "true" )
	{
		var wsHost ="ws://"+
                window.location.hostname+":"+
                window.location.port+
                "/general/streaming";
    
	    gPlotInfo = {_idx : -1};
	    var gPlotNameMap = {};

	    if(g_websocket && g_websocket.readyState === WebSocket.OPEN)
	    {
	        g_websocket.close();
	    }
	    
	    g_websocket = new WebSocket(wsHost);
	    g_websocket.onopen = function(evt)
	    {
	        // Send off the request
	        var msg = { cmd : "order", behavior : name };
	        g_websocket.send(JSON.stringify(msg));
	    }; 
	    g_websocket.onclose = function(evt)
	    { 
	    }; 
	    g_websocket.onmessage = function(evt)
	    { 
	        // Data to add to the grapha
	        var obj = JSON.parse(evt.data);
	        if(obj.name)
	        {
	            obj.time *= CONVERTTO.milliseconds;
	            var idx = 0;
	            // Is this name Mapped?
	            if(undefined !== gPlotNameMap[obj.name])
	            {
	                idx = gPlotNameMap[obj.name];
	            }
	            else
	            {
	                gPlotInfo._idx++;
	                gPlotNameMap[obj.name] = gPlotInfo._idx;
	                idx = gPlotInfo._idx;
	            }

	            var section = 'IDX'+idx;
	            var chartName = section+'_Chart';
	            if(gPlotInfo[chartName])
	            {
	                var graph = gPlotInfo[chartName];
	                if( obj.time > graph.ymax)
	                {
	                    graph.ymax = obj.time;
	                    graph.line.Set('ymax', graph.ymax);
	                    //graph.line.Set('background.hbars', [[0,obj.time,'#efefef']]);
	                }
	                
	                if(undefined == graph.codes[''+obj.code])
	                {
	                    graph.codes[''+obj.code]= 1;
	                }
	                else
	                {
	                    graph.codes[''+obj.code]++;
	                }

	                graph.data.push(obj.time);
	                var lenstore = graph.data.length;
	                if(graph.maxstore !== -1 && lenstore > graph.maxstore)
	                {
	                    graph.data = graph.data.splice(graph.droprate - 1);
	                }
	                if(!graph.refreshing)
	                {
	                    graph.refreshing = true;
	                    setTimeout(function(){
	                        gPlotInfo[chartName].refreshing = false;
	                        var displayData = null;
	                        var len = gPlotInfo[chartName].data.length;

	                        if(len > gPlotInfo[chartName].maxdisplay)
	                        {
	                            var idx = len - gPlotInfo[chartName].maxdisplay;
	                            displayData = gPlotInfo[chartName].data.slice(idx-1);
	                        }
	                        else
	                        {
	                            displayData = gPlotInfo[chartName].data;
	                        }
	                        RGraph.Clear(gPlotInfo[chartName].canvas);
	                        gPlotInfo[chartName].line.original_data[0] = displayData;
	                        gPlotInfo[chartName].line.Draw();
	                        updateSummary(obj.name, chartName);
	                    }, 200);
	                }
	            }
	            else
	            {
	                $("#MainChart").append('<div class="ChartContainer" id="'+section+'">');
	                $("#"+section).append('<canvas id="'+chartName+'" width="1000" height="250">[No canvas support]</canvas>');
	                
	                var graphr =
	                {
	                    canvas      : document.getElementById(chartName),
	                    cname       : chartName,
	                    line        : null, 
	                    data        : [obj.time, obj.time],
	                    ymax        : obj.time,
	                    refreshing  : false,
	                    maxdisplay  : 100,
	                    maxstore    : 100000,
	                    droprate    : 10000,
	                    properties  : {},
	                    codes       : {}
	                };
	                graphr.codes[''+obj.code] = 1;
	                RGraph.Clear(graphr.canvas);
	                graphr.line = new RGraph.Line(graphr.cname, []).
	                    Set('xticks', 100).
	                    Set('background.barcolor1', 'white').
	                    Set('background.barcolor2', 'white').
	                    Set('title.xaxis', '').
	                    Set('title.yaxis', 'Time (ms)').
	                    Set('title.vpos', 0.5).
	                    Set('title', obj.name).
	                    Set('title.yaxis.pos', 0.5).
	                    Set('title.xaxis.pos', 0.5).
	                    Set('colors', ['black']).
	                    Set('linewidth',1.01).
	                    Set('yaxispos', 'right').
	                    Set('ymax', graphr.ymax).
	                    Set('xticks', 25).
	                    Set('chart.shadow', true).
	                    Set('chart.resizable', true).
	                    Set('chart.contextmenu', [
	                                    ['Clear annotations', function ()
	                                                          {
	                                                            RGraph.ClearAnnotations(graphr.line.canvas);
	                                                            RGraph.Clear(graphr.line.canvas);
	                                                            graphr.line.Draw();
	                                                           }],
	                                    ['Zoom in', RGraph.Zoom]
	                                   ]).
	                    Set('chart.annotatable', true).
	                    Set('filled', true);
	                    
	                var grad = graphr.line.context.createLinearGradient(0,0,0,250);
	                //grad.addColorStop(0, '#efefef');
	                grad.addColorStop(0.0, '#00FF00');
	                grad.addColorStop(0.9, '#003300');
	                //grad.addColorStop(0.9, 'rgba(0,0,0,0)');
	                graphr.line.Set('chart.fillstyle', [grad]);
	                graphr.line.original_data[0] = graphr.data;
	                graphr.line.Draw();
	               
	                // Pull out the temp data for drawing
	                graphr.data = [obj.time];

	                gPlotInfo[chartName] = graphr;

	                $("#"+section).append(getSummary(obj.name, chartName));
	            }
	        }
	    }; 
	    g_websocket.onerror = function(evt)
	    { 
	        // display pop-up saying there was an error
	        console.log("**** Connection Error");
	    };
	}
	else	//This is the STATIC graph
	{
		gPlotInfo = {};
		RequestInfo(url, function(data) {
			if (!$.isEmptyObject(data))
			{
				var idx = 0;
				for(plot in data)
				{
					var section = 'IDX'+idx;
					var chartName = section+'_Chart';
					$("#MainChart").append('<div class=\"ChartContainer\" id="'+section+'">');
					$("#"+section).append('<div id="'+chartName+'">');
					
					gPlotInfo[chartName] = {"plot": null, "data" : [], "properties" : {}};
					
					//DEFAULTPROPERTIES.title = plot;
					
					gPlotInfo[chartName].data = data[plot];
					gPlotInfo[chartName].properties = $.extend(true, setDefaultProperties_Calc(chartName), DEFAULTPROPERTIES);
					LoadPlot(chartName);
					
					$("#"+section).append(getSummary(plot, chartName));
					++idx;
				}
			
				$("#MainChart").append('<div id="Chart_Options">');
				$("#Chart_Options").append('<input type=\"button\" class=\"btn btn-default btn-xs\" value=\"Toggle Data Points\" onclick=\"co_ToggleMarkers()\">');
				$("#Chart_Options").append('<input type=\"button\" class=\"btn btn-default btn-xs\" value=\"Toggle Mean Line\" onclick=\"co_ToggleHorizontalLine(\'mean\')\">');
				$("#Chart_Options").append('<input type=\"button\" class=\"btn btn-default btn-xs\" value=\"Toggle Median Line\" onclick=\"co_ToggleHorizontalLine(\'median\')\">');
				$("#Chart_Options").append('<input type=\"button\" class=\"btn btn-default btn-xs\" value=\"Redraw Chart(s)\" onclick=\"co_RedrawCharts()\">');
			}
			else
			{
				$("#MainChart").append('<div class=\"ChartInfo\"><center><h1>No Data Returned<\/h1><\/center><\/div>');
			}
		});
	}
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
									color: '#c75d5d'
								}},
								{horizontalLine: {
									name: 'median',
									y: median,
									show: false,
									color: '#fd9f28'
									//color: 'rgb(66, 98, 144)'
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
	
	var summaryhtml = '<table class=\"ChartInfo table table-condensed\">';
	// summaryhtml += '<caption>'+ plotName +'<\/caption>';
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