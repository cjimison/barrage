var gHideProgressOverlay = true;

var POSTURLS = ['upload_behaviors', 'upload_actions'];
var CONVERTTO = {'milliseconds' : 1/1000};

var MICRO_TO_MILLI = 1/1000;
var DEFAULTPROPERTIES =
{
    title: "",
    seriesDefaults:
    {
        showMarker: true
    },
    axesDefaults:
    {
        //pad: 0,
        //labelRenderer: $.jqplot.CanvasAxisLabelRenderer,
        //tickRenderer:  $.jqplot.CanvasAxisTickRenderer
    },
    axes:
    {
        xaxis:
        {
            label: 'Requests'
        },
        yaxis:
        {
            label: 'Time (ms)',
            tickOptions:
            {
                formatter: function (format, val)
                { 
                    var number = roundTo(val * CONVERTTO.milliseconds, 1000);
                    return number;
                }
            }
        }
    },
    highlighter:
    {
        show: true,
        tooltipOffset: 5,
        tooltipAxes: "y",
        formatString: "%s ms"
    }
};

$(document).ready(function()
{
    //fartscroll();
    RequestInfo("general/orders", function(orders)
    {
        if (!$.isEmptyObject(orders))
        {
            $("#main").append('<div id="Request_Plot">');
            for (var i = 0; i < orders.length; ++i)
            {
                $("#Request_Plot").append('<input type=\"button\" value=\"'+ 
                                            orders[i] +
                                            '\" onclick=\"rp_IssueOrder(\''+ 
                                            orders[i] +
                                            '\');\" >');
            }
            $("input[type=button]").button(); //Apply jquery-ui for buttons
        }
    });
    

});

function RequestInfo(url, callback)
{
    var loadinghtml = '<div>Requesting Data</div>\
                      <div id="loadTimer">\
                      <span class="hr">00</span>:<span class="min">00</span>:<span class="sec">00</span>\
                      </div>';

    $.ajax(
    {
        url: url,
        async: true,
        beforeSend: function()
        {
            if (!gHideProgressOverlay)
            {
                $.blockUI(
                { 
                    message: loadinghtml
                });
                $('#loadTimer').stopwatch();
            }
        },
        success: function(data)
        {
            callback(JSON.parse(data));
            $.unblockUI();
        },
        error: function (xhr, ajaxOptions, thrownError)
        {
            console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
        }
    });
}
var g_websocket = null;
var gPlotInfo = null;
var gPlotNameMap = null;

function rp_IssueOrder(name)
{

    gHideProgressOverlay = false;
    var wsHost ="ws://"+
                window.location.hostname+":"+
                window.location.port+
                "/general/streaming";
    
    // Remove any custom graphoptions
    $('#Chart_Options').remove();
    // Clear the old graphs out
    for (var oldName in gPlotInfo)
    {
        var arr = oldName.split('_');
        $('#'+arr[0]).remove();
    }
    
    gPlotInfo = {_idx : -1};
    gPlotNameMap = {};

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
            obj.time *= MICRO_TO_MILLI;
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
                $("#main").append('<div id="'+section+'">');
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

    /*
    gPlotInfo = {};
    RequestInfo(url, function(data)
    {
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
                gPlotInfo[chartName].properties = $.extend(true, 
                                                            setDefaultProperties_Calc(chartName),
                                                            DEFAULTPROPERTIES);
                LoadPlot(chartName);
                
                $("#"+section).append(getSummary(plot, chartName));
                ++idx;
            }
    
            $("#main").append('<div id="Chart_Options">');
            $("#Chart_Options").append(
                    '<input type=\"button\"\
                            value=\"Toggle Data Points\"\
                            onclick=\"co_ToggleMarkers()\">');
            $("#Chart_Options").append(
                    '<input type=\"button\"\
                            value=\"Toggle Mean Line\"\
                            onclick=\"co_ToggleHorizontalLine(\'mean\')\">');
            $("#Chart_Options").append(
                    '<input type=\"button\"\
                            value=\"Toggle Median Line\"\
                            onclick=\"co_ToggleHorizontalLine(\'median\')\">');
            $("#Chart_Options").append(
                    '<input type=\"button\"\
                            value=\"Redraw Chart(s)\"\
                            onclick=\"co_RedrawCharts()\">');
            //$("#Chart_Options").append('<input type=\"button\" value=\"Save Chart(s)\" onclick=\"co_SaveCharts()\">');
            $("input[type=button]").button();               //Apply jquery-ui for buttons
        }
        else
        {
            $("#main").append('<div class=\"ChartInfo\"><center><h1>No Data Returned<\/h1><\/center><\/div>');
        }
    });
    gHideProgressOverlay = true;
    */
}

function LoadPlot(chartName)
{
    var startTime = new Date();
    var data = scaleData(gPlotInfo[chartName].data, 250);
    gPlotInfo[chartName].plot = $.jqplot(chartName, [data], gPlotInfo[chartName].properties);
    console.log('jqPlot took ' + ((new Date()) - startTime) + ' ms');
}

function scaleData(data, interval)
{
    var scaledData = [];
    var dlength = data.length;
    if (dlength > interval)
    {
        for (var i = 0; i < dlength - 1 ; i+=interval)
        {
                scaledData.push([i, data[i]]);
        }
        scaledData.push([dlength-1, data[dlength-1]]);
    }
    else
    {
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
    prop.canvasOverlay =
    {
        show: true,
        objects:
        [
            {
                horizontalLine:
                {
                    name: 'mean',
                    y: mean,
                    show: false
                }
            },
            {
                horizontalLine:
                {
                    name: 'median',
                    y: median,
                    show: false,
                    color: 'rgb(66, 98, 144)'
                }
            }
        ]
    };
    
    if (data.length == 1)
    {
        prop.axes =
        {
            xaxis:
            {
                min: 0,
                max: 2,
                tickInterval: 1
            }
        };
    }
    
    return prop;
}

function calcMean(array)
{
    var sum = 0;
    var arrLength = array.length;
    for (var i = 0; i < arrLength; ++i)
    {
        sum += array[i];
    }

    return sum/arrLength;
}

function calcMedian(array)
{
    //Clones the array so we don't modify the ordering of the existing array
    var data = array.slice(0); 
    var dataLength = data.length;
    var middle = Math.floor(dataLength/2);
    
    data.sort( function(a,b) {return a - b;} );
 
    if(dataLength % 2)
    {
        //ODD so return the middle of the sorted array
        return data[middle];
    }
    else
    {
        //EVEN so return the average of the two middle numbers of the sorted array
        return (data[middle-1] + data[middle]) / 2;
    }
}

function roundTo(number, to)
{
    return Math.round(number * to) / to;
}

function getSummary(plotName, chartName)
{
    var graph = gPlotInfo[chartName];
    var data = graph.data;
    var numOfRequests = data.length;
    var max = roundTo(Math.max.apply(null, data) * CONVERTTO.milliseconds, 1000) + ' ms';
    var min = roundTo(Math.min.apply(null, data) * CONVERTTO.milliseconds, 1000) + ' ms';
    var mean = roundTo(calcMean(data) * CONVERTTO.milliseconds, 1000) + ' ms';
    var median = roundTo(calcMedian(data) * CONVERTTO.milliseconds, 1000) + ' ms';
    
    var summaryhtml = '<table class=\"ChartInfo\">';
    summaryhtml += '<caption>'+ plotName +'<\/caption>';
    summaryhtml += '<thead><tr id="lable_row_'+chartName+'">';
    summaryhtml +=  '<th>Total Requests<\/th>';
    summaryhtml +=  '<th>High<\/th>';
    summaryhtml +=  '<th>Low<\/th>';
    summaryhtml +=  '<th>Mean<\/th>';
    summaryhtml +=  '<th>Median<\/th>';
    for(var code in graph.codes)
    {
        summaryhtml +=  '<th>HTTP Code: '+code+'<\/th>';
    }
    summaryhtml += '<\/tr><\/thead>';
    summaryhtml += '<tbody><tr id="info_row_'+chartName+'">';
    summaryhtml +=  '<td id="numOfRequest_'+chartName+'">'+ numOfRequests +'<\/td>';
    summaryhtml +=  '<td id="max_'+chartName+'">'+ max +'<\/td>';
    summaryhtml +=  '<td id="min_'+chartName+'">'+ min +'<\/td>';
    summaryhtml +=  '<td id="mean_'+chartName+'">'+ mean +'<\/td>';
    summaryhtml +=  '<td id="median_'+chartName+'">'+ median +'<\/td>';
    for(var code2 in graph.codes)
    {
        summaryhtml +=  '<td id="code_value_'+chartName+'_'+code2+'">'+graph.codes[code2]+'<\/td>';
    }
    summaryhtml += '<\/tr><\/tbody>';
    summaryhtml += '<\/table>';
   

    return summaryhtml;
}

function updateSummary(plotName, chartName)
{
    var graph = gPlotInfo[chartName];
    var data = graph.data;
    var numOfRequests = data.length;
    var max = roundTo(Math.max.apply(null, data), 1000) + ' ms';
    var min = roundTo(Math.min.apply(null, data), 1000) + ' ms';
    var mean = roundTo(calcMean(data), 1000) + ' ms';
    var median = roundTo(calcMedian(data), 1000) + ' ms';

    $("#numOfRequest_"+chartName).text("" + numOfRequests);
    $("#max_"+chartName).text("" + max);
    $("#min_"+chartName).text("" + min);
    $("#mean_"+chartName).text("" + mean);
    $("#median_"+chartName).text("" + median);

    for(var code in graph.codes)
    {
        var jqObj = $("#code_value_"+chartName+"_"+code);
        if(window.document.getElementById("code_value_"+chartName+"_"+code))
        {
            jqObj.text(""+graph.codes[code]);
        }
        else
        {
            $("#lable_row_"+chartName).append('<th>HTTP Code: '+code+'<\/th>');
            $("#info_row_"+chartName).append('<td id="code_value_'+chartName+'_'+code+'">'+graph.codes[code]+'</td>');
        }
    }
}

function co_ToggleMarkers()
{
    for (chartName in gPlotInfo)
    {
        gPlotInfo[chartName].properties.seriesDefaults.showMarker = 
            !gPlotInfo[chartName].plot.series[0].showMarker;
        gPlotInfo[chartName].plot.destroy();
        LoadPlot(chartName);
    }
}

function co_ToggleHorizontalLine(linename)
{
    for (chartName in gPlotInfo)
    {
        var co =  gPlotInfo[chartName].plot.plugins.canvasOverlay;
        var line = co.get(linename);
        line.options.show = !line.options.show;
        co.draw(gPlotInfo[chartName].plot);
    }
}

function co_RedrawCharts()
{
    for (chartName in gPlotInfo)
    {
        gPlotInfo[chartName].properties = $.extend(true, setDefaultProperties_Calc(chartName), DEFAULTPROPERTIES);
        gPlotInfo[chartName].plot.destroy();
        LoadPlot(chartName);
    }
}

function co_SaveCharts()
{
    for (chartName in gPlotInfo)
    {
        console.log("Saving " + chartName);
    }
}

function pd_showUploadButtons(hide)
{
    $('#Upload_Buttons').remove();
    if (!hide)
    {
        $("#Post_Data").append('<div id="Upload_Buttons">');
        $("#Upload_Buttons").append('<input type=\"button\"\
                                            value=\"Upload Behaviors\"\
                                            onclick=\"pd_PostTo(\'general/upload_behaviors\')\" >');
        $("#Upload_Buttons").append('<input type=\"button\"\
                                            value=\"Upload Actions\"\
                                            onclick=\"pd_PostTo(\'general/upload_actions\')\" >');
        //Apply jquery-ui for buttons
        $("input[type=button]").button(); 
    }
}

function pd_PostTo(url)
{
    var files = $('#fileChoser')[0].files;
    if (!files.length)
    {
        console.log("No file selected");
    }
    else
    {
        var file = files[0];
        var reader = new FileReader();
        // If we use onloadend, we need to check the readyState.
        reader.onloadend = function(evt)
        {
            if (evt.target.readyState == FileReader.DONE)
            { // DONE == 2
                var data = evt.target.result;
                PostInfo(url, data, function(response)
                {
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

function PostInfo(url, data, callback)
{
    $.ajax({
        url: url,
        type: 'POST',
        data: data,
        async: true,
        success: function(data)
        {
            callback(JSON.parse(data));
        },
        error: function (xhr, ajaxOptions, thrownError)
        {
            console.log('ERROR ' + xhr.status + ' - ' + xhr.responseText + ' - ' + thrownError);
        }
    });
}
