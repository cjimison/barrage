$(document).ready(function() {
    hideNotification();
    $('#notification_message').click(function(){           
        hideNotification(true);
    });

    var container_behaviors = document.getElementById('jsoneditor_behaviors');
    var container_actions = document.getElementById('jsoneditor_actions');

    var options = {
        mode: 'tree',
        modes: ['code', 'tree'], // allowed modes
        error: function (err) {
            alert(err.toString());
        }
    };

    var json_behaviors, json_actions = {};
    RequestInfo("general/behaviors", function(json) {
        if (!$.isEmptyObject(json))
        {
            json_behaviors = json;
        }
    });

    RequestInfo("general/actions", function(json) {
        if (!$.isEmptyObject(json))
        {
            json_actions = json;
        }
    });

    var editor_behaviors = new jsoneditor.JSONEditor(container_behaviors, $.extend(options,{name:'Behaviors'}), json_behaviors);
    var editor_actions = new jsoneditor.JSONEditor(container_actions, $.extend(options,{name:'Actions'}), json_actions);
});


function RequestInfo(url, callback) {
    $.ajax({
        url: url,
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

function hideNotification(animate)
{
    var notification = $('#notification_message');
    var notificationHeight = $('#notification_message').outerHeight();
    if(animate)
    {
        notification.animate({top: -$(this).outerHeight()}, 500);
    }
    else
    {
        notification.css({'top':-notificationHeight});
    }
}

function showNotification(msg, type)
{

    var msgtype = {
        'info' : {'bgcolor' : '#4ea5cd', 'bordercolor' : '#3b8eb5'},
        'warning' : {'bgcolor' : '#eaaf51', 'bordercolor' : '#d99a36'},
        'error' : {'bgcolor' : '#de4343', 'bordercolor' : '#c43d3d'},
        'success' : {'bgcolor' : '#61b832', 'bordercolor' : '#55a12c'}
    }
    var notification = $('#notification_message');
    
    //default type to info
    if (!type || !msgtype[type])
    {
        type = 'info';
    }
    var html = '<h3>'+type.toUpperCase()+'</h3>'
    html += '<p>'+msg+'</p>'

    notification.css({'background-color': msgtype[type].bgcolor,'border-color': msgtype[type].bordercolor})
    notification.html(html);
    notification.animate({top:"0"}, 500);

    setTimeout("hideNotification(true)",3000);
}