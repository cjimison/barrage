$(document).ready(function() {
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