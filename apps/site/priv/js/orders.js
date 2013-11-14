var JSONEDITOR_BEHAVIORS;
var JSONEDITOR_ACTIONS;

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
    RequestInfoOnly("general/behaviors", function(json) {
        if (!$.isEmptyObject(json))
        {
            json_behaviors = json;
        }
    });

    RequestInfoOnly("general/actions", function(json) {
        if (!$.isEmptyObject(json))
        {
            json_actions = json;
        }
    });

    JSONEDITOR_BEHAVIORS = new jsoneditor.JSONEditor(container_behaviors, $.extend(options,{name:'Behaviors'}), json_behaviors);
    JSONEDITOR_ACTIONS = new jsoneditor.JSONEditor(container_actions, $.extend(options,{name:'Actions'}), json_actions);
});

function ClickAdjustInputFile(type)
{
    var fileInput = document.getElementById('inputfile');
    fileInput.click();

    var changeaction = function() {
        var file = fileInput.files[0];
        if (file)
        {
            if(file.type.indexOf('json') === -1)
            {
                showNotification('<i>'+file.name+'</i> - Only JSON files can be imported.');
            }
            else
            {
                var reader = new FileReader();
                // If we use onloadend, we need to check the readyState.
                reader.onloadend = function(evt) {
                    if (evt.target.readyState == FileReader.DONE) { // DONE == 2
                        var data = evt.target.result;
                        var url = 'general/upload_'+type.toLowerCase();
                        PostInfo(url, data, function(response) {
                        });
                        //The post is giving an ERROR but it is still saving
                        //Need to fix this so we can actually give a proper success or error notification
                        ReloadJson(type);
                    }
                };
                reader.readAsText(file);
            }
        }
    };

    fileInput.onchange = changeaction;
    fileInput.value = "";       // This clears the file to allow the onchange to execute if the user selects the same file again
}

function ReloadJson(type)
{
    editor = eval('JSONEDITOR_'+type.toUpperCase());
    RequestInfoOnly("general/"+type, function(json) {
        if (!$.isEmptyObject(json))
        {
            editor.set(json);
            if (editor.options.mode === 'tree')
            {
                editor.expandAll();
            }
        }
    });
}

function saveTextAsFile(textToWrite, fileNameToSaveAs)
{
    var textFileAsBlob = new Blob([textToWrite], {type:'text/plain'});

    var downloadLink = document.createElement("a");
    downloadLink.download = fileNameToSaveAs;
    downloadLink.innerHTML = "Download File";

    // Chrome allows the link to be clicked without actually adding it to the DOM.
    downloadLink.href = window.webkitURL.createObjectURL(textFileAsBlob);

    downloadLink.click();
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