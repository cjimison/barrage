(function($) {
  $.fn.stopwatch = function() {
    var stopwatch = $(this);

    stopwatch.each(function() {
      var instance = $(this);
      var timer = 0;

      var stopwatchFace = $('<div>').addClass('the-time');
      var timeHour = $('<span>').addClass('hr').text('00');
      var timeMin = $('<span>').addClass('min').text('00');
      var timeSec = $('<span>').addClass('sec').text('00');
      stopwatchFace = stopwatchFace.append(timeHour).append(timeMin).append(timeSec);
	  
	  var myHr = $('.hr');
	  var myMin = $('.min');
	  var mySec = $('.sec');
	  

	  timer = setInterval(runStopwatch, 1000);

      function runStopwatch() {
        // We need to get the current time value within the widget.
        var hour = parseFloat(timeHour.text());
        var minute = parseFloat(timeMin.text());
        var second = parseFloat(timeSec.text());
	
        second++;

        if(second > 59) {
          second = 0;
          minute = minute + 1;
        }

        if(minute > 59) {
          minute = 0;
          hour = hour + 1;
        }
		
        timeHour.html("0".substring(hour >= 10) + hour);
        timeMin.html("0".substring(minute >= 10) + minute);
        timeSec.html("0".substring(second >= 10) + second);
        
        myHr.text("0".substring(hour >= 10) + hour);
        myMin.text("0".substring(minute >= 10) + minute);
        mySec.text("0".substring(second >= 10) + second);
      }
    });
  }
})(jQuery);