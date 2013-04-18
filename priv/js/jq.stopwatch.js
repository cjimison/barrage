(function($) {
  $.fn.stopwatch = function() {
    var stopwatch = $(this);
	
    stopwatch.each(function() {
      var instance = $(this);
      var timer = 0;
	  
	  var myHr = $('.hr');
	  var myMin = $('.min');
	  var mySec = $('.sec');
	  
	  timer = setInterval(runStopwatch, 1000);

      function runStopwatch() {
        // We need to get the current time value within the widget.
        var hour = parseFloat(myHr.text());
        var minute = parseFloat(myMin.text());
        var second = parseFloat(mySec.text());
	
        second++;
		
        if(second > 59) {
          second = 0;
          minute = minute + 1;
        }

        if(minute > 59) {
          minute = 0;
          hour = hour + 1;
        }
        
        myHr.text("0".substring(hour >= 10) + hour);
        myMin.text("0".substring(minute >= 10) + minute);
        mySec.text("0".substring(second >= 10) + second);
      }
    });
  }
})(jQuery);