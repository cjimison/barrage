	$('input:radio[name=liveChart]').change(function() {
		console.log("radio changed");
	if ( $('input:radio[name=liveChart]:checked').val() === "true" ) {
		$('#chartFrame').prop( 'src', "../index2.html" );
	}
	else {
		$('#chartFrame').prop( 'src', "../index.html" );
	}
});


// $(document).ready(function() {
// 	RequestInfo("../general/orders", function(orders) {
// 		if (!$.isEmptyObject(orders))
// 		{
// 			for (var i = 0; i < orders.length; ++i)
// 			{
// 				$("#Request_Plot").append('<li><a href=\"#\" onclick=\"rp_IssueOrder(\''+ orders[i] +'\');\">'+ orders[i] +'</a></li>');
// 			}
// 		}
// 	});
// });



