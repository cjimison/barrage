$('input:radio[name=liveChart]').change(function() {
	if ( $('input:radio[name=liveChart]:checked').val() === "true" ) {
		$('#chartFrame').prop( 'src', "../index2.html" );
	}
	else {
		$('#chartFrame').prop( 'src', "../index.html" );
	}
});
