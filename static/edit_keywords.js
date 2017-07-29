function testResults()
{
    $.post( "/edit_keywords_test", {rulesFileContents: $('#rulesFileContents').val()}, processTestResults );
}

function processTestResults(result)
{
    $('#results').html(result)
}

    
function saveResults()
{
    $.post( "/edit_keywords_save", {rulesFileContents: $('#rulesFileContents').val()}, processTestResults );
}

function procesSaveResults(result)
{
    $('#results').html(result)
}



var oldVal = "";
$( document ).ready(function() {
    $("#rulesFileContents").on("change keyup paste", function() {
	var currentVal = $(this).val();
	if(currentVal == oldVal) {
            return; //check to prevent multiple simultaneous triggers
	}

	oldVal = currentVal;
	
	$('#results').html("")
    });
});
			      
