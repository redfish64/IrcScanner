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

    
