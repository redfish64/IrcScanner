
var LOOKUP = LOOKUP || (function(){
    var _boxes = [];
    var _add_rows_for_box = 1; //rows above and below box
    
    return {
        init : function(init_box_size) {
	    if(init_box_size) _add_rows_for_box = Math.trunc(init_box_size / 2);

	    $( window ).scroll(LOOKUP._refreshDisplayedBoxes)
        },
	//joins the result into separate boxes, if the lines are sequential
	join_result : function(result) {
	    $xml.each("range")
	    var $ranges = xml.find("range");
	    var r2 = []
	    var li = -1;
	    var text = "";
	    for(i = 0; i < result.length; i++)
	    {
		var e = result[i];
		
		if(i -1 == li) // if sequential
		{
		    text += e+"<br>";
		}
		else
		{
		    var e2 = { text: text};
		    r2[r2.length] = e2;
		}
	    }
	},
	_setupBoxesForRanges : function() {
	    _boxes = [];
	    var lastBox;
	    var currId = 0;

	    _ranges.forEach(function(range) {
		var srow = Math.max(range.row - _add_rows_for_box,0);
		var erow = range.row + _add_rows_for_box + 1;
		if(srow < 0) srow = 0;

		if(lastBox &&
		   lastBox.erow >= srow)
		{
		    lastBox.erow = erow;
		}
		else {
		    lastBox = {
			id: currId++,
			srow: srow,
			erow: erow,
			rowsOffset: 0,
			rows: [],
			loading: false
		    }
		    _boxes.push(lastBox);
		}
	    });
				
	},
	//creates html for boxes in page after range is found (but before lines are loaded)
	_writeBoxesInPage : function() {
	    var $t = $("#boxtemplate");
	    var $tr = $t.find(".boxtemplate")

	    //temporarily set the class to 'boxrow' so that we can make the rows
	    $tr.attr("class","boxrow")

	    var $d = $("#boxtable")
	    $d.empty();

	    _boxes.forEach(function(box) {
		$tr.attr("box",box.id);
		$tr.find(".boxExpandUp").attr("box",box.id);
		$tr.find(".boxExpandDown").attr("box",box.id);
		$d.append("<tr class='boxrow' box="+box.id+">"+$tr.html()+"</tr>");
	    });

	    //after we're done making the rows, change the class back to boxtemplate
	    //so we don't interfere with updating the boxes
	    $tr.attr("class","boxtemplate");

	    //setup touching and clicking
	    $(".boxExpandUp").click(function(){
		LOOKUP._expandBox(parseInt($(this).attr("box")),-expand_box_amt);
	    });
	    $(".boxExpandDown").click(function(){
		LOOKUP._expandBox(parseInt($(this).attr("box")),expand_box_amt);
	    });
	},
	_expandBox : function(box_id)
	{
	},
	    
	
	_resetBoxesForRanges : function(result) {
	    //first get the ranges
	    _ranges = [];

	    var $xml = $(result)
	    $xml.find("range").each(
		function()
		{
		    _ranges.push(
			{
			    row: parseInt($(this).attr("srow")),
			    scol: parseInt($(this).attr("scol")),
			    ecol: parseInt($(this).attr("ecol"))
			});
		}
	    );

	    //now update the boxes
	    LOOKUP._setupBoxesForRanges();

	    //write boxes into html
	    LOOKUP._writeBoxesInPage();

	    //refresh boxes in display
	    LOOKUP._refreshDisplayedBoxes()
	},
	//refreshes contents of dirty boxes in display
	_refreshDisplayedBoxes : function()
	{
	    var elems = getElementsInView($('.boxrow'));
	    for(i = 0; i < elems.length; i++)
	    {
		var e = elems[i];
		if(LOOKUP._refreshIfDirty(parseInt(e.getAttribute('box'))))
		{
		    //if we found a dirty box, we refresh it and quit.
		    //when the results come in, this method will be called again
		    return;
		}
	    }
	},
	//refreshes the given box id. If box needs to be updated, this will _loadRows which
	//will recall _refreshDisplayedBoxes to check for more
	_refreshIfDirty : function(boxId)
	{
	    var boxIndex = binarySearch(_boxes, function(b) { return b.id >= boxId} );
	    var box = _boxes[boxIndex];

	    //if we are already trying to load up the box
	    if(box.loading) { return false;}

	    if(box.rowsOffset != 0)
	    {
		box.loading = true;
		LOOKUP._loadRows(box.srow, box.rowsOffset);
		return true;
	    }

	    if(box.rows.length < box.erow - box.srow)
	    {
		box.loading = true;
		LOOKUP._loadRows(box.srow + box.rows.length, box.erow - box.srow + box.rows.length);
		return true;
	    }

	    return false;
	},
	_loadRows : function (srow, count)
	{
	    $.get( "/loadRows?srow="+srow+"&count="+count, LOOKUP._processRows );
	},
	_processRows : function(result)
	{
	    var $xml = $(result)

	    var rows = $xml.find("Row").map(function() {
		return { id: parseInt($(this).attr("id")),
			 text: $(this).attr("text") };
	    }).toArray();
	    
	    var fr = rows[0];

	    //get the box for the range of rows
	    var box = binarySearchFind(_boxes,function(box)
				       { return box.srow >= fr.id })
	    if(!box)
	    {
		alert ("wheres the box? "+fr.id);
	    }
	    
	    box.date = $xml.find("date").text();
	    
	    if(box.rowsOffset != 0 && fr.id < box.srow + box.rowsOffset)
	    {
		var newRowOffset = fr.id;
		box.rows.splice(0,0,rows);
	    }
	    else if(box.rows.length < box.erow - box.srow &&
		    fr.id == box.srow + box.rows.length)
	    {
		box.rows.splice(box.rows.length,0,...rows)
	    }

	    LOOKUP._redrawBox(box);
	    box.loading = false;

	    //loop back and try to refresh another box
	    LOOKUP._refreshDisplayedBoxes();
	},
	_redrawBox : function(box)
	{
	    var b = $("tr[box="+box.id+"]");
	    b.hide();
	    $(b).find("#date").empty();
	    $(b).find("#date").append(box.date);
	    var t = $(b).find("#text");
	    box.rows.forEach(function(r) {
		t.append(r.text+"<br>");
	    });
	    b.fadeIn();
	},

	updateBoxesForKeyword : function(keyword)
	{
	    $.get( "/retrieve?kw="+keyword, LOOKUP._resetBoxesForRanges );
	    //$.getJSON("/retrieve?kw="+_keyword, );
            var d = $("div")
	    //d.clear()
	    d.append("<br>ARR MATEY!!!fdsfasd")
	}
    }
}());

