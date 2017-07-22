
var LOOKUP = LOOKUP || (function(){
    var _boxes = [];
    var _add_rows_for_box = 1; //rows above and below box
    var _rows = [];
    
    return {
        init : function(init_box_size) {
	    if(init_box_size) _add_rows_for_box = Math.trunc(init_box_size / 2);
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
		   lastBox.erow <= srow)
		{
		    lastBox.erow = erow;
		}
		else {
		    lastBox = {
			id: currId++,
			srow: srow,
			erow: erow,
			rowsOffset: 0,
			rows: []
		    }
		    _boxes.push(lastBox);
		}
	    });
				
	},

	_writeBoxesInPage : function() {

	    var d = $("#boxtable")
	    d.empty()

	    _boxes.forEach(function(box) {
		d.append('<tr><td><div id="box" box="'+box.id+'">'
			 +"<table><tr>"
			 +"<td>Id "+box.id+"</td>"
			 +"<td id='date'></td></tr>"
			 +"<tr><td id='text'></td></tr>"
			 +"</table>"
			 +"</div></td></tr>");
	    });
			   
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
	    //start the update box loop
	    _boxes.forEach(function(box) {
		LOOKUP._addRowRange(box.srow, box.erow);
	    });

	    //refresh boxes in display
	    LOOKUP._refreshDisplayedBoxes()
	},
	//refreshes contents of dirty boxes in display
	_refreshDisplayedBoxes : function()
	{
	    var elems = getElementsInView($('#box'));
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
	_refreshIfDirty : function(boxId)
	{
	    var boxIndex = binarySearch(_boxes, function(b) { return b.id >= boxId} );
	    var box = _boxes[boxIndex];

	    if(box.rowsOffset != 0)
	    {
		LOOKUP._loadRows(box.srow, box.rowsOffset);
		return true;
	    }

	    if(box.rows.length < box.erow - box.srow)
	    {
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

	    var date = $xml.find("Date").text();
	    
	    var rows = $xml.find("Row").map(function() {
		return { id: $(this).attr("id"),
			 text: $(this).attr("text") };
	    }).toArray();
	    
	    var fr = rows[0];

	    //get the box for the range of rows
	    var box = binarySearchFind(_boxes,function(box)
				       { return box.srow <= fr.id })
	    if(!box)
	    {
		alert ("wheres the box? "+fr.id);
	    }
	    
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
	},
	_redrawBox : function(box)
	{
	    var b = $("div[box="+box.id+"]");
	    var t = $(b).find("#text");
	    box.rows.forEach(function(r) {
		t.append("<br>"+r.text);
	    });
	},
	_addRowRange : function (start,end)
	{
	    //find the rowIndex of the first row we already which
	    //is in the range we want to load
	    var rowIndex = binarySearch(_rows,
				     function(r)
				     {
					 return r.id >= start;
				     });

	    //fill in any gaps in the rows we already have loaded
	    var gapStart = start;
	    while(rowIndex < _rows.length)
	    {
		var rowId = _rows[rowIndex].id;
		if(rowId < end)
		{
		    if(gapStart != rowId)
			_addRowRange2(gapStart,rowId);
		    gapStart = rowId + 1;
		}
	    }
	},
	_addRowRange2 : function (start,end) //add a row range, with the expectation
	//that it has already been checked against the rows we already have
	{
	    //we still need to verify that we aren't overlapping ou
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

