
var LOOKUP = LOOKUP || (function(){
    var _boxes = [];
    var _add_rows_for_box = 1; //rows above and below box
    var _expand_box_amt = 10; //rows to add when expanding a box
    
    return {
        init : function(args) {
	    
	    if(args.init_box_size) _add_rows_for_box = Math.trunc(init_box_size / 2);
	    if(args.expand_box_amt) _expand_box_amt = args.expand_box_amt;
	    
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
		LOOKUP._expandBox(parseInt($(this).attr("box")),-_expand_box_amt);
	    });
	    $(".boxExpandDown").click(function(){
		LOOKUP._expandBox(parseInt($(this).attr("box")),_expand_box_amt);
	    });
	},
	_expandBox : function(boxIndex, amt)
	{
	    var box = _boxes[boxIndex];

	    //if we are expanding the top
	    if(amt < 0)
	    {
		var newSrow = Math.max(0,box.srow+amt);

		//if we run into the previous box, we stop there
		if(boxIndex >= 1)
		{
		    var lastBox = _boxes[boxIndex-1];
		    
		    newSrow = Math.max(lastBox.erow, newSrow);
		}

		//update rows offset for the adjusted start position
		box.rowsOffset = box.srow - newSrow;
		box.srow = newSrow;
	    }
	    else
	    {
		box.erow += amt;

		//if we run into the next box, we stop there
		if(boxIndex < _boxes.length-1)
		{
		    var nextBox = _boxes[boxIndex+1];
		    
		    box.erow = Math.min(nextBox.srow, box.erow);
		}
	    }

	    LOOKUP._refreshIfDirty(boxIndex);
	}, //TODO 2 dates are wrong, see box 2 and 3 of "cool" keyword
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

	    // //refresh boxes in display
	    // LOOKUP._refreshDisplayedBoxes()

	    //we refresh all the boxes, since otherwise, scrolling to the bottom doesn't work right, and neither does scrolling upwards after going to the bottom
	    for(i = 0; i < _boxes.length; i++)
	    {
		LOOKUP._refreshIfDirty(i);
	    }
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
	_refreshIfDirty : function(boxIndex)
	{
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
		LOOKUP._loadRows(box.srow + box.rows.length, box.erow - box.srow - box.rows.length);
		return true;
	    }

	    return false;
	},
	_loadRows : function (srow, count)
	{
	    $.get( "/loadRows?srow="+srow+"&count="+count, LOOKUP._processRows );
	    console.log("loading "+srow+" "+count);
	},
	_processRows : function(result)
	{
	    console.log("processing  "+result);
	    var $xml = $(result)

	    var rows = $xml.find("Row").map(function() {
		return { id: parseInt($(this).attr("id")),
			 text: $(this).attr("text") };
	    }).toArray();
	    
	    var fr = rows[0];

	    //get the box for the range of rows
	    var box = binarySearchFind(_boxes,function(box)
				       { return box.erow > fr.id })
	    if(!box)
	    {
		alert ("wheres the box? "+fr.id);
	    }
	    
	    box.date = $xml.find("date").text();

	    var newRowsStart;
	    
	    if(box.rowsOffset != 0 && fr.id < box.srow + box.rowsOffset)
	    {
		newRowsStart = 0;

		box.rowsOffset = fr.id - box.srow;
		box.rows.splice(0,0,...rows);
	    }
	    else if(box.rows.length < box.erow - box.srow &&
		    fr.id == box.srow + box.rows.length)
	    {	
		newRowsStart = box.rows.length;

		box.rows.splice(box.rows.length,0,...rows)
	    }

	    box.contAbove = LOOKUP._continuesAbove(box);
	    box.contBelow = LOOKUP._continuesBelow(box);

	    LOOKUP._redrawBox(box,newRowsStart, newRowsStart + rows.length);

	    if(box.contAbove)
	    {
		//if we continue above, we want to add text to the previous box to that effect
		var pBox = LOOKUP._prevBox(box);

		if(!pBox.contBelow)
		{
		    pBox.contBelow = true;
		    LOOKUP._redrawBox(pBox,-1,-1);
		}
	    }
	    if(box.contBelow)
	    {
		//if we continue above, we want to add text to the previous box to that effect
		var nBox = LOOKUP._nextBox(box);

		if(!nBox.contAbove)
		{
		    nBox.contAbove = true;
		    LOOKUP._redrawBox(nBox,-1,-1);
		}
	    }
	    
	    box.loading = false;

	    //loop back and try to refresh another box
	    LOOKUP._refreshDisplayedBoxes();
	},
	_prevBox : function(box)
	{
	    if(box.id == 0)
	    {
		return false;
	    }
	    return _boxes[box.id-1];
	},
	_nextBox : function(box)
	{
	    if(box.id == _boxes.length-1)
	    {
		return false;
	    }
	    return _boxes[box.id+1];
	},
	_continuesAbove : function(box)
	{
	    if(box.id == 0)
	    {
		return false;
	    }
	    return _boxes[box.id-1].erow == box.srow;
	},
	_continuesBelow : function(box)
	{
	    if(box.id == _boxes.length-1)
	    {
		return false;
	    }
	    return _boxes[box.id+1].srow == box.erow;
	},
	_redrawBox : function(box, newRowsStart, newRowsEnd)
	{
	    var b = $("tr[box="+box.id+"]");
	    $(b).find("#date").empty();
	    $(b).find("#date").append(box.date);
	    var t = $(b).find("#text");
	    t.empty();

	    var displayDiv = newRowsStart != 0 || newRowsEnd != box.rows.length;

	    var topMessage="";
	    var bottomMessage="";

	    if(box.srow == 0)
	    {
		topMessage = "<i>(Beginning of log)</i><br>";
	    }
	    //TODO 2 handle end of log
	    if(box.contAbove)
	    {
		$(b).find('.boxExpandUp').css('visibility','hidden');
		topMessage = "<i>(Continues above)</i><br><br>";
	    }
	    if(box.contBelow)
	    {
		$(b).find('.boxExpandDown').css('visibility','hidden');
		bottomMessage = "<br><i>(Continues below)</i>";
	    }

	    //note that we can only use one append() call, or colors don't work right for
	    //some reason (at least in chrome)
	    var text = topMessage;
	    var i;

	    for(i = 0; i < box.rows.length; i++)
	    {
		var r = box.rows[i];
		
		if(i == newRowsStart && displayDiv)
		{
		    //we put any new text in a div so we can highlight it when it loads in
		    text += "<div class='divtext' style='opacity:0'>";
		}
		else if(i == newRowsEnd && displayDiv)
		{
		    text += "</div>";
		}
	    	text += r.text + "<br>";
	    }

	    if(i == newRowsEnd && displayDiv)
	    {
		text += "</div>";
	    }
	    
	    t.append(text+bottomMessage);

	    //fade in the new text
	    if(displayDiv) t.find(".divtext").animate({opacity: 1},500);

	},

	updateBoxesForKeyword : function(keyword)
	{
	    $.get( "/retrieve?kw="+keyword, LOOKUP._resetBoxesForRanges );
	    //$.getJSON("/retrieve?kw="+_keyword, );
	    //d.clear()
	}
    }
}());

