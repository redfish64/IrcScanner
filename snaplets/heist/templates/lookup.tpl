<!DOCTYPE html>
<html>
  <head>
    <title>AutoNomic Irc Index</title>
    <link rel="stylesheet" href="/files/styles.css">
    <!-- <script src="https://code.jquery.com/jquery-1.12.4.js"></script> -->
    <!-- <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script> -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="/files/util.js"></script>
    <script src="/files/lookup.js"></script>
    <keywordMode>
      <script type="text/javascript">
	$(document).ready(function(){
        LOOKUP.init({fileNickName=$("#fnn").attr("fnn")}); 
        LOOKUP.updateBoxesForKeyword($("#kw").attr("kw"))
	})
      </script>
    </keywordMode>
    <logFollowMode>
      <script type="text/javascript">
	$(document).ready(function(){
        LOOKUP.init({fileNickName=$("#fnn").attr("fnn")}); 
        LOOKUP.createBoxForLogFollow()
	})
      </script>
    </logFollowMode>
  </head>
  <body style='background-color: gray;margin: 0%'>
    <ul class='lookup_header' style='background-color: #FFFFFF'>
      <keywordMode>
	<li class='lookup_header'><a>Keyword: <b><keyword /></b> </a> </li>
      </keywordMode>
      <logFollowMode>
	<li class='lookup_header'><a>Following #autonomic</a> </li>
      </logFollowMode>
      <li class='lookup_header'><a style='color: blue' href="javascript:void(0)" onclick="window.scrollTo(0,0)">Go to Top</a></li>
      <li class='lookup_header'><a style='color: blue' href="javascript:void(0)" onclick="window.scrollTo(0,document.body.scrollHeight)">Go to Bottom</a></li>
    </ul>

    <div style="height:48px" />
    
    <!--used by javascript to get keyword (unfortunately, snap can't substitute within
        javascript blocks, so we need to pass it-->
    <div id="kw" kw="${keyword}" />
    <div id="fnn" fnn="${fileNickName}" />
    
    <table id="boxtable" width="100%">
      <tr><td>ARR MATEY! </td></tr>
    </table>

    <!--template for box rows -->
    <table id="boxtemplate" hidden>
      <tr class="boxtemplate">
	<td>
	  <table width="100%">
	    <tr>
	      <td valign="top" align="left"><a class="boxExpandUp" href="javascript:void(0)" box=""><img src="/files/uparrow.png" /></a></td>
		<td id='date' class='dateheader' width="100%">Loading...</td>
	    </tr>
	    <tr>
	      <td valign="bottom" align="left"><a class="boxExpandDown" href="javascript:void(0)" box=""><img src="/files/downarrow.png" /></a></td>
	      <td id='text' width="100%"></td>
	    </tr>
	  </table>
	</td>
      </tr>
    </table>
  </body>
</html>
