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
    <script type="text/javascript">
      $(document).ready(function(){
        LOOKUP.init({}); 
        LOOKUP.updateBoxesForKeyword($("#kw").attr("kw"))
      })
    </script>
  </head>
  <body style='background-color: gray'>
      
      <table id="boxtemplate" hidden>
	<tr class="boxtemplate">
	  <td>
	    <table width="100%">
	      <tr>
		<td valign="top" align="left"><a class="boxExpandUp" href="javascript:void(0)" box="" onclick="LOOKUP.upClicked(); return false"><img src="/files/uparrow.png" /></a></td>
		<td id='date' class='dateheader' width="100%">Loading...</td>
	      </tr>
	      <tr>
		  <td valign="bottom" align="left"><a class="boxExpandDown" href="javascript:void(0)" box="" onclick="LOOKUP.upClicked(); return false"><img src="/files/downarrow.png" /></a></td>
		  <td id='text' width="100%"></td>
	      </tr>
	    </table>
	  </td>
	</tr>
      </table>
    <h2 id="kw" kw="${keywordName}">Keyword <keywordName /></h2>
    <table id="boxtable" width="100%">
      <tr><td>ARR MATEY! </td></tr>
    </table>

  </body>
</html>
