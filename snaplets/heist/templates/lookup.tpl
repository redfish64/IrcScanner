<!DOCTYPE html>
<html>
  <head>
    <title>AutoNomic Irc Index</title>
    <link rel="stylesheet" href="/files/styles.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="/files/util.js"></script>
    <script src="/files/lookup.js"></script>
    <script type="text/javascript">
      $(document).ready(function(){
        LOOKUP.init(); 
        LOOKUP.updateBoxesForKeyword($("#kw").attr("kw"))
      })
    </script>
  </head>
  <body>
      
      <table id="boxtemplate" hidden>
	<tr class="boxtemplate">
	  <td>
	    <table>
	      <tr>
		<td id='date'>Loading...</td>
		<td valign="top" align="right" class="boxExpandUp" box="">&and;</td>
	      </tr>
	      <tr>
		  <td id='text' width="100%"></td>
		  <td valign="bottom" align="right" class="boxExpandDown" box="">&or;</td>
	      </tr>
	    </table>
	  </td>
	</tr>
      </table>
    <h2 id="kw" kw="${keywordName}">Keyword <keywordName /></h2>
    <table id="boxtable">
      <tr><td>ARR MATEY! </td></tr>
    </table>

  </body>
</html>
