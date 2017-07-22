<!DOCTYPE html>
<html>
  <head>
    <title>AutoNomic Irc Index</title>
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
    <h2 id="kw" kw="${keywordName}">Keyword <keywordName /></h2>
    <table id="boxtable">
      <tr><td>ARR MATEY! </td></tr>
      </table>
  </body>
</html>
