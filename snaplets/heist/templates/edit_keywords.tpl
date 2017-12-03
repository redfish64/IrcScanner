<!DOCTYPE html>
<html>
  <head>
    <title>#nomiccoin Irc Log - Edit Keywords</title>
    <link rel="stylesheet" href="/files/styles.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="/files/util.js"></script>
    <script src="/files/edit_keywords.js"></script>
  </head>
  <body>
    <table>
      <tr>
	<td>
	  <h2>Edit Keywords</h2>
	</td>
      </tr>
      <tr>
	<td>
	  <textarea id="rulesFileContents" rows="15" cols="70"><rulesFileContents /></textarea> 
	</td>
	<td>
	  Format is: <br>
	  <span class="codeblock">(display name):(match type):(pattern)</span>
	  where:
	  <ul>
	    <li>(display name) - Name in index page</li>
	    <li>(match type) - One of:
	      <ul>
		<li>RegexMatcher - PCRE Regex where "i" flag for ignore case is supported PCRE Regex. Ex. <i>/foo|bar/i</i> See also <a href="https://www.debuggex.com/cheatsheet/regex/pcre">Cheatsheet</a></li>
		<li>ExactMatcher - Matches text exactly</li>
		<li>IgnoreCaseMatcher - Same as ExactMatcher except ignores upper/lower case</li>
	      </ul>
	    </li>
	    <li>(pattern) - Pattern corresponding to match type</li>
	  </ul>
	</td>
      </tr>
      <tr>
	<td>
	  <button type="button" onclick="testResults()">Test</button>
	  <button type="button" onclick="saveResults()">Save</button>
	</td>
      </tr>
      <tr>
	<td>
	  <div id="results" />
	</td>
      </tr>
    </table>
  </body>
</html>
