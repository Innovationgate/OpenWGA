function getCookieData(name) {
var nameLen = name.length
var cLen = document.cookie.length
var i = 0
var cEnd
while (i < cLen) {
var j = i + nameLen
var k = j + 1
if (document.cookie.substring(i,j) == name) {
cEnd = document.cookie.indexOf(";",j)
if (cEnd == -1) {
cEnd = document.cookie.length
}
return unescape(document.cookie.substring(k,cEnd))
}
i++
}
return ""
}
// This function adds a cookie to the client's browser
function setCookie() {
var exp = new Date();
var oneYearFromNow = exp.getTime() + (365 * 24 * 60 * 60 * 1000);
exp.setTime(oneYearFromNow);
document.cookie = "Voting_ID=" + escape(document.WebForm.Voting_ID.value) + ";" + "path=/" + ";" + "expires=" + exp.toGMTString();
}