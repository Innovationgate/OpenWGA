function go(x,y) {
open(x,y);
}

// ------------------------ ruft das Impressum auf --------------------------------------------
function Impressum (url) {
var imprint = window.open (url,"Impressum","height=450,width=438,locationbar=no,menubar=no,resizeable=no,status=no,scrollbars=yes");
	if(imprint.closed == true) imprint;
	else  imprint.focus() 
}

// ------------------------ ruft die Sitemap auf --------------------------------------------
function sitemap() {
var map = window.open ("ScriptSitemap","Sitemap","height=700,width=400,locationbar=no,menubar=no,resizeable=no,status=no,scrollbars=yes");
	if(map.closed == true) map;
	else  map.focus() 
}

// ------------------------ ruft das Login Fenster auf --------------------------------------------
function Login() {
links = screen.width/2 - 292/2;
oben = screen.height/2 - 350/2;
open("Redirect?open&Login","LoginWindow", "height=350,width=292, left="+links+" ,top="+oben+", locationbar=no,menubar=no,resizeable=no,status=no");
}

// ------------------------ öffnet die Redirect Seite nach dem Login  --------------------------------------------
function RedirectLogin(Sprache) {
alert ("Hallo");
var ig = open ("http://www.innovationgate.de/C1256B0C004BA105/ContentByKey/DSTS-4RTG32-DE-p?open&login","IG");
if (ig.closed == true) ig;
else  ig.focus()
window.self.close()
}

// ------------------------ Logout --------------------------------------------
function Logout()
{
document.logout.submit();
}

// ------------------------ PrintLayout --------------------------------------------

function printversion(url) {

open(url,"_print");

}

// ------------------------  Dokument ReleaseNotes --------------------------------------------
function ReleaseNotes() {
url ="http://www.innovationgate.de/C1256C3A0022764B/ContentByKey/WSCI-55NW4E-DE-p";
links = screen.width/2 - 750/2;
oben = screen.height/2 - 700/2;
open(url,"RN", "height=700,width=750, left="+links+" ,top="+oben+", locationbar=no,menubar=no,resizeable=true,status=no,scrollbars=yes");

}

// ------------------------ Suchfunktion --------------------------------------------
function search(url) {
	TheQuery=escape(document.search._query.value);
//alert (TheQuery);
	document.location.href=url+'?QUERY=' + TheQuery;	
}

// ------------------------ Suchfunktion Releasenotes--------------------------------------------
function RNsearch(Land) {
	TheQuery=escape(document.RNsearch._query.value);
//alert (TheQuery);
	document.location.href='../name/SearchResult_RN-'+Land+'?open&QUERY=' + TheQuery;	
}

// ------------------------ Erzeugt das Highlighting im Hauptnavigator --------------------------------------------
function highlight() {
//alert (window.event.srcElement.parentElement.parentElement.tagName);
window.event.srcElement.parentElement.parentElement.bgColor="#9FA29E";
}

function clearhighlight(colour) {
//alert (window.event.srcElement.parentElement.parentElement.tagName);
window.event.srcElement.parentElement.parentElement.bgColor=colour;
}



// ------------------------ Speichert Favoriten für die Personalisierung  --------------------------------------------
function SaveURL()
{
document.saveurl.submit();
alert ("Die Seite wurde Ihren persönlichen Favoriten hinzugefügt");
}

// ------------------------ Speichert Suchergebnisse für die Peronalisierung  --------------------------------------------
function SaveSearch()
{
document.SaveSearch.submit();
alert ("Die Suche wurde gespeichert.");
}

// ------------------------ MyWebGate als Homepage  --------------------------------------------
function StartPage()
{
document.HomePage.submit();
}


function MouseOver(img1,ref1, elMenu, e)
{
	if (ref1)
		document.images[img1].src = ref1;
	if (elMenu)
		popUp(elMenu, e);
};

function MouseOut(img1,ref1, elMenu)
{
	if (ref1)
		document.images[img1].src = ref1;
	if (elMenu)
		popDown(elMenu);

};

function showOrHidePortlet(id)
{
	status=document.all[id].style.display;
	if (status=="none")
	{
		document.all[id].style.display="block";
		document.images["I" + id].src = "../minimise.jpg";
	}
	else
	{
		document.all[id].style.display="none";
		document.images["I" + id].src = "../resize.jpg";
	}
}

function wg_openWindow(TheURL, TheTitle)
{
//window.open(TheURL, TheTitle, "width=700,height=400,screenX=0,screenY=0")
window.open(TheURL, TheTitle)
}