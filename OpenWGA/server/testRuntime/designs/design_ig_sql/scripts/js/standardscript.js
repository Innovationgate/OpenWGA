NS4 = !document.all && (document.layers != null);
IE = document.all != null;
NS6 = !document.all && (document.getElementById != null);
DOM = (document.getElementById != null);


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
function sitemap(Sprache,url) {
if(Sprache =="DE") {
x = "400";
y = "700";

}
else {
x = "400";
y = "350";

}
var map = window.open (url,"Sitemap","height="+y+",width="+x+",locationbar=no,menubar=no,resizeable=no,status=no,scrollbars=yes");
	if(map.closed == true) map;
	else  map.focus() 
}

// ------------------------ ruft das Login Fenster auf --------------------------------------------
function Login() {
links = screen.width/2 - 292/2;
oben = screen.height/2 - 350/2;
open("http://www.innovationgate.de/C1256B0C004BA105/ContentByKey/DSTS-4RTG32-DE-p?open&Login","LoginWindow", "height=350,width=292,left="+links+" ,top="+oben+", locationbar=yes,menubar=yes,resizeable=yes ,status=yes,scroolbars=yes");
}

// ------------------------ öffnet die Redirect Seite nach dem Login  --------------------------------------------
function RedirectLogin(Sprache) {
//alert ("Hallo");
var ig = open ("http://www.innovationgate.de/C1256B0C004BA105/ContentByKey/DSTS-4RTG32-DE-p","IG");
if (ig.closed == true) ig;
else  ig.focus()
window.self.close();
}


// ------------------------ ruft PDF Fenster auf --------------------------------------------
function PDFNews(nameurl,zielurl) {
//alert(nameurl);
links = screen.width/2 - 292/2;
oben = screen.height/2 - 270/2;
open(nameurl+"?URL="+zielurl,"PDFNews", "height=270,width=292,left="+links+" ,top="+oben+", locationbar=no,menubar=no,resizeable=no ,status=no,scroolbars=no");
}

function PDFName() {
var ig = open (document.PDFForm.ziel.value+"?Name="+document.PDFForm.name.value,"PDF");
if (ig.closed == true) ig;
else  ig.focus()
window.self.close();
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

 //------------------------  Dokument ReleaseNotes --------------------------------------------
function ReleaseNotes(Produkt,Kategorie) {
//alert ("Produkt= "+Produkt+"\nKategorie= "+Kategorie);
TheProdukt=document.ReleaseNotes.Produkt.value;
TheKategorie=document.ReleaseNotes.Kategorie.value;

document.location.href='GBES-5DPDYC.DE.0?open&Produkt=' + TheProdukt+'&Kategorie='+TheKategorie;

}

 //------------------------  Dokument ReleaseNotes --------------------------------------------
function WGAReleaseNotes(Produkt,build) {
//alert ("Produkt= "+Produkt+"\nKategorie= "+Kategorie);
TheProdukt=document.WGAReleaseNotes.Produkt.value;
Thebuild=document.WGAReleaseNotes.build.value;

document.location.href='GBES-5DXGWL.DE.0?open&Prod=' + TheProdukt+'&build='+Thebuild;

}
// ------------------------ Suchfunktion --------------------------------------------
function search(url) {
	TheQuery=escape(document.search._query.value);
//alert (TheQuery);
	document.location.href=url+'?QUERY=' + TheQuery;	
}

// ------------------------ Suchfunktion Releasenotes--------------------------------------------
function RNsearch() {
	TheQuery=escape(document.RNsearch._query.value);
//alert (TheQuery);
	document.location.href='SearchResult_RN?open&QUERY=' + TheQuery;	
}

// ------------------------ Erzeugt das Highlighting im Hauptnavigator --------------------------------------------
function highlight(oElement) {
//alert (window.event.srcElement.parentElement.parentElement.tagName);
//if (IE) window.event.srcElement.parentElement.parentElement.bgColor="#9FA29E";
if(DOM){
  if (IE) oElement.parentElement.parentElement.bgColor="#9FA29E";
  else{
     //alert (oElement.parentNode.tagName);
     oElement.parentNode.parentNode.bgColor="#9FA29E";
  }
}
}

function clearhighlight(oElement) {
//alert (window.event.srcElement.parentElement.parentElement.tagName);
//if (IE) window.event.srcElement.parentElement.parentElement.bgColor='#5F655F';
if(DOM){
  if (IE) oElement.parentElement.parentElement.bgColor="#5F655F";
  else   oElement.parentNode.parentNode.bgColor="#5F655F";
}
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