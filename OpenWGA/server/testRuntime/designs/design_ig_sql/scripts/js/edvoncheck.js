function EdvonCheck(uri,lang){
var Selektiert;
var pos=navigator.appName.indexOf("Microsoft");
if(pos==-1) Selektiert=document.getSelection()
else Selektiert=document.selection.createRange().text;
if(Selektiert==""){
if(lang =="DE") {
alert("Sie haben nichts ausgewählt.\nBitte markieren Sie zuerst ein Wort im Inhalt um danach zu suchen!");
}
else {
alert("first you have to mark a text in the content to search for it.");
}
}
else{
var pos;pos=Selektiert.indexOf(" ");
if(pos==0){
Selektiert=Selektiert.substring(1,Selektiert.length);
}
pos=Selektiert.lastIndexOf(" ");
if(pos==Selektiert.length-1){
Selektiert=Selektiert.substring(0,Selektiert.length-1);
}
var url=uri+"?QUERY="+escape(Selektiert);
self.location.href=url;
}
}