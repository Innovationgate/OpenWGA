define(["jquery", "appnav", "sitepanel"], function($, Appnav, SitePanel){

	return function(){

		if(Appnav.getContext().status=="w"){
			var win = SitePanel.getWindow();
			var href = ((win.location && win.location.href) || SitePanel.iframe().attr("src")) + "?$clean"
			$.get(href, function(html){
				var el = document.createElement("div");
				el.innerHTML = html;
				setDescriptionPlaceholder(el)
				updateStats(el)
			})
		}
		else{
			setDescriptionPlaceholder(SitePanel.getDocument())
			updateStats($("body", SitePanel.getDocument()))
		}
			
		function setDescriptionPlaceholder(el){
			var content = "";
			$("p,h1,h2,h3,h4,h5,h6", el).each(function(){
				if(content.length<200){
					var txt = $(this).text().replace(/[\n\t]/g, " ").trim()
					if(txt){
						content += txt + " "
					}
				}
			})
			if(content.length>160){
				var idx = content.substr(160).indexOf(" ")
				content = content.substr(0, 160+idx) + " ...";
			}
			$("#seo-searchengine .anzeige [name=description]").attr("placeholder", content).autogrow("update");
		}

		function updateStats(el){
			var words = getWords(el);
			var total = words.total;
			var ol = $("<ol>"); 
			$("#seo-word-cloud .content-words").append(ol)
			for(var i=0; i<Math.min(10,words.words.length); i++){
				var entry = words.words[i]
				var word = entry.word;
				ol.append(
					'<li>'
						+ word 	//+ ' (' + entry.count + ') '
					+ '</li>'
				)
			}
	
			$("#seo-word-cloud .stats").append(total + " Wörter gesamt<br>")
				.append(words.words.length + " unterschiedliche Wörter");
		}
		
		function getWords(el){
			var words = {};
			var index=0;;
			var stopwords = "ajax gmbh ag ab aber aehnlich ähnlich alle allein allem allen aller alles allg allgemein als also am an and ander andere anderem anderen anderer anderes anderm andern anderr anders auch auf aus außer author autor been bei beim bereits besonders besser bevor bietet bin bis bist böden boeden bzw ca da dabei dadurch dafuer daher damit daneben dann daran darauf daraus darum das dass daß dasselbe davon davor dazu dein deine deinem deinen deiner deines dem demselben den denen denn dennoch denselben der derem deren derer derselbe derselben des deshalb desselben dessen dich die dies diese dieselbe dieselben diesem diesen dieser dieses dinge dir doch dort du dunklen durch eben eher eigenen eigenes eigentlich ein eine einem einen einer eines einfach einig einige einigem einigen einiger einiges einmal er erst erste erster es etc etwa etwas euch euer eure eurem euren eurer eures fall finden for für ganz ganze ganzem ganzen ganzer ganzes gar geben gegen geht gewesen ggf gibt gleich gute guten hab habe haben hat hatte hatten hattest hattet hier hin hinter hinterher ich ihm ihn ihnen ihr ihre ihrem ihren ihrer ihres im immer in indem information ins ist ja je jede jedem jeden jeder jedes jedoch jene jenem jenen jener jenes jetzt kann kannst kein keine keinem keinen keiner keines koennen koennt kommen kommt können könnt konnte könnte langsam lassen leicht leider lesen lichten liest machen mal mag man manche manchem manchen mancher manches mehr mehrere mein meine meinem meinen meiner meines meist mich mir mit möchte moechte moeglich möglich muß müssen mußt müßt musste müsste nach nachdem nachher natürlich ncht neben nein neu neue neuem neuen neuer neues nicht nichts noch nun nur nutzung ob oder off ohne online per schnell schon schwierig sehen sehr sehrwohl seid sein seine seinem seinen seiner seines seit seite seiten selber selbst sich sie sieht sind so sodaß solch solche solchem solchen solcher solches soll sollen sollst sollt sollte sollten solltest sondern sonst soviel soweit sowie sowohl spielen statt steht suchen titel über um und uns unse unsem unsen unser unsere unseren unseres unter version viel viele vieles vom von vor vorher wachen während wann war waren warst warum was weg weil weiter weitere welche welchem welchen welcher welches wenig wenige weniger wenn wer werde werden werdet weshalb wie wieder wieso wieviel will wir wird wirklich wirst wo woher wohin wohl wollen wollte wurde würde wurden würden zu zum zur zwar zwischen".split(" ");
		
			$("*", el).not("style, script, nav *, sidebar *, header *, footer *").contents()
				.filter(function () { return this.nodeType === 3; })
				.each(function(){
					var relevanz = 1;
					var in_headings = $(this).parents("h1,h2,h3,h4,h5,h6").length>0
					if(in_headings){
						relevanz *= 2
					} 
					//console.log(this.nodeValue.trim(),in_headings, index,relevanz);
					var w = this.nodeValue.match(/[A-z_äöüÄÖÜß]+/g);
					if(w){
						if(index++ < 30){
							relevanz *= 2;
							//console.log(this, w,in_headings, index,relevanz); 
						}
						for(var i=0; i<w.length; i++){
							var word = w[i].toLowerCase();
							if(word.length<3 || stopwords.indexOf(word)>=0)
								continue;
							if(words[word]){
								words[word].count++
								words[word].weight = Math.max(relevanz, words[word].weight)
							}
							else words[word]={
								word: word,
								count: 1,
								weight: relevanz
							}
							//console.log(word, words[word].count, words[word].weight);
						}
					}				
				});
				
			var words_array = Object.getOwnPropertyNames(words).sort(function(a,b){
				return words[a].count * words[a].weight < words[b].count * words[b].weight ? 1 : -1; 
			})
			
			var ret = [];
			var total = 0;
			for(var i=0; i<words_array.length; i++){
				var word = words_array[i]
				ret.push(words[word])
				total += words[word].count;
			}
			return {
				total: total,
				words: ret
			}
		}
		
		
		$("#seo-w3c a[href='#w3c-validate']").click(function(ev){

			ev.preventDefault();

			function encodeHTML(input) {
			    return $('<span>').text(input).html().trim().replace(/\"/g, "'");
			}
		
			var win = SitePanel.getWindow();
			var href = ((win.location && win.location.href) || SitePanel.iframe().attr("src")) + "?$clean"
			$.get(href, function(html){

				$("#seo-w3c").html("Warte auf Antwort ...");
				
				$.ajax({
					method: "POST",
					contentType: "text/html;charset=UTF-8",
					url: "https://validator.w3.org/nu/?out=json",
					data: html,
					dataType: "json"
				})
				.done(function(data){
					$("#seo-w3c").html("<h3>" + data.messages.length + " Hinweise gefunden</h3>");
					for(var i=0; i<data.messages.length; i++){
						var entry = data.messages[i]
						if(entry.extract)
							$("#seo-w3c").append("<pre>" + encodeHTML(entry.extract) + "</pre>");
						$("#seo-w3c").append('<p class="' + entry.type + '">' 
							+ entry.type.toUpperCase() + ': ' + encodeHTML(entry.message) + '</p>');
					}
				})

			})
			 
		})

	}

})