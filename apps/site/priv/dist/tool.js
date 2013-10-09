jQuery.fn.extend({
	displayer: function(flag) { //show() + hide()
		if (flag || flag==null) $(this).show(); //undefined will do show()
		else $(this).hide();

		return this;
	},
	classer: function(class_str, flag) { //addClass() + removeClass()
		if (flag || flag==null) $(this).addClass(class_str);  //undefined will do addClass()
		else $(this).removeClass(class_str);

		return this;
	}
});

johnsonRod= {
	json_obj: null,
	xml_flag: null,
	xml_doctype_str: "",
	xml_root_str: "",
	jsonFormDIV_JQ: null,
	jsonFormDataDIV_JQ: null,
	format_str_arr: ["JSON", "Formatted JSON", "XML"],

	//jsonToForm_end_hook:
	jsonToForm: function(param_obj) {
		jR.jsonFormDIV_JQ.html("<span class='red'>Processing..</span>");

		jR.json_obj= null;
		jR.jsonFormDataDIV_JQ= null;
		jR.xml_flag= false;
		jR.xml_doctype_str= "";
		jR.alterCSS("IMG.pasteIMG", "display", "none");
		jR.alterCSS("IMG.pasteChildnodesIMG", "display", "none");
		jR.alterCSS("IMG.pasteAttributesIMG", "display", "none");
		setTimeout(delayedJsonToForm, 100); //timeout - only to show "Processing.."
		if (jR.jsonToForm_end_hook) jR.jsonToForm_end_hook();

		function delayedJsonToForm() {
			var msg_str= "";

			var source_str= jQuery.trim($("#jsonTEXTAREA").val());
			if (source_str) {
				if (source_str.indexOf("<")==0) { //xml
					jR.json_obj= jR.xmlToJs(source_str);

				} else { //json
					source_str= source_str.replace(/\n/g, "");
					try {
						jR.json_obj= eval("(" +source_str +")")
					} catch(e) {
						msg_str= "JS " +e.name +": " +e.message;
						jR.json_obj= null;
					}
				}
				if (jR.json_obj && typeof(jR.json_obj)!="object") {
					msg_str= jR.json_obj;
					jR.json_obj= null;
				}
			}

			if (jR.json_obj) {
				var flag= jR.json_obj.length!=undefined;
				var html_str= jR.xml_flag ? "XML Root:<br><input id='xmlRootINPUT' value='" +jR.xml_root_str +"'> " : "";
				html_str+= 
					"Form | <span onclick='jR.expandCollapseAllFormItems(true); ' class='clickable'>Expand all nodes</span> | "
					+"<span onclick='jR.expandCollapseAllFormItems(false); ' class='clickable'>Collapse all nodes</span>" 
					+"<br/><div id='jsonFormDataDIV'>" +jR.jsonToForm_step("", jR.json_obj, flag) +"</div><br/>"
				;
				for (var L=jR.format_str_arr.length, i=0; i<L; i++) {
					if (i<2 || jR.xml_flag) html_str+= "<input type='button' class='buttonINPUT' onclick='jR.formToJson(" +i +"); ' value='Convert Form to " +jR.format_str_arr[i] +"'> ";
				}
				html_str+= "<br/><textarea id='newJsonTEXTAREA' cols='120' rows='10'></textarea>";
				html_str+= "<br/><input id='evalButtonINPUT' type='button' class='buttonINPUT' onclick='jR.evalNewJson(); ' value='Eval' style='display:none; '> ";
				jR.jsonFormDIV_JQ.html(html_str);
				jR.jsonFormDataDIV_JQ= $("#jsonFormDataDIV");

			} else {
				jR.jsonFormDIV_JQ.text("");
				alert("Source was invalid.\n\n" +msg_str);
			}

		}
	},
	clipboard: {},
	activeLi_JQ: null,
	jsonToForm_step: function(a, b, c) {
		if (typeof(b)!="object") return "NOT AN OBJECT";
		var d= false;
		if (b) d= b.length!=undefined;
		var e;
		if (c) e= "arrayIndex"
		else if (d) e= "arrayNameINPUT"
		else if (typeof(b)=="object") e= "objectNameINPUT"
		else e= "nameINPUT"

		var f= jR.xml_flag ? jR.get_readonly_flag(a) : false;
		var g= jR.input_html(a, "leftINPUT " +e, true, f) +":";
		g += "\n<ol" +(d ? " class='arrayOL'": "") +">" +jR.addActions_html(a, d);
		for (var a in b) {
			if (typeof(b[a])=="object" && b[a]!=null) {
				if (b[a].length==undefined) {
					g+= "<li>"
				} else {
					g+= "<li class='arrayLI'>"
				}
				g+= jR.jsonToForm_step(a, b[a], d);
			} else {
				g+= "<li>";
				var e;
				if (d) {
					e= "arrayIndex";
				} else {
					e= "nameINPUT";
				}
				var h= typeof(b[a])=="string" ? "stringTEXTAREA": "";
				var f= jR.xml_flag ? jR.get_readonly_flag(a) : false;
				var i= b[a];
				if (typeof(i)==="undefined") i= "undefined";
				else if (typeof(i)=="number" && !i) i= "0";
				else if (i===null) i= "null";
				else if (i===false) i= "false";
				g+= jR.input_html(a, "leftINPUT " +e, false, f) +":" +jR.input_html(i, "rightTEXTAREA " +h);
			}
			g+= "</li>\n";
		}
		return g +"</ol>\n";
	},
	input_html: function(a, b, c, d) {
		if (b.indexOf("arrayIndex")>=0) {
			return jR.leftActions_html(b, jR.xml_flag ? false: c) +"<input type='hidden' class='leftINPUT'><span class='indexSPAN'>[" +a +"]</span>";
		} else {
			var e= b;
			if (d) e+= " readonlyINPUT";
			var f= e ? (" class='" +e +"'") : "";
			if (!a && b.indexOf("objectNameINPUT")>=0) {
				return "<input type='hidden' " +f +">";
			} else {
				if (b.indexOf("leftINPUT")>=0) {
					if (d) f+= " readonly";
					return jR.leftActions_html(b, c) +"<input value='" +a +"'" +f +"><span class='indexSPAN'></span>";
				} else {
					return jR.textarea_html(a) +jR.checkbox_html(b);
				}
			}
		}
	},
	leftActions_html: function(a, b) {
		if (!a) a= "";
		var html_str= "";

		if (b) html_str+= "<img src='../dist/toolimages/collapse.gif' class='clickable expandCollapseIMG' data-a='x' title='Expand/Collapse Node'> ";
		else html_str+= "<img src='../dist/toolimages/blank.gif'> ";

		if (!jR.xml_flag || a.indexOf("arrayIndex")>=0) {
			html_str+= "<img src='../dist/toolimages/up.gif' class='clickable' data-a='u' title='Move Node Up'>"
			+" <img src='../dist/toolimages/down.gif' class='clickable' data-a='d' title='Move Node Down'>"
			+" <img src='../dist/toolimages/copy.gif' class='clickable' data-a='c' title='Copy Node'>"
			+" <img src='../dist/toolimages/close.gif' class='clickable' data-a='r' title='Delete Node (Safe)'> ";
		}
		return html_str;
	},
	textarea_html: function(str) {
		if (!str) str= "";
		return "<img src='../dist/toolimages/expand2.gif' class='clickable expandCollapse2IMG' data-a='t' title='Expand/Collapse Textarea'>"
			+"<textarea class='rightTEXTAREA'>" +str +"</textarea>"
		;
	},
	checkbox_html: function(a) {
		var b= "";
		if (!jR.xml_flag) {
			b= "<label><input type='checkbox' class='checkbox'";
			if (a && a.indexOf("stringTEXTAREA")>=0) b+= " checked ";
			b += "><i>string</i></label>";
		}
		return b;
	},
	addActions_html: function(a, b) {
		var c= "";
		var d= "";
		if (!jR.xml_flag) {
			var e= b ? "" : "Name:";
			d=
				"ADD <span class='clickable' data-a='a' data-b='" +b +"' data-c='0'>" +e +"Value</span>" 
				+" | <span class='clickable' data-a='a' data-b='" +b +"' data-c='1'>" +e +"Object</span>" 
				+" | <span class='clickable' data-a='a' data-b='" +b +"' data-c='2'>" +e +"Array</span>"
			;
		} else {
			if (a=="attributes" || a=="childNodes") {
				d= "ADD ";
				if (a=="attributes") {
					c= " pasteAttributesIMG";
					d+= "<span class='clickable' data-a='a' data-c='0'>Name:Value</span>";
				} else {
					c= " pasteChildnodesIMG";
					d+= 
						"<span class='clickable' data-a='a' data-c='1'>Name:Object</span>" 
						+" | <span class='clickable' data-a='a' data-c='3'>TextNodeValue</span>"
					;
				}
			}
		}
		if (d) d+= " <img src='../dist/toolimages/paste.gif' class='clickable pasteIMG" +c +"' data-a='p' title='Paste'>";
		return d;
	},

	xmlToJs: function(a) {
		var b;
		if (window.ActiveXObject) {
			b= new ActiveXObject("Microsoft.XMLDOM");
			b.async= "false";
			b.loadXML(a);
			if (b.parseError.errorCode) {
				return "Microsoft.XMLDOM XML Parsing Error: " +b.parseError.reason +"Line Number " +b.parseError.line +", " +"Column " +b.parseError.linepos +":" +"\n\n" +b.parseError.srcText;
			}
		} else {
			b= (new DOMParser()).parseFromString(a, "text/xml");
		}
		var c = b.documentElement;
		if (c.tagName=="parserError" || c.namespaceURI=="http://www.mozilla.org/newlayout/xml/parsererror.xml") {
			return "DOMParser " +c.childNodes[0].nodeValue + "\n\n" + c.childNodes[1].childNodes[0].nodeValue;
		}
		jR.xml_flag= true;
		jR.xml_root_str= c.tagName;
		if (a.indexOf("<?xml ")==0) {
			var L= a.indexOf("?>");
			if (L>0) jR.xml_doctype_str= a.substr(0, L +2);
		}
		return jR.xmlToJs_step(c);
	},
	xmlToJs_step: function(a) {
		var b= {};
		if (a.attributes) {
			b.attributes= [];
			if (a.attributes.length > 0) {
				for (var i=0, xmlChildObj; xmlChildObj = a.attributes[i]; i++) {
					if (xmlChildObj = a.attributes[i]) {
						if (xmlChildObj.nodeName != undefined) {
							e= {};
							e[xmlChildObj.nodeName]= xmlChildObj.value;
							b.attributes.push(e);
						}
					}
				}
			}
		}
		if (a.childNodes) {
			b.childNodes= [];
			if (a.childNodes.length > 0) {
				for (var i=0, xmlChildObj; xmlChildObj = a.childNodes[i]; i++) {
					var c= xmlChildObj.nodeName;
					if (c=="#text") {
						var d= jQuery.trim(xmlChildObj.nodeValue);
						if (d) {
							e= {
								textNode: d
							};
							b.childNodes.push(e);
						}
					} else if (c != undefined) {
						var e= {};
						e[c]= jR.xmlToJs_step(xmlChildObj);
						b.childNodes.push(e);
					}
				}
			}
		}
		return b;
	},

	expandCollapseAllFormItems: function(flag) {
		$("IMG.expandCollapseIMG", jR.jsonFormDataDIV_JQ).each(function(i, el) {
			jR.expandCollapseFormItem($(el), flag);
		});
	},
	expandCollapseFormItem: function(img_JQ, flag) {
		var ol_JQ= img_JQ.siblings("OL");
		if (ol_JQ.length) {
			var el= img_JQ[0];
			if (flag==undefined) flag= el.src.indexOf("collapse")<0;
			ol_JQ.displayer(flag);
			el.src = "../dist/toolimages/" +(flag ? "collapse": "expand") +".gif";
		}
	},
	expandCollapseTextarea: function(img_JQ) {
		var ta_JQ= img_JQ.siblings("TEXTAREA");
		if (ta_JQ.length) {
			var el= img_JQ[0];
			var flag= el.src.indexOf("collapse")<0;
			ta_JQ.classer("expandedTEXTAREA", flag);
			el.src= "../dist/toolimages/" +(flag ? "collapse": "expand") +"2.gif"
		}
	},
	deleteFormItem: function(img_JQ) {
		var li_JQ= jR.activeLi_JQ;
		if (li_JQ && li_JQ.length) {
			if (li_JQ.hasClass("deleted")) {
				jR.globalRestoreLI_JQ= li_JQ;
				var html_str= 
					"<input type='button' class='buttonINPUT' onclick='jR.restoreFormItem(); ' value='Restore THIS Node'>"
					+"<br><br><input id='removeAllDeletedINPUT' type='button' onclick='jR.allDeletedFormItems(); ' value='Remove ALL Marked Nodes'>"
				;
				jR.messageRight(img_JQ, html_str, 0);
			} else {
				li_JQ.addClass("deleted");
			}
		}
	},
	restoreFormItem: function() {
		jR.globalRestoreLI_JQ.removeClass("deleted");
		jR.messageClose();
	},
	allDeletedFormItems: function() {
		jR.jsonFormDataDIV_JQ.find("LI.deleted").each(function(i, el) {
			el.parentNode.removeChild(el);
		});
		jR.messageClose();
	},
	addFormItem: function(span_JQ) {
		var ol_JQ= span_JQ.closest("OL");
		if (ol_JQ.length) {
			var b= span_JQ.data("b");
			var c= span_JQ.data("c");

			var li_JQ= $("<LI>");
			var f= false;
			var g= true;
			var h;
			if (c==0 || c==3) {
				g= false;
				h= "nameINPUT";
				if (c==3) f= true;
			} else if (c==1) {
				h= "objectNameINPUT";
			} else {
				h= "arrayNameINPUT";
			}
			if (b) h= "arrayIndex";
			var html_str= jR.input_html( (c==3 ? "textNode" : "*"), "leftINPUT " +h, g, f) +":";
			if (c==0 || c==3) {
				html_str+= jR.textarea_html() +jR.checkbox_html("stringTEXTAREA");
			} else {
				if (c==2) {
					html_str+= "<ol class='arrayOL'>" + jR.addActions_html("", true) +"</ol>"
				} else {
					if (jR.xml_flag) {
						var xmlDefaultNodesStr= 
							"<li>" +jR.leftActions_html("", true) +"<input class='leftINPUT objectNameINPUT readonlyINPUT' readonly='' value='attributes'/>:" 
							+" <ol class='arrayOL'>" +jR.addActions_html("attributes") +" </ol>" +"</li>" 
							+"<li>" +jR.leftActions_html("", true) +"<input class='leftINPUT objectNameINPUT readonlyINPUT' readonly='' value='childNodes'/>:" 
							+" <ol class='arrayOL'>" +jR.addActions_html("childNodes") +" </ol></li>"
						;
						html_str+= "<ol>" +jR.addActions_html() +xmlDefaultNodesStr +"</ol>";
					} else {
						html_str+= "<ol>" +jR.addActions_html("", false) +"</ol>";
					}
				}
			}
			if (jR.xml_flag) html_str= jR.leftActions_html("arrayIndex", false) +"<input type='hidden' class='leftINPUT'>[*]:<ol><li>" +html_str +"</li></ol>"

			if (c==2) li_JQ.addClass("arrayLI");
			li_JQ.html(html_str);
			jR.liNew(li_JQ, ol_JQ);
		}
	},
	moveFormItem: function(img_JQ, after_flag) {
		var li_JQ= jR.activeLi_JQ;
		var JQ= after_flag ? li_JQ.next("LI") : li_JQ.prev("LI");
		if (!JQ.length) {
			var OL_JQ= li_JQ.closest("OL");
			JQ= after_flag ? OL_JQ.children("LI:first") : OL_JQ.children("LI:last");
			if (JQ[0]==li_JQ[0]) return; //--> only 1 item in list
			after_flag= !after_flag;
		}
		if (after_flag) li_JQ.insertAfter(JQ);
		else li_JQ.insertBefore(JQ);
	},
	copyFormItem: function(img_JQ) {
		var li_JQ= jR.activeLi_JQ;
		if (li_JQ) {
			var ol_JQ= li_JQ.closest("OL");
			if (ol_JQ.length) {
				jR.clipboard.li_JQ= li_JQ.clone(); //copy to clipboard
				jR.clipboard.ol_class_str= ol_JQ.attr("class");
				jR.clipboard.li_JQ.removeClass("deleted");
				var ar_flag= ol_JQ.attr("class")=="arrayOL";

				var JQ;
				if (!jR.xml_flag) {
					JQ= li_JQ;
				} else {
					JQ= li_JQ.children("OL:first");
					if (JQ.length) JQ= JQ.children("LI:first"); //children or find????
				}

				var html_str= "";
				if (JQ.length) {
					var input_JQ= JQ.find("INPUT.leftINPUT:first");
					if (input_JQ.length) {
						if (input_JQ.attr("type")=="hidden") html_str+= "#";
						else html_str+= '"' +input_JQ.val() +'"';
						html_str+= ":";

						var ta_JQ= JQ.children("TEXTAREA:first");
						if (ta_JQ.length) html_str+= '"' +ta_JQ.val() +'"';
						else html_str+= JQ.hasClass("arrayLI") ? "[]" : "{}";
					}
				}
				jR.messageRight(img_JQ, "<b>" +(ar_flag ? "Array item": "Object") +" copied:</b><br>" +html_str);
				var str= "none";
				if (!jR.xml_flag) {
					str= "pasteIMG";
					jR.alterCSS("IMG.pasteIMG", "display", "inline");

				} else {
					var p_li_JQ= ol_JQ.closest("LI");
					if (p_li_JQ.length) {
						var input_JQ= p_li_JQ.children("INPUT.leftINPUT:first");
						if (input_JQ.length) {
							var at_flag= input_JQ.val()=="attributes";
							str= at_flag ? "pasteAttributesIMG": "pasteChildnodesIMG";
							var i_str= at_flag ? "pasteChildnodesIMG": "pasteAttributesIMG";
							jR.alterCSS("IMG." +str, "display", "inline");
							jR.alterCSS("IMG." +i_str, "display", "none");
						}
					}
				}
				jR.jsonFormDataDIV_JQ.find("IMG." +str).each(function(i, el) {;
					el.title= "Paste: " +html_str.replace('"', "'", "g");
				});
			}
		}
	},

	pasteFormItem: function(img_JQ) {
		if (jR.clipboard.li_JQ) {
			var ol_JQ= img_JQ.closest("OL");
			if (ol_JQ.length) {
				var li_JQ= jR.clipboard.li_JQ.clone(); //copy from clipboard
				jR.liNew(li_JQ, ol_JQ);

				if (ol_JQ.attr("class")!=jR.clipboard.ol_class_str) {
					var input_JQ= li_JQ.find("INPUT.leftINPUT:first");
					var span_JQ= li_JQ.find("SPAN.indexSPAN:first");
					if (ol_JQ.attr("class")=="arrayOL") {
						span_JQ.html("[*]");
						input_JQ[0].type= "hidden"; //skip jQ for this

					} else {
						span_JQ.html("");
						input_JQ[0].type= "text"; //skip jQ for this
						if (!input_JQ.val()) input_JQ.val("*");
					}
				}
				var html_str= img_JQ[0].title.replace("Paste: ", "<b>Pasted:</b><br>");
				jR.messageRight(img_JQ, html_str);
			}
		} else {
			jR.messageRight(img_JQ, "</b>Nothing in clipboard.</b>");
		}
	},
	formClicked: function(evt) {
		var el= evt.target;
		var JQ= $(el);
		jR.liActive(JQ.closest("LI"));

		if (JQ.hasClass("clickable")) jR.liActions[JQ.data("a")](JQ);
	},
	liActions: {
		x: function(JQ) {jR.expandCollapseFormItem(JQ); }, //expand/collapse
		u: function(JQ) {jR.moveFormItem(JQ,0); }, //up
		d: function(JQ) {jR.moveFormItem(JQ,1); }, //down
		c: function(JQ) {jR.copyFormItem(JQ); }, //copy
		r: function(JQ) {jR.deleteFormItem(JQ); }, //remove
		t: function(JQ) {jR.expandCollapseTextarea(JQ); }, //textarea expand/collapsetitle
		p: function(JQ) {jR.pasteFormItem(JQ); }, //paste
		a: function(JQ) {jR.addFormItem(JQ); } //add
	},
	liNew: function(li_JQ, ol_JQ) {
		var first_li_JQ= ol_JQ.children("LI:first");
		if (first_li_JQ.length) li_JQ.insertBefore(first_li_JQ);
		else li_JQ.appendTo(ol_JQ);

		jR.liActive(li_JQ);
	},
	liActive: function(JQ) {
		if (jR.activeLi_JQ) jR.activeLi_JQ.removeClass("activeLI");
		if (!JQ.length) return; //-->

		JQ.addClass("activeLI");
		jR.activeLi_JQ= JQ;
	},
	get_readonly_flag: function(a) {
		return (a=="attributes" || a=="childNodes" || a=="textNode");
	},
	format_num: 0,
	linebreak_str: "",
	error_ct: [],
	error1_msg: "",

	//formToJson_end_hook:
	formToJson: function(format_num) {
		jR.error_ct= [0, 0];
		jR.error1_msg= "";
		jR.format_num= format_num;
		jR.linebreak_str= format_num ? "\n": "";
		var f_div_JQ= jR.jsonFormDataDIV_JQ;
		var ta_JQ= $("#newJsonTEXTAREA");
		ta_JQ.val("Processing..");
		if (format_num==2) jR.xml_root_str= $("#xmlRootINPUT").val();
		var html_str= "";
		var flag= 0;
		if (format_num==2) {
			flag= 1;
			if (jR.xml_doctype_str) html_str+= jR.xml_doctype_str +jR.linebreak_str;
			html_str+= "<" +jR.xml_root_str;
		}
		html_str+= jR.formToJson_step(f_div_JQ, flag, "");

		if (format_num==2) html_str+= "</" +jR.xml_root_str +">";
		ta_JQ.val(html_str);
		$("#evalButtonINPUT").displayer(format_num<2);
		var msg_str= "";
		if (jR.error_ct[0]) msg_str= jR.get_plural_str(jR.error_ct[0], 'name') +'left empty, replaced by "undefined".\n';
		if (jR.error_ct[1]) msg_str+= jR.get_plural_str(jR.error_ct[1], 'nonstring value') +"left empty, replaced by 0 in " +jR.error1_msg.substr(0, jR.error1_msg.length -2) +".";
		if (msg_str) {
			msg_str= "\n\nWarning:\n" +msg_str;
			alert("Form convert to " +jR.format_str_arr[format_num] +"." +msg_str);
		}
		if (jR.formToJson_end_hook) jR.formToJson_end_hook();
	},

	formToJson_step: function(JQ, e, f, g) {
		var input_JQ= JQ.children("INPUT");
		if (input_JQ.length) {
			var j= "";
			var k= "";
			var str= processText(input_JQ[0].value);
			var m= false;
			if (input_JQ[0].type!="hidden") {
				if (!str || str=="*") {
					jR.error_ct[0]++;
					str= "undefined";
					input_JQ[0].value= str;
				}
				if (jR.format_num==2) { //xml
					m= (str=="attributes" || str=="childNodes");
					k= "";
					if (!m && str!="textNode") {
						if (g=="attributes") j+= " " +str +"=";
						else j+= pad_html("<" +str, e);
					}
				} else {
					k= '"' +str +'":';
				}
			}
			if (jR.format_num<2) j+= pad_html(k, e);
			var ol_JQ= JQ.children("OL:first");
			if (ol_JQ.length) {
				var ar_flag= ol_JQ.hasClass("arrayOL");
				if (jR.format_num<2) j+= (ar_flag ? "[": "{") +jR.linebreak_str;
				var LIs_JQ= ol_JQ.children("LI");
				if (LIs_JQ.length) {
					var ct= 0, li_JQ;
					LIs_JQ.each(function(i, el) {
						li_JQ= $(el)
						if (!li_JQ.hasClass("deleted")) {
							ct++;
							var r= "";
							var s;
							if (jR.format_num==2) { //xml
								r= str;
								s= e;
								if (r=="") r= g;
								else if (!m) s= e +1;
							} else {
								s= e +1;
							}
							j+= jR.formToJson_step(li_JQ, s, ",", r); //recurse
						}
					});

					if (jR.format_num<2 && ct) {
						var L= j.lastIndexOf(",");
						j= j.substring(0, L) +j.substring(L +1);
					}
				}
				if (jR.format_num==2) { //xml
					if (str=="attributes") j+= ">" +jR.linebreak_str;
					k= "";
					if (str!="attributes" && str!="childNodes" && str!="textNode" && str!="") {
						k= "</" +str +">" +jR.linebreak_str;
					}
				} else {
					k= ar_flag ? "]": "}";
				}
				if (k) j+= pad_html(k, e);

			} else {
				var t= JQ.find("INPUT:checked").length; //is name-value, has no nodes below
				if (jR.format_num==2 && g!="attributes") t= 0;
				var u= t ? '"': "";
				var ta_JQ= JQ.children("TEXTAREA:first");
				if (ta_JQ.length) {
					k= ta_JQ.val();
					if (jR.format_num<2) k= processText(k);
					else k= jQuery.trim(k);
				}
				if (!k && !t && jR.format_num<2) {
					k= "0";
					ta_JQ.val(k);
					jR.error_ct[1]++;
					jR.error1_msg+= (l ? "'" +l +"'": "[array item]") +", ";
				}
				k= u +k +u;
				if (jR.format_num==2 && g!="attributes") k= pad_html(k, e) +jR.linebreak_str; //xml
				j+= k
			}
			if (jR.format_num<2) j= j +f +jR.linebreak_str;
			return j;
		}
		function processText(a) {
			return jQuery.trim(a.replace(/\\/g, "\\\\").replace(/"/g, "\\\"").replace(/\n/g, "\\n"));
		}
		function pad_html(a, b) {
			if (!jR.format_num) return a;
			var c= "";
			while (b>0) {
				c+= "\t";
				b--;
			}
			return c +a;
		}
	},
	evalNewJson: function() {
		var ta_JQ= $("#newJsonTEXTAREA");
		var str= jQuery.trim(ta_JQ.val());
		if (str) {
			var b= "Eval OK";
			try {
				jR.json_obj= eval("(" +str +")");
			} catch(e) {
				b= "Invalid.\n\nJS " +e.name +": " +e.message;
			} finally {
				alert(b);
			}
		}
	},
	alterCSS: function(selector_str, attr_str, value_str) { //is this really faster? dont remember testing - in theory it probably is faster //done mostly as goof
		if (!selector_str || !attr_str || !value_str) return;
		var type_str;

		if (document.getElementById) type_str= "cssRules";
		else if (document.all) type_str= "rules"; //IE
		else return;

		//lowercasing for webkit
		selector_str= selector_str.toLowerCase();

		var rules= document.styleSheets[0][type_str];
		for (var i= 0, rule; rule= rules[i]; i++) {
			if (rule.selectorText.toLowerCase()==selector_str) {
				rule.style[attr_str]= value_str;
				break;
			}
		}
	},

	//growl-ish messenger
	messageDiv_JQ: null,
	messageClose_img_el: null,
	messageDivContent_JQ: null,
	message_timer: null,
	messageRight: function(img_JQ, html_str, fade_sec) {
		clearTimeout(jR.message_timer);
		jR.messageDivContent_JQ.html(html_str);

		var pos= img_JQ.offset(); 
		pos.left+= img_JQ[0].offsetWidth;
		jR.messageDiv_JQ[0].style.left= pos.left +"px";
		jR.messageDiv_JQ[0].style.top= pos.top +"px";
		jR.messageDiv_JQ.show();
		if (fade_sec==undefined) fade_sec= 2;
		if (fade_sec>0) jR.message_timer= window.setTimeout(delayedClose, fade_sec *1000);
		jR.messageDiv_JQ.classer("messageAutoCloseDIV", fade_sec>0);
		jR.messageClose_img_el.src= "../dist/toolimages/" +(fade_sec>0 ? "countdown": "popupClose") +".gif";

		function delayedClose() {
			if (jR.messageDiv_JQ.is(":visible")) jR.messageDiv_JQ.fadeOut();
		}
	},
	messageClose: function() {
		clearTimeout(jR.message_timer);
		jR.messageDiv_JQ.hide();
	},

	get_readonly_flag: function(a, b) {
		return a +" " +b +(a!=1 ? "s" : "") +" ";
	},
	get_plural_str: function(a, b) {
		return a +" " +b +(a!=1 ? "s": "") +" ";
	},
	sampleClicked: function(evt) {
		var JQ= $(this);
		var str= JQ.text();
		var code_JQ= $("#sample" +str);
		if (code_JQ) {
			if (jR.jsonFormDataDIV_JQ && !confirm("Clear existing Form?")) return; //-->

			var code_str= code_JQ.html();
			code_str= jQuery.trim(code_str.substring(4, code_str.length -3));
			$("#jsonTEXTAREA").val(code_str);

			jR.jsonToForm(); //auto convert
		}
	},
	init: function(a, b, c) {
		$("#pickSampleSPAN").delegate(".clickable", "click", jR.sampleClicked);

		jR.jsonFormDIV_JQ= $("#jsonFormDIV");
		//jR.jsonFormDataDIV_JQ.delegate(".clickable", "click", jR.formClicked);
		jR.jsonFormDIV_JQ.click(jR.formClicked); //not delegate, want to capture all clicks

		jR.messageDiv_JQ= $("#messageDIV");
		jR.messageDiv_JQ.hide();
		jR.messageClose_img_el= $("#messageCloseIMG")[0];
		jR.messageDivContent_JQ= $("#messageContentDIV");
	}
};
jR= johnsonRod;
