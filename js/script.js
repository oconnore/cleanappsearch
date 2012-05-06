/* Author: Eric O'Connor
   
*/

var effsq={
    results_shown: false,
    transform_page: function(){
	$("#wrapper").css({
	    "position": "static",
	    "left": "0",
	    "top": "0",
	    "width": "auto",
	    "margin": "10px"
	});
	effsq.results_shown=true;
    },
    display_results: function(res){
	effsq.transform_page();
	if(res["response"]===true) {
	    var buf="";
	    buf+="<table>";
	    var columns=res["columns"];
	    var listings=res["products"];
	    buf+="<thead>";
	    for(var a=0;a<columns.length;a++){
		buf+=[
		    "<th>",
		    columns[a],
		    "</th>"].join("");
	    }
	    buf+="</thead>";
	    for(var a=0;a<listings.length;a++){
		var product=listings[a];
		var cells=product["cells"];
		buf+="<tr>";
		for(var b=0;b<cells.length;b++){
		    var cell=cells[b];
		    buf+=[
			"<td>",
			cell,
			"</td>"
		    ].join("");
		}
		buf+="</tr>";
	    }
	    buf+="</table>";
	    $("#results").html(buf);
	    $("#results").show();
	} else {
	    $("#notice").text("There was an error with your search.");
	}
    },

    submit_search: function(){
	var search=$("#searchtextbox").prop("value");
	var filters=[];
	$.ajax({
	    "type": "POST",
	    "url": "/api/search",
	    "data": {
		"query": search,
		"filters": ["+filter:1997-2001"]
	    },
	    "success": effsq.display_results,
	    "error": function(){
		return false;
	    },
	    "dataType": "json"
	});
	return false;
    },
    working: function (){
	;
    },
    finished: function (){
	;
    }
};

$(document).ready(function (){   
    $("#searchtextbox").focus();
    $("#searchbox .submitbutton").click(effsq.submit_search);
    $("#searchbox .submitbutton").submit(effsq.submit_search);
});





