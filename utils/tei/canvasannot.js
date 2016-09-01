function stretchCanvasToBody (canvasId) {
    // document.body is undefined when generated from xslt in the
    // client, see
    // https://forum.jquery.com/topic/using-jquery-from-an-xslt-throws-error-is-undefined-after-that
    var body = document.getElementsByTagNameNS("http://www.w3.org/1999/xhtml", "body")[0],
	bodyRect = body.getBoundingClientRect(),
	canvas = document.getElementById(canvasId),
	bodyWidth = Math.ceil(bodyRect.right - bodyRect.left),
	bodyHeight = Math.ceil(bodyRect.bottom - bodyRect.top),
	bodyPixels = bodyWidth * bodyHeight;
    var maxCanvasHeight = 32000;
    console.log("Stretching drawing area #"
		+ canvasId
		+ " to (width, height, pixels): "
		+ bodyWidth + " " + bodyHeight + " "
		+ bodyPixels, Math.min(bodyHeight, maxCanvasHeight)); 
    canvas.width = bodyWidth;
    canvas.height = Math.min(bodyHeight, maxCanvasHeight);
}

function drawRelations (canvasId, relations) {
    var canvas = $('#'+canvasId),
	dx = canvas.offset().left,
	dy = canvas.offset().top;
    $.each(relations, function (count, rel) {
	console.log("Drawing relation " + rel.relationId + ": " + rel.subject + " " + rel.predicate + " " + rel.object);
	console.log($("[eid|='"+rel.subject+"'] > .relationanchor"));
	var sub = $("[eid|='"+rel.subject+"'] > .relationanchor"),
	    subOffset = sub.offset(),
	    obj = $("[eid|='"+rel.object+"'] > .relationanchor"),
	    objOffset = obj.offset(),
	    ax = subOffset.left - dx,
	    ay = subOffset.top - dy + 0.7*parseInt(sub.css("line-height")),
	    bx = objOffset.left - dx,
	    by = objOffset.top - dy + 0.7*parseInt(obj.css("line-height")),
	    // sehne s
	    sx = ax - bx,
	    sy = ay - by,
	    // lenght sehne
	    ls = Math.sqrt(Math.pow(sx,2) + Math.pow(sy,2)),
	    // q: fraction of radius, 0.5 < q < 1, determining height of
	    // arc over sehne
	    q = 0.6,
	    // radius
	    r = 0.5 * ls * (1/q),
	    // center
	    cx = (0.5 * (ax + bx)) + (r/q * 0.5 * (ay - by) / ls),
	    cy = (0.5 * (ay + by)) + (r/q * 0.5 * (bx - ax) / ls);
	console.log('(' + ax + ', ' + ay + '), (' + bx + ', ' + by + ')');
	console.log(sub.css("line-height"));
	canvas.draw({
	    type: "quadratic",
	    strokeStyle: '#333',
	    strokeWidth: 2,
	    rounded: true,
	    endArrow: true,
	    arrowRadius: 7,
	    arrowAngle: 60,
	    x1: ax, y1: ay,
	    cx1: cx, cy1: cy,
	    x2: bx, y2: by,
	    layer: true,
	    name: rel.relationId,
	    mouseover: function(layer) {
		showAnnotationInfo(rel);
		$(this).animateLayer(layer, {
		    strokeWidth: 4,
		}, 125);
	    },
	    mouseout: function(layer) {
		showAnnotationInfo(null);
		$(this).animateLayer(layer, {
		    strokeWidth: 2,
		}, 125);
	    },
	});
    });
}

String.prototype.trunc = String.prototype.trunc ||
      function(n){
          return (this.length > n) ? this.substr(0,n-1)+'...' : this;
      };

function showAnnotationInfo (obj) {
    if (obj == null) {
	$('#standoffInfobox').html("");
    } else {
	console.log(obj.tag);
	switch (obj.tag) {
	case 'Relation':
	    $('#standoffInfobox').html(
		"<h3>Relation:</h3><dl>" +
		    "<dt>ID:</dt><dd class='standoff-uuid'>" + obj.relationId + "</dd>" +
		    "<dt>Subject:</dt>" +
		    "<dd class='standoff-uuid'>" + obj.subject + "</dd>" +
		    "<dd>" + $("[eid|='"+obj.subject+"']").text().trunc(100) + "</dd>" +
		    "<dt>Predicate:</dt>" +
		    "<dd class='standoff-iri'>" + obj.predicate + "</dd>" +
		    "<dt>Object:</dt>" +
		    "<dd class='standoff-uuid'>" + obj.object + "</dd>" +
		    "<dd>" + $("[eid|='"+obj.object+"']").text().trunc(100) + "</dd>" +
		    "</dl>"
	    );
	    break;
	case 'MarkupRange':
	    $('#standoffInfobox').html("<h3>Markup Element</h3>");
	    break;
	default:
	    $('#standoffInfobox').html("");
	};
    }
}
