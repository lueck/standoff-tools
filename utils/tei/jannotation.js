$.jCanvas.extend({
    name: 'drawQuadraticRelation',
    type: 'quadraticRelation',
    props: {
	id: '',
	subject: '',
	predicate: '',
	object: '',
	dx: 0,
	dy: 0,
	layer: true,
	mouseover: function(layer) {
	    $(this).animateLayer(layer, {
		strokeWidth: 4,
	    });
	},
	mouseout: function(layer) {
	    $(this).animateLayer(layer, {
		strokeWidth: 2,
	    });
	},
    },
    fn: function (ctx, p) {
	// enable layer transformations
	$.jCanvas.transformShape(this, ctx, p);
	// get start point a and end point b
	var subOffset = $("[eid|='"+p.subject+"']").first().offset(),
	    objOffset = $("[eid|='"+p.object+"']").first().offset(),
	    ax = subOffset.left - p.dx,
	    ay = subOffset.top - p.dy,
	    bx = objOffset.left - p.dx,
	    by = objOffset.top - p.dy,
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
	$(this).draw({
	    type: "quadratic",
	    strokeStyle: '#000',
	    strokeWidth: 2,
	    rounded: true,
	    endArrow: true,
	    arrowRadius: 7,
	    arrowAngle: 60,
	    x1: ax, y1: ay,
	    cx1: cx, cy1: cy,
	    x2: bx, y2: by,
	    layer: p.layer,
	    mouseover: p.mouseover,
	    mouseout: p.mouseout,
	});
	// enable jCanvas events
	$.jCanvas.detectEvents(this, ctx, p);
    }
});

function drawRelations (canvasId, relations) {
    var canvas = $('#'+canvasId),
	dx = canvas.offset().left,
	dy = canvas.offset().top;
    $.each(relations, function (count, rel) {
	console.log("Drawing relation " + rel.relationId + ": " + rel.subject + " " + rel.predicate + " " + rel.object);
	var subOffset = $("[eid|='"+rel.subject+"']").first().offset(),
	    objOffset = $("[eid|='"+rel.object+"']").first().offset(),
	    ax = subOffset.left - dx,
	    ay = subOffset.top - dy,
	    bx = objOffset.left - dx,
	    by = objOffset.top - dy,
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
	console.log("start: (" + ax + ", " + ay + ") end: (" + bx + ", " + by + ")" );
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
		    "<dd>" + $("[eid|='"+obj.subject+"']").html() + "</dd>" +
		    "<dt>Predicate:</dt>" +
		    "<dd class='standoff-iri'>" + obj.predicate + "</dd>" +
		    "<dt>Object:</dt>" +
		    "<dd class='standoff-uuid'>" + obj.object + "</dd>" +
		    "<dd>" + $("[eid|='"+obj.object+"']").html() + "</dd>" +
		    "</dl>"
	    );
	    break;
	default:
	    $('#standoffInfobox').html("");
	};
    }
}
	 
