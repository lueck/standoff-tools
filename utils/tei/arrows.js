var barb_length = 10;
var barb_angle = degreeToRadian(20);

var drawRelation = drawArcRelation;

function drawStatic (canvasId, wrapperId) {
    stretchCanvas(canvasId, wrapperId);
    var canvas = document.getElementById(canvasId);
    var canvasOffset = $("#canvas").offset();
    if (canvas.getContext) {
	var ctx = canvas.getContext("2d");
	ctx.globalCompositeOperation='destination-over';
	drawFoo(ctx);
	ctx.fillStyle = "rgb(200,0,0)";
	drawArrowHead(ctx, 200, 200, 0.4*Math.PI, true);
	$.getJSON("relations.json", function (data) {
	    console.log("Relations file found.");
	    console.log(data);
	    drawRelations(ctx, canvasOffset.left, canvasOffset.top, data);
	});
    } else {
	alert("The browser can't draw relations because it does not support the <canvas> element!");
    }
}

function stretchCanvas (canvasId, wrapperId) {
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
    console.log("Stretching canvas (width, height, pixels): " + bodyWidth + " " + bodyHeight + " " + bodyPixels, Math.min(bodyHeight, maxCanvasHeight)); 
    canvas.width = bodyWidth;
    canvas.height = Math.min(bodyHeight, maxCanvasHeight);
}

function drawFoo (context) {
    context.fillStyle = "rgb(200,0,0)";
    context.fillRect(10, 10, 55, 50);
}

function degreeToRadian (degree) {
    return (degree / 360) * 2 * 3.1454;
}

function drawRelations (ctx, dx, dy, data) {
    //ctx.lineWidth=10;
    console.log(ctx);
    var relStyle = "rgb(200,0,0)"; // FIXME;
    ctx.strokeStyle = relStyle;
    ctx.fillStyle = relStyle;
    $.each(data, function (count, rel) {
	console.log("Drawing relation " + rel.relationId + ": " + rel.subject + " " + rel.predicate + " " + rel.object);
	//drawRelation(ctx, dx, dy, rel.relationId, rel.subject, rel.predicate, rel.object);
	drawRelation(ctx, dx, dy, rel.relationId, rel.subject, rel.predicate, rel.object);
    });
}

// Draw a straight line relation
function drawStraightRelation (ctx, dx, dy, relationId, sub, pred, obj) {
    var subOffset = $("[eid|='"+sub+"']").first().offset();
    var objOffset = $("[eid|='"+obj+"']").first().offset();
    var x1 = Math.ceil(subOffset.left) - dx;
    var y1 = Math.ceil(subOffset.top) - dy;
    var x2 = Math.ceil(objOffset.left) - dx;
    var y2 = Math.ceil(objOffset.top) - dy;
    console.log(x1, y1, x2, y2);
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.lineTo(x2, y2);
    ctx.stroke();
    //ctx.arc(x1, y1, 4, 0, 2*Math.PI, false);
    //ctx.fill();
    drawArrowHead(ctx, x2, y2, Math.atan2(y1-y2, x1-x2), true);
}

function drawArrowHead (ctx, x, y, angle, filled) {
    console.log("Drawing arrow head at (" + x + "," + y + ") with angle " + angle);
    (filled==undefined) && (filled=true);
    var a_angle = angle - barb_angle,
	a_x = x + (barb_length * Math.cos(a_angle)),
	a_y = y + (barb_length * Math.sin(a_angle)),
	b_angle = angle + barb_angle,
	b_x = x + (barb_length * Math.cos(b_angle)),
	b_y = y + (barb_length * Math.sin(b_angle));
    ctx.beginPath();
    ctx.moveTo(a_x, a_y);
    ctx.lineTo(x, y);
    ctx.lineTo(b_x, b_y);
    ctx.closePath()
    if (filled) {
	ctx.fill();
    } else {
	ctx.stroke();
    }
    //ctx.arc(x, y, 4, 0, 2*Math.PI, false);
    //ctx.fill();
}

// draw an arc line relation
function drawArcRelation (ctx, dx, dy, relationId, sub, pred, obj) {
    // start a, end b
    var subOffset = $("[eid|='"+sub+"']").first().offset(),
	objOffset = $("[eid|='"+obj+"']").first().offset(),
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
	cx = (0.5 * (ax + bx)) + (r/q * 0.5 * (by - ay) / ls),
	cy = (0.5 * (ay + by)) + (r/q * 0.5 * (ax - bx) / ls),
	// start angle sa, end angle ea
	sAngle = -0.5*Math.PI - Math.atan2((cx - ax), (cy - ay)),
	eAngle = -0.5*Math.PI - Math.atan2((cx - bx), (cy - by));
    console.log("Drawing Bow, r=" + r + " cx=" + cx + " cy=" + cy);
    ctx.beginPath();
    ctx.arc (cx, cy, r, sAngle, eAngle, true);
    ctx.stroke();
    drawArrowHead(ctx, bx, by, Math.atan2(ay-by, ax-bx)-(0.13*Math.PI), true);
}

// draw an arc line relation
function drawNoRelation (ctx, dx, dy, relationId, sub, pred, obj) {}
