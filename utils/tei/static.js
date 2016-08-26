function drawStatic (canvasId, wrapperId) {
    stretchCanvas(canvasId, wrapperId);
    $.getJSON("relations.json", function (data) {
	console.log("Relations file found.");
	console.log(data);
	drawRelations(canvasId, data);
    });
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
