function js_newCanvas(width, height) {
  var c = document.createElement('canvas');
  c.width = width;
  c.height = height;
  document.getElementsByTagName('body')[0].appendChild(c);
  return c;
}

function js_makeRGBA(r, g, b, a) {
  return 'rgba(' + r + ', ' + g + ', ' + b + ',' + a + ')';
}

function js_drawRect(x, y, w, h, r, g, b, a, ctx) {
  ctx.fillStyle = js_makeRGBA(r, g, b, a);
  ctx.fillRect(x, y, w, h);
}

function js_drawCircle(x, y, rad, r, g, b, a, ctx) {
  ctx.beginPath();
  ctx.arc(x, y, rad, 0, 2 * Math.PI, false);
  ctx.fillStyle = js_makeRGBA(r, g, b, a);
  ctx.fill();
}

function js_drawPolygon(points, r, g, b, a, ctx) {
  if (points.length < 2) return;
  ctx.beginPath();
  ctx.moveTo(points[0][0], points[0][1]);
  for (var i = 1; i < points.length; i++) {
    ctx.lineTo(points[i][0], points[i][1]);
  }
  ctx.fillStyle = js_makeRGBA(r, g, b, a);
  ctx.fill();
}
