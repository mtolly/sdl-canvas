function js_newCanvas(width, height) {
  var c = document.createElement('canvas');
  c.width = width;
  c.height = height;
  document.getElementsByTagName('body')[0].appendChild(c);
  return c;
}

function js_drawRect(x, y, w, h, r, g, b, a, ctx) {
  ctx.fillStyle = 'rgba(' + r + ', ' + g + ', ' + b + ',' + a + ')';
  ctx.fillRect(x, y, w, h);
}
