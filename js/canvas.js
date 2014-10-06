function js_newCanvas(width, height) {
  var c = document.createElement('canvas');
  c.width = width;
  c.height = height;
  document.getElementsByTagName('body')[0].appendChild(c);
  return c;
}

function js_blackRect(x, y, w, h, ctx) {
  ctx.fillStyle = "black";
  ctx.fillRect(x, y, w, h);
}
