
exports.svgline = function(svg){
  return function (x1){
  return function(y1){
  return function(x2){
  return function(y2){
  return function(stroke){
  return function(strokeWidth){
  return function(){
    let l = document.createElementNS("http://www.w3.org/2000/svg","line");
    l.setAttributeNS(null,"x1",x1);
    l.setAttributeNS(null,"y1",y1);
    l.setAttributeNS(null,"x2",x2);
    l.setAttributeNS(null,"y2",y2);
    l.setAttributeNS(null,"style", "stroke:"+stroke+"; stroke-width:"+strokeWidth+"px;");
    svg.appendChild(l);
    };};};};};};};
 };

exports.svgtext = function(svg){
  return function (x){
  return function (y){
  return function (fill){
  return function (fontStyle){
  return function (text){
  return function (){
    let t = document.createElementNS("http://www.w3.org/2000/svg","text");
    t.setAttributeNS(null,"x",x);
    t.setAttributeNS(null,"y",y);
    t.setAttributeNS(null,"style","fill:"+fill+"; font:"+fontStyle+";");
    t.textContent = text;
    svg.appendChild(t);
    };};};};};};
};

exports.svgpath = function(svg){
  return function(stroke){
  return function(strokeWidth){
  return function(fill){
  return function(path){
  return function(){
    let p = document.createElementNS("http://www.w3.org/2000/svg","path");
    p.setAttributeNS(null,"d",path);
    p.setAttributeNS(null,"style","stroke:"+stroke+"; stroke-width:"+strokeWidth+"; fill:"+fill+";");
    svg.appendChild(p);
    };};};};};
};

