exports.body = function(){
      return document.body;
};

exports.newSVG = function(node){
  return function(){
    let svgnode = document.createElementNS("http://www.w3.org/2000/svg","svg");
    svgnode.setAttribute("style","position: absolute; width:100%; height:100%; ");
    node.appendChild(svgnode);
    return svgnode};
 };
