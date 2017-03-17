function main()
{
  var canvas = document.getElementById('graph-canvas')
  var context = canvas.getContext('2d')
  var em = context.measureText('M').width
  
  var tab_width = canvas.width / graph_data.length
  
  context.textBaseline = 'top'
  context.shadowOffsetX = 1
  context.shadowOffsetY = 1
  context.shadowBlur = 1
  for (var i=0; i < graph_data.length; i++) {
    var tab = graph_data[i];
    context.fillStyle = 'rgba(0,0,200,0.5)'
    context.shadowColor = 'rgba(0,0,0,0.75)'
    context.textAlign = 'center'
    context.fillText(tab.name,tab_width / 2 * (2*i+1),0,tab_width)
    
    context.textAlign = 'start'
    for(var j=0; j < tab.bars.length; j++) {
      var bar = tab.bars[j];
      var time = Date.now() - Date.parse(bar.start)
      time = time/1000/bar.growth_seconds
      var money = time*bar.growth_amount
      var length = money/10 * canvas.width
      context.fillStyle = 'rgba(200,0,200,0.5)'
      context.shadowColor = 'rgba(0,0,0,0)'
      context.fillText(bar.name,em,em * (j+1) * 3)
      context.fillStyle = 'rgba(0,200,200,0.5)'
      context.shadowColor = 'rgba(0,0,0,0.5)'
      context.fillRect(em,em * ((j+1)*3 + 1.5),length,em)
    }
  }
}