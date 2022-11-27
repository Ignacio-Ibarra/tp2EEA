library(shiny)
library(plotly)
library(htmlwidgets)


ui <- fluidPage(
  plotlyOutput("graph")
)

server <- function(input, output, session) {
  #N <- 100
  # x <- rnorm(N, mean = 50, sd = 2.3)
  # y <- runif(N, min = 0, max = 100)
  # z <- runif(N, min = 4, max = 70)
  # luci.frame <- data.frame(x, y, z)
  #Vamos a crear un dataset sintético y graficarlo en 3D
  set.seed(911)
  
  n = 100
  
  dtrain <- data.frame(x = runif(n,4.5,13.5),y = runif(n,4.5,13.5))
  noise <- rnorm(n, mean=0, sd=0.5)
  dtrain <- dtrain %>% mutate(z = sqrt((x-9)**2+(y-9)**2)+noise)
  # luci.frame <- copy(dtrain)
  
  output$graph <- renderPlotly({
    plot_ly(
      type = "scatter3d",
      mode = "markers",
      data = dtrain,
      color = "orange",
      x = ~ x,
      y = ~ y,
      z = ~ z) %>%
      #add_markers(size = 1,color = I("orange")) %>% 
      layout(scene = list(camera = list(
        eye = list(
          x = 1.05,
          y = 1.05,
          z = 1.25
        ),
        center = list(x = 0,
                      y = 0,
                      z = 0)
      ))) %>%
      onRender("
      function(el, x){
  var id = el.getAttribute('id');
  var gd = document.getElementById(id);
  Plotly.update(id).then(attach);
  function attach() {
    var cnt = 0;
    
    function run() {
      rotate('scene', Math.PI / 180);
      requestAnimationFrame(run);
    } 
    run();
    
    function rotate(id, angle) {
      var eye0 = gd.layout[id].camera.eye
      var rtz = xyz2rtz(eye0);
      rtz.t += angle;
      
      var eye1 = rtz2xyz(rtz);
      Plotly.relayout(gd, id + '.camera.eye', eye1)
    }
    
    function xyz2rtz(xyz) {
      return {
        r: Math.sqrt(xyz.x * xyz.x + xyz.y * xyz.y),
        t: Math.atan2(xyz.y, xyz.x),
        z: xyz.z
      };
    }
    
    function rtz2xyz(rtz) {
      return {
        x: rtz.r * Math.cos(rtz.t),
        y: rtz.r * Math.sin(rtz.t),
        z: rtz.z
      };
    }
  };
}
    ")
  })
}

shinyApp(ui, server)
