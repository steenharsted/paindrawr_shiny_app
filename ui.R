# ---- UI ----
ui <- page_fillable(

  theme = bs_theme(
    bootswatch = "flatly",  # Or try: "litera", "materia", "pulse"
  ),

  titlePanel("Tegn dine smerter så præcist som muligt"),
  textInput("id", "Indtast ID:", value = "Anon"),


  layout_sidebar(
    layout = "sidebar-left",
    fillable = FALSE,
    fill = FALSE, 
    sidebar = sidebar(
      title = "Indstillinger",
      radioButtons("tool", "Tegn med:",
        choices = c("Spray"="spray","Pen"="pen"),
        selected = "spray", inline = TRUE
      ),
      sliderInput("lineWidth", "Streg/punkt bredde (px):", 1, 20, 1),
      conditionalPanel("input.tool == 'spray'",
        sliderInput("sprayRadius", "Spray radius (px):", 1, 30, 5),
        sliderInput("sprayDensity", "Spray tæthed:", 1, 10, 5)
      ),
      sliderInput("alpha", "Gennemsigtighed:", 0, 100, 50, post = " %", step = 1),
      shinyWidgets::colorPickr("lineColor", "Farve:", selected = "#ff0000")
    ),

    # Wrapper that controls width and centering of the user panel
    div(
      style = glue::glue("
        width:{config$canvas_width}px;
        margin:0 auto;
        display:flex;
        flex-direction:column;
        gap:8px;
      "),

      # top toolbar
      div(style = "display:flex; justify-content:flex-end;",
          actionButton("reset", "Genstart")
      ),

      # canvas
      div(style = "display:flex; justify-content:center;",
        tags$canvas(
          id = "drawingCanvas", 
          width = config$canvas_width, 
          height = config$canvas_height,
          style = "border:1px solid #000000; touch-action:none; overscroll-behavior:contain;"
        )
      ),

      # bottom toolbar
      div(style = "display:flex; justify-content:space-between; gap:8px; flex-wrap:wrap;",
          actionButton("save", "Save"),
          downloadButton("downloadData", "Download")
      )
    )

  ),
  

  
  tags$script(HTML("
    let canvas = document.getElementById('drawingCanvas');
    let ctx = canvas.getContext('2d');
    let drawing = false;
    let coordinates = [];         // {x,y,strokeId}
    let strokeSettings = [];      // per stroke
    let currentStrokeId = 0;
    let tool = 'spray';
    let sprayDensity = 20, sprayRadius = 10, lineWidth = 1, lineColor = '#ff0000', alpha = 1.0;
    let backgroundLoaded = false;
    let lastX = 0, lastY = 0;     // for pen
    canvas.style.touchAction = 'none'; // iOS/Android: disable panning/zoom on this element

    function loadBackgroundImage(url) {
      let img = new Image();
      img.onload = function() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
        backgroundLoaded = true;
      };
      img.onerror = function() { backgroundLoaded = true; };
      img.src = url;
    }
    loadBackgroundImage('feet.png');

    Shiny.addCustomMessageHandler('updateSettings', function(s) {
      tool = s.tool;
      lineWidth = s.lineWidth;
      lineColor = s.lineColor;
      sprayDensity = s.sprayDensity;
      sprayRadius = s.sprayRadius;
      alpha = s.alpha;

      ctx.lineWidth = lineWidth;
      ctx.strokeStyle = lineColor;
      ctx.fillStyle = lineColor;
      ctx.globalAlpha = alpha;
      ctx.lineCap = 'round';
      ctx.lineJoin = 'round';
      ctx.globalCompositeOperation = 'source-over';
    });

    canvas.addEventListener('pointerdown', function (e) {
      if (!backgroundLoaded) return;
      drawing = true;
      currentStrokeId++;
      canvas.setPointerCapture(e.pointerId);

      strokeSettings.push({
        strokeId: currentStrokeId,
        tool, lineWidth, sprayDensity, sprayRadius, lineColor, alpha,
        canvasWidth: canvas.width, canvasHeight: canvas.height
      });

      const x = e.offsetX, y = e.offsetY;
      if (tool === 'spray') {
        doSpray(x, y);
      } else {
        ctx.beginPath();
        ctx.moveTo(x, y);
        lastX = x; lastY = y;
        doPen(x, y);
      }
    });

    canvas.addEventListener('pointermove', function (e) {
      if (!drawing || !backgroundLoaded) return;
      const x = e.offsetX, y = e.offsetY;
      if (tool === 'spray') {
        doSpray(x, y);
      } else {
        doPen(x, y);
      }
    });

    canvas.addEventListener('pointerup', function (e) {
      if (!drawing) return;
      drawing = false;
      if (tool === 'pen') ctx.closePath();

      Shiny.setInputValue('stroke_coordinates', JSON.stringify(coordinates));
      Shiny.setInputValue('stroke_settings', JSON.stringify(strokeSettings));
      coordinates = [];
      strokeSettings = [];
    });

    function doSpray(x, y) {
      for (let i = 0; i < sprayDensity; i++) {
        let angle = Math.random() * 2 * Math.PI;
        let radius = Math.random() * sprayRadius;
        let sx = x + radius * Math.cos(angle);
        let sy = y + radius * Math.sin(angle);
        ctx.globalAlpha = alpha;
        ctx.fillRect(sx, sy, lineWidth, lineWidth); // squares, top-left coords
      }
      coordinates.push({ x: x, y: y, strokeId: currentStrokeId });
    }

    function doPen(x, y) {
      ctx.globalAlpha = alpha;
      ctx.lineWidth = lineWidth;
      ctx.strokeStyle = lineColor;
      ctx.lineTo(x, y);
      ctx.stroke();
      coordinates.push({ x: x, y: y, strokeId: currentStrokeId });
      lastX = x; lastY = y;
    }

    Shiny.addCustomMessageHandler('resetCanvas', function(_) {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      coordinates = []; strokeSettings = []; currentStrokeId = 0;
      if (backgroundLoaded) loadBackgroundImage('feet.png');
    });

    document.getElementById('save').addEventListener('click', function() {
      let dataURL = canvas.toDataURL('image/png');
      Shiny.setInputValue('canvas_image', dataURL);
      Shiny.setInputValue('save_trigger', Math.random());
    });

    console.log('binding resetCanvas handler');
    Shiny.addCustomMessageHandler('resetCanvas', function(_) {
      console.log('resetCanvas received @', new Date().toISOString());
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      coordinates = []; strokeSettings = []; currentStrokeId = 0;
      if (backgroundLoaded) loadBackgroundImage('feet.png');
    });

    // Optional: detect canvas re-creation (another reset cause)
    const orig = canvas;
    const checkReplaced = () => {
      const now = document.getElementById('drawingCanvas');
      if (now !== orig) console.warn('canvas element was replaced');
    };
    setInterval(checkReplaced, 1000);

    const CAN0 = document.getElementById('drawingCanvas');
    new MutationObserver(() => {
      const now = document.getElementById('drawingCanvas');
      if (now !== CAN0) console.warn('canvas REPLACED');
    }).observe(document.body, {childList: true, subtree: true});

    new MutationObserver(muts => {
      muts.forEach(m => console.warn('canvas attr changed:', m.attributeName,
                                    '→', canvas[m.attributeName]));
    }).observe(canvas, {attributes: true, attributeFilter: ['width','height']});

    document.getElementById('save').addEventListener('click', function(e){
      console.log('SAVE click @', new Date().toISOString(),
                  'w,h=', canvas.width, canvas.height);
    });



  "))
)
