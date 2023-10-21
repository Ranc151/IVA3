library(shiny)

ui <- fluidPage(
    tags$head(
        tags$style(HTML(".tab-content { display: visible; overflow: hidden; position: relative; }"))
    ),
    navbarPage(
        "Navbar Page Animation",
        tabPanel(
            "Tab 1",
            h2("Content in Tab 1"),
            actionButton("switchTab", "Switch to Tab 2")
        ),
        tabPanel(
            "Tab 2",
            h2("Content in Tab 2"),
            actionButton("switchTab", "Switch to Tab 1")
        )
    ),
    tags$script(HTML('
    $(document).on("shiny:connected", function() {
      var tabWidth = $(".tab-content").width();

      $("#switchTab").click(function() {
        var currentTab = $(".tab-content:visible");
        var nextTab = currentTab.siblings().first();

        currentTab.animate({left: -tabWidth}, 300, function() {
          currentTab.hide();
          nextTab.css({left: 0});
          nextTab.show();
          nextTab.animate({left: 0}, 300);
        });
      });
    });
  '))
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
