# About tab -- the "what is Tahoe-100M?" explainer, second in the navbar.

register_page(
  id = "about",
  title = "About",
  order = 70,
  ui = div(class = "p-2", about_ui("about")),
  server = function() about_server("about")
)
