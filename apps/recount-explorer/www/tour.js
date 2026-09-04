// The guided tour.
//
// Every step is anchored to chrome that is always present: the navbar tabs,
// the search box, the filters. Nothing here depends on a study being loaded,
// so the tour reads the same whether the visitor starts from an empty
// Browse view or comes back to it later.
document.addEventListener("DOMContentLoaded", function () {
  var button = document.getElementById("take_tour");
  if (!button || !window.driver || !window.driver.js) {
    return;
  }

  var driver = window.driver.js.driver;

  button.addEventListener("click", function () {
    // Kept on window so the tour can be driven programmatically, both for
    // testing and for anyone who wants to script it from the console.
    // drive() starts the tour but does not return the instance, so the
    // reference has to be captured before calling it.
    var tour = driver({
      showProgress: true,
      allowClose: true,
      steps: [
        {
          element: "#browser-q",
          popover: {
            title: "Search the catalog",
            description:
              "Search all 18,998 recount3 studies by accession, title, or abstract.",
          },
        },
        {
          element: "#browser-filters",
          popover: {
            title: "Filter",
            description:
              "Narrow by organism, data source, sample count, and download size.",
          },
        },
        {
          element: "#browser-catalog",
          popover: {
            title: "Browse and load",
            description:
              "Click a study to see its details on the right, then load it to explore.",
          },
        },
        {
          element: "a[data-value='Overview']",
          popover: {
            title: "Overview",
            description:
              "Headline numbers, a library size against detected genes plot, and the full sample metadata.",
          },
        },
        {
          element: "a[data-value='Quality']",
          popover: {
            title: "Quality",
            description:
              "Alignment metrics, library composition, donor sex, and sample correlation — all computed by recount3 already, plotted for the first time here.",
          },
        },
        {
          element: "a[data-value='Genes']",
          popover: {
            title: "Genes",
            description:
              "Search for one gene and plot its expression, split by any sample metadata column.",
          },
        },
        {
          element: "a[data-value='PCA']",
          popover: {
            title: "PCA",
            description:
              "See how samples separate, and which genes drive each principal component.",
          },
        },
        {
          element: "a[data-value='Export']",
          popover: {
            title: "Export",
            description:
              "Download the data, or a script that reproduces this exact session.",
          },
        },
        {
          element: "#dark_mode",
          popover: {
            title: "Light or dark",
            description: "Switch modes here. The plots follow along.",
          },
        },
      ],
    });
    window.__recountTour = tour;
    tour.drive();
  });
});
